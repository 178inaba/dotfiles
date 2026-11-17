// Package plandocs reads what a plan is drafted against and checks a drafted
// plan against the repository: Collect answers with the instruction files the
// harness loaded at launch and the path-scoped rules a task's paths bring in,
// and Check reads the plan's paths, names and recorded commands.
package plandocs

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/bmatcuk/doublestar/v4"

	"github.com/178inaba/dotfiles/go/internal/frontmatter"
)

// importHops is how far Claude Code expands @ imports from a memory file:
// "Imported files can recursively import other files, with a maximum depth of
// four hops". A file at the limit is loaded; the imports written in it are
// not, and so they are listed nowhere.
const importHops = 4

// Collection is what the harness already read for a planner, and the rules it
// has still to read.
type Collection struct {
	// The files Claude Code loaded at launch, absolute: the project
	// instruction files, everything their @ imports reach, and the project
	// rules that carry no paths field. The user's own unscoped rules are not
	// here: they belong to the user rather than to the project. A non-empty
	// list names what is already in context and must not be read again.
	// Nothing to read is this and documents both empty, and neither list says
	// it alone: with a given path a scoped rule is to read in a repository
	// that states no instructions at all, and without one an instruction file
	// leaves this one full and documents empty.
	Loaded []string `json:"loaded"`
	// The path-scoped rules at least one given path matches, absolute and
	// sorted by path; empty when no path is given. What loaded already holds
	// does not appear again, and a rule found under two spellings of the same
	// file is one entry.
	Documents []string `json:"documents"`
	// An @ import in the launch closure whose target is not there. Reported
	// rather than raised: one broken import is a file to fix, not a reason to
	// collect nothing.
	Warnings []Warning `json:"warnings"`
}

// Warning is an import that leads nowhere.
type Warning struct {
	// The path as the import writes it, minus the @ and any fragment, so that
	// it can be found in the file named below and corrected there.
	Target string `json:"target"`
	// The file the import is written in, absolute.
	Source string `json:"source"`
}

// roots are the instruction files Claude Code loads from a directory, in the
// order it loads them.
var roots = []string{"CLAUDE.md", filepath.Join(".claude", "CLAUDE.md"), "CLAUDE.local.md"}

// Collect replays what the harness loaded at launch and answers with it and
// the path-scoped rules the given paths match.
//
// A Markdown link, in any file, is no reference here and is never followed,
// and neither is a path in backticks: the harness gives neither a meaning, so
// how a document refers to another decides nothing in the answer.
//
// dir is where the command was run, which may be anywhere inside the
// repository: Claude Code loads instruction files from the working directory
// and every directory above it, so a session started in a subdirectory has
// the repository's own CLAUDE.md in context and loaded has to say so.
//
// home resolves the @~/ form of an import and is where the user's own
// .claude/rules/ is looked for. It is a parameter because a test has a fixture
// home and no business reading the real one.
//
// paths are the files a task touches, and they are subjects for pattern
// matching rather than files to open: a relative one is read from the top of
// the repository, an absolute one as it is, and neither has to exist. Given
// any, the answer also holds every path-scoped rule whose patterns match one
// of them — the rules the harness would load for those files, which is the
// input a caller reading them through the shell never gets.
//
// Nothing about the repository is an error: no instruction file at all, a
// scoped rule no path matches, an import of a file that was deleted — each is
// an ordinary answer. Only a filesystem that cannot be read is returned as one.
func Collect(dir, home string, paths ...string) (Collection, error) {
	c := collector{
		home:   home,
		seen:   map[string]bool{},
		warned: map[Warning]bool{},
	}

	dirs := directories(dir)
	for _, at := range dirs {
		for _, name := range roots {
			path := filepath.Join(at, name)
			if isFile(path) {
				if err := c.expand(path); err != nil {
					return Collection{}, err
				}
			}
		}
	}

	// The top of the repository is the first of the walk, and the basis every
	// relative path given is read from.
	top := dirs[0]
	absolute := make([]string, 0, len(paths))
	for _, path := range paths {
		if !filepath.IsAbs(path) {
			path = filepath.Join(top, path)
		}
		absolute = append(absolute, path)
	}

	scoped := map[string][]rule{}
	for _, at := range dirs {
		found, err := rulesIn(filepath.Join(at, ".claude", "rules"))
		if err != nil {
			return Collection{}, err
		}
		for _, r := range found {
			if r.scoped {
				scoped[at] = append(scoped[at], r)
				continue
			}
			c.load(r.path)
		}
	}

	// Only the user's scoped rules are taken: a scoped rule is a constraint the
	// harness loads for the project's files, while the unscoped ones are the
	// user's memory and no part of what the project states. They
	// are keyed at the top of the repository, and that basis is the one
	// deliberate difference from the harness — it matches them against the
	// directory the session was started in, so a session started in a
	// subdirectory loads fewer rules than govern the file it opens, and that
	// narrowing is a property of where somebody stood rather than of the
	// rule's scope. Keyed there rather than under the home directory also puts
	// their spelling ahead of any deeper project directory's when the two
	// reach one file, which is how this repository's stow symlink resolves.
	//
	// Read at all only when there is a path to match, since nothing else here
	// looks at the user's rules.
	if len(absolute) > 0 {
		userRules, err := rulesIn(filepath.Join(home, ".claude", "rules"))
		if err != nil {
			return Collection{}, err
		}
		for _, r := range userRules {
			if r.scoped {
				scoped[top] = append(scoped[top], r)
			}
		}
	}

	c.out.Documents = c.matched(dirs, scoped, absolute)
	return c.out, nil
}

// directories are the ones whose instruction files the harness already has in
// context, ordered as it loads them: the top of the repository first and the
// working directory last, so that what was read closest to the plan is read
// closest to the plan here too.
//
// The top is found by the .git entry rather than by asking git, which keeps
// this a walk of the filesystem and nothing else — .git is a directory in a
// checkout and a file in a linked worktree, and either one stops the walk.
// Above it is somebody else's project, and outside a repository there is
// nothing to walk up to.
func directories(dir string) []string {
	var out []string
	for at := dir; ; {
		out = append(out, at)
		if hasGitEntry(at) {
			slices.Reverse(out)
			return out
		}
		parent := filepath.Dir(at)
		if parent == at {
			return []string{dir}
		}
		at = parent
	}
}

// collector carries the answer being assembled and the sets that keep it from
// repeating itself.
type collector struct {
	home   string
	out    Collection
	seen   map[string]bool
	warned map[Warning]bool
}

// load records a file as one the harness already has in context.
func (c *collector) load(path string) {
	if c.seen[path] {
		return
	}
	c.seen[path] = true
	c.out.Loaded = append(c.out.Loaded, path)
}

// expand adds a root and everything its imports reach to the loaded set.
//
// Only imports are followed here: this is a replay of what the harness did at
// launch, and the harness expands nothing else. A rules file is not a root of
// its own closure, because the harness does not expand the imports written in
// one. Measured, since the documentation says nothing either way: a project
// holding a CLAUDE.md importing @y.md, an unscoped .claude/rules/r.md
// importing @x.md, and an InstructionsLoaded hook logging what loads, reports
// CLAUDE.md, y.md and r.md — and not x.md. So a rule's import is listed
// nowhere.
func (c *collector) expand(root string) error {
	type step struct {
		path string
		hop  int
	}
	queue := []step{{path: root}}
	for len(queue) > 0 {
		at := queue[0]
		queue = queue[1:]
		if c.seen[at.path] {
			continue
		}
		c.load(at.path)
		if at.hop == importHops {
			continue
		}

		imported, err := c.targets(at.path)
		if err != nil {
			return err
		}
		for _, path := range imported {
			queue = append(queue, step{path: path, hop: at.hop + 1})
		}
	}
	return nil
}

// targets resolves the imports written in file, dropping the ones that name no
// file at all and warning about the ones whose file is not there.
//
// A path already answered for is dropped rather than stated again: everything
// in seen was found to exist when it was added, so it can neither warn now
// nor be loaded a second time.
func (c *collector) targets(file string) ([]string, error) {
	b, err := os.ReadFile(file)
	if err != nil {
		return nil, err
	}

	var out []string
	for _, written := range imports(string(b)) {
		target, ok := resolve(written, file, c.home)
		switch {
		case !ok, c.seen[target]:
		case !isFile(target):
			c.warn(written, file)
		default:
			out = append(out, target)
		}
	}
	return out, nil
}

// warn records a broken import once per place it is written, so that the same
// missing file imported from two files is two things to fix and the same
// import written twice in one file is one.
func (c *collector) warn(target, source string) {
	w := Warning{Target: target, Source: source}
	if c.warned[w] {
		return
	}
	c.warned[w] = true
	c.out.Warnings = append(c.out.Warnings, w)
}

// matched answers with the scoped rules the given absolute paths reach: for
// each directory holding a .claude/rules/, the rules under it whose patterns
// match a given path made relative to that directory. The basis is per
// directory because that is what the harness matches a project rule against,
// and a path outside a basis matches nothing there.
//
// A rule already answered for is not listed again, by identity rather than by
// spelling: this repository's own rules are reached both as
// <repo>/claude/.claude/rules/x.md and as ~/.claude/rules/x.md through a stow
// symlink, and they are one file to read. The spelling that survives is the
// first one reached, which is why the bases are taken top first — the user's
// rules are keyed at the top, so their spelling wins over a deeper project
// directory's and the answer stays the same from wherever the command was
// run.
func (c *collector) matched(dirs []string, scoped map[string][]rule, paths []string) []string {
	if len(paths) == 0 {
		return nil
	}

	// Seeded with the loaded set, since a rule an instruction file imports is
	// in it whatever its frontmatter says, and a given path can match that
	// rule under another spelling.
	listed := make([]os.FileInfo, 0, len(c.out.Loaded))
	for _, path := range c.out.Loaded {
		if info, err := os.Stat(path); err == nil {
			listed = append(listed, info)
		}
	}

	var out []string
	for _, basis := range dirs {
		for _, r := range scoped[basis] {
			if !matchesAny(r.patterns, basis, paths) {
				continue
			}
			info, err := os.Stat(r.path)
			if err != nil {
				continue
			}
			if slices.ContainsFunc(listed, func(l os.FileInfo) bool { return os.SameFile(l, info) }) {
				continue
			}
			listed = append(listed, info)
			out = append(out, r.path)
		}
	}
	slices.Sort(out)
	return out
}

// matchesAny reports whether any pattern matches any of the paths, each made
// relative to the basis.
//
// Each pattern goes through anchored first, which gives it the meaning the
// documentation's table does. A pattern that is not a valid glob matches
// nothing and does not fail the run, as the documentation says of one the
// harness cannot read. The one documented difference in the syntax is the
// brace-expansion budget, which is not reproduced here.
func matchesAny(patterns []string, basis string, paths []string) bool {
	for _, path := range paths {
		rel, err := filepath.Rel(basis, path)
		if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
			continue
		}
		for _, pattern := range patterns {
			if ok, err := doublestar.Match(anchored(pattern), filepath.ToSlash(rel)); err == nil && ok {
				return true
			}
		}
	}
	return false
}

// anchored turns a pattern as the documentation writes it into one doublestar
// matches the same way against a path relative to the basis. A leading slash
// anchors the pattern to the basis, which a whole-path match already does once
// the slash is gone. A pattern with no slash at all matches at any depth, which
// a leading **/ gives it, since **/ matches zero directories too. Anything
// else already means what doublestar takes it to.
//
// The slash is looked for in the pattern as written, before doublestar expands
// any brace group in it.
func anchored(pattern string) string {
	if rest, ok := strings.CutPrefix(pattern, "/"); ok {
		return rest
	}
	if !strings.Contains(pattern, "/") {
		return "**/" + pattern
	}
	return pattern
}

// rulesIn lists the rules under a .claude/rules/, recursively, saying of each
// whether it declares a paths field and what that field holds. The two answers
// come from one walk because they come from one parse.
//
// Only an unscoped rule is loaded at launch. A scoped one is left out of that
// set because the harness has not loaded it, which is what makes it a rule to
// read when a given path matches it.
//
// The directory is descended through its link target, since sharing one set
// of rules across projects by symlinking .claude/rules is a documented
// arrangement and WalkDir stops at the link rather than entering it. What
// comes back is still named under the directory as it was asked for: the
// resolved spelling is the one nobody recognises, which is the same reason
// nothing else here canonicalises a path.
func rulesIn(dir string) ([]rule, error) {
	root := dir
	if resolved, err := filepath.EvalSymlinks(dir); err == nil {
		root = resolved
	}

	var out []rule
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".md") {
			return nil
		}
		patterns, scoped, err := declaredPaths(path)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		out = append(out, rule{path: filepath.Join(dir, rel), scoped: scoped, patterns: patterns})
		return nil
	})
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	return out, err
}

// rule is one .md under a .claude/rules/.
type rule struct {
	// The file, under the spelling of the directory it was asked for.
	path string
	// Whether the frontmatter declares a paths field, which is the whole of
	// what decides when the harness loads the rule.
	scoped bool
	// The patterns that field holds, before anchoring. A scoped rule can have none —
	// the field can be empty, or hold something that is neither a string nor a
	// list — and it is still scoped, since the key is what the harness reads.
	patterns []string
}

// declaredPaths reads a rule's frontmatter for its paths field.
//
// Frontmatter that does not parse declares nothing, and so does a file with
// no frontmatter at all: both are rules that load unconditionally. That is
// what makes the reader's tolerance of line endings matter here — a scoped
// rule read as an unscoped one is loaded at launch in this replay and never
// listed as a rule a path matches.
func declaredPaths(path string) ([]string, bool, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, false, err
	}
	block, found := frontmatter.Split(b)
	if !found {
		return nil, false, nil
	}
	fields, err := block.Fields()
	if err != nil {
		return nil, false, nil
	}
	declared, ok := fields["paths"]
	if !ok {
		return nil, false, nil
	}

	// The documentation accepts a list or a comma-separated string, and the
	// harness splits a list's entries at their commas too. Anything else the
	// field holds is no pattern, and the rule stays scoped either way: the key
	// is what decides that the harness does not load it at launch.
	var patterns []string
	switch v := declared.(type) {
	case string:
		patterns = splitPatterns(v)
	case []any:
		for _, entry := range v {
			if s, ok := entry.(string); ok {
				patterns = append(patterns, splitPatterns(s)...)
			}
		}
	}
	return patterns, true, nil
}

// splitPatterns cuts a comma-separated list of patterns at each comma outside
// a brace group, where a comma separates the alternatives of one pattern
// instead. Whitespace around a pattern is trimmed and an empty one dropped.
//
// Braces are the only grouping tracked. A comma inside a bracket expression is
// a separator here, and a backslash escapes nothing, so \{ opens a group: the
// documentation gives no example of either, and the harness was measured only
// on braces.
func splitPatterns(s string) []string {
	var out []string
	depth, start := 0, 0
	for i, r := range s {
		switch r {
		case '{':
			depth++
		case '}':
			depth = max(depth-1, 0)
		case ',':
			if depth == 0 {
				out = appendPattern(out, s[start:i])
				start = i + 1
			}
		}
	}
	return appendPattern(out, s[start:])
}

// appendPattern adds a piece of a pattern list unless it is blank.
func appendPattern(out []string, piece string) []string {
	if p := strings.TrimSpace(piece); p != "" {
		return append(out, p)
	}
	return out
}

// hasGitEntry reports whether a directory is a checkout of its own, which is
// what stops the walk above going further up and what stops the check's walk
// descending into another branch's. Read as an entry rather than as a
// directory because a linked worktree marks itself with a file.
func hasGitEntry(dir string) bool {
	_, err := os.Lstat(filepath.Join(dir, ".git"))
	return err == nil
}

// isFile reports whether path is there and is not a directory, since a
// directory is not an instruction file or an import to load.
func isFile(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}
