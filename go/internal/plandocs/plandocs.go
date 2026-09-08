// Package plandocs collects the documents a plan has to be drafted against.
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
// not, so this walk delivers them like any other link.
const importHops = 4

// walkDepth is how far past the already-loaded set the walk goes. Depth
// correlates with relevance in a hand-written document tree — an index links
// its rationale, and the rationale links the convention bodies — and two
// levels is where that ends: a third reaches whatever the conventions cite,
// which is generated reference material more often than anything a planner
// needs. The measurement behind the number is in the command's help.
const walkDepth = 2

// Collection is what a planner reads, and what the harness already read for
// it.
type Collection struct {
	// The files Claude Code loaded at launch, absolute: the project
	// instruction files, everything their @ imports reach, and the project
	// rules that carry no paths field. The user's own unscoped rules are not
	// here: they belong to the user rather than to the project. A non-empty
	// list names what is already in context and must not be read again.
	// Nothing to read is this and documents both empty, and neither list says
	// it alone: with a given path a scoped rule is a document in a repository
	// that states no instructions at all, and a CLAUDE.md whose paths are all
	// backticked mentions leaves this one full and documents empty.
	Loaded []string `json:"loaded"`
	// The files to read, absolute: first the linked documents in walk order,
	// then the path-scoped rules a given path matched, sorted by path. Neither
	// what loaded already holds nor anything listed once already appears
	// twice, and a rule found under two spellings of the same file is one
	// entry.
	Documents []string `json:"documents"`
	// A link whose target is not there. Reported rather than raised: one
	// broken link is a document to fix, not a reason to collect nothing.
	Warnings []Warning `json:"warnings"`
}

// Warning is a link that leads nowhere.
type Warning struct {
	// The path as the link writes it, minus any fragment, so that it can be
	// found in the file named below and corrected there.
	Target string `json:"target"`
	// The file the link is written in, absolute.
	Source string `json:"source"`
}

// roots are the instruction files Claude Code loads from a directory, in the
// order it loads them.
var roots = []string{"CLAUDE.md", filepath.Join(".claude", "CLAUDE.md"), "CLAUDE.local.md"}

// Collect walks the project's instruction files and answers with the
// documents a planner has to read.
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
// scoped rule nobody links, a link to a file that was deleted — each is an
// ordinary answer. Only a filesystem that cannot be read is returned as one.
func Collect(dir, home string, paths ...string) (Collection, error) {
	c := collector{
		home:   home,
		seen:   map[string]bool{},
		warned: map[Warning]bool{},
		cache:  map[string][]reference{},
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

	// The user's own rules are matched but never walked: a scoped rule is a
	// constraint the harness loads for the project's files, while the unscoped
	// ones are the user's memory and no part of what the project states. They
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

	frontier := c.out.Loaded
	for range walkDepth {
		next, err := c.follow(frontier)
		if err != nil {
			return Collection{}, err
		}
		c.out.Documents = append(c.out.Documents, next...)
		frontier = next
	}

	c.out.Documents = append(c.out.Documents, c.matched(dirs, scoped, absolute)...)
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
		if _, err := os.Lstat(filepath.Join(at, ".git")); err == nil {
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
	// Every loaded file is scanned twice — once to replay the harness's
	// closure, once as the first frontier of the walk — and the second scan
	// finds what the first one did.
	cache map[string][]reference
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
// CLAUDE.md, y.md and r.md — and not x.md. So a rule's import reaches a
// planner through the walk instead.
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

		imported, err := c.targets(at.path, func(r reference) bool { return r.isImport })
		if err != nil {
			return err
		}
		for _, path := range imported {
			queue = append(queue, step{path: path, hop: at.hop + 1})
		}
	}
	return nil
}

// follow reads one depth's files and answers with the next depth: every
// document they link that nothing has listed yet, in the order the files were
// read and, within a file, the order the links appear.
func (c *collector) follow(files []string) ([]string, error) {
	var out []string
	for _, file := range files {
		linked, err := c.targets(file, func(r reference) bool { return isDocument(r.target) })
		if err != nil {
			return nil, err
		}
		for _, path := range linked {
			if !c.seen[path] {
				c.seen[path] = true
				out = append(out, path)
			}
		}
	}
	return out, nil
}

// targets resolves the references in file that keep accepts, dropping the
// ones that name no file at all and warning about the ones whose file is not
// there.
//
// A path already answered for is dropped rather than stated again: everything
// in seen was found to exist when it was added, so it can neither warn now
// nor be listed a second time.
func (c *collector) targets(file string, keep func(reference) bool) ([]string, error) {
	refs, err := c.refs(file)
	if err != nil {
		return nil, err
	}

	var out []string
	for _, ref := range refs {
		if !keep(ref) {
			continue
		}
		target, ok := resolve(ref.target, file, c.home)
		switch {
		case !ok, c.seen[target]:
		case !isFile(target):
			c.warn(ref.target, file)
		default:
			out = append(out, target)
		}
	}
	return out, nil
}

// refs reads a file's links and imports, once per file.
func (c *collector) refs(path string) ([]reference, error) {
	if refs, ok := c.cache[path]; ok {
		return refs, nil
	}
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	refs := references(string(b))
	c.cache[path] = refs
	return refs, nil
}

// warn records a broken link once per place it is written, so that the same
// missing file linked from two documents is two things to fix and the same
// link seen by both the closure and the walk is one.
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
// first the walk reaches, which is why the bases are taken in the walk's own
// order — the user's rules are keyed at the top, so their spelling wins over a
// deeper project directory's and the answer stays the same from wherever the
// command was run.
func (c *collector) matched(dirs []string, scoped map[string][]rule, paths []string) []string {
	if len(paths) == 0 {
		return nil
	}

	// Seeded with everything already answered for, since a rule a document
	// links — or one an instruction file imports, which puts it in the loaded
	// set whatever its frontmatter says — is a rule a given path can match
	// under another spelling.
	answered := slices.Concat(c.out.Loaded, c.out.Documents)
	listed := make([]os.FileInfo, 0, len(answered))
	for _, path := range answered {
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
// A pattern that is not a valid glob matches nothing and does not fail the
// run, as the documentation says of one the harness cannot read. The one
// documented difference in the syntax is the brace-expansion budget, which is
// not reproduced here.
func matchesAny(patterns []string, basis string, paths []string) bool {
	for _, path := range paths {
		rel, err := filepath.Rel(basis, path)
		if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
			continue
		}
		for _, pattern := range patterns {
			if ok, err := doublestar.Match(pattern, filepath.ToSlash(rel)); err == nil && ok {
				return true
			}
		}
	}
	return false
}

// rulesIn lists the rules under a .claude/rules/, recursively, saying of each
// whether it declares a paths field and what that field holds. The two answers
// come from one walk because they come from one parse.
//
// Only an unscoped rule is loaded at launch. A scoped one is left out of that
// set because the harness has not loaded it, which is what makes it a document
// to read — when something links it, or when a given path matches it.
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
	// The patterns that field holds. A scoped rule can have none — the field
	// can be empty, or hold something that is not a list of strings — and it
	// is still scoped, since the key is what the harness reads.
	patterns []string
}

// declaredPaths reads a rule's frontmatter for its paths field.
//
// Frontmatter that does not parse declares nothing, and so does a file with
// no frontmatter at all: both are rules that load unconditionally. That is
// what makes the reader's tolerance of line endings matter here — a scoped
// rule read as an unscoped one is loaded at launch in this walk's model and
// never listed as a document in either.
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

	// Anything the field holds that is not a string is no pattern, and the
	// rule stays scoped either way: the key is what decides that the harness
	// does not load it at launch.
	list, _ := declared.([]any)
	var patterns []string
	for _, entry := range list {
		if pattern, ok := entry.(string); ok {
			patterns = append(patterns, pattern)
		}
	}
	return patterns, true, nil
}

// isFile reports whether path is there and is not a directory, since a link
// to a directory is not a document to read.
func isFile(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}
