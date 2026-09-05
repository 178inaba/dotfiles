// Package plandocs collects the documents a plan has to be drafted against.
package plandocs

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/goccy/go-yaml"
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
	// instruction files, everything their @ imports reach, and the rules that
	// carry no paths field. Two things read this. An empty list is the
	// skills' "nothing to do" signal, since it means the project states no
	// instructions at all; a non-empty one names what is already in context
	// and must not be read again. An empty documents list says nothing about
	// this one — a CLAUDE.md whose paths are all backticked mentions leaves
	// loaded full and documents empty.
	Loaded []string `json:"loaded"`
	// The files to read, absolute, in walk order. Neither what loaded already
	// holds nor anything listed at a smaller depth appears twice.
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

// roots are the project instruction files Claude Code loads at launch, in the
// order it loads them. Ancestor directories and ~/.claude/CLAUDE.md are left
// out on purpose: they are the user's own context rather than the project's
// constraints, and a plan is checked against the project.
var roots = []string{"CLAUDE.md", filepath.Join(".claude", "CLAUDE.md"), "CLAUDE.local.md"}

// Collect walks the project instruction files under root and answers with the
// documents a planner has to read.
//
// home resolves the @~/ form of an import. It is a parameter because a test
// has a fixture home and no business reading the real one.
//
// Nothing about the repository is an error: no instruction file at all, a
// scoped rule nobody links, a link to a file that was deleted — each is an
// ordinary answer. Only a filesystem that cannot be read is returned as one.
func Collect(root, home string) (Collection, error) {
	c := collector{
		home:   home,
		seen:   map[string]bool{},
		warned: map[Warning]bool{},
		cache:  map[string][]reference{},
	}

	for _, name := range roots {
		path := filepath.Join(root, name)
		if isFile(path) {
			if err := c.expand(path); err != nil {
				return Collection{}, err
			}
		}
	}
	rules, err := unscopedRules(filepath.Join(root, ".claude", "rules"))
	if err != nil {
		return Collection{}, err
	}
	for _, path := range rules {
		c.load(path)
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
	return c.out, nil
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

// unscopedRules lists the rules Claude Code loads at launch: every .md under
// the directory, recursively, that carries no paths field. A scoped rule is
// left out because the harness has not loaded it, which is what makes it a
// document to read when something links it.
//
// The directory is descended through its link target, since sharing one set
// of rules across projects by symlinking .claude/rules is a documented
// arrangement and WalkDir stops at the link rather than entering it. What
// comes back is still named under the directory as it was asked for: the
// resolved spelling is the one nobody recognises, which is the same reason
// nothing else here canonicalises a path.
func unscopedRules(dir string) ([]string, error) {
	root := dir
	if resolved, err := filepath.EvalSymlinks(dir); err == nil {
		root = resolved
	}

	var out []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".md") {
			return nil
		}
		scoped, err := hasPaths(path)
		if err != nil {
			return err
		}
		if scoped {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		out = append(out, filepath.Join(dir, rel))
		return nil
	})
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	return out, err
}

// hasPaths reports whether a rule's frontmatter declares a paths field, which
// is the whole of what decides when the rule loads.
//
// Frontmatter that does not parse declares nothing, and so does a file with
// no frontmatter at all: both are rules that load unconditionally.
func hasPaths(path string) (bool, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return false, err
	}
	// Carriage returns come off first: a rule saved with CRLF whose fences
	// then failed to match would be taken for a rule with no frontmatter, and
	// a scoped rule read as an unscoped one is loaded at launch in this
	// walk's model and never listed as a document in either.
	lines := strings.Split(strings.ReplaceAll(string(b), "\r\n", "\n"), "\n")
	if lines[0] != "---" {
		return false, nil
	}
	end := 0
	for i, line := range lines[1:] {
		if line == "---" {
			end = i + 1
			break
		}
	}
	if end == 0 {
		return false, nil
	}

	var document any
	if err := yaml.Unmarshal([]byte(strings.Join(lines[1:end], "\n")), &document); err != nil {
		return false, nil
	}
	parsed, _ := document.(map[string]any)
	_, ok := parsed["paths"]
	return ok, nil
}

// isFile reports whether path is there and is not a directory, since a link
// to a directory is not a document to read.
func isFile(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}
