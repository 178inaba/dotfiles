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
// needs.
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
	// The path as the link writes it, so that it can be found in the file
	// named below and corrected there.
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
	c := collector{home: home, seen: map[string]bool{}, warned: map[Warning]bool{}}

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
		if !c.seen[path] {
			c.seen[path] = true
			c.out.Loaded = append(c.out.Loaded, path)
		}
	}

	frontier := c.out.Loaded
	for depth := 1; depth <= walkDepth; depth++ {
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
	// Files are read twice — once for the closure, once for the walk — and
	// the second read is the same bytes as the first.
	cache map[string]string
}

// expand adds a root and everything its imports reach to the loaded set.
//
// Only imports are followed here: this is a replay of what the harness did at
// launch, and the harness expands nothing else. A rules file is not a root of
// its own closure, because the harness does not expand the imports written in
// one — measured with the InstructionsLoaded hook, which reported a file
// imported by CLAUDE.md and not the one imported by an unscoped rule beside
// it. So a rule's import reaches a planner through the walk instead.
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
		c.seen[at.path] = true
		c.out.Loaded = append(c.out.Loaded, at.path)
		if at.hop == importHops {
			continue
		}

		text, err := c.read(at.path)
		if err != nil {
			return err
		}
		for _, ref := range references(text) {
			if !ref.isImport {
				continue
			}
			target, ok := resolve(ref.target, at.path, c.home)
			switch {
			case !ok:
			case !isFile(target):
				c.warn(ref.target, at.path)
			default:
				queue = append(queue, step{path: target, hop: at.hop + 1})
			}
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
		text, err := c.read(file)
		if err != nil {
			return nil, err
		}
		for _, ref := range references(text) {
			if !isDocument(ref.target) {
				continue
			}
			target, ok := resolve(ref.target, file, c.home)
			if !ok {
				continue
			}
			if !isFile(target) {
				c.warn(ref.target, file)
				continue
			}
			if !c.seen[target] {
				c.seen[target] = true
				out = append(out, target)
			}
		}
	}
	return out, nil
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

func (c *collector) read(path string) (string, error) {
	if text, ok := c.cache[path]; ok {
		return text, nil
	}
	b, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	if c.cache == nil {
		c.cache = map[string]string{}
	}
	c.cache[path] = string(b)
	return string(b), nil
}

// unscopedRules lists the rules Claude Code loads at launch: every .md under
// the directory, recursively, that carries no paths field. A scoped rule is
// left out because the harness has not loaded it, which is what makes it a
// document to read when something links it.
func unscopedRules(dir string) ([]string, error) {
	var out []string
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
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
		if !scoped {
			out = append(out, path)
		}
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
	lines := strings.Split(string(b), "\n")
	if len(lines) == 0 || lines[0] != "---" {
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
