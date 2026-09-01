package skill

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
)

// A `@~/.claude/skills/<skill>/<file>` in a SKILL.md attaches that file when
// the skill starts — one level only. The `@` references inside an attached file
// are not followed, so nesting them loses everything past the first hop, in
// silence: issue-handle referencing deep-plan-review referencing
// fresh-reader-verification ran a verification whose convergence protocol had
// never been read. These checks make that structural risk a decidable one.

// RefViolation is one thing wrong with the references between skills.
type RefViolation string

const (
	// UncoveredNested is a reference whose target references something the
	// referring file does not. Starting the referring file would not attach
	// it: either stop referencing and start the other skill as a skill, or
	// reference the second file directly as well.
	UncoveredNested RefViolation = "uncovered_nested"
	MissingTarget   RefViolation = "missing_target"
	// RefInCode is a reference inside backticks or a fence, which suggests the
	// `@` was meant as a mention rather than as an attachment.
	RefInCode RefViolation = "ref_in_code"
)

// RefFinding is one violation. Every path is relative to the skills directory.
type RefFinding struct {
	Type RefViolation `json:"type"`
	File string       `json:"file"`
	Line int          `json:"line"`
	Ref  string       `json:"ref"`
	// Nested is the second-hop path, and only uncovered_nested has one.
	Nested string `json:"nested,omitzero"`
}

// Refs is the outcome of one check.
type Refs struct {
	SkillsDir  string       `json:"skills_dir"`
	Violations []RefFinding `json:"violations"`
	// Warnings is always empty, and is here so that the shape matches the
	// other checks a caller reads.
	Warnings []string `json:"warnings"`
}

// refPrefix is what a reference to another skill's file begins with.
const refPrefix = "@~/.claude/skills/"

// refPattern is deliberately narrow about what may follow: stopping at
// anything but these characters keeps Japanese punctuation, brackets and a
// closing backtick out of the path.
var refPattern = regexp.MustCompile(regexp.QuoteMeta(refPrefix) + `[A-Za-z0-9._/-]+`)

// reference is one `@` reference as it was written.
type reference struct {
	file string
	line int
	ref  string
	// inCode is a reference inside backticks or a fence, which attaches
	// nothing.
	inCode bool
	exists bool
}

// CheckRefs inspects the references between the skills under skillsDir.
func CheckRefs(skillsDir string) (Refs, error) {
	info, err := os.Stat(skillsDir)
	if err != nil || !info.IsDir() {
		return Refs{}, fmt.Errorf("skills directory not found: %s", skillsDir)
	}
	root, err := filepath.Abs(skillsDir)
	if err != nil {
		return Refs{}, err
	}

	queue, _, err := skillFiles(root)
	if err != nil {
		return Refs{}, fmt.Errorf("skills directory not found: %s", skillsDir)
	}
	if len(queue) == 0 {
		return Refs{}, fmt.Errorf("no */SKILL.md found under %s", root)
	}

	// The referenced files are read too, because what gets attached is the file
	// itself — so its own references are the second hop.
	var refs []reference
	scanned := map[string]bool{}
	for len(queue) > 0 {
		var next []string
		for _, rel := range queue {
			if scanned[rel] {
				continue
			}
			scanned[rel] = true
			found, err := refsIn(root, rel)
			if err != nil {
				continue
			}
			refs = append(refs, found...)
			for _, r := range found {
				if !r.inCode {
					next = append(next, r.ref)
				}
			}
		}
		queue = next
	}

	return Refs{SkillsDir: root, Violations: refViolations(refs), Warnings: []string{}}, nil
}

// refsIn reads one file's references.
func refsIn(root, rel string) ([]reference, error) {
	content, err := os.ReadFile(filepath.Join(root, rel))
	if err != nil {
		return nil, err
	}

	var out []reference
	fenced := false
	for i, line := range strings.Split(string(content), "\n") {
		if strings.HasPrefix(strings.TrimLeft(line, " \t"), "```") {
			fenced = !fenced
			continue
		}
		for _, loc := range refPattern.FindAllStringIndex(line, -1) {
			ref := line[loc[0]+len(refPrefix) : loc[1]]
			// An odd number of backticks before it means the reference is
			// inside inline code.
			inCode := fenced || strings.Count(line[:loc[0]], "`")%2 == 1
			exists := false
			if info, err := os.Stat(filepath.Join(root, ref)); err == nil && !info.IsDir() {
				exists = true
			}
			out = append(out, reference{file: rel, line: i + 1, ref: ref, inCode: inCode, exists: exists})
		}
	}
	return out, nil
}

// refViolations turns the references into findings, sorted the way the contract
// publishes them.
func refViolations(refs []reference) []RefFinding {
	// Only a reference that actually attaches counts as a dependency, so one
	// inside code or pointing at nothing is not one.
	deps := map[string][]string{}
	for _, r := range refs {
		if !r.inCode && r.exists && !slices.Contains(deps[r.file], r.ref) {
			deps[r.file] = append(deps[r.file], r.ref)
		}
	}

	out := []RefFinding{}
	for _, r := range refs {
		switch {
		case r.inCode:
			out = append(out, RefFinding{Type: RefInCode, File: r.file, Line: r.line, Ref: r.ref})
		case !r.exists:
			out = append(out, RefFinding{Type: MissingTarget, File: r.file, Line: r.line, Ref: r.ref})
		default:
			for _, nested := range deps[r.ref] {
				// A file referencing back to the referring one is not a second
				// hop that goes unattached.
				if nested == r.file || slices.Contains(deps[r.file], nested) {
					continue
				}
				out = append(out, RefFinding{Type: UncoveredNested, File: r.file, Line: r.line, Ref: r.ref, Nested: nested})
			}
		}
	}

	sortFindings(out, func(f RefFinding) (string, int, string) { return f.File, f.Line, string(f.Type) })
	return out
}
