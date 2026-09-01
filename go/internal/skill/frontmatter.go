// Package skill checks the SKILL.md files a skill is defined by.
//
// Claude Code's own frontmatter parser is forgiving enough to load a skill
// whose frontmatter is not valid YAML, so nothing goes wrong loudly: an
// argument-hint holding two flow sequences on one line sat broken in two files
// until somebody happened to look. These checks replace looking.
package skill

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"

	"github.com/goccy/go-yaml"
)

// FrontmatterViolation is one thing wrong with a SKILL.md's frontmatter.
type FrontmatterViolation string

const (
	// InvalidYAML is frontmatter that will not parse. Nothing else is reported
	// for such a file: there is no parsed value to judge it by.
	InvalidYAML FrontmatterViolation = "invalid_yaml"
	// MissingField covers a file with no frontmatter block at all, which is
	// missing both fields rather than being a kind of its own.
	MissingField FrontmatterViolation = "missing_field"
	// NameMismatch is a name field that is not the skill's directory name,
	// which is the name the model is given to invoke it by.
	NameMismatch FrontmatterViolation = "name_mismatch"
	// UnquotedFlow is a value beginning with an unquoted [ or {, which YAML
	// reads as a sequence or a mapping rather than as the string it was meant
	// to be.
	UnquotedFlow FrontmatterViolation = "unquoted_flow"
)

// Violation is one finding, with the details its own kind carries and nothing
// else.
type Violation struct {
	Type FrontmatterViolation `json:"type"`
	// File is relative to the directory scanned, or <skill>/SKILL.md for a
	// single file: a bare base name would not say which skill it belongs to.
	File string `json:"file"`
	// Message is the parser's own words, and its line numbers count from the
	// start of the frontmatter block rather than from the start of the file.
	Message  string `json:"message,omitzero"`
	Field    string `json:"field,omitzero"`
	Expected string `json:"expected,omitzero"`
	Actual   string `json:"actual,omitzero"`
	Key      string `json:"key,omitzero"`
	// Line counts from the start of the file.
	Line int `json:"line,omitzero"`
}

// Frontmatter is the outcome of one check.
type Frontmatter struct {
	// Target is absolute, so that the output alone says which copy was read —
	// ~/.claude/skills is a symlink to this repository, and both spellings
	// reach the same files.
	Target     string      `json:"target"`
	Violations []Violation `json:"violations"`
	// Warnings name the directories that hold no SKILL.md. Not violations, but
	// skipping them in silence would hide that they went unchecked.
	Warnings []string `json:"warnings"`
}

// flowKey matches a top-level frontmatter key whose value begins with an
// unquoted [ or {.
//
// Read from the text rather than from the parsed value, where the distinction
// is already gone: [--yes] parses as a sequence and "[--yes]" as a string, and
// `[a] [b]` does not parse at all. Indented lines are excluded, which takes the
// contents of a block scalar with them.
var flowKey = regexp.MustCompile(`^[A-Za-z0-9_][A-Za-z0-9_.-]*:[ \t]*[\[{]`)

// CheckFrontmatter inspects a directory of skills, or one SKILL.md.
//
// Both, because a hook checks one file as it is saved while a person checks
// them all, and one contract with two implementations would drift.
func CheckFrontmatter(target string) (Frontmatter, error) {
	info, err := os.Stat(target)
	if err != nil {
		return Frontmatter{}, fmt.Errorf("target not found: %s", target)
	}
	if !info.IsDir() {
		dir, err := filepath.Abs(filepath.Dir(target))
		if err != nil {
			return Frontmatter{}, err
		}
		path := filepath.Join(dir, filepath.Base(target))
		out := Frontmatter{
			Target:     path,
			Violations: checkFile(path, filepath.Join(filepath.Base(dir), filepath.Base(target))),
			Warnings:   []string{},
		}
		sortFindings(out.Violations, func(v Violation) (string, int, string) {
			return v.File, v.Line, string(v.Type)
		})
		return out, nil
	}

	root, err := filepath.Abs(target)
	if err != nil {
		return Frontmatter{}, err
	}
	found, missing, err := skillFiles(root)
	if err != nil {
		return Frontmatter{}, fmt.Errorf("target not found: %s", target)
	}
	if len(found) == 0 {
		return Frontmatter{}, fmt.Errorf("no */SKILL.md found under %s", root)
	}

	out := Frontmatter{Target: root, Violations: []Violation{}, Warnings: []string{}}
	for _, name := range missing {
		out.Warnings = append(out.Warnings, "no SKILL.md in "+name+"/")
	}
	for _, rel := range found {
		out.Violations = append(out.Violations, checkFile(filepath.Join(root, rel), rel)...)
	}
	sortFindings(out.Violations, func(v Violation) (string, int, string) {
		return v.File, v.Line, string(v.Type)
	})
	return out, nil
}

// skillFiles lists the SKILL.md files directly under root, and the directories
// that have none.
//
// Both checks read the same tree, and a directory without a SKILL.md is worth
// reporting rather than skipping in silence: it went unchecked.
func skillFiles(root string) (found, missing []string, err error) {
	entries, err := os.ReadDir(root)
	if err != nil {
		return nil, nil, err
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		rel := filepath.Join(e.Name(), "SKILL.md")
		if info, err := os.Stat(filepath.Join(root, rel)); err != nil || info.IsDir() {
			missing = append(missing, e.Name())
			continue
		}
		found = append(found, rel)
	}
	return found, missing, nil
}

// sortFindings puts findings in the order both contracts publish them: by file,
// then by line, then by kind, so that two runs over one tree read the same way.
func sortFindings[T any](findings []T, key func(T) (file string, line int, kind string)) {
	slices.SortStableFunc(findings, func(a, b T) int {
		aFile, aLine, aKind := key(a)
		bFile, bLine, bKind := key(b)
		if aFile != bFile {
			return strings.Compare(aFile, bFile)
		}
		if aLine != bLine {
			return aLine - bLine
		}
		return strings.Compare(aKind, bKind)
	})
}

// checkFile inspects one SKILL.md. rel is what the finding names it as, and its
// first component is the skill the name is expected to match.
func checkFile(path, rel string) []Violation {
	skill, _, _ := strings.Cut(rel, string(filepath.Separator))

	content, err := os.ReadFile(path)
	if err != nil {
		return []Violation{{Type: InvalidYAML, File: rel, Message: err.Error()}}
	}
	lines := strings.Split(string(content), "\n")

	// The block's extent is decided from the raw text. A parser's verdict
	// cannot stand in for it: with no fence at all it would read the body
	// instead, and the answer would depend on what the body happened to say.
	end, ok := frontmatterEnd(lines)
	if !ok {
		return []Violation{
			{Type: MissingField, File: rel, Field: "name"},
			{Type: MissingField, File: rel, Field: "description"},
		}
	}
	block := strings.Join(lines[1:end-1], "\n")

	// Into any rather than a map: frontmatter that parses to a sequence or a
	// scalar is not a parse failure, it is a block with no fields — which is
	// the same as having none.
	var document any
	if err := yaml.Unmarshal([]byte(block), &document); err != nil {
		return []Violation{{Type: InvalidYAML, File: rel, Message: message(err)}}
	}
	parsed, _ := document.(map[string]any)

	out := []Violation{}
	switch name := field(parsed, "name"); {
	case name == "":
		out = append(out, Violation{Type: MissingField, File: rel, Field: "name"})
	case name != skill:
		out = append(out, Violation{Type: NameMismatch, File: rel, Expected: skill, Actual: name})
	}
	if field(parsed, "description") == "" {
		out = append(out, Violation{Type: MissingField, File: rel, Field: "description"})
	}

	// Every key rather than the one that has gone wrong before: singling out
	// argument-hint would leave the next key free to repeat the mistake.
	for i, line := range lines[1 : end-1] {
		if flowKey.MatchString(line) {
			key, _, _ := strings.Cut(line, ":")
			out = append(out, Violation{Type: UnquotedFlow, File: rel, Key: key, Line: i + 2})
		}
	}
	return out
}

// frontmatterEnd returns the line number of the block's closing fence.
func frontmatterEnd(lines []string) (int, bool) {
	if len(lines) == 0 || lines[0] != "---" {
		return 0, false
	}
	for i, line := range lines[1:] {
		if line == "---" {
			return i + 2, true
		}
	}
	return 0, false
}

// field reads one frontmatter value as a string.
//
// Frontmatter that parses to a sequence or a scalar rather than a mapping
// leaves every field empty, which is what missing means here.
func field(parsed map[string]any, key string) string {
	value, ok := parsed[key]
	if !ok || value == nil {
		return ""
	}
	if s, ok := value.(string); ok {
		return s
	}
	return fmt.Sprint(value)
}

// message flattens a parser error onto one line, since it is one field of a
// JSON record and go-yaml lays its errors out over several lines with a caret.
func message(err error) string {
	return strings.Join(strings.Fields(err.Error()), " ")
}
