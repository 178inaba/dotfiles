package skill

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/178inaba/dotfiles/go/internal/frontmatter"
)

// LineGuide and CharacterGuide are the size skill-authoring's design
// principle 5 measures a SKILL.md body against. A guide rather than a limit:
// nothing here fails a body over either one, and MeasureSize reports the
// measurement for a caller to act on.
const (
	LineGuide      = 500
	CharacterGuide = 7500
)

// Size is the outcome of measuring one or more skills.
type Size struct {
	// Target follows the same rule as Frontmatter's: absolute, so that the
	// output alone says which copy was measured.
	Target         string        `json:"target"`
	LineGuide      int           `json:"line_guide"`
	CharacterGuide int           `json:"character_guide"`
	Skills         []Measurement `json:"skills"`
	// Warnings names the directories that hold no SKILL.md, the same as
	// Frontmatter's, and any SKILL.md with no frontmatter block to measure the
	// body of — that file is measured whole rather than skipped, and named
	// here so the measurement is not mistaken for one made of an actual body.
	Warnings []string `json:"warnings"`
}

// Measurement is one file's measurement.
type Measurement struct {
	// File follows the same rule as a frontmatter violation's: relative to the
	// directory scanned, or <skill>/SKILL.md for a single file.
	File string `json:"file"`
	// Lines counts the body's newline-terminated lines; a final line with no
	// trailing newline still counts as one.
	Lines int `json:"lines"`
	// Characters counts Unicode code points rather than bytes, so a body of
	// Japanese text is not inflated by its multi-byte encoding.
	Characters int `json:"characters"`
	// MostlyNonASCII is true when most of the body's non-whitespace
	// characters are outside ASCII. character_guide was sized for Japanese,
	// where a line costs several times the tokens an English line does, and
	// this is what a caller reads before deciding whether over_character_guide
	// applies to this body at all.
	MostlyNonASCII bool `json:"mostly_non_ascii"`
	// OverLineGuide is lines > line_guide.
	OverLineGuide bool `json:"over_line_guide"`
	// OverCharacterGuide is characters > character_guide, on its own — whether
	// character_guide is the guide meant for this body depends on
	// mostly_non_ascii too.
	OverCharacterGuide bool `json:"over_character_guide"`
}

// OverApplicableGuide reports whether this file is over the guide that
// applies to it. over_line_guide always applies; over_character_guide applies
// only where mostly_non_ascii, since character_guide was derived from
// Japanese and an English body was never measured against it.
func (s Measurement) OverApplicableGuide() bool {
	return s.OverLineGuide || (s.MostlyNonASCII && s.OverCharacterGuide)
}

// MeasureSize measures a directory of skills, or one SKILL.md, against
// LineGuide and CharacterGuide.
//
// Both, for the reason CheckFrontmatter takes both: a hook measures one file
// as it is saved while a person measures them all, and one contract with two
// implementations would drift.
func MeasureSize(target string) (Size, error) {
	info, err := os.Stat(target)
	if err != nil {
		return Size{}, fmt.Errorf("target not found: %s", target)
	}
	if !info.IsDir() {
		dir, err := filepath.Abs(filepath.Dir(target))
		if err != nil {
			return Size{}, err
		}
		path := filepath.Join(dir, filepath.Base(target))
		sz, warning, err := measureFile(path, filepath.Join(filepath.Base(dir), filepath.Base(target)))
		if err != nil {
			return Size{}, err
		}
		out := Size{
			Target: path, LineGuide: LineGuide, CharacterGuide: CharacterGuide,
			Skills: []Measurement{sz}, Warnings: []string{},
		}
		if warning != "" {
			out.Warnings = append(out.Warnings, warning)
		}
		return out, nil
	}

	root, err := filepath.Abs(target)
	if err != nil {
		return Size{}, err
	}
	found, missing, err := skillFiles(root)
	if err != nil {
		return Size{}, fmt.Errorf("target not found: %s", target)
	}
	if len(found) == 0 {
		return Size{}, fmt.Errorf("no */SKILL.md found under %s", root)
	}

	out := Size{
		Target: root, LineGuide: LineGuide, CharacterGuide: CharacterGuide,
		Skills: []Measurement{}, Warnings: []string{},
	}
	for _, name := range missing {
		out.Warnings = append(out.Warnings, "no SKILL.md in "+name+"/")
	}
	// found is already sorted by file: os.ReadDir, which skillFiles reads
	// from, returns its entries sorted by name.
	for _, rel := range found {
		sz, warning, err := measureFile(filepath.Join(root, rel), rel)
		if err != nil {
			return Size{}, err
		}
		out.Skills = append(out.Skills, sz)
		if warning != "" {
			out.Warnings = append(out.Warnings, warning)
		}
	}
	return out, nil
}

// measureFile measures one SKILL.md. rel is what the result names it as.
func measureFile(path, rel string) (Measurement, string, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return Measurement{}, "", err
	}

	body, warning := bodyOf(content, rel)
	lines := lineCount(body)
	characters := utf8.RuneCountInString(body)
	return Measurement{
		File:               rel,
		Lines:              lines,
		Characters:         characters,
		MostlyNonASCII:     mostlyNonASCII(body),
		OverLineGuide:      lines > LineGuide,
		OverCharacterGuide: characters > CharacterGuide,
	}, warning, nil
}

// bodyOf is the text a body is measured from: the frontmatter's body where
// the block parses, or the whole file, CRLF normalised the same way, where it
// does not — a SKILL.md broken this way is measured rather than skipped,
// since it is exactly the drift ccx skill frontmatter already exists to
// catch, and skipping it here would hide it from the size guide too.
func bodyOf(content []byte, rel string) (body string, warning string) {
	if block, ok := frontmatter.Split(content); ok {
		return block.Body, ""
	}
	return strings.ReplaceAll(string(content), "\r\n", "\n"),
		"no frontmatter block in " + rel + ", the whole file was measured as the body"
}

// lineCount counts a body's newline-terminated lines: none for an empty body,
// otherwise the newlines it holds, plus one more for a final line with
// nothing to close it.
func lineCount(body string) int {
	if body == "" {
		return 0
	}
	n := strings.Count(body, "\n")
	if !strings.HasSuffix(body, "\n") {
		n++
	}
	return n
}

// mostlyNonASCII reports whether most of a body's non-whitespace characters
// are outside ASCII. Whitespace is excluded because it says nothing about the
// language a body is written in and would dilute the answer toward false for
// a body laid out with a lot of it.
func mostlyNonASCII(body string) bool {
	var total, nonASCII int
	for _, r := range body {
		if unicode.IsSpace(r) {
			continue
		}
		total++
		if r > unicode.MaxASCII {
			nonASCII++
		}
	}
	return total > 0 && nonASCII*2 > total
}
