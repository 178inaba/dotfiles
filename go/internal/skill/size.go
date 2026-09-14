package skill

import (
	"os"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/178inaba/dotfiles/go/internal/frontmatter"
)

// LineGuide and CharacterGuide are the size skill-authoring's design
// principle サイズ上限の目安 measures a SKILL.md body against. A guide rather than a limit:
// nothing here fails a body over either one, and MeasureSize reports the
// measurement for a caller to act on.
const (
	LineGuide      = 500
	CharacterGuide = 7500
)

// Size is the outcome of measuring one or more skills.
type Size struct {
	// Target is absolute, the same rule ccx skill frontmatter's target
	// follows, so that the output alone says which copy was measured.
	Target string `json:"target"`
	// LineGuide is the most lines a body is meant to run to.
	LineGuide int `json:"line_guide"`
	// CharacterGuide is the most characters a body is meant to run to: the
	// Agent Skills specification's 5,000 tokens times the 1.5 characters per
	// token measured on this repository's Japanese skills.
	CharacterGuide int `json:"character_guide"`
	// Skills holds one measurement per SKILL.md, ordered by file.
	Skills []Measurement `json:"skills"`
	// Warnings names the directories that hold no SKILL.md, the same warning
	// ccx skill frontmatter gives, and any SKILL.md with no frontmatter block
	// to measure the body of — that file is measured whole rather than
	// skipped, and named here so the measurement is not mistaken for one made
	// of an actual body.
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
	return s.OverLineGuide || s.OverApplicableCharacterGuide()
}

// OverApplicableCharacterGuide reports whether this file is over
// character_guide and that guide applies to it.
func (s Measurement) OverApplicableCharacterGuide() bool {
	return s.MostlyNonASCII && s.OverCharacterGuide
}

// MeasureSize measures a directory of skills, or one SKILL.md, against
// LineGuide and CharacterGuide.
//
// Both, for the reason CheckFrontmatter takes both: a hook measures one file
// as it is saved while a person measures them all, and one contract with two
// implementations would drift.
func MeasureSize(name string) (Size, error) {
	resolved, err := resolveTarget(name)
	if err != nil {
		return Size{}, err
	}
	out := Size{
		Target: resolved.abs, LineGuide: LineGuide, CharacterGuide: CharacterGuide,
		Skills: []Measurement{}, Warnings: resolved.warnings,
	}
	// The files are already sorted: os.ReadDir, which skillFiles reads from,
	// returns its entries sorted by name.
	for _, f := range resolved.files {
		sz, warning, err := measureFile(f.path, f.rel)
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
	return frontmatter.Normalize(content),
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
