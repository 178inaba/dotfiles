package skill

import (
	"math"
	"os"
	"strings"
	"unicode"

	"github.com/178inaba/dotfiles/go/internal/frontmatter"
)

// LineGuide and TokenGuide are the size skill-authoring's design principle
// サイズ上限の目安 measures a SKILL.md body against. A guide rather than a limit:
// nothing here fails a body over either one, and MeasureSize reports the
// measurement for a caller to act on.
const (
	LineGuide  = 500
	TokenGuide = 5000
)

// The weights estimateTokens gives each character class, fitted by least
// squares to 58 reproduced skill injections measured on Claude Opus 5, with a
// median error of 1.4% and a maximum of 9.5% on samples of 1,000 tokens or
// more.
const (
	nonASCIIWeight    = 0.957
	letterDigitWeight = 0.190
	symbolWeight      = 1.139
	whitespaceWeight  = 0.642
)

// Size is the outcome of measuring one or more skills.
type Size struct {
	// Target is absolute, the same rule ccx skill frontmatter's target
	// follows, so that the output alone says which copy was measured.
	Target string `json:"target"`
	// LineGuide is the most lines a body is meant to run to.
	LineGuide int `json:"line_guide"`
	// TokenGuide is the most estimated tokens a body is meant to run to: the
	// Agent Skills specification's 5,000.
	TokenGuide int `json:"token_guide"`
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
	// EstimatedTokens is the command's per-class estimate over the body,
	// rounded to the nearest integer. A character outside ASCII is in that class even
	// when it is whitespace. The tokenizer the weights were fitted to produces
	// more tokens for the same text than earlier models' did, so a body within
	// token_guide there is within it on an earlier model too.
	EstimatedTokens int `json:"estimated_tokens"`
	// OverLineGuide is lines > line_guide.
	OverLineGuide bool `json:"over_line_guide"`
	// OverTokenGuide is estimated_tokens > token_guide.
	OverTokenGuide bool `json:"over_token_guide"`
}

// OverGuide reports whether this file is over either guide.
func (s Measurement) OverGuide() bool {
	return s.OverLineGuide || s.OverTokenGuide
}

// MeasureSize measures a directory of skills, or one SKILL.md, against
// LineGuide and TokenGuide.
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
		Target: resolved.abs, LineGuide: LineGuide, TokenGuide: TokenGuide,
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
	tokens := estimateTokens(body)
	return Measurement{
		File:            rel,
		Lines:           lines,
		EstimatedTokens: tokens,
		OverLineGuide:   lines > LineGuide,
		OverTokenGuide:  tokens > TokenGuide,
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

// estimateTokens is estimated_tokens for a body. The code point is tested
// before whitespace, so that whitespace outside ASCII is classed the way the
// weights were fitted: as a character outside ASCII.
func estimateTokens(body string) int {
	var sum float64
	for _, r := range body {
		switch {
		case r > unicode.MaxASCII:
			sum += nonASCIIWeight
		case unicode.IsSpace(r):
			sum += whitespaceWeight
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9':
			sum += letterDigitWeight
		default:
			sum += symbolWeight
		}
	}
	return int(math.Round(sum))
}
