package ghmd

import (
	"fmt"
	"regexp"
	"strings"
)

// A placeholder is not GitHub's notation, but where it means anything is
// GitHub's to say: a #{NAME} inside a code span or a fenced block is being
// shown rather than used, and #{NAME} is string interpolation in Ruby and in
// Elixir, so a draft quoting some is not naming an issue. That is the same
// reading the rest of this package publishes, which is why the notation lives
// beside it rather than in the one command that writes it today.

// placeholderRef is how a body names an issue that has no number yet. The
// braces are what survives GitHub's renderer: a name in angle brackets is
// dropped as an unknown tag, leaving a bare # behind.
var placeholderRef = regexp.MustCompile(`#\{([A-Z_]+)\}`)

// Placeholder is one #{NAME} in a body.
type Placeholder struct {
	Name string
	// Line is the 1-based line it is on, which is what a report about a body
	// names.
	Line int
}

// Placeholders finds the placeholders in a body, outside the code it may quote
// them in.
func Placeholders(body string) []Placeholder {
	var out []Placeholder
	for s := range Segments(body) {
		if s.Kind != Prose {
			continue
		}
		for _, m := range placeholderRef.FindAllStringSubmatch(body[s.Start:s.End], -1) {
			out = append(out, Placeholder{Name: m[1], Line: s.Line})
		}
	}
	return out
}

// Substitute replaces the placeholders numbers has a number for, leaving the
// rest — and everything inside code — alone. It answers with the ones it could
// not fill, so that a caller can tell "not yet" from "never".
//
// No judgement is made about what comes out. What the bare-#N rule guards
// against is item numbering a person typed; a number put in here names an
// issue the caller declared, and a new repository's first issues are numbered
// 1, 2 and 3.
func Substitute(body string, numbers map[string]int) (string, []Placeholder) {
	var b strings.Builder
	var left []Placeholder
	at := 0
	for s := range Segments(body) {
		if s.Kind != Prose {
			continue
		}
		text := body[s.Start:s.End]
		for _, m := range placeholderRef.FindAllStringSubmatchIndex(text, -1) {
			name := text[m[2]:m[3]]
			number, ok := numbers[name]
			if !ok {
				left = append(left, Placeholder{Name: name, Line: s.Line})
				continue
			}
			b.WriteString(body[at : s.Start+m[0]])
			fmt.Fprintf(&b, "#%d", number)
			at = s.Start + m[1]
		}
	}
	if at == 0 {
		// Nothing was replaced, so the body is already what it should be and
		// copying it through the builder would say the same thing.
		return body, left
	}
	b.WriteString(body[at:])
	return b.String(), left
}
