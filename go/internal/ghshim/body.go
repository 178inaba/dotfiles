package ghshim

import (
	"fmt"
	"regexp"
	"slices"
	"strings"
)

// The three body rules, and the quoting the messages echo commands with. The
// shell reached for awk here because it has no line scan of its own; what it
// asked awk to decide is kept, the sub-language is not.

var (
	fenceLine = regexp.MustCompile("^[[:space:]]*(```|~~~)")
	codeSpan  = regexp.MustCompile("`[^`]*`")
	// The trailing class is what excludes #12 and up, #1a2b3c and #1st; the
	// leading one is what leaves OWNER/REPO#1 alone, by skipping any token
	// that opens with an alphanumeric.
	bareHashToken = regexp.MustCompile(`^[^[:alnum:]#]*#[1-9]([^[:alnum:]]|$)`)
	// GitHub reads only the direct adjacency of keyword, optional colon, space
	// and reference, so the detection is limited to it as well.
	//
	// The same knowledge is encoded in pullrequest.closingKeyword, which reads
	// a body for the issues it closes; if GitHub ever changes the set, both
	// have to move.
	closingKeyword = regexp.MustCompile(
		`(?i)(^|[^[:alnum:]])(close[sd]?|fix(e[sd])?|resolve[sd]?):?[[:space:]]+([[:alnum:]_.-]+/[[:alnum:]_.-]+)?#[0-9]+`)
)

// countBareHashRefs counts the distinct digits of the bare #1 to #9 in body,
// ignoring the places GitHub does not autolink.
//
// [[:alnum:]] is ASCII here while awk's was whatever the locale said, so a
// digit followed by a multibyte letter counts where it might not have. That
// errs towards blocking, and a decision that does not move with the locale is
// worth more than the agreement — macOS awk compares multibyte text unreliably.
//
// A known limit carried over: an unclosed fence hides everything after it.
func countBareHashRefs(body string) int {
	seen := map[byte]bool{}
	fence := false
	for line := range strings.Lines(body) {
		if fenceLine.MatchString(line) {
			fence = !fence
			continue
		}
		if fence {
			continue
		}
		// The substitution allocates, and most lines hold no code span.
		if strings.IndexByte(line, '`') >= 0 {
			line = codeSpan.ReplaceAllString(line, "")
		}
		for token := range strings.FieldsSeq(line) {
			if !bareHashToken.MatchString(token) {
				continue
			}
			seen[token[strings.IndexByte(token, '#')+1]] = true
		}
	}
	return len(seen)
}

// hasQuotedClosingKeyword reports whether body holds a closing keyword where
// GitHub will not read it as one: inside a fence, or inside a code span.
func hasQuotedClosingKeyword(body string) bool {
	fence := false
	for line := range strings.Lines(body) {
		if fenceLine.MatchString(line) {
			fence = !fence
			continue
		}
		if fence {
			if closingKeyword.MatchString(line) {
				return true
			}
			continue
		}
		if slices.ContainsFunc(codeSpan.FindAllString(line, -1), closingKeyword.MatchString) {
			return true
		}
	}
	return false
}

// attemptedCommand echoes the command that was refused, in a form that can be
// pasted back into a shell.
func attemptedCommand(argv []string) string {
	quoted := make([]string, 0, len(argv)+1)
	quoted = append(quoted, "gh")
	for _, arg := range argv {
		quoted = append(quoted, shellQuote(arg))
	}
	return strings.Join(quoted, " ")
}

// backslashed are the characters bash 3.2's printf %q escapes in the middle of
// a word. The set is what that shell produces rather than a notion of shell
// metacharacters, which would get two of them wrong in opposite directions: the
// comma is escaped and is not a metacharacter, and # is one and is passed
// through except at the start.
var backslashed = charSet(" !\"$&'()*,;<>?[\\]^`{|}")

// charSet indexes the bytes of s, so that a refusal quoting a long body does
// not search a string once per character of it.
func charSet(s string) (set [256]bool) {
	for i := 0; i < len(s); i++ {
		set[s[i]] = true
	}
	return set
}

// cEscapes are the names bash gives the control characters inside $'...'.
var cEscapes = map[byte]byte{
	'\a': 'a', '\b': 'b', 0x1b: 'E', '\f': 'f', '\n': 'n', '\r': 'r', '\t': 't', '\v': 'v',
}

// shellQuote renders one argument the way bash's printf %q did, except for
// non-ASCII: of a UTF-8 sequence bash 3.2 escapes only the bytes from 0x80 to
// 0x9f and leaves the rest raw, so a Japanese title came back as an unreadable
// mixture of octal and text. That is a defect of an interpreter written before
// multibyte support rather than behaviour to keep, so it is passed through.
func shellQuote(s string) string {
	if s == "" {
		return "''"
	}
	if strings.ContainsFunc(s, func(r rune) bool { return r < 0x20 || r == 0x7f }) {
		return dollarQuote(s)
	}

	var b strings.Builder
	b.Grow(len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		if backslashed[c] || (c == '#' && i == 0) {
			b.WriteByte('\\')
		}
		b.WriteByte(c)
	}
	return b.String()
}

// dollarQuote is the $'...' form, which is the only one that can carry a
// control character.
func dollarQuote(s string) string {
	var b strings.Builder
	b.Grow(len(s) + 3)
	b.WriteString("$'")
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c == '\'' || c == '\\':
			b.WriteByte('\\')
			b.WriteByte(c)
		case cEscapes[c] != 0:
			b.WriteByte('\\')
			b.WriteByte(cEscapes[c])
		case c < 0x20 || c == 0x7f:
			fmt.Fprintf(&b, `\%03o`, c)
		default:
			b.WriteByte(c)
		}
	}
	b.WriteByte('\'')
	return b.String()
}
