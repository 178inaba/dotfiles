package ghshim

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

// The three body rules, and the quoting the messages echo commands with. What
// counts as prose and what counts as code is ghmd's to say, because `ccx issue
// publish` writes bodies this shim never sees and has to reach the same
// verdict about them; what is left here is what this shim decides once it has
// been told.

// GitHub reads only the direct adjacency of keyword, optional colon, space and
// reference, so the detection is limited to it as well.
//
// The same knowledge is encoded in pullrequest.closingKeyword, which reads a
// body for the issues it closes; if GitHub ever changes the set, both have to
// move.
var closingKeyword = regexp.MustCompile(
	`(?i)(^|[^[:alnum:]])(close[sd]?|fix(e[sd])?|resolve[sd]?):?[[:space:]]+([[:alnum:]_.-]+/[[:alnum:]_.-]+)?#[0-9]+`)

// hasQuotedClosingKeyword reports whether body holds a closing keyword where
// GitHub will not read it as one: inside a fence, or inside a code span.
func hasQuotedClosingKeyword(body string) bool {
	for s := range ghmd.Segments(body) {
		if s.Kind != ghmd.Prose && closingKeyword.MatchString(body[s.Start:s.End]) {
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
