// Package shellfmt reproduces the string and number handling of the bash
// script this module replaces.
//
// The statusline has to render byte-for-byte what the shell version rendered,
// and several of its conversions are shell- or awk-specific rather than
// arithmetic: a percentage is truncated at the dot instead of rounded, two
// different numeric parsers disagree about trailing garbage, and a path is
// abbreviated by a prefix substitution with no notion of a path boundary.
// Keeping them here, with the behaviour pinned by tests, stops any one of them
// from being quietly "cleaned up" at a call site.
package shellfmt

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// TruncateDecimal is ${v%%.*}: everything before the first dot.
//
// It is a truncation and not a rounding, which is why 42.9 percent of the
// context window renders as 42%.
func TruncateDecimal(s string) string {
	before, _, _ := strings.Cut(s, ".")
	return before
}

// Round is printf "%.0f". Both C and Go round half to even, so 2.5 is 2.
func Round(s string) string {
	return fmt.Sprintf("%.0f", bashNumber(s))
}

// RoundFloat is printf "%.0f" applied to a value that has already been parsed,
// for the two places the shell reached for awk to multiply before formatting.
func RoundFloat(f float64) string {
	return fmt.Sprintf("%.0f", f)
}

// Money is printf "%.2f".
func Money(s string) string {
	return fmt.Sprintf("%.2f", bashNumber(s))
}

// bashNumber parses the way bash's printf does: the whole string, surrounding
// blanks aside, and zero when that is not a number. "1.23xyz" is zero here and
// 1.23 to awk, and the shell script relies on both.
//
// Known divergence: bash also accepts C hexadecimal floats such as 0x10, which
// Go's parser rejects without a binary exponent. No field of the status line's
// input has ever carried one.
func bashNumber(s string) float64 {
	f, err := strconv.ParseFloat(strings.TrimSpace(s), 64)
	if err != nil {
		return 0
	}
	return f
}

// awkPrefix is awk's idea of a number at the start of a string.
var awkPrefix = regexp.MustCompile(`^[ \t\n]*[+-]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?`)

// AwkNumber coerces a string the way awk does, by reading its longest numeric
// prefix and yielding zero when there is not one.
func AwkNumber(s string) float64 {
	m := awkPrefix.FindString(s)
	if m == "" {
		return 0
	}
	f, err := strconv.ParseFloat(strings.TrimLeft(m, " \t\n"), 64)
	if err != nil {
		return 0
	}
	return f
}

// Capture is $(command): the output with every trailing newline removed, and
// nothing else touched. Trimming whitespace instead would silently accept
// output the shell would have kept.
func Capture(out []byte) string {
	return strings.TrimRight(string(out), "\n")
}

// AbbreviateHome is ${path/#$HOME/~} as bash 3.2 performs it.
//
// The version matters. bash 5 expands the tilde in the replacement and hands
// back the absolute path unchanged; bash 3.2 leaves it literal and produces the
// abbreviated form. The script runs under /bin/bash, which on macOS is 3.2, so
// the abbreviated form is what the status line has always shown — even though
// its own test suite ran under the newer bash on PATH and only ever exercised
// the other branch.
//
// It is a plain prefix substitution: no path boundary is required, so a home of
// /Users/x turns /Users/xyz into ~yz, and an empty home matches at position
// zero and prefixes everything.
//
// Known divergence: bash treats the pattern as a glob, so a home directory
// containing ? or * would match more than itself. Reproducing that would make
// the common case slower for no one's benefit.
func AbbreviateHome(path, home string) string {
	if strings.HasPrefix(path, home) {
		return "~" + path[len(home):]
	}
	return path
}
