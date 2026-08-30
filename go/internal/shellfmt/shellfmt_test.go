package shellfmt

import "testing"

// The want values below were taken from bash 3.2.57 (/bin/bash, the one the
// shebang of the script being replaced selects) and the macOS awk, so this file
// is the record of what "byte-identical output" actually meant.

func TestTruncateDecimal(t *testing.T) {
	tests := []struct{ in, want string }{
		// ${v%%.*} cuts at the first dot: a truncation, never a rounding, so
		// 42.9 percent of the context window still reads as 42%.
		{in: "42.5", want: "42"},
		{in: "42.9", want: "42"},
		{in: "42", want: "42"},
		{in: "", want: ""},
		{in: "0.9", want: "0"},
		{in: "1.2.3", want: "1"},
		{in: ".5", want: ""},
	}
	for _, tt := range tests {
		if got := TruncateDecimal(tt.in); got != tt.want {
			t.Errorf("TruncateDecimal(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func TestRound(t *testing.T) {
	tests := []struct{ in, want string }{
		// C printf rounds half to even, and so does Go's fmt.
		{in: "0.5", want: "0"},
		{in: "1.5", want: "2"},
		{in: "2.5", want: "2"},
		{in: "72.5", want: "72"},
		{in: "35", want: "35"},
		{in: " 4.5 ", want: "4"},
		{in: "1e2", want: "100"},
		// bash parses the whole string, so trailing garbage is not a number at
		// all and prints as zero. awk disagrees; see AwkNumber.
		{in: "1.23xyz", want: "0"},
		{in: "abc", want: "0"},
		{in: "", want: "0"},
	}
	for _, tt := range tests {
		if got := Round(tt.in); got != tt.want {
			t.Errorf("Round(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func TestMoney(t *testing.T) {
	tests := []struct{ in, want string }{
		{in: "1.23", want: "1.23"},
		{in: "1.005", want: "1.00"},
		{in: "0", want: "0.00"},
		{in: "abc", want: "0.00"},
		{in: "", want: "0.00"},
		{in: "-3.7", want: "-3.70"},
	}
	for _, tt := range tests {
		if got := Money(tt.in); got != tt.want {
			t.Errorf("Money(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func TestAwkNumber(t *testing.T) {
	tests := []struct {
		in   string
		want float64
	}{
		// awk coerces a string by reading its longest numeric prefix, which is
		// why the cost threshold and the yen conversion accept what bash's
		// printf would call zero.
		{in: "1.23xyz", want: 1.23},
		{in: "1.23", want: 1.23},
		{in: " 4.5 ", want: 4.5},
		{in: "1e2", want: 100},
		{in: ".5", want: 0.5},
		{in: "-3.7", want: -3.7},
		{in: "abc", want: 0},
		{in: "", want: 0},
	}
	for _, tt := range tests {
		if got := AwkNumber(tt.in); got != tt.want {
			t.Errorf("AwkNumber(%q) = %v, want %v", tt.in, got, tt.want)
		}
	}
}

func TestCapture(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want string
	}{
		// $(...) drops every trailing newline and nothing else, so inner blank
		// lines and leading spaces survive.
		{name: "one trailing newline", in: "main\n", want: "main"},
		{name: "several trailing newlines", in: "main\n\n\n", want: "main"},
		{name: "nothing but newlines", in: "\n\n", want: ""},
		{name: "inner newlines are kept", in: "a\nb\n", want: "a\nb"},
		{name: "spaces are not trimmed", in: "  x  \n", want: "  x  "},
		{name: "empty", in: "", want: ""},
	}
	for _, tt := range tests {
		if got := Capture([]byte(tt.in)); got != tt.want {
			t.Errorf("%s: Capture(%q) = %q, want %q", tt.name, tt.in, got, tt.want)
		}
	}
}

func TestAbbreviateHome(t *testing.T) {
	tests := []struct {
		name       string
		path, home string
		want       string
	}{
		{name: "under home", path: "/Users/x/proj", home: "/Users/x", want: "~/proj"},
		{name: "home itself", path: "/Users/x", home: "/Users/x", want: "~"},
		// ${p/#$HOME/~} is a plain prefix replacement with no notion of a path
		// boundary, so a sibling directory whose name starts with home's is
		// mangled exactly like this.
		{name: "prefix without a boundary", path: "/Users/xyz", home: "/Users/x", want: "~yz"},
		{name: "elsewhere", path: "/tmp/a", home: "/Users/x", want: "/tmp/a"},
		// An empty pattern matches at position zero, so an unset HOME prefixes
		// every path with a tilde.
		{name: "empty home", path: "/Users/x/proj", home: "", want: "~/Users/x/proj"},
		{name: "not a prefix", path: "/Users", home: "/Users/x", want: "/Users"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := AbbreviateHome(tt.path, tt.home); got != tt.want {
				t.Errorf("AbbreviateHome(%q, %q) = %q, want %q", tt.path, tt.home, got, tt.want)
			}
		})
	}
}
