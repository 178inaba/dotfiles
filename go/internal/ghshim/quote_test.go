package ghshim

import "testing"

// TestShellQuote pins the forms bash 3.2.57 — the interpreter the shell shim
// ran under — produces for printf %q. The goldens only reach two of them
// through the messages that echo a command, so the set is held here instead:
// it was arrived at by measurement rather than by reasoning about which
// characters a shell treats specially, and reasoning would get two of them
// backwards in opposite directions.
func TestShellQuote(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		in   string
		want string
	}{
		{name: "empty", in: "", want: "''"},
		{name: "nothing to escape", in: "plain", want: "plain"},
		{name: "a space", in: "a b", want: `a\ b`},
		{name: "a dollar", in: "x$y", want: `x\$y`},
		{name: "a quote", in: "a'b", want: `a\'b`},
		{name: "a backslash", in: `a\b`, want: `a\\b`},
		{name: "a backtick", in: "a`b", want: "a\\`b"},

		// The comma is not a shell metacharacter and is escaped; the hash is
		// one and is not, except where it opens a word and would start a
		// comment.
		{name: "a comma", in: "note #1, #2", want: `note\ #1\,\ #2`},
		{name: "a leading hash", in: "#a", want: `\#a`},

		// Passed through: a path is the commonest argument there is, and none
		// of these needs quoting.
		{name: "a path", in: "/tmp/a-b.md", want: "/tmp/a-b.md"},
		{name: "a tilde", in: "~/x", want: "~/x"},
		{name: "an equals", in: "a=b", want: "a=b"},
		{name: "a colon and an at", in: "a:b@c", want: "a:b@c"},

		// A control character forces the $'...' form, which quotes the whole
		// argument and escapes differently inside it.
		{name: "a newline", in: "l1\nl2", want: `$'l1\nl2'`},
		{name: "a tab", in: "a\tb", want: `$'a\tb'`},
		{name: "an escape", in: "a\x1bb", want: `$'a\Eb'`},
		{name: "a delete", in: "a\x7fb", want: `$'a\177b'`},
		{name: "a space inside the C form", in: "a b\nc", want: `$'a b\nc'`},
		{name: "a quote inside the C form", in: "a'b\nc", want: `$'a\'b\nc'`},
		{name: "a backslash inside the C form", in: "a\\b\nc", want: `$'a\\b\nc'`},

		// Not reproduced: bash 3.2 escapes only the 0x80 to 0x9f bytes of a
		// UTF-8 sequence and leaves the rest raw, which is unreadable. See
		// shellQuote.
		{name: "non-ASCII is left alone", in: "日本語", want: "日本語"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			if got := shellQuote(tt.in); got != tt.want {
				t.Errorf("shellQuote(%q) = %q, want %q", tt.in, got, tt.want)
			}
		})
	}
}
