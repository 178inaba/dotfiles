package plandocs

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

// What counts as an import, at the level of one document's text. Collect's own
// tests cover what is then done with them; these cover the reading, which is
// where a form the harness treats one way and this treats another would go
// unnoticed.
func TestImports(t *testing.T) {
	tests := map[string]struct {
		text string
		want []string
	}{
		"imports in the order written": {
			text: "@a.md then @b.md\n@c.md\n",
			want: []string{"a.md", "b.md", "c.md"},
		},
		"a link is not an import": {text: "[b](b.md)\n[c](c.md \"Title\")\n"},
		"a fragment comes off": {
			text: "@docs/x.md#other\n",
			want: []string{"docs/x.md"},
		},
		"a bare fragment names no path": {text: "@#section"},
		"an import in prose leaves the sentence behind": {
			text: "see @docs/x.md. and @docs/z.md,\n",
			want: []string{"docs/x.md", "docs/z.md"},
		},
		// Only whitespace opens an import, which is what excludes an e-mail
		// address.
		"an @ that does not open a word is not an import": {text: "(@docs/y.md)"},
		"an e-mail address is not an import":              {text: "write to someone@example.com"},
		// A link is matched only so that its label is not read as prose: an @
		// there would otherwise take the rest of the link as its path.
		"an @ in a link's label is not an import": {text: "[see @docs/y.md](docs/x.md) and [see @a.md now](b.md)"},
		"a code span hides an import":             {text: "`@a.md` and ``@b.md``"},
		"a span keeps the width it took": {
			text: "`code` @a.md",
			want: []string{"a.md"},
		},
		"a multi-backtick span holds a backtick": {
			text: "``a ` @a.md`` and @b.md",
			want: []string{"b.md"},
		},
		// A run with no closing run of the same length opens no span, so what
		// follows it is prose — which is what CommonMark says and what the
		// import parser has to agree with.
		"an unclosed backtick opens no span": {
			text: "` @a.md",
			want: []string{"a.md"},
		},
		"a backtick fence hides an import": {
			text: "```\n@a.md\n```\n@c.md\n",
			want: []string{"c.md"},
		},
		"a tilde fence hides an import": {
			text: "~~~\n@a.md\n~~~\n@c.md\n",
			want: []string{"c.md"},
		},
		"only the fence that opened a block closes it": {
			text: "~~~\n```\n@a.md\n```\n~~~\n@c.md\n",
			want: []string{"c.md"},
		},
		"CRLF does not hide a fence": {
			text: "```\r\n@a.md\r\n```\r\n@c.md\r\n",
			want: []string{"c.md"},
		},
		// The rest are places where CommonMark's reading of code, which this
		// delegates to, differs from a naive one. The last is ghmd's own
		// deliberate deviation from CommonMark, which would read it as an
		// indented code block. The run of two below has no partner and neither
		// does the run of three, so the two single runs are free to pair with
		// each other — a reading that finds a closing run by substring pairs
		// the two with the first two backticks of the three instead, and
		// uncovers the import.
		"a span after an unmatched run still hides its import": {
			text: "a ``b``` ` @x.md` c",
		},
		"a block opened by four backticks is not closed by three": {
			text: "````\n```\n@a.md\n````\n",
		},
		// CommonMark's example 145: a backtick fence's info string may not
		// hold a backtick, so this line opens no block and is a paragraph.
		"a backtick in an info string leaves the line a paragraph": {
			text: "```a` @x.md",
			want: []string{"x.md"},
		},
		"a closing run followed by text closes nothing": {
			text: "```\n@a.md\n``` x\n@b.md\n",
		},
		"a fence marker may be indented past three spaces": {
			text: "    ```\n@a.md\n    ```\n",
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			if diff := cmp.Diff(tt.want, imports(tt.text)); diff != "" {
				t.Errorf("imports() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// Where a target is read from, which decides whether an import lands in the
// project, in the home directory, or nowhere.
func TestResolve(t *testing.T) {
	const file = "/repo/docs/a.md"

	tests := map[string]struct {
		target string
		want   string
	}{
		"beside the file that imports it":        {target: "b.md", want: "/repo/docs/b.md"},
		"up out of the directory":                {target: "../CLAUDE.md", want: "/repo/CLAUDE.md"},
		"a leading slash is the filesystem root": {target: "/etc/x.md", want: "/etc/x.md"},
		"a tilde is the home directory":          {target: "~/x.md", want: "/home/x.md"},
		"a URL names no file":                    {target: "https://example.com/x.md"},
		"a mailto names no file":                 {target: "mailto:someone@example.com"},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got, ok := resolve(tt.target, file, "/home")
			if ok != (tt.want != "") {
				t.Fatalf("resolve(%q) ok = %v, want %v", tt.target, ok, tt.want != "")
			}
			if got != tt.want {
				t.Errorf("resolve(%q) = %q, want %q", tt.target, got, tt.want)
			}
		})
	}
}
