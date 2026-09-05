package plandocs

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

// What counts as a reference, at the level of one document's text. The walk's
// own tests cover what is then done with them; these cover the reading, which
// is where a form the harness treats one way and this treats another would go
// unnoticed.
func TestReferences(t *testing.T) {
	tests := map[string]struct {
		text string
		want []reference
	}{
		"both forms, in the order written": {
			text: "@a.md then [b](b.md)\n[c](c.md) then @d.md\n",
			want: []reference{
				{target: "a.md", isImport: true},
				{target: "b.md"},
				{target: "c.md"},
				{target: "d.md", isImport: true},
			},
		},
		"a link title is not part of the path": {
			text: `[x](docs/x.md "The Title")`,
			want: []reference{{target: "docs/x.md"}},
		},
		"a fragment comes off": {
			text: "[x](docs/x.md#heading) and @docs/x.md#other\n",
			want: []reference{{target: "docs/x.md"}, {target: "docs/x.md", isImport: true}},
		},
		"a bare anchor names no path": {text: "[here](#section)"},
		"a reference-style link is out of scope": {
			text: "[x][ref]\n\n[ref]: docs/x.md\n",
		},
		"an import in prose leaves the sentence behind": {
			text: "see @docs/x.md. and @docs/z.md,\n",
			want: []reference{
				{target: "docs/x.md", isImport: true},
				{target: "docs/z.md", isImport: true},
			},
		},
		// The same rule that excludes an e-mail address: only whitespace
		// opens an import.
		"an @ that does not open a word is not an import": {text: "(@docs/y.md)"},
		"an e-mail address is not an import":              {text: "write to someone@example.com"},
		"a label is read once, as the link it is in": {
			text: "[see @docs/y.md](docs/x.md)",
			want: []reference{{target: "docs/x.md"}},
		},
		"a code span is a mention": {text: "a mention of `@a.md` and `[b](b.md)`"},
		"a span keeps the width it took": {
			text: "`code` @a.md",
			want: []reference{{target: "a.md", isImport: true}},
		},
		"a multi-backtick span holds a backtick": {
			text: "``a ` @a.md`` and @b.md",
			want: []reference{{target: "b.md", isImport: true}},
		},
		// A run with no closing run of the same length opens no span, so what
		// follows it is prose — which is what CommonMark says and what the
		// import parser has to agree with.
		"an unclosed backtick opens no span": {
			text: "` @a.md and [b](b.md)",
			want: []reference{{target: "a.md", isImport: true}, {target: "b.md"}},
		},
		"a backtick fence is a mention": {
			text: "```\n@a.md\n[b](b.md)\n```\n@c.md\n",
			want: []reference{{target: "c.md", isImport: true}},
		},
		"a tilde fence is a mention": {
			text: "~~~\n@a.md\n~~~\n@c.md\n",
			want: []reference{{target: "c.md", isImport: true}},
		},
		"only the fence that opened a block closes it": {
			text: "~~~\n```\n@a.md\n```\n~~~\n@c.md\n",
			want: []reference{{target: "c.md", isImport: true}},
		},
		"CRLF does not hide a fence": {
			text: "```\r\n@a.md\r\n```\r\n@c.md\r\n",
			want: []reference{{target: "c.md", isImport: true}},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			if diff := cmp.Diff(tt.want, references(tt.text), cmp.AllowUnexported(reference{})); diff != "" {
				t.Errorf("references() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// Where a target is read from, which decides whether the walk lands in the
// project, in the home directory, or nowhere.
func TestResolve(t *testing.T) {
	const file = "/repo/docs/a.md"

	tests := map[string]struct {
		target string
		want   string
	}{
		"beside the file that links it": {target: "b.md", want: "/repo/docs/b.md"},
		"up out of the directory":       {target: "../CLAUDE.md", want: "/repo/CLAUDE.md"},
		// Not the repository root GitHub would render it against; see resolve.
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
