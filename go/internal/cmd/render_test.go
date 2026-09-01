package cmd

import (
	"bytes"
	"flag"
	"os"
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"
)

var update = flag.Bool("update", false, "update .golden files")

// sample carries every shape the script subcommands' output contracts use, so
// that one pair of goldens covers the whole format: a nested object, a null
// from a nil pointer beside a value from a set one, an array of objects, an
// empty array from a nil slice, and a string holding the characters the
// encoders disagree about.
type sample struct {
	Repo     string   `json:"repo"`
	Parent   *ref     `json:"parent"`
	Self     *ref     `json:"self"`
	Subs     []ref    `json:"sub_issues"`
	Summary  summary  `json:"summary"`
	Closed   bool     `json:"all_closed"`
	Warnings []string `json:"warnings"`
	Note     string   `json:"note"`
}

type ref struct {
	Number int    `json:"number"`
	Title  string `json:"title"`
	Line   *int   `json:"line"`
}

type summary struct {
	Total     int `json:"total"`
	Completed int `json:"completed"`
}

func fixture() sample {
	return sample{
		Repo:   "178inaba/dotfiles",
		Parent: nil,
		Self:   &ref{Number: 121, Title: "Port the scripts", Line: new(30)},
		Subs: []ref{
			{Number: 122, Title: `a "quoted" & <angled> \ backslash`, Line: nil},
			{Number: 123, Title: "tab\tand newline\nand 日本語", Line: new(0)},
		},
		Summary:  summary{Total: 2, Completed: 1},
		Closed:   false,
		Warnings: nil,
		Note:     "",
	}
}

func TestRenderJSON(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		render func(*bytes.Buffer, any) error
		golden string
	}{
		{name: "indented", render: func(b *bytes.Buffer, v any) error { return renderJSON(b, v) }, golden: "render.golden"},
		{name: "compact", render: func(b *bytes.Buffer, v any) error { return renderCompactJSON(b, v) }, golden: "render-compact.golden"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var got bytes.Buffer
			if err := tt.render(&got, fixture()); err != nil {
				t.Fatalf("render: %v", err)
			}

			path := filepath.Join("testdata", tt.golden)
			if *update {
				if err := os.WriteFile(path, got.Bytes(), 0o644); err != nil {
					t.Fatalf("WriteFile(%q): %v", path, err)
				}
			}
			want, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("ReadFile(%q): %v", path, err)
			}
			if diff := cmp.Diff(string(want), got.String()); diff != "" {
				t.Errorf("render(fixture) differs from %s (-want +got):\n%s", tt.golden, diff)
			}
		})
	}
}
