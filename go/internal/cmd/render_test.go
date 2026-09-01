package cmd

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
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

// TestGoldensAreJQShaped is the half of the parity claim that a committed file
// cannot make on its own: it checks that the goldens are what jq itself would
// print. Feeding a golden back through jq has to be a fixed point, since jq
// reformats whatever it reads — so if the indent, the escaping or the trailing
// newline ever stopped matching, this fails while the goldens still agree with
// the Go output.
//
// It reads the goldens rather than the Go output so that there is no second
// jq program to keep in step with the fixture, and it skips where jq is absent:
// this repository is removing its last runtime dependency on jq, and the other
// test is what guards the format everywhere.
func TestGoldensAreJQShaped(t *testing.T) {
	t.Parallel()

	if _, err := exec.LookPath("jq"); err != nil {
		t.Skip("jq is not installed")
	}

	pretty := filepath.Join("testdata", "render.golden")
	tree := filepath.Join("testdata", "issue-tree.golden")
	annotated := filepath.Join("testdata", "issue-tree-annotated.golden")
	tests := []struct {
		name   string
		args   []string
		golden string
	}{
		{name: "indented", args: []string{".", pretty}, golden: pretty},
		{name: "compact", args: []string{"-c", ".", pretty}, golden: filepath.Join("testdata", "render-compact.golden")},
		// The subcommands' own goldens go through the same fixed point, so a
		// contract recorded from a shell script stays comparable with what jq
		// would have printed for it.
		{name: "issue tree", args: []string{".", tree}, golden: tree},
		{name: "issue tree annotated", args: []string{".", annotated}, golden: annotated},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			out, err := exec.CommandContext(t.Context(), "jq", tt.args...).Output()
			if err != nil {
				t.Fatalf("jq %v: %v", tt.args, err)
			}
			want, err := os.ReadFile(tt.golden)
			if err != nil {
				t.Fatalf("ReadFile(%q): %v", tt.golden, err)
			}
			if diff := cmp.Diff(string(want), string(out)); diff != "" {
				t.Errorf("jq %v differs from %s (-want +got):\n%s", tt.args, tt.golden, diff)
			}
		})
	}
}
