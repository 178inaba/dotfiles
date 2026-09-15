package skill_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/skill"
)

func TestMeasureSizeFile(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		want skill.Measurement
	}{
		{
			// One class at a time, a thousand characters of it, so that each
			// weight shows up in the estimate on its own.
			name: "non-ASCII characters",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("あ", 1000),
			want: skill.Measurement{Lines: 1, EstimatedTokens: 957},
		},
		{
			name: "ASCII letters and digits",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a1", 500),
			want: skill.Measurement{Lines: 1, EstimatedTokens: 190},
		},
		{
			name: "other ASCII characters",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("-`", 500),
			want: skill.Measurement{Lines: 1, EstimatedTokens: 1139},
		},
		{
			name: "ASCII whitespace",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat(" \t", 500),
			want: skill.Measurement{Lines: 1, EstimatedTokens: 642},
		},
		{
			// Whitespace outside ASCII is a non-ASCII character, not
			// whitespace: the classes are by code point first.
			name: "a no-break space counts as non-ASCII",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("\u00a0", 1000),
			want: skill.Measurement{Lines: 1, EstimatedTokens: 957},
		},
		{
			// 2 non-ASCII, 3 letters and digits, 1 other ASCII and 2 whitespace
			// come to 4.907, rounded to the nearest integer.
			name: "a body mixing every class",
			body: "---\nname: x\ndescription: y\n---\nab1 あ-\u00a0\n",
			want: skill.Measurement{Lines: 1, EstimatedTokens: 5},
		},
		{
			// A fenced example is not special: it is prose to this check, the
			// same as ccx skill contract treats it as a reference.
			name: "fenced content counts like any other line",
			body: "---\nname: x\ndescription: y\n---\nprose\n```\nfenced\n```\n",
			want: skill.Measurement{Lines: 4, EstimatedTokens: 11},
		},
		{
			// The last line still counts even with nothing to close it.
			name: "a body with no trailing newline",
			body: "---\nname: x\ndescription: y\n---\nline one\nline two",
			want: skill.Measurement{Lines: 2, EstimatedTokens: 5},
		},
		{
			name: "exactly the line guide is not over it",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a\n", skill.LineGuide),
			want: skill.Measurement{Lines: skill.LineGuide, EstimatedTokens: 416, OverLineGuide: false},
		},
		{
			name: "one line over the guide is over it",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a\n", skill.LineGuide+1),
			want: skill.Measurement{Lines: skill.LineGuide + 1, EstimatedTokens: 417, OverLineGuide: true},
		},
		{
			// 26,318 letters estimate at 5,000.42, which rounds to the guide.
			name: "an estimate of exactly the token guide is not over it",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a", 26318),
			want: skill.Measurement{Lines: 1, EstimatedTokens: skill.TokenGuide, OverTokenGuide: false},
		},
		{
			// One letter more is 5,000.61, which rounds past it.
			name: "an estimate one token over the guide is over it",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a", 26319),
			want: skill.Measurement{Lines: 1, EstimatedTokens: skill.TokenGuide + 1, OverTokenGuide: true},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			path := write(t, t.TempDir(), "x", tc.body)
			got, err := skill.MeasureSize(path)
			if err != nil {
				t.Fatalf("MeasureSize: %v", err)
			}

			tc.want.File = "x/SKILL.md"
			if diff := cmp.Diff([]skill.Measurement{tc.want}, got.Skills); diff != "" {
				t.Errorf("skills (-want +got):\n%s", diff)
			}
			if got.Target != path {
				t.Errorf("target = %q, want the absolute path %q", got.Target, path)
			}
			if got.LineGuide != skill.LineGuide || got.TokenGuide != skill.TokenGuide {
				t.Errorf("guides = %d/%d, want %d/%d", got.LineGuide, got.TokenGuide, skill.LineGuide, skill.TokenGuide)
			}
			if len(got.Warnings) != 0 {
				t.Errorf("warnings = %v, want none", got.Warnings)
			}
		})
	}
}

func TestMeasureSizeWithNoFrontmatterBlock(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		// tokens is the estimate of the whole file.
		tokens int
	}{
		{name: "no closing fence", body: "---\nname: x\ndescription: y\nno closing fence here\n", tokens: 18},
		{name: "no opening fence", body: "just prose, no frontmatter at all\n", tokens: 10},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			path := write(t, t.TempDir(), "x", tc.body)
			got, err := skill.MeasureSize(path)
			if err != nil {
				t.Fatalf("MeasureSize: %v", err)
			}

			// The whole file, not just what would have been the body, since
			// there is no fence to say where a body would have started. Both
			// fixtures end in a newline, so their line count is the newline
			// count with no adjustment for a final line missing one.
			want := skill.Measurement{
				File: "x/SKILL.md", Lines: strings.Count(tc.body, "\n"), EstimatedTokens: tc.tokens,
			}
			if diff := cmp.Diff([]skill.Measurement{want}, got.Skills); diff != "" {
				t.Errorf("skills (-want +got):\n%s", diff)
			}
			if len(got.Warnings) != 1 || !strings.Contains(got.Warnings[0], "x/SKILL.md") {
				t.Errorf("warnings = %v, want one mentioning x/SKILL.md", got.Warnings)
			}
		})
	}
}

func TestMeasureSizeDirectory(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	write(t, root, "short", "---\nname: short\ndescription: d\n---\na\n")
	write(t, root, "long", "---\nname: long\ndescription: d\n---\n"+strings.Repeat("a\n", skill.LineGuide+1))
	// A directory with no SKILL.md is not a violation, but skipping it in
	// silence would hide that it went unchecked — the same rule
	// CheckFrontmatter follows.
	if err := os.MkdirAll(filepath.Join(root, "empty"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	got, err := skill.MeasureSize(root)
	if err != nil {
		t.Fatalf("MeasureSize: %v", err)
	}

	// Sorted by file, the same order CheckFrontmatter's findings are in.
	want := []skill.Measurement{
		{File: "long/SKILL.md", Lines: skill.LineGuide + 1, EstimatedTokens: 417, OverLineGuide: true},
		{File: "short/SKILL.md", Lines: 1, EstimatedTokens: 1},
	}
	if diff := cmp.Diff(want, got.Skills); diff != "" {
		t.Errorf("skills (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff([]string{"no SKILL.md in empty/"}, got.Warnings); diff != "" {
		t.Errorf("warnings (-want +got):\n%s", diff)
	}
	if got.Target != root {
		t.Errorf("target = %q, want %q", got.Target, root)
	}
}

func TestMeasureSizeFails(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	if err := os.MkdirAll(filepath.Join(root, "empty"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	tests := []struct {
		name    string
		target  string
		wantErr string
	}{
		{name: "a target that is not there", target: filepath.Join(root, "nope"), wantErr: "target not found"},
		{name: "a directory holding no skills", target: root, wantErr: "no */SKILL.md found"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := skill.MeasureSize(tc.target)
			if err == nil {
				t.Fatalf("MeasureSize = %+v, want an error mentioning %q", got, tc.wantErr)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
			}
		})
	}
}

func TestOverGuide(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		size skill.Measurement
		want bool
	}{
		{name: "over the line guide only", size: skill.Measurement{OverLineGuide: true}, want: true},
		{name: "over the token guide only", size: skill.Measurement{OverTokenGuide: true}, want: true},
		{name: "over neither guide", size: skill.Measurement{}, want: false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			if got := tc.size.OverGuide(); got != tc.want {
				t.Errorf("OverGuide() = %v, want %v", got, tc.want)
			}
		})
	}
}
