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
			// Non-whitespace characters are all outside ASCII, which is the
			// case the character guide exists for.
			name: "a Japanese body",
			body: "---\nname: x\ndescription: y\n---\nこれはテストです。\n",
			want: skill.Measurement{Lines: 1, Characters: 10, MostlyNonASCII: true},
		},
		{
			name: "an ASCII body",
			body: "---\nname: x\ndescription: y\n---\nThis is a test.\n",
			want: skill.Measurement{Lines: 1, Characters: 16, MostlyNonASCII: false},
		},
		{
			// A fenced example is not special: it is prose to this check, the
			// same as ccx skill contract treats it as a reference.
			name: "fenced content counts like any other line",
			body: "---\nname: x\ndescription: y\n---\nprose\n```\nfenced\n```\n",
			want: skill.Measurement{Lines: 4, Characters: 21, MostlyNonASCII: false},
		},
		{
			// The last line still counts even with nothing to close it.
			name: "a body with no trailing newline",
			body: "---\nname: x\ndescription: y\n---\nline one\nline two",
			want: skill.Measurement{Lines: 2, Characters: 17, MostlyNonASCII: false},
		},
		{
			name: "exactly the line guide is not over it",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a\n", skill.LineGuide),
			want: skill.Measurement{Lines: skill.LineGuide, Characters: skill.LineGuide * 2, OverLineGuide: false},
		},
		{
			name: "one line over the guide is over it",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a\n", skill.LineGuide+1),
			want: skill.Measurement{
				Lines: skill.LineGuide + 1, Characters: (skill.LineGuide + 1) * 2, OverLineGuide: true,
			},
		},
		{
			name: "exactly the character guide is not over it",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a", skill.CharacterGuide),
			want: skill.Measurement{Lines: 1, Characters: skill.CharacterGuide, OverCharacterGuide: false},
		},
		{
			name: "one character over the guide is over it",
			body: "---\nname: x\ndescription: y\n---\n" + strings.Repeat("a", skill.CharacterGuide+1),
			want: skill.Measurement{Lines: 1, Characters: skill.CharacterGuide + 1, OverCharacterGuide: true},
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
			if got.LineGuide != skill.LineGuide || got.CharacterGuide != skill.CharacterGuide {
				t.Errorf("guides = %d/%d, want %d/%d", got.LineGuide, got.CharacterGuide, skill.LineGuide, skill.CharacterGuide)
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
	}{
		{name: "no closing fence", body: "---\nname: x\ndescription: y\nno closing fence here\n"},
		{name: "no opening fence", body: "just prose, no frontmatter at all\n"},
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
				File: "x/SKILL.md", Lines: strings.Count(tc.body, "\n"), Characters: len([]rune(tc.body)),
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
		{File: "long/SKILL.md", Lines: skill.LineGuide + 1, Characters: (skill.LineGuide + 1) * 2, OverLineGuide: true},
		{File: "short/SKILL.md", Lines: 1, Characters: 2},
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

func TestOverApplicableGuide(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		size skill.Measurement
		want bool
	}{
		{
			name: "over the line guide, ASCII",
			size: skill.Measurement{OverLineGuide: true, MostlyNonASCII: false, OverCharacterGuide: false},
			want: true,
		},
		{
			name: "over the character guide, non-ASCII",
			size: skill.Measurement{OverLineGuide: false, MostlyNonASCII: true, OverCharacterGuide: true},
			want: true,
		},
		{
			// The character guide was derived from Japanese and does not
			// apply to an English body, however many characters it has.
			name: "over the character guide, ASCII",
			size: skill.Measurement{OverLineGuide: false, MostlyNonASCII: false, OverCharacterGuide: true},
			want: false,
		},
		{
			name: "over neither guide",
			size: skill.Measurement{OverLineGuide: false, MostlyNonASCII: true, OverCharacterGuide: false},
			want: false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			if got := tc.size.OverApplicableGuide(); got != tc.want {
				t.Errorf("OverApplicableGuide() = %v, want %v", got, tc.want)
			}
		})
	}
}
