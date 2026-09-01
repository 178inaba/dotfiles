package skill_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"

	"github.com/178inaba/dotfiles/go/internal/skill"
)

// write puts a SKILL.md holding body under a directory named for the skill.
func write(t *testing.T, root, name, body string) string {
	t.Helper()

	path := filepath.Join(root, name, "SKILL.md")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	return path
}

// ignoreMessage compares violations without the parser's own words, which are
// go-yaml's and not a contract.
var ignoreMessage = cmpopts.IgnoreFields(skill.Violation{}, "Message")

func TestCheckFrontmatterFile(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		skill string
		body  string
		want  []skill.Violation
	}{
		{
			name: "a well-formed skill", skill: "clean",
			body: "---\nname: clean\ndescription: a clean skill\nargument-hint: \"[--yes]\"\n---\n\nbody\n",
		},
		{
			// The violation that went unnoticed in two files: two flow
			// sequences on one line, which YAML will not parse at all.
			name: "frontmatter that will not parse", skill: "badyaml",
			body: "---\nname: badyaml\ndescription: a skill\nargument-hint: [<a>] [--b]\n---\n",
			// Nothing else is reported: there is no parsed value to judge the
			// file by, and the unquoted flow it also has stays unmentioned.
			want: []skill.Violation{{Type: skill.InvalidYAML, File: "badyaml/SKILL.md"}},
		},
		{
			name: "a value read as a sequence rather than a string", skill: "seqhint",
			body: "---\nname: seqhint\ndescription: a skill\nargument-hint: [--yes]\n---\n",
			want: []skill.Violation{{Type: skill.UnquotedFlow, File: "seqhint/SKILL.md", Key: "argument-hint", Line: 4}},
		},
		{
			// Every key, not the one that has gone wrong before: singling out
			// argument-hint would leave the next key free to repeat it.
			name: "a value read as a mapping", skill: "maphint",
			body: "---\nname: maphint\ndescription: a skill\nextra: {a: 1}\n---\n",
			want: []skill.Violation{{Type: skill.UnquotedFlow, File: "maphint/SKILL.md", Key: "extra", Line: 4}},
		},
		{
			name: "no description", skill: "nodesc",
			body: "---\nname: nodesc\n---\n",
			want: []skill.Violation{{Type: skill.MissingField, File: "nodesc/SKILL.md", Field: "description"}},
		},
		{
			name: "a name that is not the directory's", skill: "mismatched",
			body: "---\nname: something-else\ndescription: a skill\n---\n",
			want: []skill.Violation{{Type: skill.NameMismatch, File: "mismatched/SKILL.md", Expected: "mismatched", Actual: "something-else"}},
		},
		{
			// No frontmatter block at all is missing both fields rather than
			// being a kind of its own.
			name: "no frontmatter block", skill: "noblock",
			body: "just a body\n",
			want: []skill.Violation{
				{Type: skill.MissingField, File: "noblock/SKILL.md", Field: "name"},
				{Type: skill.MissingField, File: "noblock/SKILL.md", Field: "description"},
			},
		},
		{
			// An opening fence with no closing one is the same: the extent of
			// the block is read from the text, not guessed at.
			name: "an unterminated frontmatter block", skill: "unterminated",
			body: "---\nname: unterminated\ndescription: a skill\n",
			want: []skill.Violation{
				{Type: skill.MissingField, File: "unterminated/SKILL.md", Field: "name"},
				{Type: skill.MissingField, File: "unterminated/SKILL.md", Field: "description"},
			},
		},
		{
			// Frontmatter that parses to something other than a mapping has no
			// fields to find.
			name: "frontmatter that is a sequence", skill: "seq",
			body: "---\n- one\n- two\n---\n",
			want: []skill.Violation{
				{Type: skill.MissingField, File: "seq/SKILL.md", Field: "name"},
				{Type: skill.MissingField, File: "seq/SKILL.md", Field: "description"},
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			path := write(t, t.TempDir(), tc.skill, tc.body)
			got, err := skill.CheckFrontmatter(path)
			if err != nil {
				t.Fatalf("CheckFrontmatter: %v", err)
			}

			want := tc.want
			if want == nil {
				want = []skill.Violation{}
			}
			if diff := cmp.Diff(want, got.Violations, ignoreMessage); diff != "" {
				t.Errorf("violations (-want +got):\n%s", diff)
			}
			if got.Target != path {
				t.Errorf("target = %q, want the absolute path %q", got.Target, path)
			}
			for _, v := range got.Violations {
				if v.Type == skill.InvalidYAML && v.Message == "" {
					t.Error("invalid_yaml carries no message")
				}
			}
		})
	}
}

func TestCheckFrontmatterDirectory(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	write(t, root, "clean", "---\nname: clean\ndescription: a clean skill\n---\n")
	write(t, root, "seqhint", "---\nname: seqhint\ndescription: a skill\nargument-hint: [--yes]\n---\n")
	write(t, root, "nodesc", "---\nname: nodesc\n---\n")
	// A directory with no SKILL.md is not a violation, but skipping it in
	// silence would hide that it went unchecked.
	if err := os.MkdirAll(filepath.Join(root, "empty"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	got, err := skill.CheckFrontmatter(root)
	if err != nil {
		t.Fatalf("CheckFrontmatter: %v", err)
	}

	// Sorted by file, then by line, then by kind, so that two runs over the
	// same tree read the same way.
	want := []skill.Violation{
		{Type: skill.MissingField, File: "nodesc/SKILL.md", Field: "description"},
		{Type: skill.UnquotedFlow, File: "seqhint/SKILL.md", Key: "argument-hint", Line: 4},
	}
	if diff := cmp.Diff(want, got.Violations, ignoreMessage); diff != "" {
		t.Errorf("violations (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff([]string{"no SKILL.md in empty/"}, got.Warnings); diff != "" {
		t.Errorf("warnings (-want +got):\n%s", diff)
	}
}

func TestCheckFrontmatterFails(t *testing.T) {
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

			got, err := skill.CheckFrontmatter(tc.target)
			if err == nil {
				t.Fatalf("CheckFrontmatter = %+v, want an error mentioning %q", got, tc.wantErr)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
			}
		})
	}
}

// TestCheckFrontmatterOnThisRepository is what the fixtures cannot show: that
// every skill actually in this repository passes. It is the same check the
// skill-authoring skill documents as covering the real tree.
func TestCheckFrontmatterOnThisRepository(t *testing.T) {
	t.Parallel()

	skills := filepath.Join("..", "..", "..", "claude", ".claude", "skills")
	if _, err := os.Stat(skills); err != nil {
		t.Skipf("the repository's skills are not there: %v", err)
	}

	got, err := skill.CheckFrontmatter(skills)
	if err != nil {
		t.Fatalf("CheckFrontmatter: %v", err)
	}
	if len(got.Violations) != 0 {
		t.Errorf("this repository's own skills have violations: %+v", got.Violations)
	}
}
