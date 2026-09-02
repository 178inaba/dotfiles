package skill_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/skill"
)

// tree writes a skills directory from a map of relative paths to contents.
func tree(t *testing.T, files map[string]string) string {
	t.Helper()

	root := t.TempDir()
	for rel, body := range files {
		path := filepath.Join(root, rel)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatalf("MkdirAll: %v", err)
		}
		if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
			t.Fatalf("WriteFile(%q): %v", path, err)
		}
	}
	return root
}

// skillFile wraps a body in the frontmatter a SKILL.md needs to be one.
func skillFile(name, body string) string {
	return "---\nname: " + name + "\ndescription: a skill\n---\n\n" + body
}

func TestCheckRefs(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		files map[string]string
		want  []skill.RefFinding
	}{
		{
			// A reference attaches its target and stops there, so the second
			// hop never arrives — the failure that ran a verification with its
			// convergence protocol unread.
			name: "a reference whose target references something else",
			files: map[string]string{
				"a/SKILL.md": skillFile("a", "See @~/.claude/skills/b/SKILL.md for the procedure\n"),
				"b/SKILL.md": skillFile("b", "Read @~/.claude/skills/c/SKILL.md first\n"),
				"c/SKILL.md": skillFile("c", "nothing\n"),
			},
			want: []skill.RefFinding{{
				Type: skill.UncoveredNested, File: "a/SKILL.md", Line: 6,
				Ref: "b/SKILL.md", Nested: "c/SKILL.md",
			}},
		},
		{
			// Referencing both directly is the way to keep the second one, and
			// is not a violation.
			name: "a reference that covers the second hop as well",
			files: map[string]string{
				"a/SKILL.md": skillFile("a", "@~/.claude/skills/b/SKILL.md and @~/.claude/skills/c/SKILL.md\n"),
				"b/SKILL.md": skillFile("b", "@~/.claude/skills/c/SKILL.md\n"),
				"c/SKILL.md": skillFile("c", "nothing\n"),
			},
		},
		{
			// Two skills pointing at each other is not a second hop that goes
			// unattached: the file is already the one being started.
			name: "skills that reference each other",
			files: map[string]string{
				"x/SKILL.md": skillFile("x", "@~/.claude/skills/y/SKILL.md\n"),
				"y/SKILL.md": skillFile("y", "@~/.claude/skills/x/SKILL.md\n"),
			},
		},
		{
			name: "a reference to a file that is not there",
			files: map[string]string{
				"d/SKILL.md": skillFile("d", "Missing: @~/.claude/skills/gone/SKILL.md\n"),
			},
			want: []skill.RefFinding{{Type: skill.MissingTarget, File: "d/SKILL.md", Line: 6, Ref: "gone/SKILL.md"}},
		},
		{
			// Inside backticks the `@` attaches nothing, so writing one there
			// suggests it was meant as a mention.
			name: "a reference inside inline code",
			files: map[string]string{
				"d/SKILL.md": skillFile("d", "Mentioned: `@~/.claude/skills/c/SKILL.md`\n"),
				"c/SKILL.md": skillFile("c", "nothing\n"),
			},
			want: []skill.RefFinding{{Type: skill.RefInCode, File: "d/SKILL.md", Line: 6, Ref: "c/SKILL.md"}},
		},
		{
			name: "a reference inside a fence",
			files: map[string]string{
				"d/SKILL.md": skillFile("d", "```\n@~/.claude/skills/c/SKILL.md\n```\n"),
				"c/SKILL.md": skillFile("c", "nothing\n"),
			},
			want: []skill.RefFinding{{Type: skill.RefInCode, File: "d/SKILL.md", Line: 7, Ref: "c/SKILL.md"}},
		},
		{
			// The path stops at the first character it cannot hold, so the
			// punctuation around a reference does not become part of it.
			name: "a reference followed by punctuation",
			files: map[string]string{
				"a/SKILL.md": skillFile("a", "手順は @~/.claude/skills/c/SKILL.md（参照）にある。\n"),
				"c/SKILL.md": skillFile("c", "nothing\n"),
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			root := tree(t, tc.files)
			got, err := skill.CheckRefs(root, skill.Contract{})
			if err != nil {
				t.Fatalf("CheckRefs: %v", err)
			}

			want := tc.want
			if want == nil {
				want = []skill.RefFinding{}
			}
			if diff := cmp.Diff(want, got.Violations); diff != "" {
				t.Errorf("violations (-want +got):\n%s", diff)
			}
			if got.SkillsDir != root {
				t.Errorf("skills_dir = %q, want %q", got.SkillsDir, root)
			}
		})
	}
}

// TestCheckRefsReadsBeyondSkillFiles is why the referenced files are read too:
// what a start attaches is the file itself, so its own references are the
// second hop even when it is not a SKILL.md.
func TestCheckRefsReadsBeyondSkillFiles(t *testing.T) {
	t.Parallel()

	root := tree(t, map[string]string{
		"a/SKILL.md":     skillFile("a", "@~/.claude/skills/a/reference.md\n"),
		"a/reference.md": "See @~/.claude/skills/c/SKILL.md too\n",
		"c/SKILL.md":     skillFile("c", "nothing\n"),
	})

	got, err := skill.CheckRefs(root, skill.Contract{})
	if err != nil {
		t.Fatalf("CheckRefs: %v", err)
	}
	want := []skill.RefFinding{{
		Type: skill.UncoveredNested, File: "a/SKILL.md", Line: 6,
		Ref: "a/reference.md", Nested: "c/SKILL.md",
	}}
	if diff := cmp.Diff(want, got.Violations); diff != "" {
		t.Errorf("violations (-want +got):\n%s", diff)
	}
}

func TestCheckRefsFails(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	if err := os.MkdirAll(filepath.Join(root, "sub"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	tests := []struct {
		name    string
		target  string
		wantErr string
	}{
		{name: "a directory that is not there", target: filepath.Join(root, "nope"), wantErr: "skills directory not found"},
		{name: "a directory holding no skills", target: root, wantErr: "no */SKILL.md found"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := skill.CheckRefs(tc.target, skill.Contract{})
			if err == nil {
				t.Fatalf("CheckRefs = %+v, want an error mentioning %q", got, tc.wantErr)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
			}
		})
	}
}
