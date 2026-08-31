package skillcheck

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// TestRunAgainstTheRealChecker is what the fakes cannot show: that the script
// is where this expects it, that it is started the way it expects, and that
// what it answers with is what this knows how to read. The one-implementation
// rule makes that seam the whole hook.
func TestRunAgainstTheRealChecker(t *testing.T) {
	t.Parallel()
	if _, err := exec.LookPath("yq"); err != nil {
		t.Skip("yq is not installed")
	}
	// The tests run in the package directory, four levels below the root; the
	// stow symlink Default resolves is not there to follow on a CI checkout.
	path, err := filepath.Abs(filepath.Join("..", "..", "..", "..", checker))
	if err != nil {
		t.Fatalf("Abs: %v", err)
	}
	if _, err := os.Stat(path); err != nil {
		t.Fatalf("Stat: %v", err)
	}

	tests := []struct {
		name string
		// skill is the directory name, and lines are its frontmatter.
		skill string
		lines []string
		want  hooks.Decision
		// stderr is what the message has to carry, and is only checked when the
		// decision is to block.
		stderr []string
	}{
		{
			name: "a quoted value is fine", skill: "clean",
			lines: []string{"name: clean", "description: a clean skill", `argument-hint: "[--yes]"`},
		},
		{
			// The violation that went unnoticed in two files and is the reason
			// this hook exists.
			name: "an unquoted flow value", skill: "seqhint",
			lines:  []string{"name: seqhint", "description: a skill", "argument-hint: [--yes]"},
			want:   hooks.Block,
			stderr: []string{"unquoted_flow", "argument-hint", "line 4"},
		},
		{
			name: "frontmatter that will not parse", skill: "badyaml",
			lines:  []string{"name: badyaml", "description: a skill", "argument-hint: [<a>] [--b]"},
			want:   hooks.Block,
			stderr: []string{"invalid_yaml"},
		},
		{
			name: "a missing description", skill: "nodesc",
			lines:  []string{"name: nodesc"},
			want:   hooks.Block,
			stderr: []string{"missing_field", "description"},
		},
		{
			name: "a name that does not match the directory", skill: "mismatched",
			lines:  []string{"name: something-else", "description: a skill"},
			want:   hooks.Block,
			stderr: []string{"name_mismatch", "mismatched", "something-else"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			target := filepath.Join(t.TempDir(), tt.skill, "SKILL.md")
			if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
				t.Fatalf("MkdirAll: %v", err)
			}
			body := "---\n" + strings.Join(tt.lines, "\n") + "\n---\n\n# /" + tt.skill + "\n"
			if err := os.WriteFile(target, []byte(body), 0o644); err != nil {
				t.Fatalf("WriteFile: %v", err)
			}

			var stderr strings.Builder
			got := New(realDeps(path)).Run(t.Context(), edit(target), &stderr)
			if got.Decision != tt.want {
				t.Fatalf("Decision = %d, want %d (stderr=%q)", got.Decision, tt.want, stderr.String())
			}
			if tt.want == hooks.Allow {
				if stderr.Len() != 0 {
					t.Errorf("stderr = %q, want empty", stderr.String())
				}
				return
			}
			for _, want := range append(tt.stderr, target) {
				if !strings.Contains(stderr.String(), want) {
					t.Errorf("stderr does not contain %q:\n%s", want, stderr.String())
				}
			}
		})
	}

	t.Run("a file that is not there", func(t *testing.T) {
		t.Parallel()
		var stderr strings.Builder
		target := filepath.Join(t.TempDir(), "ghost", "SKILL.md")
		if got := New(realDeps(path)).Run(t.Context(), edit(target), &stderr); got.Decision != hooks.Block {
			t.Fatalf("Decision = %d, want %d", got.Decision, hooks.Block)
		}
		if !strings.Contains(stderr.String(), "was not checked") {
			t.Errorf("stderr does not say the check did not happen:\n%s", stderr.String())
		}
	})
}

func realDeps(path string) Deps {
	return Deps{Runner: runner.Exec{}, Script: func() (string, bool) { return path, true }}
}
