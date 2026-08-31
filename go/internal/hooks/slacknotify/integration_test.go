package slacknotify

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// TestProjectAgainstRealGit is what the table over label cannot replace: it is
// the only thing that proves rev-parse's two paths are the pair the label was
// written against, in every tree layout a session actually runs in. The shell
// suite this replaces built the same fixtures for the same reason.
func TestProjectAgainstRealGit(t *testing.T) {
	t.Parallel()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}

	base := t.TempDir()
	main := filepath.Join(base, "myrepo")
	initRepo(t, main)
	mkdir(t, filepath.Join(main, "api"))
	git(t, main, "worktree", "add", "-b", "wt-feature", filepath.Join(main, ".claude", "worktrees", "feature-x"))
	git(t, main, "worktree", "add", "-b", "wt-nested", filepath.Join(main, ".claude", "worktrees", "feat", "nested"))
	git(t, main, "worktree", "add", "-b", "wt-manual", filepath.Join(base, "manual-wt"))
	mkdir(t, filepath.Join(main, ".claude", "worktrees", "feature-x", "api"))

	bare := filepath.Join(base, "bare.git")
	git(t, base, "clone", "--bare", main, bare)
	git(t, bare, "worktree", "add", "-b", "wt-bare", filepath.Join(base, "bare-wt"))

	outside := filepath.Join(base, "outside")
	mkdir(t, outside)

	tests := []struct {
		name string
		dir  string
		want string
	}{
		{"main tree", main, "myrepo"},
		{"subdirectory of the main tree", filepath.Join(main, "api"), "myrepo"},
		{"worktree", filepath.Join(main, ".claude", "worktrees", "feature-x"), "myrepo:feature-x"},
		{"subdirectory of a worktree", filepath.Join(main, ".claude", "worktrees", "feature-x", "api"), "myrepo:feature-x"},
		{"worktree named with a slash", filepath.Join(main, ".claude", "worktrees", "feat", "nested"), "myrepo:feat/nested"},
		{"worktree somewhere else", filepath.Join(base, "manual-wt"), "myrepo:manual-wt"},
		{"worktree of a bare repository", filepath.Join(base, "bare-wt"), "bare:bare-wt"},
		{"no repository at all", outside, "outside"},
	}

	h := New(Deps{Runner: runner.Exec{}})
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := h.project(t.Context(), tt.dir); got != tt.want {
				t.Errorf("project = %q, want %q", got, tt.want)
			}
		})
	}
}

func initRepo(t *testing.T, dir string) {
	t.Helper()
	mkdir(t, dir)
	git(t, dir, "init", "-q")
	git(t, dir, "config", "user.email", "test@example.com")
	git(t, dir, "config", "user.name", "test")
	if err := os.WriteFile(filepath.Join(dir, "file.txt"), []byte("x\n"), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	git(t, dir, "add", ".")
	git(t, dir, "commit", "-qm", "first")
}

func mkdir(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
}

func git(t *testing.T, dir string, args ...string) {
	t.Helper()
	cmd := exec.Command("git", args...)
	cmd.Dir = dir
	// The user's own configuration must not reach the fixtures, and this is set
	// on the command rather than with t.Setenv so the test can stay parallel.
	cmd.Env = append(os.Environ(), "GIT_CONFIG_GLOBAL="+os.DevNull, "GIT_CONFIG_SYSTEM="+os.DevNull)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git %v: %v\n%s", args, err, out)
	}
}
