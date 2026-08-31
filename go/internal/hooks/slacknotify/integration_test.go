package slacknotify

import (
	"path/filepath"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks/hooktest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// TestProjectAgainstRealGit is what the table over label cannot replace: it is
// the only thing that proves rev-parse's two paths are the pair the label was
// written against, in every tree layout a session actually runs in. The shell
// suite this replaces built the same fixtures for the same reason.
func TestProjectAgainstRealGit(t *testing.T) {
	t.Parallel()
	hooktest.SkipWithoutGit(t)

	base := t.TempDir()
	main := filepath.Join(base, "myrepo")
	hooktest.InitRepo(t, main)
	hooktest.Write(t, filepath.Join(main, "api", "keep"), "x\n")
	hooktest.Git(t, main, "worktree", "add", "-b", "wt-feature", filepath.Join(main, ".claude", "worktrees", "feature-x"))
	hooktest.Git(t, main, "worktree", "add", "-b", "wt-nested", filepath.Join(main, ".claude", "worktrees", "feat", "nested"))
	hooktest.Git(t, main, "worktree", "add", "-b", "wt-manual", filepath.Join(base, "manual-wt"))
	hooktest.Write(t, filepath.Join(main, ".claude", "worktrees", "feature-x", "api", "keep"), "x\n")

	bare := filepath.Join(base, "bare.git")
	hooktest.Git(t, base, "clone", "--bare", main, bare)
	hooktest.Git(t, bare, "worktree", "add", "-b", "wt-bare", filepath.Join(base, "bare-wt"))

	outside := filepath.Join(base, "outside")
	hooktest.Write(t, filepath.Join(outside, "notes.md"), "x\n")

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
