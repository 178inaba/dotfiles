package worktree

import (
	"path/filepath"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// TestIsCleanCountsUntrackedFilesThatIsDirtyLeavesOut pins the one difference
// between the two definitions: an untracked file does not stop a
// synchronisation, but it does mean a pull request is not ready.
func TestIsCleanCountsUntrackedFilesThatIsDirtyLeavesOut(t *testing.T) {
	t.Parallel()
	gittest.SkipWithoutGit(t)

	dir := gittest.InitWithCommit(t, t.TempDir())
	gittest.Write(t, filepath.Join(dir, "untracked.txt"), "x\n")

	clean, err := IsClean(t.Context(), runner.Exec{}, dir)
	if err != nil {
		t.Fatalf("IsClean: %v", err)
	}
	if clean {
		t.Error("IsClean = true, want false for a tree with an untracked file")
	}

	dirty, err := isDirty(t.Context(), runner.Exec{}, dir)
	if err != nil {
		t.Fatalf("isDirty: %v", err)
	}
	if dirty {
		t.Error("isDirty = true, want false: an untracked file must not stop a synchronisation")
	}
}

// TestIsCleanOnACleanTree is the ordinary case both definitions agree on.
func TestIsCleanOnACleanTree(t *testing.T) {
	t.Parallel()
	gittest.SkipWithoutGit(t)

	dir := gittest.InitWithCommit(t, t.TempDir())

	clean, err := IsClean(t.Context(), runner.Exec{}, dir)
	if err != nil {
		t.Fatalf("IsClean: %v", err)
	}
	if !clean {
		t.Error("IsClean = false, want true for a tree with nothing uncommitted")
	}
}
