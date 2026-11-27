package worktree

import (
	"os"
	"path/filepath"
	"slices"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// realDir is dir as git reports it, with the temporary directory's symlinks
// resolved.
func realDir(t *testing.T, dir string) string {
	t.Helper()
	resolved, err := filepath.EvalSymlinks(dir)
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	return resolved
}

func removedBranchNames(c CleanupReport) []string {
	var out []string
	for _, b := range c.Removed.Branches {
		out = append(out, b.Branch)
	}
	return out
}

func TestCleanup(t *testing.T) {
	t.Parallel()

	repo, prs := cleanupFixture(t)
	gh := cleanupGitHub(t, prs, false)
	wtMerged := filepath.Join(realDir(t, filepath.Dir(repo)), "wt-merged")

	got, err := Cleanup(t.Context(), runner.Exec{}, gh, repo)
	if err != nil {
		t.Fatalf("Cleanup: %v", err)
	}

	// The worktree goes with its branch, and carries the judgement it went on.
	i := slices.IndexFunc(got.Removed.Worktrees, func(w RemovedWorktree) bool { return w.Branch == "wt-merged" })
	if i < 0 {
		t.Fatalf("wt-merged was not removed; removed were %+v", got.Removed.Worktrees)
	}
	if w := got.Removed.Worktrees[i]; w.Path != wtMerged || !w.BranchRemoved || w.Verdict != VerdictMergedNoPR || w.Detail == "" {
		t.Errorf("removed wt-merged = %+v, want its path, its branch removed, and its verdict and detail", w)
	}
	if _, err := os.Stat(wtMerged); !os.IsNotExist(err) {
		t.Errorf("%s survived its removal", wtMerged)
	}
	if !slices.Contains(removedBranchNames(got), "merged-nopr") {
		t.Errorf("merged-nopr was not removed; removed branches were %v", removedBranchNames(got))
	}
	// What collect skips is reported alongside, not dropped.
	if !slices.ContainsFunc(got.Skipped, func(s Skipped) bool { return s.Branch == "wt-dirty" && s.Reason == SkipUncommittedChanges }) {
		t.Errorf("the dirty worktree is not reported as skipped; skipped were %+v", got.Skipped)
	}

	// Run again, there is nothing left to delete, and that is not a failure.
	again, err := Cleanup(t.Context(), runner.Exec{}, gh, repo)
	if err != nil {
		t.Fatalf("Cleanup again: %v", err)
	}
	if len(again.Removed.Worktrees)+len(again.Removed.Branches)+len(again.Failures) != 0 {
		t.Errorf("second run = removed %+v, failures %+v, want nothing", again.Removed, again.Failures)
	}
}

// TestCleanupLeavesWhereTheCallerStands covers the step the caller takes
// between runs: what it stands in is reported rather than removed, so that it
// can leave and run the command again.
func TestCleanupLeavesWhereTheCallerStands(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		setup  func(t *testing.T, repo string) string
		target func(t *testing.T, repo string) string
		kind   TargetKind
		branch string
	}{
		{
			name:  "the worktree the caller is in",
			setup: func(_ *testing.T, repo string) string { return filepath.Join(filepath.Dir(repo), "wt-merged") },
			target: func(t *testing.T, repo string) string {
				return filepath.Join(realDir(t, filepath.Dir(repo)), "wt-merged")
			},
			kind: KindWorktree, branch: "wt-merged",
		},
		{
			name: "the main worktree's current branch",
			setup: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "switch", "-q", "merged-nopr")
				return repo
			},
			target: func(*testing.T, string) string { return "merged-nopr" },
			kind:   KindBranch, branch: "merged-nopr",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo, prs := cleanupFixture(t)
			dir := tc.setup(t, repo)

			got, err := Cleanup(t.Context(), runner.Exec{}, cleanupGitHub(t, prs, false), dir)
			if err != nil {
				t.Fatalf("Cleanup: %v", err)
			}
			target := tc.target(t, repo)
			if !slices.ContainsFunc(got.Skipped, func(s Skipped) bool {
				return s.Type == tc.kind && s.Target == target && s.Reason == SkipCurrent
			}) {
				t.Errorf("%s is not skipped as is_current; skipped were %+v", target, got.Skipped)
			}
			if !branchExists(t, repo, tc.branch) {
				t.Errorf("%s was deleted", tc.branch)
			}
			if tc.kind == KindWorktree {
				if _, err := os.Stat(target); err != nil {
					t.Errorf("the worktree the caller is in was removed: %v", err)
				}
			}
			// Everything else still goes.
			if !slices.Contains(removedBranchNames(got), "closedpr") {
				t.Errorf("closedpr was not removed; removed branches were %v", removedBranchNames(got))
			}
		})
	}
}
