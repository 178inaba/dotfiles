package worktree

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// issueWorktree adds a worktree the way ccx worktree create lays one out, on a
// new branch off the fixture base, and returns its path.
func issueWorktree(t *testing.T, repo, name, branch string) string {
	t.Helper()

	wt := filepath.Join(repo, ".claude", "worktrees", name)
	gittest.Run(t, repo, "worktree", "add", "-q", wt, "-b", branch, "origin/"+base)
	return wt
}

func TestDetect(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// setUp prepares the clone, and returns the branch it expects Detect to
		// find, or empty for a clone where nothing should match.
		setUp      func(t *testing.T, repo string) string
		issue      int
		wantStatus DetectStatus
		wantReason LeftoverReason
	}{
		{
			// With an ignored file in it, as ccx worktree create leaves one
			// after copying what .worktreeinclude lists: that file is not work,
			// and a removal without --force must not be refused over it.
			name: "a leftover in the current naming",
			setUp: func(t *testing.T, repo string) string {
				wt := issueWorktree(t, repo, "wt", "feature/42-x")
				gittest.Write(t, filepath.Join(wt, ".env"), "SECRET=1\n")
				return "feature/42-x"
			},
			issue: 42, wantStatus: DetectRemoved,
		},
		{
			// The worktrees EnterWorktree(name:) made are still on disk, and one
			// left empty is as much a leftover as any other.
			name: "a leftover in the naming EnterWorktree used",
			setUp: func(t *testing.T, repo string) string {
				issueWorktree(t, repo, "legacy", "worktree-feature-42-old-style")
				return "worktree-feature-42-old-style"
			},
			issue: 42, wantStatus: DetectRemoved,
		},
		{
			name: "a number that is only a prefix",
			setUp: func(t *testing.T, repo string) string {
				issueWorktree(t, repo, "wt", "feature/42-x")
				return ""
			},
			issue: 4, wantStatus: DetectNone,
		},
		{
			name: "a number that is only a suffix",
			setUp: func(t *testing.T, repo string) string {
				issueWorktree(t, repo, "wt", "feature/42-x")
				return ""
			},
			issue: 142, wantStatus: DetectNone,
		},
		{
			// The main worktree is the repository itself, and removing its
			// branch is never what a leftover check is for.
			name: "the main worktree has the branch checked out",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "switch", "-qc", "feature/42-x", "origin/"+base)
				return ""
			},
			issue: 42, wantStatus: DetectNone,
		},
		{
			name:       "nothing matches",
			setUp:      func(*testing.T, string) string { return "" },
			issue:      42,
			wantStatus: DetectNone,
		},
		{
			name: "a locked worktree",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "worktree", "lock", issueWorktree(t, repo, "wt", "feature/42-x"))
				return "feature/42-x"
			},
			issue: 42, wantStatus: DetectKept, wantReason: LeftoverLocked,
		},
		{
			name: "a worktree a process is standing in",
			setUp: func(t *testing.T, repo string) string {
				hold(t, issueWorktree(t, repo, "wt", "feature/42-x"))
				return "feature/42-x"
			},
			issue: 42, wantStatus: DetectKept, wantReason: LeftoverInUseByProcess,
		},
		{
			// Untracked rather than modified: a file never added is the easiest
			// work to lose, and the one a narrower notion of dirty would miss.
			name: "an untracked file",
			setUp: func(t *testing.T, repo string) string {
				wt := issueWorktree(t, repo, "wt", "feature/42-x")
				gittest.Write(t, filepath.Join(wt, "notes.txt"), "work\n")
				return "feature/42-x"
			},
			issue: 42, wantStatus: DetectKept, wantReason: LeftoverDirty,
		},
		{
			name: "a commit beyond the start ref",
			setUp: func(t *testing.T, repo string) string {
				wt := issueWorktree(t, repo, "wt", "feature/42-x")
				gittest.Run(t, wt, "commit", "-q", "--allow-empty", "-m", "work")
				return "feature/42-x"
			},
			issue: 42, wantStatus: DetectKept, wantReason: LeftoverBeyondStartRef,
		},
		{
			// A branch of its own, since every case pushes into the one bare
			// origin and a name another case uses would read as pushed there.
			name: "a branch on origin",
			setUp: func(t *testing.T, repo string) string {
				issueWorktree(t, repo, "wt", "feature/43-pushed")
				gittest.Run(t, repo, "push", "-q", "origin", "feature/43-pushed")
				return "feature/43-pushed"
			},
			issue: 43, wantStatus: DetectKept, wantReason: LeftoverOnOrigin,
		},
		{
			// The remote-tracking refs stay, so the start ref still resolves and
			// only the question put to origin goes unanswered.
			name: "an origin that cannot be reached",
			setUp: func(t *testing.T, repo string) string {
				issueWorktree(t, repo, "wt", "feature/42-x")
				gittest.Run(t, repo, "remote", "set-url", "origin", filepath.Join(t.TempDir(), "missing.git"))
				return "feature/42-x"
			},
			issue: 42, wantStatus: DetectKept, wantReason: LeftoverOriginUnreachable,
		},
	}

	bare, _ := origin(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo := clone(t, bare)
			branch := tc.setUp(t, repo)

			got, err := Detect(t.Context(), runner.Exec{}, repo, tc.issue, base)
			if err != nil {
				t.Fatalf("Detect(%d): %v", tc.issue, err)
			}
			if got.Status != tc.wantStatus {
				t.Fatalf("Detect(%d) = %+v, want status %q", tc.issue, got, tc.wantStatus)
			}
			if tc.wantReason == "" {
				if got.Reason != nil {
					t.Errorf("Detect(%d) reason = %q, want null", tc.issue, *got.Reason)
				}
			} else if got.Reason == nil || *got.Reason != tc.wantReason {
				t.Errorf("Detect(%d) reason = %v, want %q", tc.issue, got.Reason, tc.wantReason)
			}
			if tc.wantStatus == DetectNone {
				if got.Path != nil || got.Branch != nil {
					t.Errorf("Detect(%d) = %+v, want the path and branch null", tc.issue, got)
				}
				return
			}

			if got.Branch == nil || *got.Branch != branch {
				t.Fatalf("Detect(%d) branch = %v, want %q", tc.issue, got.Branch, branch)
			}
			// git answers with the resolved path, and on macOS the temporary
			// directory is reached through a symlink.
			resolved, err := filepath.EvalSymlinks(filepath.Join(repo, ".claude", "worktrees"))
			if err != nil {
				t.Fatalf("EvalSymlinks: %v", err)
			}
			if got.Path == nil || !strings.HasPrefix(*got.Path, resolved) {
				t.Fatalf("Detect(%d) path = %v, want one under %s", tc.issue, got.Path, resolved)
			}

			_, statErr := os.Stat(*got.Path)
			if tc.wantStatus == DetectRemoved {
				if !os.IsNotExist(statErr) {
					t.Errorf("%s survived its removal", *got.Path)
				}
				if branchExists(t, repo, branch) {
					t.Errorf("branch %s survived its removal", branch)
				}
				return
			}
			if statErr != nil {
				t.Errorf("a kept worktree is gone: %v", statErr)
			}
			if !branchExists(t, repo, branch) {
				t.Errorf("the branch of a kept worktree is gone: %s", branch)
			}
		})
	}
}

// TestDetectWithoutABase covers a base that names nothing, which is a broken
// premise rather than a reason to keep: the check against the start ref cannot
// be made at all.
func TestDetectWithoutABase(t *testing.T) {
	t.Parallel()

	bare, _ := origin(t)
	repo := clone(t, bare)
	wt := issueWorktree(t, repo, "wt", "feature/42-x")

	got, err := Detect(t.Context(), runner.Exec{}, repo, 42, "no-such-base")
	if err == nil {
		t.Fatalf("Detect = %+v, want a failure", got)
	}
	if want := "base branch not found"; !strings.Contains(err.Error(), want) {
		t.Errorf("Detect error = %q, want it to mention %q", err, want)
	}
	if _, err := os.Stat(wt); err != nil {
		t.Errorf("the worktree is gone after a failed check: %v", err)
	}
}
