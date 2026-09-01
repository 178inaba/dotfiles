package worktree

import (
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const headRef = "feature/x"

// prOrigin builds a repository whose head branch has two commits past its
// first, so that a clone can be put behind, ahead or beside the pull request's
// head. It returns the bare repository and the two commits of the head branch.
func prOrigin(t *testing.T) (bare, head, previous string) {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	bare = gittest.Init(t, filepath.Join(base, "origin.git"), "--bare", "-b", "main")
	seed := gittest.Clone(t, bare, filepath.Join(base, "seed"))
	gittest.Write(t, filepath.Join(seed, "file.txt"), "base\n")
	// The ignore rules belong to the repository as a whole, so they go on the
	// default branch; the list of what to carry into a worktree belongs to the
	// commit being checked out, so it goes on the head branch below.
	gittest.Write(t, filepath.Join(seed, ".gitignore"), ".env\n")
	gittest.Run(t, seed, "add", "file.txt", ".gitignore")
	gittest.Run(t, seed, "commit", "-qm", "initial")
	gittest.Run(t, seed, "push", "-q", "origin", "main")

	gittest.Run(t, seed, "switch", "-qc", headRef)
	gittest.Write(t, filepath.Join(seed, "file.txt"), "base\none\n")
	gittest.Write(t, filepath.Join(seed, ".worktreeinclude"), ".env\n")
	gittest.Run(t, seed, "add", ".worktreeinclude")
	gittest.Run(t, seed, "commit", "-qam", "one")
	gittest.Write(t, filepath.Join(seed, "file.txt"), "base\none\ntwo\n")
	gittest.Run(t, seed, "commit", "-qam", "two")
	gittest.Run(t, seed, "push", "-q", "origin", headRef)

	return bare, gittest.Rev(t, seed, headRef), gittest.Rev(t, seed, headRef+"^")
}

// checkoutOf clones the origin onto the head branch, which is where every case
// starts from.
func checkoutOf(t *testing.T, bare string) string {
	t.Helper()

	repo := clone(t, bare)
	gittest.Run(t, repo, "switch", "-qc", headRef, "origin/"+headRef)
	return repo
}

func TestCheckFreshness(t *testing.T) {
	t.Parallel()

	bare, head, previous := prOrigin(t)

	tests := []struct {
		name    string
		setUp   func(t *testing.T, repo string)
		isOwnPR bool
		headRef string
		want    Freshness
		// wantMoved says the checkout should end at the pull request's head.
		wantMoved bool
	}{
		{name: "already at the head", want: FreshnessOK, wantMoved: true},
		{
			// The author's own unpushed commits, which is ordinary work rather
			// than a stale checkout.
			name:    "the author is ahead",
			setUp:   func(t *testing.T, repo string) { gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "local") },
			isOwnPR: true,
			want:    FreshnessAheadOwn,
		},
		{
			// The same commits on somebody else's pull request are history
			// this command has no business rewriting.
			name:  "a reviewer is ahead",
			setUp: func(t *testing.T, repo string) { gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "local") },
			want:  FreshnessDiverged,
		},
		{
			name:      "behind with nothing to lose",
			setUp:     func(t *testing.T, repo string) { gittest.Run(t, repo, "reset", "-q", "--hard", previous) },
			want:      FreshnessSynced,
			wantMoved: true,
		},
		{
			name: "behind with uncommitted changes",
			setUp: func(t *testing.T, repo string) {
				gittest.Run(t, repo, "reset", "-q", "--hard", previous)
				gittest.Write(t, filepath.Join(repo, "file.txt"), "dirty\n")
			},
			want: FreshnessBehindDirty,
		},
		{
			// Untracked files are not git's to move, and a review leaves them
			// behind constantly; treating them as dirty would stop every
			// synchronisation.
			name: "behind with only untracked files",
			setUp: func(t *testing.T, repo string) {
				gittest.Run(t, repo, "reset", "-q", "--hard", previous)
				gittest.Write(t, filepath.Join(repo, "untracked.txt"), "x\n")
			},
			want:      FreshnessSynced,
			wantMoved: true,
		},
		{
			name: "commits on both sides",
			setUp: func(t *testing.T, repo string) {
				gittest.Run(t, repo, "reset", "-q", "--hard", previous)
				gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "own")
			},
			want: FreshnessDiverged,
		},
		{
			name:  "another branch is checked out",
			setUp: func(t *testing.T, repo string) { gittest.Run(t, repo, "switch", "-q", "main") },
			want:  FreshnessBranchMismatch,
		},
		{
			name:  "a detached head",
			setUp: func(t *testing.T, repo string) { gittest.Run(t, repo, "switch", "-q", "--detach", "HEAD") },
			want:  FreshnessBranchMismatch,
		},
		{
			// What a pull request from a fork looks like from here: its head
			// branch is not on this origin at all.
			name:    "the head branch cannot be fetched",
			headRef: "no-such-branch",
			want:    FreshnessFetchFailed,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo := checkoutOf(t, bare)
			if tc.setUp != nil {
				tc.setUp(t, repo)
			}
			before := gittest.Rev(t, repo, "HEAD")

			want := tc.headRef
			if want == "" {
				want = headRef
			}
			pr := PullRequest{HeadRef: want, HeadOID: head, BaseRef: "main", IsOwnPR: tc.isOwnPR}

			got, err := CheckFreshness(t.Context(), runner.Exec{}, repo, pr)
			if err != nil {
				t.Fatalf("CheckFreshness: %v", err)
			}

			if got.Status != tc.want {
				t.Errorf("status = %q, want %q", got.Status, tc.want)
			}
			if got.HeadRef != want || got.HeadOID != head {
				t.Errorf("report = %+v, want it to echo %s at %s", got, want, head)
			}
			after := gittest.Rev(t, repo, "HEAD")
			if got.LocalHead != after {
				t.Errorf("local_head = %s, want the checkout's own head %s", got.LocalHead, after)
			}
			if tc.wantMoved && after != head {
				t.Errorf("the checkout is at %s, want it moved to %s", after, head)
			}
			// Every stop leaves the checkout exactly where it was: the work at
			// stake is the reason it stopped.
			if !tc.wantMoved && after != before {
				t.Errorf("the checkout moved to %s, want it left at %s", after, before)
			}
		})
	}
}

// TestCheckFreshnessKeepsDirtyWork is the promise behind the behind_dirty stop:
// the change that stopped it is still there afterwards.
func TestCheckFreshnessKeepsDirtyWork(t *testing.T) {
	t.Parallel()

	bare, head, previous := prOrigin(t)
	repo := checkoutOf(t, bare)
	gittest.Run(t, repo, "reset", "-q", "--hard", previous)
	gittest.Write(t, filepath.Join(repo, "file.txt"), "dirty\n")

	if _, err := CheckFreshness(t.Context(), runner.Exec{}, repo,
		PullRequest{HeadRef: headRef, HeadOID: head, BaseRef: "main"}); err != nil {
		t.Fatalf("CheckFreshness: %v", err)
	}

	got := gittest.Run(t, repo, "show", "--no-patch", "--format=%H", "HEAD")
	if strings.TrimSpace(got) != previous {
		t.Errorf("the checkout moved to %s, want it left at %s", got, previous)
	}
	if content := gittest.Run(t, repo, "diff", "--name-only"); !strings.Contains(content, "file.txt") {
		t.Errorf("the uncommitted change is gone; git diff says %q", content)
	}
}
