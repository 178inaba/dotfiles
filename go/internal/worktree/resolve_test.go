package worktree

import (
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

var prRepo = ghapi.Repo{Owner: "owner", Name: "repo"}

// forkURL is where a fork's branch config points: contributor's fork, in the
// https form gh uses when nothing in its configuration asks otherwise.
const forkURL = "https://github.com/contributor/repo.git"

// remote is a gh configuration that says nothing, which is what an
// installation that was never configured looks like.
func remote(t *testing.T) ghapi.RemoteOptions {
	t.Helper()

	return ghapi.RemoteOptions{ConfigDir: t.TempDir(), Host: "github.com"}
}

// fakePR is the pull request the fake API answers with. An empty HeadRef makes
// the lookup fail, which is what a number naming nothing and a branch with no
// pull request both look like.
type fakePR struct {
	HeadRef string
	HeadOID string
	// Fork puts the head on contributor's fork; Gone deletes that fork.
	Fork bool
	Gone bool
}

// github answers the one question Resolve and Checkout ask it: what the pull
// request's head is, and where it lives.
func github(t *testing.T, pr fakePR) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		if pr.HeadRef == "" {
			fmt.Fprint(w, `{"errors":[{"type":"NOT_FOUND","message":"no pull request"}]}`)
			return
		}
		owner := "owner"
		if pr.Fork {
			owner = "contributor"
		}
		headRepository := fmt.Sprintf(`{"nameWithOwner":%q}`, owner+"/repo")
		if pr.Gone {
			headRepository = "null"
		}
		node := fmt.Sprintf(`{"number":%d,"state":"OPEN","headRefName":%q,"headRefOid":%q,"baseRefName":"main",
			"isCrossRepository":%t,"headRepository":%s}`,
			prNumber, pr.HeadRef, pr.HeadOID, pr.Fork, headRepository)

		body, err := io.ReadAll(r.Body)
		if err != nil {
			t.Errorf("read the request body: %v", err)
			return
		}
		// One handler for both queries: asked by branch it answers with a
		// list, asked by number with the pull request itself.
		if strings.Contains(string(body), `pullRequests(`) {
			fmt.Fprintf(w, `{"data":{"repository":{"pullRequests":{"nodes":[%s]}}}}`, node)
			return
		}
		fmt.Fprintf(w, `{"data":{"repository":{"pullRequest":%s}}}`, node)
	}))
}

// fake is the pull request o carries, as the API describes it.
func (o prSource) fake() fakePR {
	return fakePR{HeadRef: headRef, HeadOID: o.head, Fork: o.branch == forkBranch}
}

// worktreeName is the directory a worktree for o's pull request is made in.
func (o prSource) worktreeName() string {
	return strings.ReplaceAll(o.branch, "/", "-")
}

// worktreeOn adds a linked worktree for o's pull request, checked out on its
// local branch at the given commit, or at the head where at is empty.
func worktreeOn(t *testing.T, repo string, o prSource, at string) string {
	t.Helper()

	path := filepath.Join(repo, ".claude", "worktrees", o.worktreeName())
	gittest.Run(t, repo, "fetch", "-q", "origin", fmt.Sprintf("refs/pull/%d/head", prNumber))
	gittest.Run(t, repo, "worktree", "add", "-q", path, "-b", o.branch, "FETCH_HEAD")
	if at != "" {
		gittest.Run(t, path, "reset", "-q", "--hard", at)
	}
	// git reports the resolved path, and on macOS the temporary directory is
	// reached through a symlink.
	resolved, err := filepath.EvalSymlinks(path)
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	return resolved
}

// config reads one key of dir's git configuration, empty where it is unset.
func config(t *testing.T, dir, key string) string {
	t.Helper()

	got, err := runner.Git(t.Context(), runner.Exec{}, dir, "config", "--get", key)
	if err != nil {
		return ""
	}
	return got
}

// wantTracking checks the branch config o's local branch is given: origin's
// head branch for a pull request in this repository, and the fork's for one
// from a fork — the form `gh pr checkout` writes.
func wantTracking(t *testing.T, dir string, o prSource) {
	t.Helper()

	want := map[string]string{
		"remote": "origin", "pushRemote": "", "merge": "refs/heads/" + headRef,
	}
	if o.branch == forkBranch {
		want = map[string]string{"remote": forkURL, "pushRemote": forkURL, "merge": "refs/heads/" + headRef}
	}
	for key, value := range want {
		if got := config(t, dir, "branch."+o.branch+"."+key); got != value {
			t.Errorf("branch.%s.%s = %q, want %q", o.branch, key, got, value)
		}
	}
}

func TestResolveWithoutAWorktree(t *testing.T) {
	t.Parallel()

	for _, o := range prSources(t) {
		t.Run(o.name, func(t *testing.T) {
			t.Parallel()

			repo := clone(t, o.bare)
			got, err := Resolve(t.Context(), runner.Exec{}, github(t, o.fake()), prRepo, repo, prNumber, remote(t))
			if err != nil {
				t.Fatalf("Resolve: %v", err)
			}

			want := Resolution{
				Status: ResolveOK, Action: ActionCreate, PRNumber: prNumber,
				HeadRef: headRef, LocalBranch: o.branch, WorktreeName: o.worktreeName(),
			}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Resolve (-want +got):\n%s", diff)
			}
		})
	}
}

// TestResolveInfersThePullRequest is the ordinary way /deep-review starts: the
// reviewer is on the branch and never types the number.
func TestResolveInfersThePullRequest(t *testing.T) {
	t.Parallel()

	bare, head, _ := prOrigin(t)
	repo := clone(t, bare)

	got, err := Resolve(t.Context(), runner.Exec{}, github(t, fakePR{HeadRef: headRef, HeadOID: head}), prRepo, repo, 0, remote(t))
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if got.PRNumber != prNumber || got.LocalBranch != headRef {
		t.Errorf("Resolve = %+v, want pull request %d on %s", got, prNumber, headRef)
	}
}

func TestResolveWithAnExistingWorktree(t *testing.T) {
	t.Parallel()

	for _, o := range prSources(t) {
		t.Run(o.name, func(t *testing.T) {
			t.Parallel()

			head, previous := o.head, o.previous
			tests := []struct {
				name  string
				setUp func(t *testing.T, worktreePath string)
				want  ResolveStatus
				// wantSynced and wantMoved say the worktree was
				// fast-forwarded.
				wantSynced bool
				wantMoved  bool
			}{
				{name: "already at the head", want: ResolveOK},
				{
					name:       "behind with nothing to lose",
					setUp:      func(t *testing.T, path string) { gittest.Run(t, path, "reset", "-q", "--hard", previous) },
					want:       ResolveOK,
					wantSynced: true, wantMoved: true,
				},
				{
					name: "behind with uncommitted changes",
					setUp: func(t *testing.T, path string) {
						gittest.Run(t, path, "reset", "-q", "--hard", previous)
						gittest.Write(t, filepath.Join(path, "file.txt"), "dirty\n")
					},
					want: ResolveBehindDirty,
				},
				{
					// A local commit the pull request has never seen is the
					// one thing here that cannot be reconstructed.
					name: "commits of its own",
					setUp: func(t *testing.T, path string) {
						gittest.Run(t, path, "commit", "-q", "--allow-empty", "-m", "own")
					},
					want: ResolveDiverged,
				},
			}

			for _, tc := range tests {
				t.Run(tc.name, func(t *testing.T) {
					t.Parallel()

					repo := clone(t, o.bare)
					path := worktreeOn(t, repo, o, "")
					if tc.setUp != nil {
						tc.setUp(t, path)
					}
					before := gittest.Rev(t, path, "HEAD")

					got, err := Resolve(t.Context(), runner.Exec{}, github(t, o.fake()), prRepo, repo, prNumber, remote(t))
					if err != nil {
						t.Fatalf("Resolve: %v", err)
					}

					want := Resolution{
						Status: tc.want, Action: ActionEnterExisting, PRNumber: prNumber,
						HeadRef: headRef, LocalBranch: o.branch, WorktreeName: o.worktreeName(), Path: &path, Synced: tc.wantSynced,
					}
					if diff := cmp.Diff(want, got); diff != "" {
						t.Errorf("Resolve (-want +got):\n%s", diff)
					}
					// Written whatever the synchronisation found, since the
					// config is about where the branch belongs rather than
					// where it stands.
					wantTracking(t, path, o)

					after := gittest.Rev(t, path, "HEAD")
					if tc.wantMoved && after != head {
						t.Errorf("the worktree is at %s, want it fast-forwarded to %s", after, head)
					}
					// Every stop leaves the worktree where it was; the work in
					// it is the reason it stopped.
					if !tc.wantMoved && after != before {
						t.Errorf("the worktree moved to %s, want it left at %s", after, before)
					}
				})
			}
		})
	}
}

// TestResolveFromInsideAWorktree is how these commands are actually reached:
// the session may already be in a worktree, and the answer must still be about
// the repository rather than about wherever it is standing.
func TestResolveFromInsideAWorktree(t *testing.T) {
	t.Parallel()

	o := prSources(t)[0]
	repo := clone(t, o.bare)
	path := worktreeOn(t, repo, o, "")

	got, err := Resolve(t.Context(), runner.Exec{}, github(t, o.fake()), prRepo, path, prNumber, remote(t))
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if got.Action != ActionEnterExisting || got.Path == nil || *got.Path != path {
		t.Errorf("Resolve = %+v, want it to find the worktree it is standing in (%s)", got, path)
	}
}

func TestResolveEvacuatesTheMainRepository(t *testing.T) {
	t.Parallel()

	// git allows one checkout of a branch at a time, so the main repository has
	// to move off the pull request's local branch before a worktree can have
	// it.
	for _, o := range prSources(t) {
		t.Run(o.name+", clean, so it moves", func(t *testing.T) {
			t.Parallel()

			repo := o.checkout(t, o.bare)

			got, err := Resolve(t.Context(), runner.Exec{}, github(t, o.fake()), prRepo, repo, prNumber, remote(t))
			if err != nil {
				t.Fatalf("Resolve: %v", err)
			}
			want := Resolution{
				Status: ResolveOK, Action: ActionCreate, PRNumber: prNumber,
				HeadRef: headRef, LocalBranch: o.branch, WorktreeName: o.worktreeName(), Evacuated: true,
			}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Resolve (-want +got):\n%s", diff)
			}
			if branch := strings.TrimSpace(gittest.Run(t, repo, "branch", "--show-current")); branch != "main" {
				t.Errorf("the main repository is on %q, want it moved to the default branch", branch)
			}
		})
	}

	t.Run("dirty, so it stops", func(t *testing.T) {
		t.Parallel()

		o := prSources(t)[0]
		repo := checkoutOf(t, o.bare)
		gittest.Write(t, filepath.Join(repo, "file.txt"), "dirty\n")

		got, err := Resolve(t.Context(), runner.Exec{}, github(t, o.fake()), prRepo, repo, prNumber, remote(t))
		if err != nil {
			t.Fatalf("Resolve: %v", err)
		}
		want := Resolution{
			Status: ResolveEvacuationDirty, Action: ActionCreate, PRNumber: prNumber,
			HeadRef: headRef, LocalBranch: headRef, WorktreeName: "feature-x",
		}
		if diff := cmp.Diff(want, got); diff != "" {
			t.Errorf("Resolve (-want +got):\n%s", diff)
		}
		if branch := strings.TrimSpace(gittest.Run(t, repo, "branch", "--show-current")); branch != headRef {
			t.Errorf("the main repository moved to %q, want it left on %q with its changes", branch, headRef)
		}
	})
}

func TestResolveWithoutAPullRequest(t *testing.T) {
	t.Parallel()

	bare, _, _ := prOrigin(t)
	repo := clone(t, bare)

	for _, number := range []int{0, prNumber} {
		if got, err := Resolve(t.Context(), runner.Exec{}, github(t, fakePR{}), prRepo, repo, number, remote(t)); err == nil {
			t.Errorf("Resolve(%d) = %+v, want a failure", number, got)
		}
	}
}

// TestCheckoutOfADeletedFork is what `gh pr checkout` does with a pull request
// whose fork is gone: refs/pull/<n>/head outlives the fork, so the pull request
// is checked out all the same, tracking refs/pull/<n>/head on origin. With no
// owner to name the branch after, the pull request's number stands in.
func TestCheckoutOfADeletedFork(t *testing.T) {
	t.Parallel()

	bare, head, _ := forkOrigin(t)
	gone := fakePR{HeadRef: headRef, HeadOID: head, Fork: true, Gone: true}
	const branch = "pr-42/" + headRef
	repo := clone(t, bare)

	resolved, err := Resolve(t.Context(), runner.Exec{}, github(t, gone), prRepo, repo, prNumber, remote(t))
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if resolved.LocalBranch != branch || resolved.WorktreeName != "pr-42-feature-x" {
		t.Errorf("Resolve = %+v, want %s in pr-42-feature-x", resolved, branch)
	}

	got, err := Checkout(t.Context(), runner.Exec{}, github(t, gone), prRepo, repo, prNumber, remote(t))
	if err != nil {
		t.Fatalf("Checkout: %v", err)
	}
	if got.Status != ResolveOK || gittest.Rev(t, got.Path, "HEAD") != head {
		t.Errorf("Checkout = %+v, want ok at %s", got, head)
	}
	for key, want := range map[string]string{"remote": "origin", "pushRemote": "origin", "merge": "refs/pull/42/head"} {
		if got := config(t, got.Path, "branch."+branch+"."+key); got != want {
			t.Errorf("branch.%s.%s = %q, want %q", branch, key, got, want)
		}
	}
}

func TestCheckout(t *testing.T) {
	t.Parallel()

	for _, o := range prSources(t) {
		t.Run(o.name, func(t *testing.T) {
			t.Parallel()

			head, previous := o.head, o.previous
			tests := []struct {
				name       string
				setUp      func(t *testing.T, repo string)
				want       ResolveStatus
				wantSynced bool
				wantAtHead bool
			}{
				{name: "a fresh worktree", want: ResolveOK, wantAtHead: true},
				{
					// A local branch left over from earlier work on the same
					// pull request, or by the evacuation of the main
					// repository, which is reused rather than recreated.
					name: "an old local branch behind the head",
					setUp: func(t *testing.T, repo string) {
						gittest.Run(t, repo, "fetch", "-q", "origin", fmt.Sprintf("refs/pull/%d/head", prNumber))
						gittest.Run(t, repo, "branch", o.branch, previous)
					},
					want: ResolveOK, wantSynced: true, wantAtHead: true,
				},
				{
					name: "an old local branch with commits of its own",
					setUp: func(t *testing.T, repo string) {
						gittest.Run(t, repo, "fetch", "-q", "origin", fmt.Sprintf("refs/pull/%d/head", prNumber))
						gittest.Run(t, repo, "branch", o.branch, previous)
						gittest.Run(t, repo, "switch", "-q", o.branch)
						gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "own")
						gittest.Run(t, repo, "switch", "-q", "main")
					},
					want: ResolveDiverged,
				},
			}

			for _, tc := range tests {
				t.Run(tc.name, func(t *testing.T) {
					t.Parallel()

					repo := clone(t, o.bare)
					if tc.setUp != nil {
						tc.setUp(t, repo)
					}
					mainBefore := gittest.Rev(t, repo, "HEAD")

					got, err := Checkout(t.Context(), runner.Exec{}, github(t, o.fake()), prRepo, repo, prNumber, remote(t))
					if err != nil {
						t.Fatalf("Checkout: %v", err)
					}

					path := filepath.Join(repo, ".claude", "worktrees", o.worktreeName())
					want := CheckedOut{Status: tc.want, Path: path, Synced: tc.wantSynced}
					if diff := cmp.Diff(want, got); diff != "" {
						t.Errorf("Checkout (-want +got):\n%s", diff)
					}
					if branch := strings.TrimSpace(gittest.Run(t, path, "branch", "--show-current")); branch != o.branch {
						t.Errorf("the worktree is on %q, want %q", branch, o.branch)
					}
					if tc.wantAtHead {
						if got := gittest.Rev(t, path, "HEAD"); got != head {
							t.Errorf("the worktree is at %s, want %s", got, head)
						}
					}
					wantTracking(t, path, o)
					// A stopping status still leaves the worktree behind, since
					// somebody may want to work in it as it stands.
					if _, err := os.Stat(filepath.Join(path, ".git")); err != nil {
						t.Errorf("the worktree was not created: %v", err)
					}
					if got := gittest.Rev(t, repo, "HEAD"); got != mainBefore {
						t.Errorf("the main repository moved to %s, want it left at %s", got, mainBefore)
					}
				})
			}
		})
	}
}

// TestCheckoutRefusesToPushAFork is what the fork's branch config is for: a
// branch named <owner>/<head_ref> tracking <head_ref> is one git will not push
// with push.default=simple, where no upstream at all would let
// push.autoSetupRemote create <owner>/<head_ref> on the base repository. git
// refuses before it contacts the remote, so nothing leaves this machine.
func TestCheckoutRefusesToPushAFork(t *testing.T) {
	t.Parallel()

	bare, head, _ := forkOrigin(t)
	repo := clone(t, bare)

	got, err := Checkout(t.Context(), runner.Exec{}, github(t, fakePR{HeadRef: headRef, HeadOID: head, Fork: true}), prRepo, repo, prNumber, remote(t))
	if err != nil {
		t.Fatalf("Checkout: %v", err)
	}

	out, err := runner.Exec{}.Run(t.Context(), runner.Command{
		Name: "git", Args: []string{"-C", got.Path, "-c", "push.default=simple", "-c", "push.autoSetupRemote=true", "push"},
	})
	if err == nil {
		t.Fatalf("git push succeeded, want it refused: %s", out)
	}
	var runErr *runner.Error
	if !errors.As(err, &runErr) || !strings.Contains(string(runErr.Stderr), "does not match") {
		t.Errorf("git push failed with %v, want the upstream name mismatch", err)
	}
}

// TestCheckoutWithoutTheHeadBranch is a pull request in this repository whose
// head branch was deleted after it closed: refs/pull/<n>/head still has it, so
// the worktree is made, just without an upstream to pull from.
func TestCheckoutWithoutTheHeadBranch(t *testing.T) {
	t.Parallel()

	bare, head, _ := forkOrigin(t)
	repo := clone(t, bare)

	got, err := Checkout(t.Context(), runner.Exec{}, github(t, fakePR{HeadRef: headRef, HeadOID: head}), prRepo, repo, prNumber, remote(t))
	if err != nil {
		t.Fatalf("Checkout: %v", err)
	}
	if got.Status != ResolveOK || len(got.Warnings) == 0 {
		t.Errorf("Checkout = %+v, want ok with a warning about the missing upstream", got)
	}
	if upstream := config(t, got.Path, "branch."+headRef+".merge"); upstream != "" {
		t.Errorf("branch.%s.merge = %q, want no upstream", headRef, upstream)
	}
}

// TestCheckoutRefusesAHeadItCannotFind is the presence check on this side: a
// head the fetch did not bring in is a pull request that moved, not a failure
// to create a branch.
func TestCheckoutRefusesAHeadItCannotFind(t *testing.T) {
	t.Parallel()

	bare, _, _ := prOrigin(t)
	repo := clone(t, bare)

	moved := fakePR{HeadRef: headRef, HeadOID: strings.Repeat("1", 40)}
	got, err := Checkout(t.Context(), runner.Exec{}, github(t, moved), prRepo, repo, prNumber, remote(t))
	if err == nil || !strings.Contains(err.Error(), "moved while the pull request was being read") {
		t.Errorf("Checkout = %+v, %v; want an error saying the pull request moved", got, err)
	}
}

// TestCheckoutCopiesTheIncludedFiles checks the wiring, the same way the issue
// path's test does: the edge cases belong to TestCopyWorktreeInclude.
func TestCheckoutCopiesTheIncludedFiles(t *testing.T) {
	t.Parallel()

	bare, head, _ := prOrigin(t)
	repo := clone(t, bare)
	gittest.Write(t, filepath.Join(repo, ".env"), "SECRET=1\n")

	got, err := Checkout(t.Context(), runner.Exec{}, github(t, fakePR{HeadRef: headRef, HeadOID: head}), prRepo, repo, prNumber, remote(t))
	if err != nil {
		t.Fatalf("Checkout: %v", err)
	}
	if got.CopiedFiles != 1 {
		t.Errorf("CopiedFiles = %d, want 1", got.CopiedFiles)
	}
}
