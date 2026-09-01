package worktree

import (
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

// github answers the one question Resolve asks it: which branch a pull request
// has as its head. An empty ref makes the lookup fail, which is what a number
// naming nothing and a branch with no pull request both look like.
func github(t *testing.T, ref string) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		if ref == "" {
			fmt.Fprint(w, `{"errors":[{"type":"NOT_FOUND","message":"no pull request"}]}`)
			return
		}
		node := fmt.Sprintf(`{"number":42,"state":"OPEN","headRefName":%q,"baseRefName":"main",
			"headRepositoryOwner":{"login":"owner"}}`, ref)

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

// worktreeOn adds a linked worktree checked out on the pull request's head
// branch at the given commit.
func worktreeOn(t *testing.T, repo, at string) string {
	t.Helper()

	path := filepath.Join(repo, ".claude", "worktrees", "feature-x")
	gittest.Run(t, repo, "worktree", "add", "-q", path, "-b", headRef, "origin/"+headRef)
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

func TestResolveWithoutAWorktree(t *testing.T) {
	t.Parallel()

	bare, head, _ := prOrigin(t)

	tests := []struct {
		name string
		// number is the pull request asked for, zero to infer it from the
		// branch checked out here.
		number int
	}{
		{name: "asked by number", number: 42},
		// The ordinary way /deep-review starts: the reviewer is on the branch
		// and never types the number.
		{name: "inferred from the branch"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo := clone(t, bare)
			got, err := Resolve(t.Context(), runner.Exec{}, github(t, headRef), prRepo, repo, tc.number)
			if err != nil {
				t.Fatalf("Resolve: %v", err)
			}

			want := Resolution{
				Status: ResolveOK, Action: ActionCreate, PRNumber: 42,
				HeadRef: headRef, WorktreeName: "feature-x",
			}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Resolve (-want +got):\n%s", diff)
			}
			// The fetch is the point of this branch of the command: Checkout
			// builds on the remote-tracking ref it leaves behind.
			if got := ref(t, repo, "refs/remotes/origin/"+headRef); got != head {
				t.Errorf("origin/%s is at %s, want %s", headRef, got, head)
			}
		})
	}
}

func TestResolveWithAnExistingWorktree(t *testing.T) {
	t.Parallel()

	bare, head, previous := prOrigin(t)

	tests := []struct {
		name  string
		setUp func(t *testing.T, worktreePath string)
		want  ResolveStatus
		// wantSynced and wantMoved say the worktree was fast-forwarded.
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
			// A local commit the remote has never seen is the one thing here
			// that cannot be reconstructed.
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

			repo := clone(t, bare)
			path := worktreeOn(t, repo, "")
			if tc.setUp != nil {
				tc.setUp(t, path)
			}
			before := ref(t, path, "HEAD")

			got, err := Resolve(t.Context(), runner.Exec{}, github(t, headRef), prRepo, repo, 42)
			if err != nil {
				t.Fatalf("Resolve: %v", err)
			}

			want := Resolution{
				Status: tc.want, Action: ActionEnterExisting, PRNumber: 42,
				HeadRef: headRef, WorktreeName: "feature-x", Path: &path, Synced: tc.wantSynced,
			}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Resolve (-want +got):\n%s", diff)
			}

			after := ref(t, path, "HEAD")
			if tc.wantMoved && after != head {
				t.Errorf("the worktree is at %s, want it fast-forwarded to %s", after, head)
			}
			// Every stop leaves the worktree where it was; the work in it is
			// the reason it stopped.
			if !tc.wantMoved && after != before {
				t.Errorf("the worktree moved to %s, want it left at %s", after, before)
			}
		})
	}
}

// TestResolveFromInsideAWorktree is how these commands are actually reached:
// the session may already be in a worktree, and the answer must still be about
// the repository rather than about wherever it is standing.
func TestResolveFromInsideAWorktree(t *testing.T) {
	t.Parallel()

	bare, _, _ := prOrigin(t)
	repo := clone(t, bare)
	path := worktreeOn(t, repo, "")

	got, err := Resolve(t.Context(), runner.Exec{}, github(t, headRef), prRepo, path, 42)
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if got.Action != ActionEnterExisting || got.Path == nil || *got.Path != path {
		t.Errorf("Resolve = %+v, want it to find the worktree it is standing in (%s)", got, path)
	}
}

func TestResolveEvacuatesTheMainRepository(t *testing.T) {
	t.Parallel()

	bare, _, _ := prOrigin(t)

	// git allows one checkout of a branch at a time, so the main repository has
	// to move off the head branch before a worktree can have it.
	t.Run("clean, so it moves", func(t *testing.T) {
		t.Parallel()

		repo := clone(t, bare)
		gittest.Run(t, repo, "switch", "-qc", headRef, "origin/"+headRef)

		got, err := Resolve(t.Context(), runner.Exec{}, github(t, headRef), prRepo, repo, 42)
		if err != nil {
			t.Fatalf("Resolve: %v", err)
		}
		want := Resolution{
			Status: ResolveOK, Action: ActionCreate, PRNumber: 42,
			HeadRef: headRef, WorktreeName: "feature-x", Evacuated: true,
		}
		if diff := cmp.Diff(want, got); diff != "" {
			t.Errorf("Resolve (-want +got):\n%s", diff)
		}
		if branch := strings.TrimSpace(gittest.Run(t, repo, "branch", "--show-current")); branch != "main" {
			t.Errorf("the main repository is on %q, want it moved to the default branch", branch)
		}
	})

	t.Run("dirty, so it stops", func(t *testing.T) {
		t.Parallel()

		repo := clone(t, bare)
		gittest.Run(t, repo, "switch", "-qc", headRef, "origin/"+headRef)
		gittest.Write(t, filepath.Join(repo, "file.txt"), "dirty\n")

		got, err := Resolve(t.Context(), runner.Exec{}, github(t, headRef), prRepo, repo, 42)
		if err != nil {
			t.Fatalf("Resolve: %v", err)
		}
		want := Resolution{
			Status: ResolveEvacuationDirty, Action: ActionCreate, PRNumber: 42,
			HeadRef: headRef, WorktreeName: "feature-x",
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

	for _, number := range []int{0, 42} {
		if got, err := Resolve(t.Context(), runner.Exec{}, github(t, ""), prRepo, repo, number); err == nil {
			t.Errorf("Resolve(%d) = %+v, want a failure", number, got)
		}
	}
}

func TestCheckout(t *testing.T) {
	t.Parallel()

	bare, head, previous := prOrigin(t)

	tests := []struct {
		name       string
		setUp      func(t *testing.T, repo string)
		want       ResolveStatus
		wantSynced bool
		wantAtHead bool
	}{
		{name: "a fresh worktree", want: ResolveOK, wantAtHead: true},
		{
			// A local branch left over from earlier work on the same pull
			// request, which the switch below picks up rather than the remote.
			name: "an old local branch behind the remote",
			setUp: func(t *testing.T, repo string) {
				gittest.Run(t, repo, "branch", headRef, previous)
			},
			want: ResolveOK, wantSynced: true, wantAtHead: true,
		},
		{
			name: "an old local branch with commits of its own",
			setUp: func(t *testing.T, repo string) {
				gittest.Run(t, repo, "branch", headRef, previous)
				gittest.Run(t, repo, "switch", "-q", headRef)
				gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "own")
				gittest.Run(t, repo, "switch", "-q", "main")
			},
			want: ResolveDiverged,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo := clone(t, bare)
			if tc.setUp != nil {
				tc.setUp(t, repo)
			}
			mainBefore := ref(t, repo, "HEAD")

			got, err := Checkout(t.Context(), runner.Exec{}, repo, "feature-x", headRef)
			if err != nil {
				t.Fatalf("Checkout: %v", err)
			}

			path := filepath.Join(repo, ".claude", "worktrees", "feature-x")
			want := CheckedOut{Status: tc.want, Path: path, Synced: tc.wantSynced}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Checkout (-want +got):\n%s", diff)
			}
			// Detached first and switched afterwards, so the worktree ends up
			// on the branch rather than on a commit.
			if branch := strings.TrimSpace(gittest.Run(t, path, "branch", "--show-current")); branch != headRef {
				t.Errorf("the worktree is on %q, want %q", branch, headRef)
			}
			if tc.wantAtHead {
				if got := ref(t, path, "HEAD"); got != head {
					t.Errorf("the worktree is at %s, want %s", got, head)
				}
			}
			// A stopping status still leaves the worktree behind, since
			// somebody may want to work in it as it stands.
			if _, err := os.Stat(filepath.Join(path, ".git")); err != nil {
				t.Errorf("the worktree was not created: %v", err)
			}
			if got := ref(t, repo, "HEAD"); got != mainBefore {
				t.Errorf("the main repository moved to %s, want it left at %s", got, mainBefore)
			}
		})
	}
}

// TestCheckoutCopiesTheIncludedFiles checks the wiring, the same way the issue
// path's test does: the edge cases belong to TestCopyWorktreeInclude.
func TestCheckoutCopiesTheIncludedFiles(t *testing.T) {
	t.Parallel()

	bare, _, _ := prOrigin(t)
	repo := clone(t, bare)
	gittest.Write(t, filepath.Join(repo, ".env"), "SECRET=1\n")

	got, err := Checkout(t.Context(), runner.Exec{}, repo, "feature-x", headRef)
	if err != nil {
		t.Fatalf("Checkout: %v", err)
	}
	if got.CopiedFiles != 1 {
		t.Errorf("CopiedFiles = %d, want 1", got.CopiedFiles)
	}
}

func TestCheckoutWithoutTheRemoteRef(t *testing.T) {
	t.Parallel()

	bare, _, _ := prOrigin(t)
	repo := clone(t, bare)
	gittest.Run(t, repo, "update-ref", "-d", "refs/remotes/origin/"+headRef)

	got, err := Checkout(t.Context(), runner.Exec{}, repo, "feature-x", headRef)
	if err == nil {
		t.Fatalf("Checkout = %+v, want a failure", got)
	}
	if want := "run the resolve subcommand first"; !strings.Contains(err.Error(), want) {
		t.Errorf("Checkout error = %q, want it to mention %q", err, want)
	}
}
