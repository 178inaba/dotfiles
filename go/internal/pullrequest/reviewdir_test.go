package pullrequest_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

func TestContextFileName(t *testing.T) {
	t.Parallel()

	// The @ is what keeps a-b/c and a/b-c from collapsing onto one name, which
	// is the whole reason a parallel review of another pull request cannot
	// read this one's file.
	got := pullrequest.ContextFileName(ghapi.Repo{Owner: "a-b", Name: "c"}, 5)
	if want := "pr-context-a-b@c-5.json"; got != want {
		t.Errorf("ContextFileName = %q, want %q", got, want)
	}
	if other := pullrequest.ContextFileName(ghapi.Repo{Owner: "a", Name: "b-c"}, 5); other == got {
		t.Errorf("a-b/c and a/b-c both answer %q", got)
	}
}

func TestWorkDir(t *testing.T) {
	t.Parallel()

	// The identifier is the context file's own name with its prefix and
	// extension taken off, so that the two commands that write into the
	// directory and the one that hands it out cannot disagree about it.
	got := pullrequest.WorkDir("/scratch/pr-context-owner@repo-5.json")
	if want := "/scratch/pr-owner@repo-5"; got != want {
		t.Errorf("WorkDir = %q, want %q", got, want)
	}
}

func TestEnsureWorkFiles(t *testing.T) {
	t.Parallel()

	scratch := t.TempDir()
	contextFile := filepath.Join(scratch, "pr-context-owner@repo-5.json")

	got, err := pullrequest.EnsureWorkFiles(contextFile)
	if err != nil {
		t.Fatalf("EnsureWorkFiles: %v", err)
	}
	want := pullrequest.WorkFiles{
		Dir:         filepath.Join(scratch, "pr-owner@repo-5"),
		ReviewPath:  filepath.Join(scratch, "pr-owner@repo-5", "review.json"),
		ThreadsPath: filepath.Join(scratch, "pr-owner@repo-5", "threads.json"),
		DiffPath:    filepath.Join(scratch, "pr-owner@repo-5", "diff.patch"),
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("EnsureWorkFiles (-want +got):\n%s", diff)
	}
	// The directory has to be there, not merely named: RequireInWorkDir reads
	// it, and both commands write into it.
	if info, err := os.Stat(got.Dir); err != nil || !info.IsDir() {
		t.Errorf("work dir %s was named but not created (%v)", got.Dir, err)
	}

	// Called again for the same pull request — which is what a second fetch
	// does — it answers the same thing rather than failing on the directory.
	again, err := pullrequest.EnsureWorkFiles(contextFile)
	if err != nil {
		t.Fatalf("EnsureWorkFiles on an existing dir: %v", err)
	}
	if diff := cmp.Diff(got, again); diff != "" {
		t.Errorf("EnsureWorkFiles is not idempotent (-first +second):\n%s", diff)
	}
}

func TestRequireInWorkDir(t *testing.T) {
	t.Parallel()

	scratch := t.TempDir()
	contextFile := filepath.Join(scratch, "pr-context-owner@repo-5.json")
	work, err := pullrequest.EnsureWorkFiles(contextFile)
	if err != nil {
		t.Fatalf("EnsureWorkFiles: %v", err)
	}

	if err := pullrequest.RequireInWorkDir(work.ReviewPath, "review_path", contextFile); err != nil {
		t.Errorf("a file in the work dir was rejected: %v", err)
	}
	// A fixed name in the shared scratch directory is what a parallel review of
	// another pull request overwrites.
	err = pullrequest.RequireInWorkDir(filepath.Join(scratch, "review.json"), "review_path", contextFile)
	if err == nil {
		t.Fatal("a file outside the work dir was accepted")
	}
	if !strings.Contains(err.Error(), "review_path") {
		t.Errorf("error = %q, want it to name the field that would have been right", err)
	}
}

// TestContextCheckout pins the projection alone; what a context file has to
// carry is the declaration's and is covered where that is enforced.
func TestContextCheckout(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name string
		in   pullrequest.Context
		want worktree.PullRequest
	}{
		{
			name: "somebody else's pull request",
			in: pullrequest.Context{
				PR: pullrequest.PR{HeadOID: "abc123", HeadRef: "feature/x", BaseRef: "main"},
			},
			want: worktree.PullRequest{HeadRef: "feature/x", HeadOID: "abc123", BaseRef: "main"},
		},
		{
			// false is an answer, and reading its absence as one would treat a
			// reviewer's checkout as the author's. The declaration is what
			// keeps the two apart, so this reads a plain bool.
			name: "our own pull request",
			in: pullrequest.Context{
				IsOwnPR: true,
				PR:      pullrequest.PR{HeadOID: "abc123", HeadRef: "feature/x", BaseRef: "main"},
			},
			want: worktree.PullRequest{HeadRef: "feature/x", HeadOID: "abc123", BaseRef: "main", IsOwnPR: true},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			if diff := cmp.Diff(tc.want, tc.in.Checkout()); diff != "" {
				t.Errorf("Checkout (-want +got):\n%s", diff)
			}
		})
	}
}
