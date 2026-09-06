package pullrequest_test

import (
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/runner"
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

// TestContextCheckout pins the projection alone. The projections are pure, so
// what a context file has to carry for one to be meaningful is the
// declaration's business and is covered where that is enforced, in
// TestParseContextRefusesADocumentAgainstItsDeclaration.
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

// prHandler serves the pull request lookup RequirePushedHead makes, and fails
// the test on anything else: the check is all these cases exercise, so another
// endpoint being reached is the test's own mistake.
func prHandler(t *testing.T, liveHead string) http.Handler {
	t.Helper()

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/graphql" {
			t.Errorf("unexpected request to %s", r.URL.Path)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprintf(w, `{"data":{"repository":{"pullRequest":%s}}}`, prNode("owner", liveHead))
	})
}

// unrelatedCommit is a commit in a repository of its own, so that it is a name
// the checkout under test cannot resolve — which is what a head pushed during
// the run looks like from here.
func unrelatedCommit(t *testing.T, name string) string {
	t.Helper()

	return gittest.Rev(t, gittest.InitWithCommit(t, filepath.Join(t.TempDir(), name)), "HEAD")
}

// TestRequirePushedHead is the check the two reply commands make now that a run
// fetches the document once: the push landed, and what the replies were written
// against is behind where the checkout stands.
func TestRequirePushedHead(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	head := gittest.Rev(t, repo, "HEAD")
	previous := gittest.Rev(t, repo, "HEAD~")
	target := func(docHead string) pullrequest.Target {
		return pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: docHead}
	}

	// The pushed-fixes case, and the whole point of the check: the run
	// committed and pushed, so the checkout is what GitHub holds and the
	// document it judged from is behind it. No second fetch was needed.
	t.Run("the push landed", func(t *testing.T) {
		t.Parallel()

		c := ghapitest.New(t, prHandler(t, head))
		if err := pullrequest.RequirePushedHead(t.Context(), runner.Exec{}, c, repo, target(previous), "replying"); err != nil {
			t.Errorf("RequirePushedHead on a pushed checkout = %v, want it to accept", err)
		}
	})

	// The run's own commits sit on top of what GitHub holds, so a reply would
	// be about code nobody else can see.
	t.Run("a commit is unpushed", func(t *testing.T) {
		t.Parallel()

		c := ghapitest.New(t, prHandler(t, previous))
		err := pullrequest.RequirePushedHead(t.Context(), runner.Exec{}, c, repo, target(previous), "replying")
		if err == nil {
			t.Fatal("RequirePushedHead with an unpushed commit succeeded, want a refusal")
		}
		for _, want := range []string{head, previous, "push"} {
			if !strings.Contains(err.Error(), want) {
				t.Errorf("error = %q, want it to mention %q", err, want)
			}
		}
		if strings.Contains(err.Error(), "sync") {
			t.Errorf("error = %q, want it to ask for a push rather than a sync", err)
		}
	})

	// The live head is one the author pushed during the run, so this checkout
	// cannot resolve it — the ordinary shape of this case, and what an ancestry
	// test asked the other way round would answer wrongly.
	t.Run("the checkout is behind", func(t *testing.T) {
		t.Parallel()

		elsewhere := unrelatedCommit(t, "elsewhere")
		c := ghapitest.New(t, prHandler(t, elsewhere))
		err := pullrequest.RequirePushedHead(t.Context(), runner.Exec{}, c, repo, target(head), "replying")
		if err == nil {
			t.Fatal("RequirePushedHead behind the live head succeeded, want a refusal")
		}
		for _, want := range []string{head, elsewhere, "sync"} {
			if !strings.Contains(err.Error(), want) {
				t.Errorf("error = %q, want it to mention %q", err, want)
			}
		}
	})

	// A rebase or a force-push: the checkout is the live head, but nothing the
	// replies were written against is on the way to it.
	t.Run("the document's head is not an ancestor", func(t *testing.T) {
		t.Parallel()

		aside := unrelatedCommit(t, "aside")
		c := ghapitest.New(t, prHandler(t, head))
		err := pullrequest.RequirePushedHead(t.Context(), runner.Exec{}, c, repo, target(aside), "replying")
		if err == nil {
			t.Fatal("RequirePushedHead with an unrelated document head succeeded, want a refusal")
		}
		for _, want := range []string{aside, head, "ccx pr context"} {
			if !strings.Contains(err.Error(), want) {
				t.Errorf("error = %q, want it to mention %q", err, want)
			}
		}
	})

	// Fail closed: a live head that cannot be read is not a live head that
	// matches, and nothing undoes a reply posted on the guess.
	t.Run("the live head cannot be read", func(t *testing.T) {
		t.Parallel()

		c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
			w.WriteHeader(http.StatusInternalServerError)
		}))
		if err := pullrequest.RequirePushedHead(t.Context(), runner.Exec{}, c, repo, target(previous), "replying"); err == nil {
			t.Error("RequirePushedHead with an unreadable live head succeeded, want a refusal")
		}
	})
}
