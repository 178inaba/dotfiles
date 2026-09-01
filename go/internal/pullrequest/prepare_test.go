package pullrequest_test

import (
	"fmt"
	"io"
	"net/http"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// prepareRepo builds a clone checked out on the pull request's head branch,
// with the remote refs the freshness check fetches.
func prepareRepo(t *testing.T) (repo, headOID string) {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	bare := filepath.Join(base, "origin.git")
	repo = filepath.Join(base, "repo")
	gittest.Init(t, bare, "--bare", "-b", "main")
	gittest.Clone(t, bare, repo)
	gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "init")
	gittest.Run(t, repo, "push", "-q", "-u", "origin", "main")
	gittest.Run(t, repo, "remote", "set-head", "origin", "main")

	gittest.Run(t, repo, "switch", "-qc", "feature/x")
	gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "work")
	gittest.Run(t, repo, "push", "-q", "-u", "origin", "feature/x")
	return repo, gittest.Rev(t, repo, "HEAD")
}

// prepareGitHub answers the probe and the body query for one pull request.
// author is who opened it, and an empty one makes every lookup fail — which is
// what a branch with no pull request looks like.
func prepareGitHub(t *testing.T, headOID, author string, threads string) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		if author == "" {
			fmt.Fprint(w, `{"errors":[{"type":"NOT_FOUND","message":"no pull request"}]}`)
			return
		}
		body, err := io.ReadAll(r.Body)
		if err != nil {
			t.Errorf("read the request body: %v", err)
			return
		}
		node := fmt.Sprintf(`{"number":5,"title":"t","body":"Closes #10","url":"https://example.com/pr/5",
			"state":"OPEN","author":{"login":%q},"headRefName":"feature/x","baseRefName":"main",
			"headRefOid":%q,"headRepositoryOwner":{"login":"owner"}}`, author, headOID)

		switch {
		case strings.Contains(string(body), "viewer"):
			fmt.Fprintf(w, `{"data":{"viewer":{"login":"me"},"repository":{
				"headCommit":{"committedDate":"2026-01-15T00:00:00Z"},
				"pullRequest":{
					"comments":{"totalCount":0,"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]},
					"reviews":{"totalCount":0,"nodes":[]},
					"reviewThreads":%s}}}}`, threads)
		case strings.Contains(string(body), "pullRequests("):
			fmt.Fprintf(w, `{"data":{"repository":{"pullRequests":{"nodes":[%s]}}}}`, node)
		default:
			fmt.Fprintf(w, `{"data":{"repository":{"pullRequest":%s}}}`, node)
		}
	}))
}

// noThreads is a pull request with nothing on its diff.
const noThreads = `{"totalCount":0,"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}`

// store records the contexts it is given and writes nothing, since where the
// bytes go is the command layer's business.
func store(t *testing.T, dir string, seen *[]pullrequest.Context) pullrequest.Store {
	t.Helper()

	return func(c pullrequest.Context) (string, error) {
		*seen = append(*seen, c)
		owner, name, _ := strings.Cut(c.Repo, "/")
		return filepath.Join(dir, fmt.Sprintf("pr-context-%s@%s-%d.json", owner, name, c.PR.Number)), nil
	}
}

func TestPrepareWithoutAPullRequest(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		o    pullrequest.Options
		want pullrequest.Modes
	}{
		// A local review is reviewed the way one's own work is: this
		// repository's conventions apply and the findings are acted on.
		{name: "by default", want: pullrequest.Modes{PersonalRules: true, Autofix: true}},
		{name: "local only", o: pullrequest.Options{LocalOnly: true}, want: pullrequest.Modes{PersonalRules: true, Autofix: true}},
		{name: "no autofix", o: pullrequest.Options{NoAutofix: true}, want: pullrequest.Modes{PersonalRules: true}},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo, _ := prepareRepo(t)
			var seen []pullrequest.Context
			got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, "", "", noThreads),
				ghapi.Repo{Owner: "owner", Name: "repo"}, repo, tc.o, store(t, t.TempDir(), &seen))
			if err != nil {
				t.Fatalf("Prepare: %v", err)
			}

			if got.Status != "ok" || got.PRExists {
				t.Errorf("status/pr_exists = %q/%v, want ok and false", got.Status, got.PRExists)
			}
			if diff := cmp.Diff(&tc.want, got.Modes); diff != "" {
				t.Errorf("modes (-want +got):\n%s", diff)
			}
			if got.BaseBranch == nil || *got.BaseBranch != "origin/main" {
				t.Errorf("base_branch = %v, want origin/main", got.BaseBranch)
			}
			// Nothing was fetched, so none of the paths a review writes to
			// exist yet.
			if got.ContextPath != nil || got.WorkDir != nil || got.Freshness != nil {
				t.Errorf("prepare = %+v, want the fetched fields left null", got)
			}
			if len(seen) != 0 {
				t.Errorf("a context was fetched for a branch with no pull request")
			}
		})
	}
}

func TestPrepare(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		author string
		o      pullrequest.Options
		want   pullrequest.Modes
	}{
		// Our own: the conventions apply and the findings are acted on.
		{name: "our own pull request", author: "me", want: pullrequest.Modes{PersonalRules: true, Autofix: true}},
		// Somebody else's: commented on instead, and their repository's
		// conventions are not ours to enforce.
		{name: "somebody else's", author: "other", want: pullrequest.Modes{Comment: true}},
		{
			name: "somebody else's, without posting", author: "other",
			o: pullrequest.Options{LocalOnly: true}, want: pullrequest.Modes{},
		},
		{
			name: "our own, without fixing", author: "me",
			o: pullrequest.Options{NoAutofix: true}, want: pullrequest.Modes{PersonalRules: true},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo, head := prepareRepo(t)
			scratch := t.TempDir()
			var seen []pullrequest.Context
			got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, head, tc.author, noThreads),
				ghapi.Repo{Owner: "owner", Name: "repo"}, repo, tc.o, store(t, scratch, &seen))
			if err != nil {
				t.Fatalf("Prepare: %v", err)
			}

			if got.Status != "ok" || !got.PRExists {
				t.Errorf("status/pr_exists = %q/%v, want ok and true", got.Status, got.PRExists)
			}
			if diff := cmp.Diff(&tc.want, got.Modes); diff != "" {
				t.Errorf("modes (-want +got):\n%s", diff)
			}
			if got.Flags.PRNumber == nil || *got.Flags.PRNumber != 5 {
				t.Errorf("flags.pr_number = %v, want the inferred 5", got.Flags.PRNumber)
			}
			// The three paths are handed out rather than left to the prompt,
			// and all sit in the directory paired with the context file.
			want := filepath.Join(scratch, "deep-review-owner@repo-5")
			if got.WorkDir == nil || *got.WorkDir != want {
				t.Errorf("work_dir = %v, want %q", got.WorkDir, want)
			}
			if got.ReviewPath == nil || *got.ReviewPath != filepath.Join(want, "review.json") {
				t.Errorf("review_path = %v, want it inside the work dir", got.ReviewPath)
			}
			if got.ThreadsPath == nil || *got.ThreadsPath != filepath.Join(want, "threads.json") {
				t.Errorf("threads_path = %v, want it inside the work dir", got.ThreadsPath)
			}
			if got.Freshness == nil || got.Freshness.Status != "ok" {
				t.Errorf("freshness = %+v, want an ok report", got.Freshness)
			}
			if got.BaseBranch == nil || *got.BaseBranch != "origin/main" {
				t.Errorf("base_branch = %v, want origin/main", got.BaseBranch)
			}
			// The issues come from the body's closing keywords.
			if diff := cmp.Diff([]pullrequest.LinkedIssue{{Number: 10}}, got.Issues); diff != "" {
				t.Errorf("issues (-want +got):\n%s", diff)
			}
		})
	}
}

// TestPrepareWithAnIssue covers the override: a review told which issue it is
// about does not take the pull request body's word for it.
func TestPrepareWithAnIssue(t *testing.T) {
	t.Parallel()

	repo, head := prepareRepo(t)
	var seen []pullrequest.Context
	got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, head, "me", noThreads),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo, pullrequest.Options{Issue: 42}, store(t, t.TempDir(), &seen))
	if err != nil {
		t.Fatalf("Prepare: %v", err)
	}
	if diff := cmp.Diff([]pullrequest.LinkedIssue{{Number: 42}}, got.Issues); diff != "" {
		t.Errorf("issues (-want +got):\n%s", diff)
	}
	if got.Flags.Issue == nil || *got.Flags.Issue != 42 {
		t.Errorf("flags.issue = %v, want 42", got.Flags.Issue)
	}
}

// TestPrepareStopsOnAMismatchedBranch is the guard against reviewing one
// branch's diff as though it were another's.
func TestPrepareStopsOnAMismatchedBranch(t *testing.T) {
	t.Parallel()

	repo, head := prepareRepo(t)
	gittest.Run(t, repo, "switch", "-q", "main")

	var seen []pullrequest.Context
	got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, head, "me", noThreads),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo, pullrequest.Options{Number: 5}, store(t, t.TempDir(), &seen))
	if err != nil {
		t.Fatalf("Prepare: %v", err)
	}

	if got.Status != "branch_mismatch" {
		t.Errorf("status = %q, want branch_mismatch", got.Status)
	}
	// It stops before fetching anything, so everything the fetch would settle
	// is still null.
	if got.ContextPath != nil || got.Modes != nil || got.Freshness != nil || len(seen) != 0 {
		t.Errorf("prepare = %+v, want it stopped before the fetch", got)
	}
	if got.HeadRef == nil || *got.HeadRef != "feature/x" {
		t.Errorf("head_ref = %v, want the branch it should have been on", got.HeadRef)
	}
}

// TestPrepareStopsOnAMissingPullRequest is the distinction the degradation
// rests on: a number that names nothing is a failure, where no number and no
// pull request is a local review.
func TestPrepareStopsOnAMissingPullRequest(t *testing.T) {
	t.Parallel()

	repo, _ := prepareRepo(t)
	var seen []pullrequest.Context
	got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, "", "", noThreads),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo, pullrequest.Options{Number: 999}, store(t, t.TempDir(), &seen))
	if err == nil {
		t.Fatalf("Prepare = %+v, want a failure", got)
	}
	if !strings.Contains(err.Error(), "999") {
		t.Errorf("error = %q, want it to name the pull request", err)
	}
}

// TestPrepareRaisesTheLimits covers the one rerun: whatever was cut short is
// fetched again with its own limit raised to the total, and a second truncation
// is reported rather than retried.
func TestPrepareRaisesTheLimits(t *testing.T) {
	t.Parallel()

	// More threads than arrived, so the first answer is truncated.
	const truncated = `{"totalCount":400,"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}`

	repo, head := prepareRepo(t)
	var seen []pullrequest.Context
	got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, head, "me", truncated),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo, pullrequest.Options{}, store(t, t.TempDir(), &seen))
	if err != nil {
		t.Fatalf("Prepare: %v", err)
	}

	// Stored once, though it was fetched twice: writing the truncated document
	// first would put hundreds of kilobytes on disk only to replace them.
	if len(seen) != 1 {
		t.Errorf("the context was stored %d times, want once", len(seen))
	}
	// The warning only comes from the rerun path, so its presence is what says
	// the second fetch happened — and that the rerun answered the same way, so
	// the caller is told rather than kept waiting on a third attempt.
	if len(got.Warnings) != 1 || !strings.Contains(got.Warnings[0], "MAX_THREADS to 400") {
		t.Errorf("warnings = %v, want one naming the raised limit", got.Warnings)
	}
}
