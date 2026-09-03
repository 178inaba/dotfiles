package pullrequest_test

import (
	"fmt"
	"io"
	"maps"
	"net/http"
	"os"
	"path/filepath"
	"slices"
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
	// The ref the change is read through, which the base repository carries
	// for every pull request and this fixture has to as well.
	gittest.Run(t, repo, "push", "-q", "origin", "HEAD:refs/pull/5/head")
	return repo, gittest.Rev(t, repo, "HEAD")
}

// prepareGitHub answers the probe and the body query for one pull request.
// author is who opened it, and an empty one makes every lookup fail — which is
// what a branch with no pull request looks like.
func prepareGitHub(t *testing.T, headOID, author string, threads string) *ghapi.Client {
	t.Helper()

	return prepareGitHubKnowing(t, headOID, author, threads, prepareIssues)
}

// prepareGitHubKnowing is prepareGitHub with the issues it knows about named,
// so that a test can leave one out and see what the run makes of that.
func prepareGitHubKnowing(t *testing.T, headOID, author, threads string, issues map[string]string) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		// The REST endpoints are routed apart before anything reads the body:
		// answering an issue lookup out of the GraphQL branch below decodes a
		// pull request into an issue and passes, which is how a fake reports a
		// parent that does not exist.
		if r.URL.Path != "/graphql" {
			serveIssue(w, r, pages{issues: issues})
			return
		}
		if author == "" {
			fmt.Fprint(w, `{"errors":[{"type":"NOT_FOUND","message":"no pull request"}]}`)
			return
		}
		body, err := io.ReadAll(r.Body)
		if err != nil {
			t.Errorf("read the request body: %v", err)
			return
		}
		node := prNode(author, headOID)

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

// prNode is the fixture pull request as both lookups answer with it.
func prNode(author, headOID string) string {
	return fmt.Sprintf(`{"number":5,"title":"t","body":"Closes #10","url":"https://example.com/pr/5",
		"state":"OPEN","author":{"login":%q},"headRefName":"feature/x","baseRefName":"main",
		"headRefOid":%q,"headRepositoryOwner":{"login":"owner"}}`, author, headOID)
}

// prepareGitHubLosingTheConversation answers the probe and then fails the query
// that fetches the conversation, which is what a run that gets as far as
// reading the change and no further looks like.
func prepareGitHubLosingTheConversation(t *testing.T, headOID string) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		if r.URL.Path != "/graphql" {
			serveIssue(w, r, pages{issues: prepareIssues})
			return
		}
		body, err := io.ReadAll(r.Body)
		if err != nil {
			t.Errorf("read the request body: %v", err)
			return
		}
		if strings.Contains(string(body), "viewer") {
			fmt.Fprint(w, `{"errors":[{"message":"conversation unavailable"}]}`)
			return
		}
		fmt.Fprintf(w, `{"data":{"repository":{"pullRequest":%s}}}`, prNode("me", headOID))
	}))
}

// noThreads is a pull request with nothing on its diff.
const noThreads = `{"totalCount":0,"pageInfo":{"hasNextPage":false,"endCursor":""},"nodes":[]}`

// prepareIssues is what the pull request's body closes, plus the unrelated
// issue --issue names instead of it.
var prepareIssues = func() map[string]string {
	m := maps.Clone(linkedIssues)
	m[issuePath("owner/repo", 42)] = issueJSON("owner/repo", 42, "Issue 42", "The overriding body")
	return m
}()

// store records the contexts it is given, and the paths, and writes nothing:
// turning one into bytes is the command layer's business.
func store(seen *[]pullrequest.Context, paths *[]string) pullrequest.Store {
	return func(path string, c pullrequest.Context) error {
		*seen = append(*seen, c)
		*paths = append(*paths, path)
		return nil
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
			o := tc.o
			o.OutDir = t.TempDir()
			var seen []pullrequest.Context
			var paths []string
			got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, "", "", noThreads),
				ghapi.Repo{Owner: "owner", Name: "repo"}, repo, o, store(&seen, &paths))
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
			o := tc.o
			o.OutDir = scratch
			var seen []pullrequest.Context
			var paths []string
			got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, head, tc.author, noThreads),
				ghapi.Repo{Owner: "owner", Name: "repo"}, repo, o, store(&seen, &paths))
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
			want := filepath.Join(scratch, "pr-owner@repo-5")
			if got.WorkDir == nil || *got.WorkDir != want {
				t.Errorf("work_dir = %v, want %q", got.WorkDir, want)
			}
			if got.ReviewPath == nil || *got.ReviewPath != filepath.Join(want, "review.json") {
				t.Errorf("review_path = %v, want it inside the work dir", got.ReviewPath)
			}
			if got.ThreadsPath == nil || *got.ThreadsPath != filepath.Join(want, "threads.json") {
				t.Errorf("threads_path = %v, want it inside the work dir", got.ThreadsPath)
			}
			// This command writes the same document the other one does, and
			// the patch goes in the same work dir the three paths are in.
			if len(seen) != 1 {
				t.Fatalf("%d contexts were stored, want 1", len(seen))
			}
			if len(seen[0].Commits) != 1 {
				t.Errorf("commits = %+v, want the one commit of the branch", seen[0].Commits)
			}
			if seen[0].Diff.Path != filepath.Join(want, "diff.patch") {
				t.Errorf("diff.path = %q, want it inside the work dir", seen[0].Diff.Path)
			}
			// One path, settled once: the document was written where
			// context_path says it is, and the work dir is the one paired with
			// that very file.
			file := filepath.Join(scratch, "pr-context-owner@repo-5.json")
			if got.ContextPath == nil || *got.ContextPath != file {
				t.Errorf("context_path = %v, want %q", got.ContextPath, file)
			}
			if diff := cmp.Diff([]string{file}, paths); diff != "" {
				t.Errorf("the document was written to (-want +got):\n%s", diff)
			}
			if got.Freshness == nil || got.Freshness.Status != "ok" {
				t.Errorf("freshness = %+v, want an ok report", got.Freshness)
			}
			if got.BaseBranch == nil || *got.BaseBranch != "origin/main" {
				t.Errorf("base_branch = %v, want origin/main", got.BaseBranch)
			}
			// The issues come from the body's closing keywords, read rather
			// than merely numbered.
			if diff := cmp.Diff([]pullrequest.LinkedIssue{{
				Number: 10, Title: new("Issue 10"), Body: new("The tenth body"),
				Parent: &pullrequest.IssueParent{Number: 9, Title: "Issue 9", Body: "The parent body"},
			}}, got.Issues); diff != "" {
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
	var paths []string
	got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, head, "me", noThreads),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo,
		pullrequest.Options{OutDir: t.TempDir(), Issue: 42}, store(&seen, &paths))
	if err != nil {
		t.Fatalf("Prepare: %v", err)
	}
	// Read the way the body's own issues are: a bare number would leave the
	// review with nothing to check the work against.
	want := []pullrequest.LinkedIssue{{Number: 42, Title: new("Issue 42"), Body: new("The overriding body")}}
	if diff := cmp.Diff(want, got.Issues); diff != "" {
		t.Errorf("issues (-want +got):\n%s", diff)
	}
	if got.Flags.Issue == nil || *got.Flags.Issue != 42 {
		t.Errorf("flags.issue = %v, want 42", got.Flags.Issue)
	}
	// The override applies to what the review checks against, not to the
	// document, which goes on saying what the pull request closes.
	if len(seen) != 1 {
		t.Fatalf("%d contexts were stored, want 1", len(seen))
	}
	if diff := cmp.Diff([]pullrequest.LinkedIssue{{
		Number: 10, Title: new("Issue 10"), Body: new("The tenth body"),
		Parent: &pullrequest.IssueParent{Number: 9, Title: "Issue 9", Body: "The parent body"},
	}}, seen[0].LinkedIssues); diff != "" {
		t.Errorf("the document's linked_issues (-want +got):\n%s", diff)
	}
}

// TestPrepareRefusesAMovedHead is what keeps a document whose head_oid and
// diff disagree from ever being written: nothing downstream could tell one
// from a sound document, so the run stops before there is one.
func TestPrepareRefusesAMovedHead(t *testing.T) {
	t.Parallel()

	repo, _ := prepareRepo(t)
	moved := "0000000000000000000000000000000000000001"

	var seen []pullrequest.Context
	var paths []string
	_, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, moved, "me", noThreads),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo,
		pullrequest.Options{OutDir: t.TempDir()}, store(&seen, &paths))
	if err == nil {
		t.Fatal("Prepare succeeded, want it to refuse a head that is not there")
	}
	if !strings.Contains(err.Error(), "run this again") {
		t.Errorf("Prepare error = %v, want it to say to run the command again", err)
	}
	if len(seen) != 0 {
		t.Errorf("%d contexts were stored for a head that could not be resolved", len(seen))
	}
}

// TestPrepareRemovesTheDocumentItReplaces is the other half of that guard.
//
// The patch is overwritten before the conversation is fetched, so a run that
// stops after it would leave this run's patch under the previous run's
// document — which points at it by path and says nothing about the head it was
// taken at, exactly the disagreement the head check exists to prevent.
func TestPrepareRemovesTheDocumentItReplaces(t *testing.T) {
	t.Parallel()

	repo, head := prepareRepo(t)
	scratch := t.TempDir()
	stale := filepath.Join(scratch, "pr-context-owner@repo-5.json")
	gittest.Write(t, stale, `{"pr":{"head_oid":"older"}}`)

	var seen []pullrequest.Context
	var paths []string
	_, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHubLosingTheConversation(t, head),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo,
		pullrequest.Options{OutDir: scratch, Number: 5}, store(&seen, &paths))
	if err == nil {
		t.Fatal("Prepare succeeded, want the failed conversation fetch to stop it")
	}
	if _, err := os.Stat(stale); !os.IsNotExist(err) {
		t.Errorf("the previous document is still there (%v); it now points at this run's patch", err)
	}
}

// TestPrepareCarriesTheIssueWarnings pins the channel: prepare-review's caller
// reads standard output and never opens the document, so a title that came
// back null with no word of why would be unexplainable from what it has.
func TestPrepareCarriesTheIssueWarnings(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		o    pullrequest.Options
		want string
	}{
		// The body's own issue, read while the document is built.
		{name: "an issue the body closes", want: "owner/repo#10: the issue could not be read (HTTP 404)"},
		// And the one --issue names instead, read separately.
		{
			name: "the issue --issue names", o: pullrequest.Options{Issue: 43},
			want: "owner/repo#43: the issue could not be read (HTTP 404)",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo, head := prepareRepo(t)
			o := tc.o
			o.OutDir = t.TempDir()
			var seen []pullrequest.Context
			var paths []string
			// #43 is in no fixture, so the endpoint answers 404 for it; for
			// the other case #10 is taken out of the ones it knows.
			known := prepareIssues
			if tc.o.Issue == 0 {
				known = maps.Clone(prepareIssues)
				delete(known, issuePath("owner/repo", 10))
			}
			gh := prepareGitHubKnowing(t, head, "me", noThreads, known)
			got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, gh,
				ghapi.Repo{Owner: "owner", Name: "repo"}, repo, o, store(&seen, &paths))
			if err != nil {
				t.Fatalf("Prepare: %v", err)
			}
			if !slices.Contains(got.Warnings, tc.want) {
				t.Errorf("warnings = %v, want one saying %q", got.Warnings, tc.want)
			}
		})
	}
}

// TestPrepareStopsOnAMismatchedBranch is the guard against reviewing one
// branch's diff as though it were another's.
func TestPrepareStopsOnAMismatchedBranch(t *testing.T) {
	t.Parallel()

	repo, head := prepareRepo(t)
	gittest.Run(t, repo, "switch", "-q", "main")

	var seen []pullrequest.Context
	var paths []string
	got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, head, "me", noThreads),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo,
		pullrequest.Options{OutDir: t.TempDir(), Number: 5}, store(&seen, &paths))
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
	var paths []string
	got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, "", "", noThreads),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo,
		pullrequest.Options{OutDir: t.TempDir(), Number: 999}, store(&seen, &paths))
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
	var paths []string
	got, err := pullrequest.Prepare(t.Context(), runner.Exec{}, prepareGitHub(t, head, "me", truncated),
		ghapi.Repo{Owner: "owner", Name: "repo"}, repo,
		pullrequest.Options{OutDir: t.TempDir()}, store(&seen, &paths))
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
