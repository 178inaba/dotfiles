package pullrequest_test

import (
	"encoding/json/v2"
	"fmt"
	"io"
	"net/http"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const readyBranch = "feature/x"

// readyOrigin builds a bare repository whose head branch has two commits past
// main, so a clone can be put behind or ahead of the pull request's head. main
// carries a tracked file, so a case can dirty the tree with a tracked
// modification rather than only an untracked one. It returns the bare
// repository, the head branch's tip, and the commit before it.
func readyOrigin(t *testing.T) (bare, head, previous string) {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	bare = gittest.Init(t, filepath.Join(base, "origin.git"), "--bare", "-b", "main")
	seed := gittest.Clone(t, bare, filepath.Join(base, "seed"))
	gittest.Write(t, filepath.Join(seed, "file.txt"), "base\n")
	gittest.Run(t, seed, "add", "file.txt")
	gittest.Run(t, seed, "commit", "-qm", "initial")
	gittest.Run(t, seed, "push", "-q", "origin", "main")

	gittest.Run(t, seed, "switch", "-qc", readyBranch)
	gittest.Run(t, seed, "commit", "-q", "--allow-empty", "-m", "one")
	previous = gittest.Rev(t, seed, readyBranch)
	gittest.Run(t, seed, "commit", "-q", "--allow-empty", "-m", "two")
	gittest.Run(t, seed, "push", "-q", "origin", readyBranch)

	return bare, gittest.Rev(t, seed, readyBranch), previous
}

// readyCheckout clones bare onto the head branch, which is where every case
// starts from.
func readyCheckout(t *testing.T, bare string) string {
	t.Helper()

	repo := gittest.Clone(t, bare, filepath.Join(t.TempDir(), "repo"))
	gittest.Run(t, repo, "switch", "-qc", readyBranch, "origin/"+readyBranch)
	return repo
}

// readyID is the node id of the pull request every case is about.
const readyID = "PR_kwDO1"

// readyPR is the pull request Run is asked about: number 7, its own author.
func readyPR(headOID string, draft bool) ghapi.PullRequest {
	return ghapi.PullRequest{
		ID: readyID, Number: 7, State: ghapi.StateOpen, Author: "178inaba",
		HeadRefName: readyBranch, BaseRefName: "main", HeadRefOid: headOID,
		IsDraft: draft, IsOwn: true,
	}
}

// readyRun is Run pointed at client and the checkout in repo.
func readyRun(client *ghapi.Client, repo string) pullrequest.Ready {
	return pullrequest.Ready{Client: client, Runner: runner.Exec{}, Repo: ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}, Dir: repo}
}

// readyCalls is what readyServer saw: every request it answered, counted by
// what it was, and the node id the last mutation named.
type readyCalls struct {
	counts map[string]int
	sentID string
}

// readyServer answers a re-read of pull request 7 with rereadHeadOID as its
// headRefOid, and the ready mutation with success.
func readyServer(t *testing.T, rereadHeadOID string, rereadDraft bool) (*ghapi.Client, *readyCalls) {
	t.Helper()

	calls := &readyCalls{counts: map[string]int{}}
	client := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		body, err := io.ReadAll(r.Body)
		if err != nil {
			t.Errorf("read the request body: %v", err)
			return
		}
		switch {
		case strings.Contains(string(body), "markPullRequestReadyForReview"):
			calls.counts["mutation"]++
			var sent struct {
				Variables struct {
					PullRequestID string `json:"pullRequestId"`
				} `json:"variables"`
			}
			if err := json.Unmarshal(body, &sent); err != nil {
				t.Errorf("decode the request body: %v", err)
				return
			}
			calls.sentID = sent.Variables.PullRequestID
			fmt.Fprint(w, `{"data":{"markPullRequestReadyForReview":{"pullRequest":{"isDraft":false}}}}`)
		case strings.Contains(string(body), "pullRequest(number:"):
			calls.counts["reread"]++
			fmt.Fprintf(w, `{"data":{"viewer":{"login":"178inaba"},"repository":{"pullRequest":{
				"id":%q,"number":7,"state":"OPEN","author":{"login":"178inaba"},
				"headRefName":%q,"baseRefName":"main","headRefOid":%q,"isDraft":%t
			}}}}`, readyID, readyBranch, rereadHeadOID, rereadDraft)
		default:
			t.Errorf("unexpected GraphQL request: %s", body)
		}
	}))
	return client, calls
}

func TestRun(t *testing.T) {
	t.Parallel()

	bare, head, _ := readyOrigin(t)

	tests := []struct {
		name  string
		setUp func(t *testing.T, repo string)
		draft bool
		// headRef overrides the pull request's HeadRefName, for a head branch
		// this origin never had.
		headRef string
		want    pullrequest.ReadyStatus
		// wantStatusLine pins what `git status --porcelain` shows after setUp,
		// for the two dirty cases whose only difference is that line.
		wantStatusLine string
	}{
		{name: "clean and at the head, still a draft", draft: true, want: pullrequest.ReadyMarked},
		{name: "clean and at the head, already out of draft", draft: false, want: pullrequest.ReadyAlready},
		{
			// A tracked file, changed but not staged — distinct from the
			// untracked case below, which git status reports with a
			// different marker (`??` rather than ` M`).
			name:           "an uncommitted change",
			setUp:          func(t *testing.T, repo string) { gittest.Write(t, filepath.Join(repo, "file.txt"), "changed\n") },
			want:           pullrequest.ReadyDirty,
			wantStatusLine: "M file.txt",
		},
		{
			// Untracked is still dirty here, unlike worktree.isDirty: a file
			// that was never added is exactly a fix that exists only
			// locally.
			name:           "an untracked file and nothing else",
			setUp:          func(t *testing.T, repo string) { gittest.Write(t, filepath.Join(repo, "untracked.txt"), "x\n") },
			want:           pullrequest.ReadyDirty,
			wantStatusLine: "?? untracked.txt",
		},
		{
			name:  "a different branch is checked out",
			setUp: func(t *testing.T, repo string) { gittest.Run(t, repo, "switch", "-q", "main") },
			want:  pullrequest.ReadyBranchMismatch,
		},
		{
			// What a pull request from a fork looks like from here: its head
			// branch is not on this origin at all.
			name:    "the head branch cannot be fetched",
			headRef: "no-such-branch",
			want:    pullrequest.ReadyFetchFailed,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo := readyCheckout(t, bare)
			if tc.setUp != nil {
				tc.setUp(t, repo)
			}
			if tc.wantStatusLine != "" {
				if got := strings.TrimSpace(gittest.Run(t, repo, "status", "--porcelain")); got != tc.wantStatusLine {
					t.Fatalf("git status --porcelain = %q, want %q", got, tc.wantStatusLine)
				}
			}
			before := gittest.Rev(t, repo, "HEAD")

			headRef := tc.headRef
			if headRef == "" {
				headRef = readyBranch
			}
			pr := readyPR(head, tc.draft)
			pr.HeadRefName = headRef

			// A mutation is the only call the ready and already_ready cases
			// make; every other case makes none, and a request the fixture
			// does not recognise fails the test rather than being ignored.
			client, calls := readyServer(t, head, false)
			rd := readyRun(client, repo)

			got, err := rd.Run(t.Context(), pr)
			if err != nil {
				t.Fatalf("Run: %v", err)
			}
			if got.Status != tc.want {
				t.Errorf("status = %q, want %q", got.Status, tc.want)
			}
			if got.Number != 7 || got.HeadRef != headRef || got.HeadOID != head {
				t.Errorf("report = %+v, want it to echo #7, %s at %s", got, headRef, head)
			}

			wantMutations := 0
			if tc.want == pullrequest.ReadyMarked {
				wantMutations = 1
			}
			if calls.counts["mutation"] != wantMutations {
				t.Errorf("mutation calls = %d, want %d", calls.counts["mutation"], wantMutations)
			}
			if wantMutations == 1 && calls.sentID != readyID {
				t.Errorf("pullRequestId = %q, want %q", calls.sentID, readyID)
			}
			if calls.counts["reread"] != 0 {
				t.Errorf("reread calls = %d, want 0: only ahead_own retries", calls.counts["reread"])
			}
			if after := gittest.Rev(t, repo, "HEAD"); after != before {
				t.Errorf("the checkout moved to %s, want it left at %s", after, before)
			}
		})
	}
}

// TestRunOnABehindCheckout is apart from the table above because it pins the
// promise CheckFreshness does not share with this command: a behind checkout
// is reported, never fast-forwarded.
func TestRunOnABehindCheckout(t *testing.T) {
	t.Parallel()

	bare, head, previous := readyOrigin(t)
	repo := readyCheckout(t, bare)
	gittest.Run(t, repo, "reset", "-q", "--hard", previous)

	pr := readyPR(head, true)
	client, calls := readyServer(t, head, false)
	rd := readyRun(client, repo)

	got, err := rd.Run(t.Context(), pr)
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Status != pullrequest.ReadyBehind {
		t.Errorf("status = %q, want %q", got.Status, pullrequest.ReadyBehind)
	}
	if got.LocalHead != previous {
		t.Errorf("local_head = %s, want the checkout left at %s", got.LocalHead, previous)
	}
	if len(calls.counts) != 0 {
		t.Errorf("GitHub was called: %+v, want no calls at all", calls.counts)
	}
	if after := gittest.Rev(t, repo, "HEAD"); after != previous {
		t.Errorf("the checkout moved to %s, want it left at %s", after, previous)
	}
}

// TestRunRetriesAheadOwnOnce covers the one retry this command makes: a local
// commit not yet registered as the pull request's head is indistinguishable
// from a lagging read until the re-read.
func TestRunRetriesAheadOwnOnce(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name         string
		rereadCaught bool
		want         pullrequest.ReadyStatus
		wantMutation int
	}{
		{
			name:         "the second read still lags",
			rereadCaught: false,
			want:         pullrequest.ReadyAheadOwn,
			wantMutation: 0,
		},
		{
			name:         "the second read has caught up",
			rereadCaught: true,
			want:         pullrequest.ReadyMarked,
			wantMutation: 1,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			bare, head, _ := readyOrigin(t)
			repo := readyCheckout(t, bare)
			gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "local")
			local := gittest.Rev(t, repo, "HEAD")

			reread := head
			if tc.rereadCaught {
				reread = local
			}
			pr := readyPR(head, true)
			client, calls := readyServer(t, reread, true)

			var waited int
			rd := readyRun(client, repo)
			rd.Wait = func() { waited++ }

			got, err := rd.Run(t.Context(), pr)
			if err != nil {
				t.Fatalf("Run: %v", err)
			}
			if got.Status != tc.want {
				t.Errorf("status = %q, want %q", got.Status, tc.want)
			}
			if waited != 1 {
				t.Errorf("Wait was called %d times, want exactly 1", waited)
			}
			if calls.counts["reread"] != 1 {
				t.Errorf("reread calls = %d, want exactly 1", calls.counts["reread"])
			}
			if calls.counts["mutation"] != tc.wantMutation {
				t.Errorf("mutation calls = %d, want %d", calls.counts["mutation"], tc.wantMutation)
			}
			if after := gittest.Rev(t, repo, "HEAD"); after != local {
				t.Errorf("the checkout moved to %s, want it left at %s", after, local)
			}
		})
	}
}
