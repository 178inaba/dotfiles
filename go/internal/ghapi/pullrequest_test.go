package ghapi_test

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
)

var repo = ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}

// graphQL replies with body and hands the variables it received back through
// vars, which is how a test checks that the right question was asked.
func graphQL(t *testing.T, body string, vars *map[string]any) http.Handler {
	t.Helper()

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var req struct {
			Variables map[string]any `json:"variables"`
		}
		b, err := io.ReadAll(r.Body)
		if err != nil {
			t.Errorf("read the request body: %v", err)
			return
		}
		if err := json.Unmarshal(b, &req); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		if vars != nil {
			*vars = req.Variables
		}
		fmt.Fprint(w, body)
	})
}

// node is one pull request as GraphQL returns it.
func node(number int, state, headOwner string) string {
	return fmt.Sprintf(`{
		"number": %d,
		"title": "Port the scripts",
		"body": "Closes #121",
		"url": "https://github.com/178inaba/dotfiles/pull/%d",
		"state": %q,
		"author": {"login": "178inaba"},
		"headRefName": "feature/121-port-scripts-to-ccx",
		"baseRefName": "main",
		"headRefOid": "379223e",
		"reviewDecision": "APPROVED",
		"isDraft": true,
		"headRepositoryOwner": {"login": %q}
	}`, number, number, state, headOwner)
}

func wantPR(number int, state ghapi.PRState) ghapi.PullRequest {
	return ghapi.PullRequest{
		Number:      number,
		Title:       "Port the scripts",
		Body:        "Closes #121",
		URL:         fmt.Sprintf("https://github.com/178inaba/dotfiles/pull/%d", number),
		State:       state,
		Author:      "178inaba",
		HeadRefName: "feature/121-port-scripts-to-ccx",
		BaseRefName: "main",
		HeadRefOid:  "379223e",
		// The badge is the one reader of these two, and it needs them from
		// whichever query the caller took, so the fixture carries them on both.
		ReviewDecision: "APPROVED",
		IsDraft:        true,
	}
}

func TestPullRequest(t *testing.T) {
	t.Parallel()

	var vars map[string]any
	c := ghapitest.New(t, graphQL(t, `{"data":{"repository":{"pullRequest":`+node(128, "OPEN", "178inaba")+`}}}`, &vars))

	got, err := c.PullRequest(t.Context(), repo, 128)
	if err != nil {
		t.Fatalf("PullRequest: %v", err)
	}

	if diff := cmp.Diff(wantPR(128, ghapi.StateOpen), got); diff != "" {
		t.Errorf("PullRequest (-want +got):\n%s", diff)
	}
	wantVars := map[string]any{"owner": "178inaba", "name": "dotfiles", "number": float64(128)}
	if diff := cmp.Diff(wantVars, vars); diff != "" {
		t.Errorf("query variables (-want +got):\n%s", diff)
	}
}

// TestPullRequestKeepsAMissingAuthor pins what gh produced for a pull request
// whose author has deleted their account: a login of empty string rather than a
// null the output contracts would then have to carry.
func TestPullRequestKeepsAMissingAuthor(t *testing.T) {
	t.Parallel()

	body := `{"data":{"repository":{"pullRequest":{"number":9,"state":"MERGED","author":null}}}}`
	c := ghapitest.New(t, graphQL(t, body, nil))

	got, err := c.PullRequest(t.Context(), repo, 9)
	if err != nil {
		t.Fatalf("PullRequest: %v", err)
	}
	if got.Author != "" {
		t.Errorf("Author = %q, want it empty", got.Author)
	}
	if got.State != ghapi.StateMerged {
		t.Errorf("State = %q, want %q", got.State, ghapi.StateMerged)
	}
}

// TestPullRequestNotFound is the distinction issue-hierarchy needs from every
// other failure, so it has to survive the wrapping this adds.
func TestPullRequestNotFound(t *testing.T) {
	t.Parallel()

	body := `{"errors":[{"type":"NOT_FOUND","message":"Could not resolve to a PullRequest"}]}`
	c := ghapitest.New(t, graphQL(t, body, nil))

	_, err := c.PullRequest(t.Context(), repo, 999)
	if err == nil {
		t.Fatal("PullRequest succeeded, want a failure")
	}
	if !ghapi.IsNotFound(err) {
		t.Errorf("IsNotFound(%v) = false, want true", err)
	}
}

func TestPullRequestForCurrentBranch(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		nodes []string
		want  ghapi.PullRequest
	}{
		{
			name:  "the one pull request on the branch",
			nodes: []string{node(128, "OPEN", "178inaba")},
			want:  wantPR(128, ghapi.StateOpen),
		},
		{
			// The branch was reused after its first pull request merged, and
			// GitHub returns the newer one first; the open one is still the
			// work in progress.
			name:  "an open pull request beats a merged one",
			nodes: []string{node(130, "MERGED", "178inaba"), node(128, "OPEN", "178inaba")},
			want:  wantPR(128, ghapi.StateOpen),
		},
		{
			// Nothing is open, so the newest answers — which is how a merged
			// branch still resolves to the pull request that merged it.
			name:  "the newest of two closed pull requests",
			nodes: []string{node(130, "MERGED", "178inaba"), node(128, "CLOSED", "178inaba")},
			want:  wantPR(130, ghapi.StateMerged),
		},
		{
			// A fork's branch of the same name is a different branch, and the
			// local one is not it.
			name:  "a fork's pull request is not this branch",
			nodes: []string{node(130, "OPEN", "someone"), node(128, "MERGED", "178inaba")},
			want:  wantPR(128, ghapi.StateMerged),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			var vars map[string]any
			body := `{"data":{"repository":{"pullRequests":{"nodes":[` + strings.Join(tc.nodes, ",") + `]}}}}`
			c := ghapitest.New(t, graphQL(t, body, &vars))

			run := &fakeRunner{out: "feature/121-port-scripts-to-ccx\n"}
			got, err := c.PullRequestForCurrentBranch(t.Context(), run, "/repo", repo)
			if err != nil {
				t.Fatalf("PullRequestForCurrentBranch: %v", err)
			}

			if diff := cmp.Diff(tc.want, got); diff != "" {
				t.Errorf("PullRequestForCurrentBranch (-want +got):\n%s", diff)
			}
			if got, want := vars["headRefName"], "feature/121-port-scripts-to-ccx"; got != want {
				t.Errorf("headRefName = %v, want %q", got, want)
			}
			wantCalls := [][]string{{"git", "-C", "/repo", "branch", "--show-current"}}
			if diff := cmp.Diff(wantCalls, run.calls); diff != "" {
				t.Errorf("commands run (-want +got):\n%s", diff)
			}
		})
	}
}

func TestPullRequestForCurrentBranchFailures(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		branch string
		fail   bool
		nodes  []string
	}{
		{name: "no pull request has this head", branch: "feature/121-port-scripts-to-ccx"},
		{
			name:   "only a fork's pull request has this head",
			branch: "feature/121-port-scripts-to-ccx",
			nodes:  []string{node(130, "OPEN", "someone")},
		},
		{
			// git prints nothing on a detached head, and there is no branch to
			// infer from.
			name: "a detached head", branch: "\n",
		},
		{name: "git could not run", fail: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			body := `{"data":{"repository":{"pullRequests":{"nodes":[` + strings.Join(tc.nodes, ",") + `]}}}}`
			c := ghapitest.New(t, graphQL(t, body, nil))

			got, err := c.PullRequestForCurrentBranch(t.Context(), &fakeRunner{out: tc.branch, fail: tc.fail}, "/repo", repo)
			if err == nil {
				t.Fatalf("PullRequestForCurrentBranch = %v, want an error", got)
			}
		})
	}
}

// TestPullRequestForBranchHeadOwner covers what PullRequestForCurrentBranch
// cannot reach: it always passes repo's own owner, so the empty filter has only
// the badge as a caller. That is the fork checkout, where the head lives on
// someone else's copy and the pull request is still one of this repository's.
func TestPullRequestForBranchHeadOwner(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name      string
		headOwner string
		nodes     []string
		want      ghapi.PullRequest
		wantErr   bool
	}{
		{
			name:      "an empty owner accepts a head on a fork",
			headOwner: "",
			nodes:     []string{node(130, "OPEN", "someone")},
			want:      wantPR(130, ghapi.StateOpen),
		},
		{
			// The narrowing is still there when an owner is named, so the
			// existing callers keep the behaviour they had.
			name:      "a named owner rejects the same head",
			headOwner: "178inaba",
			nodes:     []string{node(130, "OPEN", "someone")},
			wantErr:   true,
		},
		{
			// Open still beats merged with the filter off, rather than the
			// widening changing which of two candidates wins.
			name:      "an empty owner still prefers the open one",
			headOwner: "",
			nodes:     []string{node(130, "MERGED", "someone"), node(128, "OPEN", "someone")},
			want:      wantPR(128, ghapi.StateOpen),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			body := `{"data":{"repository":{"pullRequests":{"nodes":[` + strings.Join(tc.nodes, ",") + `]}}}}`
			c := ghapitest.New(t, graphQL(t, body, nil))

			got, err := c.PullRequestForBranch(t.Context(), repo, "feature/121-port-scripts-to-ccx", tc.headOwner)
			if tc.wantErr {
				if err == nil {
					t.Fatalf("PullRequestForBranch = %v, want an error", got)
				}
				return
			}
			if err != nil {
				t.Fatalf("PullRequestForBranch: %v", err)
			}
			if diff := cmp.Diff(tc.want, got); diff != "" {
				t.Errorf("PullRequestForBranch (-want +got):\n%s", diff)
			}
		})
	}
}

// TestPullRequestForBranchUsesTheGivenRef is the other half of what the badge
// needs: the head ref is the argument, not the checked-out branch, so no git
// call decides which pull request is looked up.
func TestPullRequestForBranchUsesTheGivenRef(t *testing.T) {
	t.Parallel()

	var vars map[string]any
	body := `{"data":{"repository":{"pullRequests":{"nodes":[` + node(128, "OPEN", "178inaba") + `]}}}}`
	c := ghapitest.New(t, graphQL(t, body, &vars))

	if _, err := c.PullRequestForBranch(t.Context(), repo, "fork-side-name", ""); err != nil {
		t.Fatalf("PullRequestForBranch: %v", err)
	}
	if got, want := vars["headRefName"], "fork-side-name"; got != want {
		t.Errorf("headRefName = %v, want %q", got, want)
	}
}
