package ghapi_test

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/runner"
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

// configRunner answers git config by the setting being read, so a test can
// describe a branch's tracking configuration rather than a command sequence.
type configRunner struct{ settings map[string]string }

func (c configRunner) Run(_ context.Context, cmd runner.Command) ([]byte, error) {
	out, ok := c.settings[cmd.Args[len(cmd.Args)-1]]
	if !ok {
		// git config exits non-zero for a setting that is not set, and the
		// caller has to see that rather than an empty value.
		return nil, &runner.Error{Name: cmd.Name, Err: os.ErrNotExist}
	}
	return []byte(out), nil
}

// TestPullRequestForBranch covers the resolution PullRequestForCurrentBranch
// leaves out. `gh pr checkout` on a pull request from a fork writes
// branch.<name>.merge and branch.<name>.remote, and without reading them such a
// branch resolves to nothing at all.
func TestPullRequestForBranch(t *testing.T) {
	t.Parallel()

	const branch = "feature/121-port-scripts-to-ccx"

	tests := []struct {
		name     string
		settings map[string]string
		// headOwner is who owns the head of the pull request GitHub returns.
		headOwner string
		wantRef   string
		wantErr   bool
	}{
		{
			name:      "no branch config uses the local name",
			headOwner: "178inaba",
			wantRef:   branch,
		},
		{
			name: "a fork checkout follows merge and remote",
			settings: map[string]string{
				"branch." + branch + ".merge":  "refs/heads/their-branch\n",
				"branch." + branch + ".remote": "git@github.com:someone/dotfiles.git\n",
			},
			headOwner: "someone",
			wantRef:   "their-branch",
		},
		{
			// Otherwise a fork's branch of the same name could answer for it.
			name: "a remote pointing at this repository keeps the narrowing",
			settings: map[string]string{
				"branch." + branch + ".merge":  "refs/heads/" + branch + "\n",
				"branch." + branch + ".remote": "origin\n",
				"remote.origin.url":            "git@github.com:178inaba/dotfiles.git\n",
			},
			headOwner: "someone",
			wantRef:   branch,
			wantErr:   true,
		},
		{
			name: "a named remote is resolved through its url",
			settings: map[string]string{
				"branch." + branch + ".merge":  "refs/heads/their-branch\n",
				"branch." + branch + ".remote": "fork\n",
				"remote.fork.url":              "git@github.com:someone/dotfiles.git\n",
			},
			headOwner: "someone",
			wantRef:   "their-branch",
		},
		{
			// The ref is still the one merge names; nothing says it is elsewhere.
			name: "merge without a remote keeps the narrowing",
			settings: map[string]string{
				"branch." + branch + ".merge": "refs/heads/their-branch\n",
			},
			headOwner: "someone",
			wantRef:   "their-branch",
			wantErr:   true,
		},
		{
			name: "a merge that is not a branch ref is ignored",
			settings: map[string]string{
				"branch." + branch + ".merge":  "refs/tags/v1\n",
				"branch." + branch + ".remote": "git@github.com:someone/dotfiles.git\n",
			},
			headOwner: "178inaba",
			wantRef:   branch,
		},
		{
			name: "an unresolvable remote keeps the narrowing",
			settings: map[string]string{
				"branch." + branch + ".merge":  "refs/heads/their-branch\n",
				"branch." + branch + ".remote": "gone\n",
			},
			headOwner: "someone",
			wantRef:   "their-branch",
			wantErr:   true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			var vars map[string]any
			body := `{"data":{"repository":{"pullRequests":{"nodes":[` + node(130, "OPEN", tc.headOwner) + `]}}}}`
			c := ghapitest.New(t, graphQL(t, body, &vars))

			got, err := c.PullRequestForBranch(t.Context(), configRunner{settings: tc.settings}, "/repo", repo, branch)
			if tc.wantErr {
				if err == nil {
					t.Errorf("PullRequestForBranch = %v, want an error", got)
				}
			} else {
				if err != nil {
					t.Fatalf("PullRequestForBranch: %v", err)
				}
				if diff := cmp.Diff(wantPR(130, ghapi.StateOpen), got); diff != "" {
					t.Errorf("PullRequestForBranch (-want +got):\n%s", diff)
				}
			}
			// Asserted on both paths: which ref was asked about is the whole
			// point of reading the config, and a rejected owner still proves it.
			if got := vars["headRefName"]; got != tc.wantRef {
				t.Errorf("headRefName = %v, want %q", got, tc.wantRef)
			}
		})
	}
}
