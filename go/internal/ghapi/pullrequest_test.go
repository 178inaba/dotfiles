package ghapi_test

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"strconv"
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

// viewerLogin is who every fixture in this file answers viewer { login } with,
// which is also node's author — so a case wanting IsOwn false has to say so
// itself, by naming a different author or leaving the viewer field out.
const viewerLogin = "178inaba"

// node is one pull request as GraphQL returns it.
func node(number int, state, headOwner string) string {
	return fmt.Sprintf(`{
		"id": "PR_%d",
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
	}`, number, number, number, state, headOwner)
}

// withViewer puts data.viewer.login beside data.repository, which is where
// every query in this package asks for it.
func withViewer(login, body string) string {
	return `{"data":{"viewer":{"login":` + strconv.Quote(login) + `},"repository":` + body + `}}`
}

func wantPR(number int, state ghapi.PRState) ghapi.PullRequest {
	return ghapi.PullRequest{
		ID:          fmt.Sprintf("PR_%d", number),
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
		// Every fixture's viewer is the author, so an owned pull request is
		// the default a case has to opt out of rather than into.
		IsOwn: true,
	}
}

func TestPullRequest(t *testing.T) {
	t.Parallel()

	var vars map[string]any
	c := ghapitest.New(t, graphQL(t, withViewer(viewerLogin, `{"pullRequest":`+node(128, "OPEN", "178inaba")+`}`), &vars))

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
// null the output contracts would then have to carry. It also pins IsOwn
// against the vacuous pass an empty author and an empty viewer would both
// answer true to: the viewer here is somebody, so an empty author is not them.
func TestPullRequestKeepsAMissingAuthor(t *testing.T) {
	t.Parallel()

	body := withViewer(viewerLogin, `{"pullRequest":{"number":9,"state":"MERGED","author":null}}`)
	c := ghapitest.New(t, graphQL(t, body, nil))

	got, err := c.PullRequest(t.Context(), repo, 9)
	if err != nil {
		t.Fatalf("PullRequest: %v", err)
	}
	if got.Author != "" {
		t.Errorf("Author = %q, want it empty", got.Author)
	}
	if got.IsOwn {
		t.Error("IsOwn = true, want false: a missing author is not the viewer")
	}
	if got.State != ghapi.StateMerged {
		t.Errorf("State = %q, want %q", got.State, ghapi.StateMerged)
	}
}

// TestPullRequestEmptyViewerOwnsNothing pins the other half of the vacuous pass:
// a response whose viewer login is empty does not own a pull request whose
// author is empty too.
func TestPullRequestEmptyViewerOwnsNothing(t *testing.T) {
	t.Parallel()

	body := withViewer("", `{"pullRequest":{"number":9,"state":"MERGED","author":null}}`)
	c := ghapitest.New(t, graphQL(t, body, nil))

	got, err := c.PullRequest(t.Context(), repo, 9)
	if err != nil {
		t.Fatalf("PullRequest: %v", err)
	}
	if got.IsOwn {
		t.Error("IsOwn = true, want false: an empty viewer is nobody")
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
			body := withViewer(viewerLogin, `{"pullRequests":{"nodes":[`+strings.Join(tc.nodes, ",")+`]}}`)
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

			body := withViewer(viewerLogin, `{"pullRequests":{"nodes":[`+strings.Join(tc.nodes, ",")+`]}}`)
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
			body := withViewer(viewerLogin, `{"pullRequests":{"nodes":[`+node(130, "OPEN", tc.headOwner)+`]}}`)
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

// edit is what reached the pull request endpoint, so that a case asserts the
// body GitHub was asked to store without a second copy of the join.
type edit struct {
	path string
	body string
}

// appendServer answers the read with live as the pull request's body and
// captures the edit that follows it.
//
// One handler for both halves, because the append is one function's
// read-modify-write: a case that stubbed them apart could not tell that the
// body sent was built from the body read.
func appendServer(t *testing.T, live string, sent *edit) http.Handler {
	t.Helper()

	read := graphQL(t, fmt.Sprintf(
		`{"data":{"repository":{"pullRequest":{"number":7,"body":%s,"url":"https://github.com/178inaba/dotfiles/pull/7"}}}}`,
		strconv.Quote(live)), nil)

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPatch {
			read.ServeHTTP(w, r)
			return
		}
		var req struct {
			Body string `json:"body"`
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
		*sent = edit{path: r.URL.Path, body: req.Body}
		fmt.Fprint(w, `{"html_url":"https://github.com/178inaba/dotfiles/pull/7"}`)
	})
}

func TestAppendToPullRequestBody(t *testing.T) {
	t.Parallel()

	const section = "## The decision\n\nKept as it is.\n"
	for _, tt := range []struct {
		name, live, want string
	}{
		// The body the document was fetched with is not what is appended to:
		// somebody edited the pull request in between, and what they wrote is
		// still there afterwards.
		{
			name: "onto the body GitHub holds",
			live: "The original description.\n",
			want: "The original description.\n\n" + section,
		},
		// A body with nothing in it takes the section alone, rather than
		// opening with the blank line a join would leave.
		{name: "onto an empty body", want: section},
		// The current body is kept whatever it says: it is what a person
		// typed, and refusing it would lose it rather than protect anybody.
		{
			name: "onto a body that would fail the numbering rule itself",
			live: "#1 one\n#2 two\n#3 three\n",
			want: "#1 one\n#2 two\n#3 three\n\n" + section,
		},
	} {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var sent edit
			c := ghapitest.New(t, appendServer(t, tt.live, &sent))

			body, err := ghapi.NewPullRequestBody(section)
			if err != nil {
				t.Fatalf("NewPullRequestBody: %v", err)
			}
			url, err := c.AppendToPullRequestBody(t.Context(), repo, 7, body)
			if err != nil {
				t.Fatalf("AppendToPullRequestBody: %v", err)
			}

			if want := "https://github.com/178inaba/dotfiles/pull/7"; url != want {
				t.Errorf("url = %q, want %q", url, want)
			}
			if want := "/repos/178inaba/dotfiles/pulls/7"; sent.path != want {
				t.Errorf("edited %q, want %q", sent.path, want)
			}
			if sent.body != tt.want {
				t.Errorf("body sent = %q, want %q", sent.body, tt.want)
			}
		})
	}
}

// One blank line between the body and the section, whatever the section file
// happens to open and end with: a file written by hand carries blank lines
// nobody meant as content, and two of them read as a gap in the rendering.
func TestAppendToPullRequestBodyKeepsTheJoinToOneBlankLine(t *testing.T) {
	t.Parallel()

	var sent edit
	c := ghapitest.New(t, appendServer(t, "The original description.\n\n", &sent))

	body, err := ghapi.NewPullRequestBody("\n\n## The decision\n\nKept as it is.\n\n\n")
	if err != nil {
		t.Fatalf("NewPullRequestBody: %v", err)
	}
	if _, err := c.AppendToPullRequestBody(t.Context(), repo, 7, body); err != nil {
		t.Fatalf("AppendToPullRequestBody: %v", err)
	}

	want := "The original description.\n\n## The decision\n\nKept as it is.\n"
	if sent.body != want {
		t.Errorf("body sent = %q, want %q", sent.body, want)
	}
}

// A second run of the same escalation would otherwise write the section twice,
// which is what a retry after a reply that never reached GitHub looks like.
// The stored body comes back with CRLF line endings, so the comparison cannot
// be of the bytes as they arrive.
func TestAppendToPullRequestBodyRefusesASectionAlreadyThere(t *testing.T) {
	t.Parallel()

	const section = "## The decision\n\nKept as it is.\n"

	var sent edit
	c := ghapitest.New(t, appendServer(t, "Original.\r\n\r\n## The decision\r\n\r\nKept as it is.\r\n", &sent))

	body, err := ghapi.NewPullRequestBody(section)
	if err != nil {
		t.Fatalf("NewPullRequestBody: %v", err)
	}
	if _, err := c.AppendToPullRequestBody(t.Context(), repo, 7, body); err == nil {
		t.Fatal("AppendToPullRequestBody appended a section already there, want a refusal")
	} else if !strings.Contains(err.Error(), "already") {
		t.Errorf("error = %q, want it to say the section is already in the body", err)
	}
	if sent != (edit{}) {
		t.Errorf("an edit was sent: %+v", sent)
	}
}

func TestMarkPullRequestReadyForReview(t *testing.T) {
	t.Parallel()

	var sent struct {
		Variables struct {
			PullRequestID string `json:"pullRequestId"`
		} `json:"variables"`
	}
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if err := json.NewDecoder(r.Body).Decode(&sent); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		fmt.Fprint(w, `{"data":{"markPullRequestReadyForReview":{"pullRequest":{"isDraft":false}}}}`)
	}))

	if err := c.MarkPullRequestReadyForReview(t.Context(), "PR_kwDO1"); err != nil {
		t.Fatalf("MarkPullRequestReadyForReview: %v", err)
	}

	if want := "PR_kwDO1"; sent.Variables.PullRequestID != want {
		t.Errorf("pullRequestId = %q, want %q", sent.Variables.PullRequestID, want)
	}
}
