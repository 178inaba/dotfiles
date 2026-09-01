package issue_test

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"net/http/httptest"
	"slices"
	"strings"
	"sync"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/issue"
)

const repoName = "owner/repo"

var repo = ghapi.Repo{Owner: "owner", Name: "repo"}

// at builds an api path under the repository the tests work in.
func at(suffix string) string { return "/repos/" + repoName + "/issues/" + suffix }

// fixtures are what one test's GitHub knows.
//
// A path answers with the body registered for it, a status registered for it,
// or 404; the two GraphQL lookups answer by the url they are asked about, and
// a url with no entry fails the way an unreadable one did.
type fixtures struct {
	// rest maps an api path to its body.
	rest map[string]string
	// status maps an api path to a failing status, which wins over rest.
	status map[string]int
	// pages maps an api path to a second page, reached through a Link header.
	pages map[string]string
	// closing maps an issue's html url to the urls of the pull requests that
	// close it.
	closing map[string][]string
	// prs maps a pull request's html url to its GraphQL body.
	prs map[string]string
	// nullResource makes a url answer with a null resource rather than an
	// error, which is the other way GraphQL says it could not read one.
	nullResource map[string]bool
}

// fake serves one set of fixtures and records every request, because half of
// what this command promises is the round trips it does not make.
type fake struct {
	fixtures

	mu    sync.Mutex
	calls []string
}

func (f *fake) record(s string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls = append(f.calls, s)
}

func (f *fake) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path == "/graphql" {
		f.serveGraphQL(w, r)
		return
	}
	f.record(r.URL.RequestURI())

	// The content type decides whether go-gh reads the message out of the
	// body, and the message is what reaches warnings[].
	w.Header().Set("Content-Type", "application/json")
	if s, ok := f.status[r.URL.Path]; ok {
		w.WriteHeader(s)
		fmt.Fprint(w, `{"message":"Not Found"}`)
		return
	}
	body, ok := f.rest[r.URL.Path]
	if !ok {
		w.WriteHeader(http.StatusNotFound)
		fmt.Fprint(w, `{"message":"Not Found"}`)
		return
	}
	if page2, ok := f.pages[r.URL.Path]; ok {
		if r.URL.Query().Get("page") == "2" {
			fmt.Fprint(w, page2)
			return
		}
		// The absolute url GitHub sends; the test client's transport sends it
		// back here whatever host it names.
		w.Header().Set("Link", fmt.Sprintf(`<https://api.github.com%s?per_page=100&page=2>; rel="next"`, r.URL.Path))
	}
	fmt.Fprint(w, body)
}

func (f *fake) serveGraphQL(w http.ResponseWriter, r *http.Request) {
	var req struct {
		Variables struct {
			URL string `json:"url"`
		} `json:"variables"`
	}
	if err := json.UnmarshalRead(r.Body, &req); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	f.record(req.Variables.URL)

	if f.nullResource[req.Variables.URL] {
		fmt.Fprint(w, `{"data":{"resource":null}}`)
		return
	}
	if strings.Contains(req.Variables.URL, "/pull/") {
		body, ok := f.prs[req.Variables.URL]
		if !ok {
			fmt.Fprint(w, `{"errors":[{"type":"NOT_FOUND","message":"could not resolve to a PullRequest"}]}`)
			return
		}
		fmt.Fprintf(w, `{"data":{"resource":%s}}`, body)
		return
	}
	urls, ok := f.closing[req.Variables.URL]
	if !ok {
		fmt.Fprint(w, `{"errors":[{"type":"NOT_FOUND","message":"could not resolve to an Issue"}]}`)
		return
	}
	nodes := make([]string, 0, len(urls))
	for _, u := range urls {
		nodes = append(nodes, fmt.Sprintf(`{"url":%q}`, u))
	}
	fmt.Fprintf(w, `{"data":{"resource":{"closedByPullRequestsReferences":{"nodes":[%s]}}}}`, strings.Join(nodes, ","))
}

func (f *fake) asked(substr string) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.ContainsFunc(f.calls, func(c string) bool { return strings.Contains(c, substr) })
}

// fixtureIssue is the GitHub issue object every one of these endpoints answers
// with, whether it is the issue itself, its parent, or an element of a list.
type fixtureIssue struct {
	Number       int
	Title        string
	State        string
	Repo         string
	SubTotal     int
	SubCompleted int
	BlockedBy    int
}

func (f fixtureIssue) json() string {
	r := f.Repo
	if r == "" {
		r = repoName
	}
	return fmt.Sprintf(`{"number":%d,"title":%q,"state":%q,
		"html_url":"https://github.com/%s/issues/%d",
		"repository_url":"https://api.github.com/repos/%s",
		"sub_issues_summary":{"total":%d,"completed":%d,"percent_completed":0},
		"issue_dependencies_summary":{"blocked_by":0,"blocking":0,"total_blocked_by":%d,"total_blocking":0}}`,
		f.Number, f.Title, f.State, r, f.Number, r, f.SubTotal, f.SubCompleted, f.BlockedBy)
}

// list renders issue objects as one page of a list endpoint.
func list(issues ...fixtureIssue) string {
	bodies := make([]string, 0, len(issues))
	for _, i := range issues {
		bodies = append(bodies, i.json())
	}
	return "[" + strings.Join(bodies, ",") + "]"
}

// sub and blocker name the fixtures the way the list endpoints title them, so
// that a mixed-up expectation is visible rather than merely off by a number.
func sub(n int, state string, opts ...func(*fixtureIssue)) fixtureIssue {
	f := fixtureIssue{Number: n, Title: fmt.Sprintf("Sub %d", n), State: state}
	for _, o := range opts {
		o(&f)
	}
	return f
}

func blocker(n int, state string, opts ...func(*fixtureIssue)) fixtureIssue {
	f := fixtureIssue{Number: n, Title: fmt.Sprintf("Blocker %d", n), State: state}
	for _, o := range opts {
		o(&f)
	}
	return f
}

func in(r string) func(*fixtureIssue)     { return func(f *fixtureIssue) { f.Repo = r } }
func blockedBy(n int) func(*fixtureIssue) { return func(f *fixtureIssue) { f.BlockedBy = n } }

// wantRef is the expected reference to an issue titled "<prefix> <n>".
func wantRef(prefix string, n int, state, r string) issue.Ref {
	return issue.Ref{
		Number:   n,
		Title:    fmt.Sprintf("%s %d", prefix, n),
		State:    state,
		URL:      fmt.Sprintf("https://github.com/%s/issues/%d", r, n),
		Repo:     r,
		SameRepo: r == repoName,
	}
}

func wantSub(n int, state string) issue.SubIssue {
	return issue.SubIssue{
		Number: n,
		Title:  fmt.Sprintf("Sub %d", n),
		State:  state,
		URL:    fmt.Sprintf("https://github.com/%s/issues/%d", repoName, n),
	}
}

// treeCase is one run of Tree against a fixed GitHub.
type treeCase struct {
	name   string
	server fixtures
	number int
	opts   issue.TreeOptions
	want   issue.Hierarchy
	// asked and notAsked are the round trips the answer is allowed to cost.
	asked    []string
	notAsked []string
}

func runTreeCases(t *testing.T, tests []treeCase) {
	t.Helper()

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			server := &fake{fixtures: tc.server}
			srv := httptest.NewServer(server)
			t.Cleanup(srv.Close)
			c := ghapitest.NewAt(t, srv.URL)

			got, err := issue.Tree(t.Context(), c, repo, tc.number, tc.opts)
			if err != nil {
				t.Fatalf("Tree(%d): %v", tc.number, err)
			}
			// A warning carrying a failed request quotes its url, and the test
			// server's is a fresh port every run.
			for i, warning := range got.Warnings {
				got.Warnings[i] = strings.ReplaceAll(warning, srv.URL, "https://api.github.com")
			}
			if diff := cmp.Diff(tc.want, got); diff != "" {
				t.Errorf("Tree(%d) (-want +got):\n%s", tc.number, diff)
			}
			for _, want := range tc.asked {
				if !server.asked(want) {
					t.Errorf("Tree(%d) did not request %q; requests were %v", tc.number, want, server.calls)
				}
			}
			for _, unwanted := range tc.notAsked {
				if server.asked(unwanted) {
					t.Errorf("Tree(%d) requested %q, want it skipped", tc.number, unwanted)
				}
			}
		})
	}
}

func TestTree(t *testing.T) {
	t.Parallel()

	runTreeCases(t, []treeCase{
		{
			name: "an issue with neither parent nor children",
			server: fixtures{
				rest:   map[string]string{at("10"): fixtureIssue{Number: 10, Title: "Issue 10", State: "open"}.json()},
				status: map[string]int{at("10/parent"): http.StatusNotFound},
			},
			number: 10,
			want: issue.Hierarchy{
				Repo: repoName, Number: 10, Title: "Issue 10", State: "open",
				URL:  "https://github.com/owner/repo/issues/10",
				Kind: issue.KindStandalone,
				// No children and no blockers, so neither list costs a round
				// trip: the summaries arrive with the issue, and this runs at
				// the start of every skill that reads an issue.
				BlockersClosed: true,
				SubIssues:      []issue.SubIssue{},
			},
			notAsked: []string{"10/sub_issues", "dependencies/blocked_by"},
		},
		{
			name: "a sub-issue with open siblings",
			server: fixtures{
				rest: map[string]string{
					at("21"):            fixtureIssue{Number: 21, Title: "Issue 21", State: "open"}.json(),
					at("21/parent"):     fixtureIssue{Number: 20, Title: "Issue 20", State: "open", SubTotal: 3, SubCompleted: 1}.json(),
					at("20/sub_issues"): list(sub(21, "open"), sub(22, "closed"), sub(23, "open")),
				},
			},
			number: 21,
			want: issue.Hierarchy{
				Repo: repoName, Number: 21, Title: "Issue 21", State: "open",
				URL:            "https://github.com/owner/repo/issues/21",
				Kind:           issue.KindSub,
				Parent:         new(wantRef("Issue", 20, "open", repoName)),
				BlockersClosed: true,
				SubIssues:      []issue.SubIssue{},
				Siblings:       []issue.SubIssue{wantSub(22, "closed"), wantSub(23, "open")},
			},
			// The parent's own parent is never asked for: one level up is all
			// any caller of this reads.
			notAsked: []string{"20/parent"},
		},
		{
			name: "the last open sibling",
			server: fixtures{
				rest: map[string]string{
					at("21"):            fixtureIssue{Number: 21, Title: "Issue 21", State: "open"}.json(),
					at("21/parent"):     fixtureIssue{Number: 20, Title: "Issue 20", State: "open", SubTotal: 3, SubCompleted: 2}.json(),
					at("20/sub_issues"): list(sub(22, "closed"), sub(21, "open"), sub(23, "closed")),
				},
			},
			number: 21,
			want: issue.Hierarchy{
				Repo: repoName, Number: 21, Title: "Issue 21", State: "open",
				URL:               "https://github.com/owner/repo/issues/21",
				Kind:              issue.KindSub,
				Parent:            new(wantRef("Issue", 20, "open", repoName)),
				BlockersClosed:    true,
				SubIssues:         []issue.SubIssue{},
				Siblings:          []issue.SubIssue{wantSub(22, "closed"), wantSub(23, "closed")},
				AllSiblingsClosed: true,
			},
		},
		{
			name: "a parent whose children span two pages",
			server: fixtures{
				rest: map[string]string{
					at("30"):            fixtureIssue{Number: 30, Title: "Issue 30", State: "open", SubTotal: 3, SubCompleted: 3}.json(),
					at("30/sub_issues"): list(sub(31, "closed"), sub(32, "closed")),
				},
				pages:  map[string]string{at("30/sub_issues"): list(sub(33, "closed"))},
				status: map[string]int{at("30/parent"): http.StatusNotFound},
			},
			number: 30,
			want: issue.Hierarchy{
				Repo: repoName, Number: 30, Title: "Issue 30", State: "open",
				URL:                "https://github.com/owner/repo/issues/30",
				Kind:               issue.KindParent,
				BlockersClosed:     true,
				SubIssues:          []issue.SubIssue{wantSub(31, "closed"), wantSub(32, "closed"), wantSub(33, "closed")},
				SubIssuesSummary:   issue.Summary{Total: 3, Completed: 3},
				AllSubIssuesClosed: true,
			},
			asked: []string{"30/sub_issues?per_page=100"},
		},
		{
			name: "a parent with a child still open",
			server: fixtures{
				rest: map[string]string{
					at("30"):            fixtureIssue{Number: 30, Title: "Issue 30", State: "open", SubTotal: 2, SubCompleted: 1}.json(),
					at("30/sub_issues"): list(sub(31, "closed"), sub(32, "open")),
				},
				status: map[string]int{at("30/parent"): http.StatusNotFound},
			},
			number: 30,
			want: issue.Hierarchy{
				Repo: repoName, Number: 30, Title: "Issue 30", State: "open",
				URL:              "https://github.com/owner/repo/issues/30",
				Kind:             issue.KindParent,
				BlockersClosed:   true,
				SubIssues:        []issue.SubIssue{wantSub(31, "closed"), wantSub(32, "open")},
				SubIssuesSummary: issue.Summary{Total: 2, Completed: 1},
			},
		},
		{
			name: "an issue that is both a parent and a child",
			server: fixtures{
				rest: map[string]string{
					at("41"):            fixtureIssue{Number: 41, Title: "Issue 41", State: "open", SubTotal: 1}.json(),
					at("41/parent"):     fixtureIssue{Number: 40, Title: "Issue 40", State: "open", SubTotal: 1}.json(),
					at("41/sub_issues"): list(sub(42, "open")),
					at("40/sub_issues"): list(sub(41, "open")),
				},
			},
			number: 41,
			want: issue.Hierarchy{
				Repo: repoName, Number: 41, Title: "Issue 41", State: "open",
				URL:              "https://github.com/owner/repo/issues/41",
				Kind:             issue.KindParentAndSub,
				Parent:           new(wantRef("Issue", 40, "open", repoName)),
				BlockersClosed:   true,
				SubIssues:        []issue.SubIssue{wantSub(42, "open")},
				SubIssuesSummary: issue.Summary{Total: 1},
				Siblings:         []issue.SubIssue{},
				// An only child has closed all of its siblings vacuously, which
				// is what the caller asking "am I the last one" needs.
				AllSiblingsClosed: true,
			},
		},
		{
			name: "siblings carry no annotations",
			server: fixtures{
				rest: map[string]string{
					at("71"):            fixtureIssue{Number: 71, Title: "Issue 71", State: "open"}.json(),
					at("71/parent"):     fixtureIssue{Number: 70, Title: "Issue 70", State: "open", SubTotal: 2}.json(),
					at("70/sub_issues"): list(sub(71, "open"), sub(72, "open", blockedBy(3))),
				},
			},
			number: 71,
			want: issue.Hierarchy{
				Repo: repoName, Number: 71, Title: "Issue 71", State: "open",
				URL:            "https://github.com/owner/repo/issues/71",
				Kind:           issue.KindSub,
				Parent:         new(wantRef("Issue", 70, "open", repoName)),
				BlockersClosed: true,
				SubIssues:      []issue.SubIssue{},
				Siblings:       []issue.SubIssue{wantSub(72, "open")},
			},
			// Blockers are attached to children, never to siblings.
			notAsked: []string{"72/dependencies/blocked_by"},
		},
	})
}

func TestTreeDegrades(t *testing.T) {
	t.Parallel()

	runTreeCases(t, []treeCase{
		{
			name: "the parent lookup fails for a reason other than absence",
			server: fixtures{
				rest:   map[string]string{at("10"): fixtureIssue{Number: 10, Title: "Issue 10", State: "open"}.json()},
				status: map[string]int{at("10/parent"): http.StatusInternalServerError},
			},
			number: 10,
			want: issue.Hierarchy{
				Repo: repoName, Number: 10, Title: "Issue 10", State: "open",
				URL: "https://github.com/owner/repo/issues/10",
				// Safe in the direction that matters: an issue whose parent
				// could not be read is treated as having none, and the warning
				// is what says the answer is not certain.
				Kind:           issue.KindStandalone,
				BlockersClosed: true,
				SubIssues:      []issue.SubIssue{},
				Warnings: []string{
					"parent lookup failed for #10: HTTP 500: Not Found (https://api.github.com/repos/owner/repo/issues/10/parent)",
				},
			},
		},
		{
			name: "the child list cannot be read",
			server: fixtures{
				rest: map[string]string{
					at("30"): fixtureIssue{Number: 30, Title: "Issue 30", State: "open", SubTotal: 2, SubCompleted: 1}.json(),
				},
				status: map[string]int{
					at("30/parent"):     http.StatusNotFound,
					at("30/sub_issues"): http.StatusInternalServerError,
				},
			},
			number: 30,
			want: issue.Hierarchy{
				Repo: repoName, Number: 30, Title: "Issue 30", State: "open",
				URL: "https://github.com/owner/repo/issues/30",
				// The summary still says it is a parent, so the kind survives
				// the list being unreadable.
				Kind:             issue.KindParent,
				BlockersClosed:   true,
				SubIssues:        []issue.SubIssue{},
				SubIssuesSummary: issue.Summary{Total: 2, Completed: 1},
				Warnings:         []string{"sub_issues lookup failed for #30"},
			},
		},
		{
			name: "fewer children arrive than the summary counts",
			server: fixtures{
				rest: map[string]string{
					at("30"):            fixtureIssue{Number: 30, Title: "Issue 30", State: "open", SubTotal: 3, SubCompleted: 3}.json(),
					at("30/sub_issues"): list(sub(31, "closed"), sub(32, "closed")),
				},
				status: map[string]int{at("30/parent"): http.StatusNotFound},
			},
			number: 30,
			want: issue.Hierarchy{
				Repo: repoName, Number: 30, Title: "Issue 30", State: "open",
				URL:              "https://github.com/owner/repo/issues/30",
				Kind:             issue.KindParent,
				BlockersClosed:   true,
				SubIssues:        []issue.SubIssue{wantSub(31, "closed"), wantSub(32, "closed")},
				SubIssuesSummary: issue.Summary{Total: 3, Completed: 3},
				// Every child that arrived is closed, and the answer is still
				// no: a caller about to close the parent must not act on a list
				// that is missing one.
				Warnings: []string{"sub_issues count mismatch for #30: summary=3 fetched=2"},
			},
		},
		{
			name: "the parent's child list cannot be read",
			server: fixtures{
				rest: map[string]string{
					at("21"):        fixtureIssue{Number: 21, Title: "Issue 21", State: "open"}.json(),
					at("21/parent"): fixtureIssue{Number: 20, Title: "Issue 20", State: "open", SubTotal: 2, SubCompleted: 1}.json(),
				},
				status: map[string]int{at("20/sub_issues"): http.StatusInternalServerError},
			},
			number: 21,
			want: issue.Hierarchy{
				Repo: repoName, Number: 21, Title: "Issue 21", State: "open",
				URL:            "https://github.com/owner/repo/issues/21",
				Kind:           issue.KindSub,
				Parent:         new(wantRef("Issue", 20, "open", repoName)),
				BlockersClosed: true,
				SubIssues:      []issue.SubIssue{},
				Warnings:       []string{"sub_issues lookup failed for parent #20 (siblings unknown)"},
			},
		},
		{
			name: "the parent lives in another repository",
			server: fixtures{
				rest: map[string]string{
					at("21"):        fixtureIssue{Number: 21, Title: "Issue 21", State: "open"}.json(),
					at("21/parent"): fixtureIssue{Number: 7, Title: "Issue 7", State: "open", Repo: "owner/other", SubTotal: 2, SubCompleted: 1}.json(),
				},
			},
			number: 21,
			want: issue.Hierarchy{
				Repo: repoName, Number: 21, Title: "Issue 21", State: "open",
				URL:  "https://github.com/owner/repo/issues/21",
				Kind: issue.KindSub,
				// same_repo is what tells the caller to write owner/repo#7
				// rather than #7, which would name a different issue.
				Parent:         new(wantRef("Issue", 7, "open", "owner/other")),
				BlockersClosed: true,
				SubIssues:      []issue.SubIssue{},
				Warnings:       []string{"parent #7 is in another repository (owner/other); siblings unknown"},
			},
			notAsked: []string{"issues/7/sub_issues"},
		},
	})
}

func TestTreeBlockers(t *testing.T) {
	t.Parallel()

	self := func(blocked int) string {
		return fixtureIssue{Number: 50, Title: "Issue 50", State: "open", BlockedBy: blocked}.json()
	}
	base := issue.Hierarchy{
		Repo: repoName, Number: 50, Title: "Issue 50", State: "open",
		URL:       "https://github.com/owner/repo/issues/50",
		Kind:      issue.KindStandalone,
		SubIssues: []issue.SubIssue{},
	}
	with := func(f func(*issue.Hierarchy)) issue.Hierarchy {
		h := base
		f(&h)
		return h
	}

	runTreeCases(t, []treeCase{
		{
			name: "a blocker is still open",
			server: fixtures{
				rest: map[string]string{
					at("50"):                         self(2),
					at("50/dependencies/blocked_by"): list(blocker(51, "closed"), blocker(52, "open")),
				},
				status: map[string]int{at("50/parent"): http.StatusNotFound},
			},
			number: 50,
			want: with(func(h *issue.Hierarchy) {
				h.BlockedBy = issue.RefList{Refs: []issue.Ref{
					wantRef("Blocker", 51, "closed", repoName),
					wantRef("Blocker", 52, "open", repoName),
				}}
			}),
			asked: []string{"50/dependencies/blocked_by?per_page=100"},
		},
		{
			name: "every blocker is closed",
			server: fixtures{
				rest: map[string]string{
					at("50"):                         self(2),
					at("50/dependencies/blocked_by"): list(blocker(51, "closed"), blocker(52, "closed")),
				},
				status: map[string]int{at("50/parent"): http.StatusNotFound},
			},
			number: 50,
			want: with(func(h *issue.Hierarchy) {
				// Closed blockers stay in the list, which is GitHub's own
				// behaviour and lets a caller show what the issue waited on.
				h.BlockedBy = issue.RefList{Refs: []issue.Ref{
					wantRef("Blocker", 51, "closed", repoName),
					wantRef("Blocker", 52, "closed", repoName),
				}}
				h.BlockersClosed = true
			}),
		},
		{
			name: "the blocker list cannot be read",
			server: fixtures{
				rest:   map[string]string{at("50"): self(1)},
				status: map[string]int{at("50/parent"): http.StatusNotFound, at("50/dependencies/blocked_by"): http.StatusInternalServerError},
			},
			number: 50,
			want: with(func(h *issue.Hierarchy) {
				h.BlockedBy = issue.RefList{Unknown: true}
				h.Warnings = []string{"blocked_by lookup failed for #50"}
			}),
		},
		{
			name: "fewer blockers arrive than the summary counts",
			server: fixtures{
				rest: map[string]string{
					at("50"):                         self(3),
					at("50/dependencies/blocked_by"): list(blocker(51, "closed"), blocker(52, "closed")),
				},
				status: map[string]int{at("50/parent"): http.StatusNotFound},
			},
			number: 50,
			want: with(func(h *issue.Hierarchy) {
				h.BlockedBy = issue.RefList{Refs: []issue.Ref{
					wantRef("Blocker", 51, "closed", repoName),
					wantRef("Blocker", 52, "closed", repoName),
				}}
				h.Warnings = []string{"blocked_by count mismatch for #50: summary=3 fetched=2"}
			}),
		},
		{
			name: "a blocker in another repository",
			server: fixtures{
				rest: map[string]string{
					at("50"):                         self(1),
					at("50/dependencies/blocked_by"): list(blocker(7, "open", in("owner/other"))),
				},
				status: map[string]int{at("50/parent"): http.StatusNotFound},
			},
			number: 50,
			want: with(func(h *issue.Hierarchy) {
				h.BlockedBy = issue.RefList{Refs: []issue.Ref{wantRef("Blocker", 7, "open", "owner/other")}}
			}),
		},
	})
}

func TestTreeWithPRs(t *testing.T) {
	t.Parallel()

	const (
		sub31 = "https://github.com/owner/repo/issues/31"
		sub32 = "https://github.com/owner/repo/issues/32"
		sub33 = "https://github.com/owner/repo/issues/33"
		pr310 = "https://github.com/owner/repo/pull/310"
		pr320 = "https://github.com/owner/repo/pull/320"
		pr321 = "https://github.com/other/repo/pull/321"
	)
	prJSON := func(n int, state, baseRef, url string) string {
		return fmt.Sprintf(`{"number":%d,"state":%q,"baseRefName":%q,"url":%q}`, n, state, baseRef, url)
	}
	parent := fixtureIssue{Number: 30, Title: "Issue 30", State: "open", SubTotal: 3, SubCompleted: 3}.json()
	subs := list(sub(31, "closed"), sub(32, "closed"), sub(33, "closed"))

	runTreeCases(t, []treeCase{
		{
			name: "the pull requests closing each child",
			server: fixtures{
				rest:   map[string]string{at("30"): parent, at("30/sub_issues"): subs},
				status: map[string]int{at("30/parent"): http.StatusNotFound},
				closing: map[string][]string{
					sub31: {pr310},
					sub32: {pr320, pr321},
					sub33: {},
				},
				prs: map[string]string{
					pr310: prJSON(310, "MERGED", "main", pr310),
					pr320: prJSON(320, "MERGED", "main", pr320),
					pr321: prJSON(321, "OPEN", "develop", pr321),
				},
			},
			number: 30,
			opts:   issue.TreeOptions{WithPRs: true},
			want: issue.Hierarchy{
				Repo: repoName, Number: 30, Title: "Issue 30", State: "open",
				URL:            "https://github.com/owner/repo/issues/30",
				Kind:           issue.KindParent,
				BlockersClosed: true,
				SubIssues: []issue.SubIssue{
					withPRs(wantSub(31, "closed"), issue.PRList{PRs: []issue.PR{
						{Number: 310, State: ghapi.StateMerged, BaseRef: "main", Merged: true, URL: pr310},
					}}),
					withPRs(wantSub(32, "closed"), issue.PRList{PRs: []issue.PR{
						{Number: 320, State: ghapi.StateMerged, BaseRef: "main", Merged: true, URL: pr320},
						{Number: 321, State: ghapi.StateOpen, BaseRef: "develop", Merged: false, URL: pr321},
					}}),
					// A child closed by hand has no pull request, which is an
					// empty list rather than the null a failure produces.
					withPRs(wantSub(33, "closed"), issue.PRList{PRs: []issue.PR{}}),
				},
				SubIssuesSummary:   issue.Summary{Total: 3, Completed: 3},
				AllSubIssuesClosed: true,
			},
			// Both lookups go by url, so a child or a pull request in another
			// repository is the one that is read.
			asked: []string{sub31, pr321},
		},
		{
			name: "neither lookup can be read",
			server: fixtures{
				rest:    map[string]string{at("30"): parent, at("30/sub_issues"): list(sub(31, "closed"), sub(32, "closed"))},
				status:  map[string]int{at("30/parent"): http.StatusNotFound},
				closing: map[string][]string{sub32: {pr320}},
			},
			number: 30,
			opts:   issue.TreeOptions{WithPRs: true},
			want: issue.Hierarchy{
				Repo: repoName, Number: 30, Title: "Issue 30", State: "open",
				URL:            "https://github.com/owner/repo/issues/30",
				Kind:           issue.KindParent,
				BlockersClosed: true,
				SubIssues: []issue.SubIssue{
					withPRs(wantSub(31, "closed"), issue.PRList{Unknown: true}),
					withPRs(wantSub(32, "closed"), issue.PRList{Unknown: true}),
				},
				SubIssuesSummary:   issue.Summary{Total: 3, Completed: 3},
				AllSubIssuesClosed: false,
				Warnings: []string{
					"closing PR lookup failed for Sub #31",
					"pr lookup failed for " + pr320 + " (closing Sub #32)",
					"sub_issues count mismatch for #30: summary=3 fetched=2",
				},
			},
		},
		{
			name: "GraphQL answers with no resource at all",
			server: fixtures{
				rest:         map[string]string{at("30"): fixtureIssue{Number: 30, Title: "Issue 30", State: "open", SubTotal: 1, SubCompleted: 1}.json(), at("30/sub_issues"): list(sub(31, "closed"))},
				status:       map[string]int{at("30/parent"): http.StatusNotFound},
				nullResource: map[string]bool{sub31: true},
			},
			number: 30,
			opts:   issue.TreeOptions{WithPRs: true},
			want: issue.Hierarchy{
				Repo: repoName, Number: 30, Title: "Issue 30", State: "open",
				URL:                "https://github.com/owner/repo/issues/30",
				Kind:               issue.KindParent,
				BlockersClosed:     true,
				SubIssues:          []issue.SubIssue{withPRs(wantSub(31, "closed"), issue.PRList{Unknown: true})},
				SubIssuesSummary:   issue.Summary{Total: 1, Completed: 1},
				AllSubIssuesClosed: true,
				Warnings:           []string{"closing PR lookup failed for Sub #31"},
			},
		},
		{
			name: "without the flag no pull request is read",
			server: fixtures{
				rest:    map[string]string{at("30"): fixtureIssue{Number: 30, Title: "Issue 30", State: "open", SubTotal: 1, SubCompleted: 1}.json(), at("30/sub_issues"): list(sub(31, "closed"))},
				status:  map[string]int{at("30/parent"): http.StatusNotFound},
				closing: map[string][]string{sub31: {pr310}},
			},
			number: 30,
			want: issue.Hierarchy{
				Repo: repoName, Number: 30, Title: "Issue 30", State: "open",
				URL:                "https://github.com/owner/repo/issues/30",
				Kind:               issue.KindParent,
				BlockersClosed:     true,
				SubIssues:          []issue.SubIssue{wantSub(31, "closed")},
				SubIssuesSummary:   issue.Summary{Total: 1, Completed: 1},
				AllSubIssuesClosed: true,
			},
			notAsked: []string{sub31, pr310},
		},
	})
}

func TestTreeWithDeps(t *testing.T) {
	t.Parallel()

	parent := fixtureIssue{Number: 60, Title: "Issue 60", State: "open", SubTotal: 3}.json()

	runTreeCases(t, []treeCase{
		{
			name: "each child's blockers",
			server: fixtures{
				rest: map[string]string{
					at("60"):                         parent,
					at("60/sub_issues"):              list(sub(61, "open"), sub(62, "open", blockedBy(1)), sub(63, "open", blockedBy(2))),
					at("62/dependencies/blocked_by"): list(blocker(61, "open")),
					at("63/dependencies/blocked_by"): list(blocker(61, "closed"), blocker(64, "closed")),
				},
				status: map[string]int{at("60/parent"): http.StatusNotFound},
			},
			number: 60,
			opts:   issue.TreeOptions{WithDeps: true},
			want: issue.Hierarchy{
				Repo: repoName, Number: 60, Title: "Issue 60", State: "open",
				URL:            "https://github.com/owner/repo/issues/60",
				Kind:           issue.KindParent,
				BlockersClosed: true,
				SubIssues: []issue.SubIssue{
					withDeps(wantSub(61, "open"), issue.RefList{}, true),
					withDeps(wantSub(62, "open"), issue.RefList{Refs: []issue.Ref{wantRef("Blocker", 61, "open", repoName)}}, false),
					withDeps(wantSub(63, "open"), issue.RefList{Refs: []issue.Ref{
						wantRef("Blocker", 61, "closed", repoName),
						wantRef("Blocker", 64, "closed", repoName),
					}}, true),
				},
				SubIssuesSummary: issue.Summary{Total: 3},
			},
			asked: []string{"62/dependencies/blocked_by"},
			// A child the summary says has no blockers costs no round trip.
			notAsked: []string{"61/dependencies/blocked_by"},
		},
		{
			name: "a child in another repository is asked about in its own",
			server: fixtures{
				rest: map[string]string{
					at("60"):            fixtureIssue{Number: 60, Title: "Issue 60", State: "open", SubTotal: 1}.json(),
					at("60/sub_issues"): list(sub(8, "open", in("other/repo"), blockedBy(1))),
					"/repos/other/repo/issues/8/dependencies/blocked_by": list(blocker(9, "closed", in("other/repo"))),
				},
				status: map[string]int{at("60/parent"): http.StatusNotFound},
			},
			number: 60,
			opts:   issue.TreeOptions{WithDeps: true},
			want: issue.Hierarchy{
				Repo: repoName, Number: 60, Title: "Issue 60", State: "open",
				URL:            "https://github.com/owner/repo/issues/60",
				Kind:           issue.KindParent,
				BlockersClosed: true,
				SubIssues: []issue.SubIssue{
					withDeps(
						issue.SubIssue{Number: 8, Title: "Sub 8", State: "open", URL: "https://github.com/other/repo/issues/8"},
						issue.RefList{Refs: []issue.Ref{wantRef("Blocker", 9, "closed", "other/repo")}}, true),
				},
				SubIssuesSummary: issue.Summary{Total: 1},
			},
			asked: []string{"/repos/other/repo/issues/8/dependencies/blocked_by"},
		},
		{
			name: "both annotations on the same child",
			server: fixtures{
				rest: map[string]string{
					at("60"):                         fixtureIssue{Number: 60, Title: "Issue 60", State: "open", SubTotal: 1, SubCompleted: 1}.json(),
					at("60/sub_issues"):              list(sub(61, "closed", blockedBy(1))),
					at("61/dependencies/blocked_by"): list(blocker(62, "closed")),
				},
				status:  map[string]int{at("60/parent"): http.StatusNotFound},
				closing: map[string][]string{"https://github.com/owner/repo/issues/61": {"https://github.com/owner/repo/pull/610"}},
				prs: map[string]string{
					"https://github.com/owner/repo/pull/610": `{"number":610,"state":"MERGED","baseRefName":"main","url":"https://github.com/owner/repo/pull/610"}`,
				},
			},
			number: 60,
			opts:   issue.TreeOptions{WithPRs: true, WithDeps: true},
			want: issue.Hierarchy{
				Repo: repoName, Number: 60, Title: "Issue 60", State: "open",
				URL:            "https://github.com/owner/repo/issues/60",
				Kind:           issue.KindParent,
				BlockersClosed: true,
				SubIssues: []issue.SubIssue{
					withDeps(
						withPRs(wantSub(61, "closed"), issue.PRList{PRs: []issue.PR{
							{Number: 610, State: ghapi.StateMerged, BaseRef: "main", Merged: true, URL: "https://github.com/owner/repo/pull/610"},
						}}),
						issue.RefList{Refs: []issue.Ref{wantRef("Blocker", 62, "closed", repoName)}}, true),
				},
				SubIssuesSummary:   issue.Summary{Total: 1, Completed: 1},
				AllSubIssuesClosed: true,
			},
		},
		{
			name: "without the flag no blocker of a child is read",
			server: fixtures{
				rest: map[string]string{
					at("60"):            fixtureIssue{Number: 60, Title: "Issue 60", State: "open", SubTotal: 1}.json(),
					at("60/sub_issues"): list(sub(61, "open", blockedBy(2))),
				},
				status: map[string]int{at("60/parent"): http.StatusNotFound},
			},
			number: 60,
			want: issue.Hierarchy{
				Repo: repoName, Number: 60, Title: "Issue 60", State: "open",
				URL:              "https://github.com/owner/repo/issues/60",
				Kind:             issue.KindParent,
				BlockersClosed:   true,
				SubIssues:        []issue.SubIssue{wantSub(61, "open")},
				SubIssuesSummary: issue.Summary{Total: 1},
			},
			notAsked: []string{"61/dependencies/blocked_by"},
		},
	})
}

// TestTreeFailsOnlyForTheIssueItself is the one failure that is not a
// degradation: without the issue there is nothing to describe.
func TestTreeFailsOnlyForTheIssueItself(t *testing.T) {
	t.Parallel()

	c := ghapitest.New(t, &fake{})
	if _, err := issue.Tree(t.Context(), c, repo, 99, issue.TreeOptions{}); err == nil {
		t.Fatal("Tree succeeded, want a failure")
	}
}

func withPRs(s issue.SubIssue, prs issue.PRList) issue.SubIssue {
	s.PRs = &prs
	return s
}

func withDeps(s issue.SubIssue, refs issue.RefList, closed bool) issue.SubIssue {
	s.BlockedBy = &refs
	s.BlockersClosed = &closed
	return s
}
