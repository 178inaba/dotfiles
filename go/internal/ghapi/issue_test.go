package ghapi_test

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
)

var issueRepo = ghapi.Repo{Owner: "owner", Name: "repo"}

// issues answers the issue and parent endpoints from a fixed set of bodies and
// failing statuses; a path in neither answers 404, which is how the parent
// endpoint says an issue is nobody's child.
func issues(t *testing.T, bodies map[string]string, status map[string]int) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		if s, ok := status[r.URL.Path]; ok {
			w.WriteHeader(s)
			fmt.Fprint(w, `{"message":"unavailable"}`)
			return
		}
		body, ok := bodies[r.URL.Path]
		if !ok {
			w.WriteHeader(http.StatusNotFound)
			fmt.Fprint(w, `{"message":"Not Found"}`)
			return
		}
		fmt.Fprint(w, body)
	}))
}

// issueBody is a GitHub issue object, as much of one as Issue reads.
func issueBody(repo string, number int, title, body string) string {
	return fmt.Sprintf(`{"number":%d,"title":%q,"body":%q,"state":"open",
		"html_url":"https://github.com/%s/issues/%d",
		"repository_url":"https://api.github.com/repos/%s"}`,
		number, title, body, repo, number, repo)
}

func TestIssue(t *testing.T) {
	t.Parallel()

	c := issues(t, map[string]string{
		"/repos/owner/repo/issues/10": issueBody("owner/repo", 10, "Issue 10", "The body"),
	}, nil)

	got, err := c.Issue(t.Context(), issueRepo, 10)
	if err != nil {
		t.Fatalf("Issue: %v", err)
	}
	want := ghapi.Issue{
		Number: 10, Title: "Issue 10", Body: "The body", State: "open",
		URL: "https://github.com/owner/repo/issues/10", Repo: issueRepo,
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Issue (-want +got):\n%s", diff)
	}

	if _, err := c.Issue(t.Context(), issueRepo, 11); err == nil {
		t.Error("Issue succeeded for an issue that is not there, want a failure")
	}
}

// TestIssueReadsTheCommentCount is the reason the count is decoded here rather
// than counted: it arrives with the body the enrichment already fetches, so the
// total a truncated comment list is measured against costs no request.
func TestIssueReadsTheCommentCount(t *testing.T) {
	t.Parallel()

	c := issues(t, map[string]string{
		"/repos/owner/repo/issues/10": `{"number":10,"title":"Issue 10","body":"","state":"open","comments":3,
			"html_url":"https://github.com/owner/repo/issues/10",
			"repository_url":"https://api.github.com/repos/owner/repo"}`,
	}, nil)

	got, err := c.Issue(t.Context(), issueRepo, 10)
	if err != nil {
		t.Fatalf("Issue: %v", err)
	}
	if got.Comments != 3 {
		t.Errorf("Issue.Comments = %d, want 3", got.Comments)
	}
}

// TestIssueParent pins the distinction the whole lookup exists for: the
// endpoint's 404 is an issue with no parent, and every other failure is a
// failure, because the two callers degrade differently.
func TestIssueParent(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		bodies map[string]string
		status map[string]int
		want   *ghapi.Issue
		fails  bool
	}{
		{
			name: "a parent, with its body",
			bodies: map[string]string{
				"/repos/owner/repo/issues/21/parent": issueBody("owner/repo", 20, "Issue 20", "The parent body"),
			},
			want: &ghapi.Issue{
				Number: 20, Title: "Issue 20", Body: "The parent body", State: "open",
				URL: "https://github.com/owner/repo/issues/20", Repo: issueRepo,
			},
		},
		{
			name: "a parent in another repository",
			bodies: map[string]string{
				"/repos/owner/repo/issues/21/parent": issueBody("owner/other", 7, "Issue 7", ""),
			},
			want: &ghapi.Issue{
				Number: 7, Title: "Issue 7", State: "open",
				URL: "https://github.com/owner/other/issues/7", Repo: ghapi.Repo{Owner: "owner", Name: "other"},
			},
		},
		{name: "no parent"},
		{
			name:   "the lookup fails for another reason",
			status: map[string]int{"/repos/owner/repo/issues/21/parent": http.StatusInternalServerError},
			fails:  true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := issues(t, tc.bodies, tc.status).IssueParent(t.Context(), issueRepo, 21)
			if tc.fails {
				if err == nil {
					t.Fatal("IssueParent succeeded, want a failure")
				}
				return
			}
			if err != nil {
				t.Fatalf("IssueParent: %v", err)
			}
			if diff := cmp.Diff(tc.want, got); diff != "" {
				t.Errorf("IssueParent (-want +got):\n%s", diff)
			}
		})
	}
}

// commentBody is a GitHub issue comment, as much of one as IssueComment reads.
// user is written verbatim so that a deleted author can be given as null.
func commentBody(id int, user, body string) string {
	return fmt.Sprintf(`{"user":%s,"body":%q,"created_at":"2026-01-0%dT00:00:00Z",
		"html_url":"https://github.com/owner/repo/issues/10#issuecomment-%d"}`,
		user, body, id, id)
}

// TestIssueCommentsFollowsTheLinkHeader covers the whole mapping in one pass:
// the pages are concatenated in the order GitHub sends them, which is oldest
// first, and every field of every shape of author arrives.
func TestIssueCommentsFollowsTheLinkHeader(t *testing.T) {
	t.Parallel()

	var srv *httptest.Server
	var gotPerPage string
	srv = httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		if r.URL.Query().Get("page") == "2" {
			fmt.Fprintf(w, `[%s]`, commentBody(3, `null`, "third"))
			return
		}
		gotPerPage = r.URL.Query().Get("per_page")
		w.Header().Set("Link", fmt.Sprintf(`<%s/repos/owner/repo/issues/10/comments?page=2>; rel="next"`, srv.URL))
		fmt.Fprintf(w, `[%s,%s]`,
			commentBody(1, `{"login":"178inaba","type":"User"}`, "first"),
			commentBody(2, `{"login":"github-actions","type":"Bot"}`, "second"))
	}))
	t.Cleanup(srv.Close)

	got, err := ghapitest.NewAt(t, srv.URL).IssueComments(t.Context(), issueRepo, 10, 100)
	if err != nil {
		t.Fatalf("IssueComments: %v", err)
	}

	want := []ghapi.IssueComment{
		{
			Author: new("178inaba"), AuthorType: new("User"), Body: "first",
			CreatedAt: "2026-01-01T00:00:00Z",
			URL:       "https://github.com/owner/repo/issues/10#issuecomment-1",
		},
		{
			Author: new("github-actions"), AuthorType: new("Bot"), Body: "second",
			CreatedAt: "2026-01-02T00:00:00Z",
			URL:       "https://github.com/owner/repo/issues/10#issuecomment-2",
		},
		// A deleted author leaves both null rather than an empty login, so that
		// a reader tells "nobody" from a person named "".
		{
			Body:      "third",
			CreatedAt: "2026-01-03T00:00:00Z",
			URL:       "https://github.com/owner/repo/issues/10#issuecomment-3",
		},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("IssueComments (-want +got):\n%s", diff)
	}
	// Asking for a hundred is what makes MAX_ISSUE_COMMENTS=2 write two rather
	// than a whole page of them.
	if gotPerPage != "100" {
		t.Errorf("per_page = %q, want %q", gotPerPage, "100")
	}
}

// TestIssueCommentsAsksForNoMoreThanItKeeps is what the limit does to the
// request rather than to the answer: a page larger than the limit would be
// fetched whole and written whole, since the walk never trims.
func TestIssueCommentsAsksForNoMoreThanItKeeps(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		limit int
		want  string
	}{
		{2, "2"},
		{99, "99"},
		{100, "100"},
		{500, "100"},
		{0, "100"},
	} {
		t.Run(fmt.Sprint(tc.limit), func(t *testing.T) {
			t.Parallel()

			var gotPerPage string
			c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				gotPerPage = r.URL.Query().Get("per_page")
				w.Header().Set("Content-Type", "application/json")
				fmt.Fprint(w, `[]`)
			}))

			if _, err := c.IssueComments(t.Context(), issueRepo, 10, tc.limit); err != nil {
				t.Fatalf("IssueComments: %v", err)
			}
			if gotPerPage != tc.want {
				t.Errorf("per_page = %q, want %q", gotPerPage, tc.want)
			}
		})
	}
}

// TestIssueCommentsWithNoneIsEmptyRatherThanNil keeps the document's promise
// where it is made: comments is a list that may be empty, never null.
func TestIssueCommentsWithNoneIsEmptyRatherThanNil(t *testing.T) {
	t.Parallel()

	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `[]`)
	}))

	got, err := c.IssueComments(t.Context(), issueRepo, 10, 100)
	if err != nil {
		t.Fatalf("IssueComments: %v", err)
	}
	if got == nil {
		t.Error("IssueComments returned nil, want an empty list")
	}
	if len(got) != 0 {
		t.Errorf("IssueComments returned %d comments, want none", len(got))
	}
}

func TestIssueCommentsReportsAFailure(t *testing.T) {
	t.Parallel()

	c := issues(t, nil, map[string]int{
		"/repos/owner/repo/issues/10/comments": http.StatusInternalServerError,
	})

	if got, err := c.IssueComments(t.Context(), issueRepo, 10, 100); err == nil {
		t.Errorf("IssueComments = %+v, want a failure", got)
	}
}
