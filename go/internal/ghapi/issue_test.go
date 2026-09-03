package ghapi_test

import (
	"fmt"
	"net/http"
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
