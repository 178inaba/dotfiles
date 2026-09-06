package ghapi_test

import (
	"encoding/json/v2"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
)

type issue struct {
	Number int    `json:"number"`
	Title  string `json:"title"`
}

func TestGet(t *testing.T) {
	t.Parallel()

	var gotPath, gotAuth string
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath, gotAuth = r.URL.Path, r.Header.Get("Authorization")
		fmt.Fprint(w, `{"number":121,"title":"Port the scripts"}`)
	}))

	var got issue
	if err := c.Get(t.Context(), "repos/o/r/issues/121", &got); err != nil {
		t.Fatalf("Get: %v", err)
	}

	if want := (issue{Number: 121, Title: "Port the scripts"}); got != want {
		t.Errorf("Get decoded %+v, want %+v", got, want)
	}
	if want := "/repos/o/r/issues/121"; gotPath != want {
		t.Errorf("Get requested %q, want %q", gotPath, want)
	}
	// The token has to reach the request, or every test would be exercising an
	// unauthenticated client and the production path would be untested.
	if want := "token test-token"; gotAuth != want {
		t.Errorf("Authorization = %q, want %q", gotAuth, want)
	}
}

// TestCreateIssue and TestEditIssue are what covers the request side of the
// client: the verb, the JSON that goes out and the response that comes back.
// They come through the typed writers because the raw ones are package-private
// — a body may reach GitHub only as one that has been judged.
func TestCreateIssue(t *testing.T) {
	t.Parallel()

	var gotMethod, gotPath string
	var sent map[string]any
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotMethod, gotPath = r.Method, r.URL.Path
		if err := json.UnmarshalRead(r.Body, &sent); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		fmt.Fprint(w, `{"number":7,"title":"created","html_url":"https://github.com/owner/repo/issues/7"}`)
	}))

	title, body, labels := "created", ghapitest.Body(t, "the body\n"), []string{"enhancement"}
	got, err := c.CreateIssue(t.Context(), issueRepo, ghapi.IssueChange{
		Title: &title, Body: &body, Labels: &labels,
	})
	if err != nil {
		t.Fatalf("CreateIssue: %v", err)
	}

	if gotMethod != http.MethodPost {
		t.Errorf("method = %q, want %q", gotMethod, http.MethodPost)
	}
	if want := "/repos/owner/repo/issues"; gotPath != want {
		t.Errorf("path = %q, want %q", gotPath, want)
	}
	// Compared field by field rather than as encoded text: the request is
	// built from a map, whose keys json/v2 does not promise in any order.
	want := map[string]any{"title": "created", "body": "the body\n", "labels": []any{"enhancement"}}
	if diff := cmp.Diff(want, sent); diff != "" {
		t.Errorf("request body (-want +got):\n%s", diff)
	}
	if got.Number != 7 || got.Title != "created" {
		t.Errorf("CreateIssue decoded %+v, want number 7 titled created", got)
	}
}

func TestEditIssue(t *testing.T) {
	t.Parallel()

	var gotMethod, gotPath string
	var sent map[string]any
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotMethod, gotPath = r.Method, r.URL.Path
		if err := json.UnmarshalRead(r.Body, &sent); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		fmt.Fprint(w, `{"number":7,"title":"edited"}`)
	}))

	// Only the body: an edit says what to change and leaves the rest alone,
	// which is what the pointers are for.
	body := ghapitest.Body(t, "rewritten\n")
	got, err := c.EditIssue(t.Context(), issueRepo, 7, ghapi.IssueChange{Body: &body})
	if err != nil {
		t.Fatalf("EditIssue: %v", err)
	}

	if gotMethod != http.MethodPatch {
		t.Errorf("method = %q, want %q", gotMethod, http.MethodPatch)
	}
	if want := "/repos/owner/repo/issues/7"; gotPath != want {
		t.Errorf("path = %q, want %q", gotPath, want)
	}
	if diff := cmp.Diff(map[string]any{"body": "rewritten\n"}, sent); diff != "" {
		t.Errorf("request body (-want +got):\n%s", diff)
	}
	if got.Title != "edited" {
		t.Errorf(`EditIssue decoded %+v, want title "edited"`, got)
	}
}

func TestGetAllFollowsLinkHeader(t *testing.T) {
	t.Parallel()

	var srv *httptest.Server
	srv = httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Query().Get("page") {
		case "", "1":
			// The header names an absolute URL, which is what GitHub sends and
			// what go-gh passes through without prefixing the API root.
			w.Header().Set("Link", fmt.Sprintf(`<%s/repos/o/r/issues?page=2>; rel="next", <%s/repos/o/r/issues?page=2>; rel="last"`, srv.URL, srv.URL))
			fmt.Fprint(w, `[{"number":1,"title":"one"}]`)
		case "2":
			fmt.Fprint(w, `[{"number":2,"title":"two"},{"number":3,"title":"three"}]`)
		default:
			t.Errorf("unexpected page %q", r.URL.Query().Get("page"))
		}
	}))
	t.Cleanup(srv.Close)

	c := ghapitest.NewAt(t, srv.URL)

	got, err := ghapi.GetAll[issue](t.Context(), c, "repos/o/r/issues")
	if err != nil {
		t.Fatalf("GetAll: %v", err)
	}
	want := []issue{{1, "one"}, {2, "two"}, {3, "three"}}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("GetAll (-want +got):\n%s", diff)
	}
}

func TestGetAllStopsWithoutNext(t *testing.T) {
	t.Parallel()

	var calls atomic.Int32
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		calls.Add(1)
		// A last page still carries a Link header, just without rel="next".
		w.Header().Set("Link", `<https://api.github.com/repos/o/r/issues?page=1>; rel="prev"`)
		fmt.Fprint(w, `[{"number":9,"title":"last"}]`)
	}))

	got, err := ghapi.GetAll[issue](t.Context(), c, "repos/o/r/issues")
	if err != nil {
		t.Fatalf("GetAll: %v", err)
	}
	if len(got) != 1 || calls.Load() != 1 {
		t.Errorf("GetAll returned %d elements after %d requests, want 1 and 1", len(got), calls.Load())
	}
}

func TestGraphQL(t *testing.T) {
	t.Parallel()

	var gotPath string
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath = r.URL.Path
		fmt.Fprint(w, `{"data":{"viewer":{"login":"178inaba"}}}`)
	}))

	var got struct {
		Viewer struct {
			Login string `json:"login"`
		} `json:"viewer"`
	}
	if err := c.GraphQL(t.Context(), "query { viewer { login } }", nil, &got); err != nil {
		t.Fatalf("GraphQL: %v", err)
	}

	if got.Viewer.Login != "178inaba" {
		t.Errorf("viewer.login = %q, want %q", got.Viewer.Login, "178inaba")
	}
	if want := "/graphql"; gotPath != want {
		t.Errorf("GraphQL posted to %q, want %q", gotPath, want)
	}
}

func TestIsNotFound(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		status  int
		body    string
		graphql bool
		want    bool
	}{
		{name: "rest 404 is not found", status: http.StatusNotFound, body: `{"message":"Not Found"}`, want: true},
		{name: "rest 500 is not", status: http.StatusInternalServerError, body: `{"message":"boom"}`, want: false},
		{name: "rest 403 is not", status: http.StatusForbidden, body: `{"message":"nope"}`, want: false},
		{
			// The shell grepped gh's stderr for "HTTP 404", so a message that
			// merely mentioned one counted. This must not.
			name: "rest 500 mentioning 404 is not", status: http.StatusInternalServerError,
			body: `{"message":"upstream said HTTP 404"}`, want: false,
		},
		{
			name: "graphql NOT_FOUND is not found", status: http.StatusOK, graphql: true,
			body: `{"errors":[{"type":"NOT_FOUND","message":"Could not resolve to an Issue"}]}`, want: true,
		},
		{
			name: "graphql other error is not", status: http.StatusOK, graphql: true,
			body: `{"errors":[{"type":"FORBIDDEN","message":"nope"}]}`, want: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				w.WriteHeader(tt.status)
				fmt.Fprint(w, tt.body)
			}))

			var err error
			var out struct{}
			if tt.graphql {
				err = c.GraphQL(t.Context(), "query { viewer { login } }", nil, &out)
			} else {
				err = c.Get(t.Context(), "repos/o/r", &out)
			}
			if err == nil {
				t.Fatal("want an error, got nil")
			}
			if got := ghapi.IsNotFound(err); got != tt.want {
				t.Errorf("IsNotFound(%v) = %v, want %v", err, got, tt.want)
			}
		})
	}
}

func TestHTTPStatus(t *testing.T) {
	t.Parallel()

	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusForbidden)
		fmt.Fprint(w, `{"message":"Forbidden"}`)
	}))

	var out struct{}
	err := c.Get(t.Context(), "repos/o/r/issues/1", &out)
	if err == nil {
		t.Fatal("want an error, got nil")
	}
	if got, ok := ghapi.HTTPStatus(err); !ok || got != http.StatusForbidden {
		t.Errorf("HTTPStatus(%v) = %d, %v, want %d, true", err, got, ok, http.StatusForbidden)
	}

	// A failure that never reached a response carries no status, and answering
	// zero without saying so would read as one.
	if got, ok := ghapi.HTTPStatus(errors.New("dial tcp: connection refused")); ok || got != 0 {
		t.Errorf("HTTPStatus of a network error = %d, %v, want 0, false", got, ok)
	}
}

// TestForbiddenNamesSSOAuthorisation pins the one cause this package names for
// its caller, and the two ways it stays out of the way otherwise.
//
// It is attached here rather than by the callers so that a lookup added later
// cannot forget it; the status has to survive underneath, since IsNotFound and
// HTTPStatus read it after the wrapping.
func TestForbiddenNamesSSOAuthorisation(t *testing.T) {
	t.Parallel()

	for _, status := range []int{http.StatusForbidden, http.StatusInternalServerError, http.StatusNotFound} {
		t.Run(fmt.Sprintf("HTTP %d", status), func(t *testing.T) {
			t.Parallel()

			c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				w.Header().Set("Content-Type", "application/json")
				w.WriteHeader(status)
				fmt.Fprint(w, `{"message":"no"}`)
			}))

			var out struct{}
			err := c.Get(t.Context(), "repos/o/r/issues/1", &out)
			if err == nil {
				t.Fatalf("HTTP %d: want an error, got nil", status)
			}
			want := status == http.StatusForbidden
			if got := strings.Contains(err.Error(), "SSO authorisation"); got != want {
				t.Errorf("HTTP %d: error = %v, want the clause present = %v", status, err, want)
			}
			if got, ok := ghapi.HTTPStatus(err); !ok || got != status {
				t.Errorf("HTTP %d: HTTPStatus = %d, %v, want the status to survive the wrapping", status, got, ok)
			}
		})
	}

	// A failure that never reached a response has no status to judge, and must
	// pass through rather than be guessed at.
	t.Run("a network error", func(t *testing.T) {
		t.Parallel()

		c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `not json`)
		}))
		var out struct{}
		if err := c.Get(t.Context(), "repos/o/r/issues/1", &out); err == nil {
			t.Fatal("want an error, got nil")
		} else if strings.Contains(err.Error(), "SSO authorisation") {
			t.Errorf("error = %v, want no clause on a failure with no status", err)
		}
	})
}

func TestGetCachedReusesTheResponse(t *testing.T) {
	t.Parallel()

	var calls atomic.Int32
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		calls.Add(1)
		fmt.Fprint(w, `{"number":1,"title":"cached"}`)
	}))

	for range 2 {
		var got issue
		if err := c.GetCached(t.Context(), "user", 24*time.Hour, &got); err != nil {
			t.Fatalf("GetCached: %v", err)
		}
		if got.Title != "cached" {
			t.Errorf("GetCached decoded %+v, want title %q", got, "cached")
		}
	}
	if calls.Load() != 1 {
		t.Errorf("server saw %d requests, want 1 — the second should come from the cache", calls.Load())
	}
}

func TestNewRefusesAPartialTestClient(t *testing.T) {
	t.Parallel()

	// Every field but one, so that the check cannot be passing for another
	// reason. Leaving any of them empty sends go-gh to the real configuration.
	full := ghapi.Options{Host: "github.com", AuthToken: "t", CacheDir: t.TempDir(), Transport: http.DefaultTransport}
	for _, tt := range []struct {
		name  string
		clear func(*ghapi.Options)
	}{
		{name: "no host", clear: func(o *ghapi.Options) { o.Host = "" }},
		{name: "no token", clear: func(o *ghapi.Options) { o.AuthToken = "" }},
		{name: "no cache dir", clear: func(o *ghapi.Options) { o.CacheDir = "" }},
	} {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			o := full
			tt.clear(&o)
			_, err := ghapi.New(o)
			if err == nil {
				t.Fatal("want an error, got nil")
			}
			if !strings.Contains(err.Error(), "must also set") {
				t.Errorf("New error = %v, want it to name the missing options", err)
			}
		})
	}
}
