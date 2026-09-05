package ghapi_test

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"strconv"
	"sync/atomic"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
)

// pagesOfIssues serves one issue per page, numbered from one, linking each page
// to the next until last, and counts the requests it answered.
func pagesOfIssues(t *testing.T, last int, calls *atomic.Int32) *ghapi.Client {
	t.Helper()

	var srv *httptest.Server
	srv = httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		calls.Add(1)
		page := 1
		if p := r.URL.Query().Get("page"); p != "" {
			n, err := strconv.Atoi(p)
			if err != nil {
				t.Errorf("page = %q, want a number", p)
				return
			}
			page = n
		}
		if page < last {
			w.Header().Set("Link", fmt.Sprintf(`<%s/repos/o/r/issues?page=%d>; rel="next"`, srv.URL, page+1))
		}
		fmt.Fprintf(w, `[{"number":%d,"title":"page %d"}]`, page, page)
	}))
	t.Cleanup(srv.Close)

	return ghapitest.NewAt(t, srv.URL)
}

// TestGetUpToStopsAtTheLimit is the rule GetUpTo exists for: the limit is
// checked before the next request rather than applied to the result, so what
// stops is the round trips.
func TestGetUpToStopsAtTheLimit(t *testing.T) {
	t.Parallel()

	var calls atomic.Int32
	c := pagesOfIssues(t, 5, &calls)

	got, err := ghapi.GetUpTo[issue](t.Context(), c, "repos/o/r/issues", 2)
	if err != nil {
		t.Fatalf("GetUpTo: %v", err)
	}
	want := []issue{{1, "page 1"}, {2, "page 2"}}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("GetUpTo (-want +got):\n%s", diff)
	}
	if n := calls.Load(); n != 2 {
		t.Errorf("GetUpTo made %d requests, want 2", n)
	}
}

// TestGetUpToWithoutRoomTakesTheFirstPage pins the meaning of a limit no page
// can fit under, MAX_ISSUE_COMMENTS=0 included: the first page always arrives,
// exactly as pullrequest.pages leaves it, so that zero means the same on both
// sides of the module.
func TestGetUpToWithoutRoomTakesTheFirstPage(t *testing.T) {
	t.Parallel()

	for _, max := range []int{0, -1} {
		t.Run(fmt.Sprint(max), func(t *testing.T) {
			t.Parallel()

			var calls atomic.Int32
			c := pagesOfIssues(t, 5, &calls)

			got, err := ghapi.GetUpTo[issue](t.Context(), c, "repos/o/r/issues", max)
			if err != nil {
				t.Fatalf("GetUpTo: %v", err)
			}
			if want := []issue{{1, "page 1"}}; !cmp.Equal(want, got) {
				t.Errorf("GetUpTo = %+v, want %+v", got, want)
			}
			if n := calls.Load(); n != 1 {
				t.Errorf("GetUpTo made %d requests, want 1", n)
			}
		})
	}
}

// TestGetUpToWithRoomToSpareReachesTheEnd is the other end: a limit nothing
// reaches walks the collection whole, and does not ask for a page after the
// last one.
func TestGetUpToWithRoomToSpareReachesTheEnd(t *testing.T) {
	t.Parallel()

	var calls atomic.Int32
	c := pagesOfIssues(t, 3, &calls)

	got, err := ghapi.GetUpTo[issue](t.Context(), c, "repos/o/r/issues", 100)
	if err != nil {
		t.Fatalf("GetUpTo: %v", err)
	}
	want := []issue{{1, "page 1"}, {2, "page 2"}, {3, "page 3"}}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("GetUpTo (-want +got):\n%s", diff)
	}
	if n := calls.Load(); n != 3 {
		t.Errorf("GetUpTo made %d requests, want 3", n)
	}
}

func TestGetUpToReportsAFailedPage(t *testing.T) {
	t.Parallel()

	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprint(w, `{"message":"unavailable"}`)
	}))

	if got, err := ghapi.GetUpTo[issue](t.Context(), c, "repos/o/r/issues", 10); err == nil {
		t.Errorf("GetUpTo = %+v, want a failure", got)
	}
}
