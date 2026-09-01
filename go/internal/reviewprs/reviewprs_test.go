package reviewprs_test

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/reviewprs"
)

// fixtures are what one test's GitHub knows: the authenticated login, the
// search hits, and the reviews on each pull request.
type fixtures struct {
	// login is the authenticated user, empty to make /user fail.
	login string
	// items are the search hits, already rendered as JSON objects.
	items []string
	// reviews maps owner/repo/number to the review pages of that pull request;
	// more than one page is served through a Link header.
	reviews map[string][]string
	// failReviews names a pull request whose reviews cannot be read.
	failReviews string
	// failSearch makes the search itself fail.
	failSearch bool
}

type fake struct {
	fixtures

	mu    sync.Mutex
	calls []string
}

func (f *fake) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	f.mu.Lock()
	f.calls = append(f.calls, r.URL.RequestURI())
	f.mu.Unlock()

	w.Header().Set("Content-Type", "application/json")
	switch {
	case r.URL.Path == "/user":
		if f.login == "" {
			w.WriteHeader(http.StatusUnauthorized)
			fmt.Fprint(w, `{"message":"Bad credentials"}`)
			return
		}
		fmt.Fprintf(w, `{"login":%q}`, f.login)
	case r.URL.Path == "/search/issues":
		if f.failSearch {
			w.WriteHeader(http.StatusServiceUnavailable)
			fmt.Fprint(w, `{"message":"unavailable"}`)
			return
		}
		fmt.Fprintf(w, `{"total_count":%d,"items":[%s]}`, len(f.items), strings.Join(f.items, ","))
	case strings.HasSuffix(r.URL.Path, "/reviews"):
		f.serveReviews(w, r)
	default:
		w.WriteHeader(http.StatusNotFound)
		fmt.Fprint(w, `{"message":"Not Found"}`)
	}
}

func (f *fake) serveReviews(w http.ResponseWriter, r *http.Request) {
	// repos/{owner}/{repo}/pulls/{number}/reviews
	parts := strings.Split(strings.Trim(r.URL.Path, "/"), "/")
	key := strings.Join([]string{parts[1], parts[2], parts[4]}, "/")
	if key == f.failReviews {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprint(w, `{"message":"boom"}`)
		return
	}

	pages := f.reviews[key]
	if len(pages) == 0 {
		fmt.Fprint(w, "[]")
		return
	}
	page := 0
	if r.URL.Query().Get("page") == "2" {
		page = 1
	}
	if page == 0 && len(pages) > 1 {
		w.Header().Set("Link", fmt.Sprintf(`<https://api.github.com%s?page=2>; rel="next"`, r.URL.Path))
	}
	fmt.Fprint(w, pages[page])
}

func (f *fake) asked(substr string) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	for _, c := range f.calls {
		if strings.Contains(c, substr) {
			return true
		}
	}
	return false
}

// hit renders one search result.
func hit(owner, repo string, number int, author string) string {
	user := "null"
	if author != "" {
		user = fmt.Sprintf(`{"login":%q}`, author)
	}
	return fmt.Sprintf(`{"number":%d,"html_url":"https://github.com/%s/%s/pull/%d",
		"repository_url":"https://api.github.com/repos/%s/%s","user":%s}`,
		number, owner, repo, number, owner, repo, user)
}

// by renders one review.
func by(login, kind, state string) string {
	return fmt.Sprintf(`{"state":%q,"user":{"login":%q,"type":%q}}`, state, login, kind)
}

func page(reviews ...string) string { return "[" + strings.Join(reviews, ",") + "]" }

func serve(t *testing.T, f fixtures) (*fake, *ghapi.Client) {
	t.Helper()

	server := &fake{fixtures: f}
	srv := httptest.NewServer(server)
	t.Cleanup(srv.Close)
	return server, ghapitest.NewAt(t, srv.URL)
}

func TestListPending(t *testing.T) {
	t.Parallel()

	const (
		me     = "me"
		author = "author1"
		other  = "someone"
	)

	tests := []struct {
		name    string
		reviews []string
		want    bool
	}{
		{name: "nobody has reviewed", want: true},
		{name: "only bots have reviewed", reviews: []string{by("copilot", "Bot", "COMMENTED"), by("coderabbit", "Bot", "COMMENTED")}, want: true},
		{name: "somebody else reviewed first", reviews: []string{by(other, "User", "APPROVED")}},
		{name: "a bot and somebody else", reviews: []string{by("copilot", "Bot", "COMMENTED"), by(other, "User", "COMMENTED")}},
		// Already reviewed and asked again: the request is explicit, so the
		// pull request comes back whatever anyone else did.
		{name: "this user reviewed before", reviews: []string{by(me, "User", "CHANGES_REQUESTED")}, want: true},
		{name: "this user reviewed and so did somebody else", reviews: []string{by(me, "User", "CHANGES_REQUESTED"), by(other, "User", "APPROVED")}, want: true},
		// GitHub records a reply in a thread as a COMMENTED review, so an
		// author answering a bot must not read as a human reviewer.
		{name: "the author answered a bot", reviews: []string{by("copilot", "Bot", "COMMENTED"), by(author, "User", "COMMENTED"), by(author, "User", "COMMENTED")}, want: true},
		{name: "a bot and this user", reviews: []string{by("copilot", "Bot", "COMMENTED"), by(me, "User", "CHANGES_REQUESTED")}, want: true},
		{name: "a bot, the author and somebody else", reviews: []string{by("copilot", "Bot", "COMMENTED"), by(author, "User", "COMMENTED"), by(other, "User", "APPROVED")}},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			f := fixtures{login: me, items: []string{hit("acme", "foo", 100, author)}}
			if len(tc.reviews) > 0 {
				f.reviews = map[string][]string{"acme/foo/100": {page(tc.reviews...)}}
			}
			_, c := serve(t, f)

			got, err := reviewprs.ListPending(t.Context(), c)
			if err != nil {
				t.Fatalf("ListPending: %v", err)
			}

			var want []reviewprs.PR
			if tc.want {
				want = []reviewprs.PR{{Owner: "acme", Repo: "foo", Number: 100, URL: "https://github.com/acme/foo/pull/100"}}
			} else {
				want = []reviewprs.PR{}
			}
			if diff := cmp.Diff(reviewprs.Pending{PRs: want}, got); diff != "" {
				t.Errorf("ListPending (-want +got):\n%s", diff)
			}
		})
	}
}

// TestListPendingReadsEveryReviewPage is why the reviews are paginated: a pull
// request with a page of bot reviews would otherwise hide the human review on
// the next one, or the user's own.
func TestListPendingReadsEveryReviewPage(t *testing.T) {
	t.Parallel()

	_, c := serve(t, fixtures{
		login: "me",
		items: []string{hit("acme", "page", 104, "author1")},
		reviews: map[string][]string{"acme/page/104": {
			page(by("copilot", "Bot", "COMMENTED")),
			page(by("someone", "User", "APPROVED")),
		}},
	})

	got, err := reviewprs.ListPending(t.Context(), c)
	if err != nil {
		t.Fatalf("ListPending: %v", err)
	}
	if len(got.PRs) != 0 {
		t.Errorf("ListPending = %+v, want the reviewer on the second page to exclude it", got.PRs)
	}
}

func TestListPendingSearch(t *testing.T) {
	t.Parallel()

	server, c := serve(t, fixtures{login: "me"})
	got, err := reviewprs.ListPending(t.Context(), c)
	if err != nil {
		t.Fatalf("ListPending: %v", err)
	}

	if diff := cmp.Diff(reviewprs.Pending{PRs: []reviewprs.PR{}}, got); diff != "" {
		t.Errorf("ListPending with no hits (-want +got):\n%s", diff)
	}
	// The server excludes drafts, because a draft is not asking for review and
	// filtering afterwards would spend a round trip on each one.
	for _, want := range []string{"draft%3Afalse", "user-review-requested%3A%40me", "state%3Aopen", "per_page=30"} {
		if !server.asked(want) {
			t.Errorf("the search did not carry %q; requests were %v", want, server.calls)
		}
	}
}

func TestListPendingDegrades(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name        string
		items       []string
		failReviews string
		wantWarning string
	}{
		{
			name:        "the reviews of one pull request cannot be read",
			items:       []string{hit("acme", "foo", 100, "author1"), hit("acme", "dead", 200, "author1")},
			failReviews: "acme/dead/200",
			wantWarning: "failed to fetch reviews for acme/dead#200",
		},
		{
			// A deleted account: with no author to exclude, every reply the
			// author left would count as somebody else's review.
			name:        "a pull request has no author",
			items:       []string{hit("acme", "foo", 100, "author1"), hit("acme", "ghost", 300, "")},
			wantWarning: "missing author for acme/ghost#300",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			_, c := serve(t, fixtures{login: "me", items: tc.items, failReviews: tc.failReviews})
			got, err := reviewprs.ListPending(t.Context(), c)
			if err != nil {
				t.Fatalf("ListPending: %v", err)
			}

			want := reviewprs.Pending{
				// The pull request that could not be judged is left out, and
				// the one before it is still answered for.
				PRs:      []reviewprs.PR{{Owner: "acme", Repo: "foo", Number: 100, URL: "https://github.com/acme/foo/pull/100"}},
				Degraded: true,
				Warnings: []string{tc.wantWarning},
			}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("ListPending (-want +got):\n%s", diff)
			}
		})
	}
}

func TestListPendingFails(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		f    fixtures
	}{
		{name: "the authenticated user is unknown", f: fixtures{}},
		{name: "the search fails", f: fixtures{login: "me", failSearch: true}},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			_, c := serve(t, tc.f)
			if got, err := reviewprs.ListPending(t.Context(), c); err == nil {
				t.Fatalf("ListPending = %+v, want a failure", got)
			}
		})
	}
}

func TestVerifyPosted(t *testing.T) {
	t.Parallel()

	specs := []reviewprs.Spec{
		{Owner: "acme", Repo: "foo", Number: 100},
		{Owner: "acme", Repo: "bar", Number: 101},
		{Owner: "acme", Repo: "baz", Number: 102},
		{Owner: "acme", Repo: "multi", Number: 300},
		{Owner: "acme", Repo: "pending", Number: 400},
	}
	_, c := serve(t, fixtures{
		login: "me",
		reviews: map[string][]string{
			"acme/foo/100": {page(by("someone", "User", "APPROVED"), by("me", "User", "COMMENTED"))},
			"acme/bar/101": {page(by("someone", "User", "APPROVED"), by("copilot", "Bot", "COMMENTED"))},
			// acme/baz/102 has no reviews at all.
			"acme/multi/300": {page(by("someone", "User", "APPROVED")), page(by("me", "User", "COMMENTED"))},
			// A draft only its author can see, left by a POST with no event.
			"acme/pending/400": {page(by("me", "User", "PENDING"))},
		},
	})

	got, err := reviewprs.VerifyPosted(t.Context(), c, specs)
	if err != nil {
		t.Fatalf("VerifyPosted: %v", err)
	}

	want := reviewprs.Verification{Results: []reviewprs.Result{
		{Owner: "acme", Repo: "foo", Number: 100, Posted: true},
		{Owner: "acme", Repo: "bar", Number: 101},
		{Owner: "acme", Repo: "baz", Number: 102},
		{Owner: "acme", Repo: "multi", Number: 300, Posted: true},
		{Owner: "acme", Repo: "pending", Number: 400},
	}}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("VerifyPosted (-want +got):\n%s", diff)
	}
}

func TestVerifyPostedDegrades(t *testing.T) {
	t.Parallel()

	_, c := serve(t, fixtures{
		login:       "me",
		reviews:     map[string][]string{"acme/foo/100": {page(by("me", "User", "COMMENTED"))}},
		failReviews: "acme/dead/200",
	})

	got, err := reviewprs.VerifyPosted(t.Context(), c, []reviewprs.Spec{
		{Owner: "acme", Repo: "foo", Number: 100},
		{Owner: "acme", Repo: "dead", Number: 200},
	})
	if err != nil {
		t.Fatalf("VerifyPosted: %v", err)
	}

	want := reviewprs.Verification{
		Results:  []reviewprs.Result{{Owner: "acme", Repo: "foo", Number: 100, Posted: true}},
		Degraded: true,
		Warnings: []string{"failed to fetch reviews for acme/dead#200"},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("VerifyPosted (-want +got):\n%s", diff)
	}
}

func TestVerifyPostedFailsWithoutAUser(t *testing.T) {
	t.Parallel()

	_, c := serve(t, fixtures{})
	if got, err := reviewprs.VerifyPosted(t.Context(), c, []reviewprs.Spec{{Owner: "acme", Repo: "foo", Number: 1}}); err == nil {
		t.Fatalf("VerifyPosted = %+v, want a failure", got)
	}
}

func TestParseSpec(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		in      string
		want    reviewprs.Spec
		wantErr bool
	}{
		{name: "owner, repo and number", in: "acme/foo#100", want: reviewprs.Spec{Owner: "acme", Repo: "foo", Number: 100}},
		{name: "no number", in: "acme/foo", wantErr: true},
		{name: "no repository", in: "foo#1", wantErr: true},
		{name: "a number that is not one", in: "acme/foo#x", wantErr: true},
		{name: "whitespace inside", in: "acme/f oo#1", wantErr: true},
		{name: "empty", in: "", wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := reviewprs.ParseSpec(tc.in)
			if tc.wantErr {
				if err == nil {
					t.Fatalf("ParseSpec(%q) = %v, want an error", tc.in, got)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseSpec(%q): %v", tc.in, err)
			}
			if got != tc.want {
				t.Errorf("ParseSpec(%q) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}
