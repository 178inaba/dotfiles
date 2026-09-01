// Package ghapitest builds ghapi clients that cannot reach GitHub.
//
// It exists to be the only way a test constructs one. go-gh resolves whatever
// its options leave empty by reading ~/.config/gh, and for a token it may exec
// `gh auth token --secure-storage` — so a test that set the transport and
// forgot the host would still be reading the developer's real credentials, and
// would pass. New fills in all of them; ghapi.New refuses a transport without
// them, so the two together make that mistake a failure rather than a leak.
package ghapitest

import (
	"net/http"
	"net/http/httptest"
	"net/url"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// Host is what the client believes it is talking to.
//
// It stays github.com even though the requests go to a local server, because
// go-gh derives the path prefix from the host: anything it does not recognise
// is an enterprise install, and the handler would see /api/v3/... instead of
// the paths GitHub actually serves.
const Host = "github.com"

// New starts a server for h and returns a client pointed at it.
func New(t *testing.T, h http.Handler) *ghapi.Client {
	t.Helper()

	srv := httptest.NewServer(h)
	t.Cleanup(srv.Close)
	return NewAt(t, srv.URL)
}

// NewAt returns a client pointed at a server the caller already started.
//
// A handler that has to name its own address — a Link header pointing at the
// next page, say — cannot be registered before the server exists, so those
// tests start it themselves and hand the address over here.
func NewAt(t *testing.T, baseURL string) *ghapi.Client {
	t.Helper()

	u, err := url.Parse(baseURL)
	if err != nil {
		t.Fatalf("parse %q: %v", baseURL, err)
	}

	c, err := ghapi.New(ghapi.Options{
		Host:      Host,
		AuthToken: "test-token",
		// Never the real one: go-gh's cache defaults to gh's own directory,
		// and a test that populated it would change what the user's gh sees.
		CacheDir:  t.TempDir(),
		Transport: redirect{to: u},
	})
	if err != nil {
		t.Fatalf("ghapi.New: %v", err)
	}
	return c
}

// redirect sends every request to the test server while leaving the path, and
// therefore what the handler matches on, alone.
type redirect struct{ to *url.URL }

// RoundTrip implements http.RoundTripper.
func (r redirect) RoundTrip(req *http.Request) (*http.Response, error) {
	// Cloned because a RoundTripper may not modify the request it is given.
	out := req.Clone(req.Context())
	out.URL.Scheme = r.to.Scheme
	out.URL.Host = r.to.Host
	out.Host = ""
	return http.DefaultTransport.RoundTrip(out)
}
