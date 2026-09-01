// Package ghapi is how this module talks to GitHub.
//
// The shell scripts it replaces ran the gh binary and read its standard
// output; every one of them documented a GH_BIN environment variable so that a
// test could point at a stub instead. Both go away here: go-gh performs the
// same requests in process, with gh's own credential resolution (GH_TOKEN and
// GH_HOST, then the stored OAuth token), and a test replaces the transport
// rather than the executable.
//
// Two things are deliberately left to go-gh rather than reimplemented. Its
// clients decode responses with encoding/json v1, which is what gh itself
// decodes with — so a comment body carrying invalid UTF-8 arrives with the
// replacement character rather than as an error, exactly as it did before, and
// the v2 encoder that writes our own output never sees a string it would
// reject. And NewHTTPClient wraps the transport in a sanitizer that rewrites
// ANSI escapes inside JSON strings, which gh applies too; a hand-rolled
// http.Client would quietly drop that.
package ghapi

import (
	"bytes"
	"context"
	"encoding/json/v2"
	"errors"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/cli/go-gh/v2/pkg/api"
)

// Options are the seams. The zero value is production: go-gh resolves the host
// and the token the way gh does.
type Options struct {
	// Host is the GitHub host. Empty resolves through go-gh, which honours
	// GH_HOST.
	Host string
	// AuthToken authenticates the requests. Empty resolves through go-gh,
	// which honours GH_TOKEN and then the stored OAuth token.
	AuthToken string
	// CacheDir holds the responses of requests made through GetCached. Empty
	// is gh's own cache directory, which is what sharing the mechanism with gh
	// is worth.
	CacheDir string
	// Transport replaces the network. Tests set it; see ghapitest.
	Transport http.RoundTripper
}

// Client reaches the GitHub REST and GraphQL APIs.
//
// A concrete type rather than an interface: the seam callers need is the
// transport, not a second implementation, and go.dev/wiki/CodeReviewComments
// asks that the interface be declared by whoever consumes one.
type Client struct {
	rest *api.RESTClient
	// cached is built on demand for GetCached, because the cache TTL is a
	// per-client setting in go-gh and only one endpoint wants it.
	cached *api.RESTClient
	gql    *api.GraphQLClient
	opts   api.ClientOptions
}

// New returns a client for o.
func New(o Options) (*Client, error) {
	// A transport means a test, and a test that left the host or the token
	// empty would send go-gh to ~/.config/gh — where it may exec
	// `gh auth token --secure-storage` — for the one it is missing. Refusing
	// here is what keeps that from being a silent read of real credentials.
	if o.Transport != nil && (o.Host == "" || o.AuthToken == "" || o.CacheDir == "") {
		return nil, errors.New("ghapi: a client with a transport must also set Host, AuthToken and CacheDir")
	}

	opts := api.ClientOptions{
		Host:      o.Host,
		AuthToken: o.AuthToken,
		CacheDir:  o.CacheDir,
		Transport: o.Transport,
		// Otherwise a GH_DEBUG left in the environment attaches a request log
		// to standard error, and these subcommands promise standard output
		// that is JSON and nothing else.
		LogIgnoreEnv: true,
	}

	rest, err := api.NewRESTClient(opts)
	if err != nil {
		return nil, fmt.Errorf("ghapi: rest client: %w", err)
	}
	gql, err := api.NewGraphQLClient(opts)
	if err != nil {
		return nil, fmt.Errorf("ghapi: graphql client: %w", err)
	}
	return &Client{rest: rest, gql: gql, opts: opts}, nil
}

// Get sends a GET to a REST path and decodes the response into out.
//
// path is what `gh api` took: a path relative to the API root, or a whole URL,
// which is what makes a pagination link usable as one.
func (c *Client) Get(ctx context.Context, path string, out any) error {
	return c.rest.DoWithContext(ctx, http.MethodGet, path, nil, out)
}

// GetCached is Get with a response cache, which is what `gh api --cache` gave
// the shell version.
//
// The TTL belongs to the client rather than the request in go-gh, so this
// builds a second one on first use instead of enabling the cache on the shared
// client: list-pending-reviews caches the current user for a day and must not
// cache the review pages it fetches in the same run.
func (c *Client) GetCached(ctx context.Context, path string, ttl time.Duration, out any) error {
	if c.cached == nil {
		opts := c.opts
		opts.EnableCache = true
		opts.CacheTTL = ttl
		cached, err := api.NewRESTClient(opts)
		if err != nil {
			return fmt.Errorf("ghapi: cached rest client: %w", err)
		}
		c.cached = cached
	}
	return c.cached.DoWithContext(ctx, http.MethodGet, path, nil, out)
}

// Post sends body as JSON to a REST path and decodes the response into out.
func (c *Client) Post(ctx context.Context, path string, body, out any) error {
	b, err := json.Marshal(body)
	if err != nil {
		return fmt.Errorf("ghapi: encode request body: %w", err)
	}
	return c.rest.DoWithContext(ctx, http.MethodPost, path, bytes.NewReader(b), out)
}

// GraphQL runs one query or mutation and decodes the response into out.
func (c *Client) GraphQL(ctx context.Context, query string, vars map[string]any, out any) error {
	return c.gql.DoWithContext(ctx, query, vars, out)
}

// IsNotFound reports whether err is GitHub saying the thing does not exist.
//
// issue-hierarchy depends on the distinction: the sub-issue parent endpoint
// answers 404 for an issue that simply has no parent, which is an ordinary
// result, while any other failure is a degradation it records in warnings[].
func IsNotFound(err error) bool {
	if e, ok := errors.AsType[*api.HTTPError](err); ok {
		return e.StatusCode == http.StatusNotFound
	}
	if e, ok := errors.AsType[*api.GraphQLError](err); ok {
		return e.Match("NOT_FOUND", "")
	}
	return false
}

// bodyOf reads a response body, which the paginating path needs because it
// keeps the response rather than letting go-gh decode and close it.
func bodyOf(resp *http.Response) ([]byte, error) {
	defer resp.Body.Close()
	return io.ReadAll(resp.Body)
}
