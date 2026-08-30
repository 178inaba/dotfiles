// Package fxrate keeps the US dollar to yen rate the cost segment converts
// with.
//
// The rate is served stale while it revalidates. A status line redraw is a hot
// path — every five seconds, in every session — so it never waits on the
// network: the cached value is rendered as it is, and a refresh is started in
// the background when the value is old or missing.
package fxrate

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"regexp"
	"time"

	"github.com/178inaba/dotfiles/go/internal/cache"
)

const (
	// CachePath is shared by every session rather than kept per session. A
	// per-session cache would refetch once per parallel session and would pin a
	// long-lived session to whatever the rate was when it started.
	CachePath = "/tmp/claude-statusline-usd-jpy"
	// APIURL serves the European Central Bank's published rates.
	APIURL = "https://api.frankfurter.dev/v1/latest?base=USD&symbols=JPY"

	// maxAge is half a day, which is ample: the published rate moves once per
	// working day.
	maxAge = 43200
	// retryInterval keeps an offline machine from starting a fetch on every
	// redraw.
	retryInterval = 60
	// timeout bounds the background fetch. Nothing waits on it, but a hung
	// request should not leave a process around indefinitely.
	timeout = 5 * time.Second
)

// rateFormat is what counts as a rate. Anything else — an error page, a
// truncated write, a field that moved — is treated as no rate at all rather
// than rendered.
var rateFormat = regexp.MustCompile(`^[0-9]+(\.[0-9]+)?$`)

// Lookup returns the rate to render, which is empty when there is none, and
// whether the caller should start a refresh.
//
// A stale rate is still returned: yesterday's conversion is far better than
// dropping the cost from the display while a fetch happens.
//
// Deciding to refresh records the attempt, so a second redraw arriving while
// the first fetch is still in flight does not start another one.
func Lookup(cachePath string, now int64) (string, bool) {
	rate := ""
	at, value, ok := cache.ReadPair(cachePath)
	if ok && rateFormat.MatchString(value) {
		rate = value
	}

	if rate != "" && cache.Fresh(now, at, maxAge) {
		return rate, false
	}

	attemptPath := cachePath + ".attempt"
	if last, ok := cache.ReadAttempt(attemptPath); ok && cache.Fresh(now, last, retryInterval) {
		return rate, false
	}
	// Best effort: a write that fails only costs one duplicate fetch.
	_ = cache.WriteAttempt(attemptPath, now)
	return rate, true
}

// Refresh fetches the rate and stores it. It is meant to run detached, so it
// reports nothing: a failure simply leaves the previous cache in place, and the
// recorded attempt keeps the next redraw from trying again immediately.
func Refresh(ctx context.Context, client *http.Client, url, cachePath string, now int64) {
	rate, ok := fetch(ctx, client, url)
	if !ok {
		return
	}
	// Best effort: a write that fails leaves the previous rate rendering.
	_ = cache.WritePair(cachePath, now, rate)
}

func fetch(ctx context.Context, client *http.Client, url string) (string, bool) {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return "", false
	}
	resp, err := client.Do(req)
	if err != nil {
		return "", false
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return "", false
	}

	dec := json.NewDecoder(io.LimitReader(resp.Body, 1<<16))
	// The rate is kept as the literal the response carried, so the conversion
	// works from the same digits the shell version did.
	dec.UseNumber()
	var body struct {
		Rates struct {
			JPY json.Number `json:"JPY"`
		} `json:"rates"`
	}
	if err := dec.Decode(&body); err != nil {
		return "", false
	}
	rate := body.Rates.JPY.String()
	if !rateFormat.MatchString(rate) {
		return "", false
	}
	return rate, true
}
