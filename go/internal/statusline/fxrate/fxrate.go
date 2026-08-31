// Package fxrate keeps the US dollar to yen rate the cost segment converts
// with.
//
// The rate is served stale while it revalidates: the cached value is rendered
// as it is and a refresh is started in the background when it is old or
// missing, so a redraw never waits on the network.
package fxrate

import (
	"context"
	"encoding/json/v2"
	"io"
	"net/http"
	"time"

	"github.com/178inaba/dotfiles/go/internal/cache"
)

const (
	// CacheName is one entry shared by every session rather than one per
	// session. A per-session cache would refetch once per parallel session and
	// would pin a long-lived session to whatever the rate was when it started.
	CacheName = "usd-jpy"
	// APIURL serves the European Central Bank's published rates.
	APIURL = "https://api.frankfurter.dev/v1/latest?base=USD&symbols=JPY"
	// cacheKey is fixed: the rate is the same for every session.
	cacheKey = "usd-jpy"

	// maxAge is half a day, which is ample: the published rate moves once per
	// working day.
	maxAge = 12 * time.Hour
	// retryInterval keeps an offline machine from starting a fetch on every
	// redraw.
	retryInterval = time.Minute
	// timeout bounds the background fetch. Nothing waits on it, but a hung
	// request should not leave a process around indefinitely.
	timeout = 5 * time.Second
)

// Lookup returns the rate to render, which is zero when there is none, and
// whether the caller should start a refresh.
//
// A stale rate is still returned: yesterday's conversion is far better than
// dropping the cost from the display while a fetch happens.
func Lookup(dir string, now time.Time) (float64, bool) {
	rec, ok := cache.Read[float64](dir, cacheKey)
	if ok && rec.Value > 0 && cache.Fresh(now, rec.At, maxAge) {
		return rec.Value, false
	}
	rate := rec.Value
	if rate < 0 {
		// A negative rate is not one; it would render a negative cost.
		rate = 0
	}
	return rate, cache.ShouldAttempt(dir, now, retryInterval)
}

// Refresh fetches the rate and stores it. It is meant to run detached, so it
// reports nothing: a failure simply leaves the previous cache in place.
func Refresh(ctx context.Context, client *http.Client, url, dir string, now time.Time) {
	rate, ok := fetch(ctx, client, url)
	if !ok {
		return
	}
	// Best effort: a write that fails leaves the previous rate rendering.
	_ = cache.Write(dir, cacheKey, now, rate)
}

func fetch(ctx context.Context, client *http.Client, url string) (float64, bool) {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return 0, false
	}
	resp, err := client.Do(req)
	if err != nil {
		return 0, false
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return 0, false
	}

	var body struct {
		Rates struct {
			JPY float64 `json:"JPY"`
		} `json:"rates"`
	}
	if err := json.UnmarshalRead(io.LimitReader(resp.Body, 1<<16), &body); err != nil {
		return 0, false
	}
	// A missing or nonsensical rate is treated as no answer, so the previous
	// one keeps rendering rather than being replaced by zero.
	if body.Rates.JPY <= 0 {
		return 0, false
	}
	return body.Rates.JPY, true
}
