package fxrate

import (
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"testing"
	"time"

	"github.com/178inaba/dotfiles/go/internal/cache"
)

var now = time.Date(2026, 8, 31, 12, 0, 0, 0, time.UTC)

func TestLookup(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// cached is the rate to seed, zero for none; at is when it was written.
		cached float64
		at     time.Time
		// attempt is when a refresh was last started, zero for never.
		attempt time.Time

		wantRate    float64
		wantRefresh bool
	}{
		{
			name:   "a fresh rate is used and nothing is fetched",
			cached: 162.22, at: now,
			wantRate: 162.22,
		},
		{
			name:        "no cache renders nothing and starts a fetch",
			wantRefresh: true,
		},
		{
			// The whole point of the cache: an old rate keeps the cost visible
			// while a new one is fetched behind it.
			name:   "a stale rate is still rendered while it refreshes",
			cached: 100, at: now.Add(-24 * time.Hour),
			wantRate: 100, wantRefresh: true,
		},
		{
			// A negative rate would render a negative cost.
			name:   "a negative rate is discarded",
			cached: -162.22, at: now,
			wantRefresh: true,
		},
		{
			name:    "a recent attempt suppresses the fetch",
			attempt: now.Add(-10 * time.Second),
		},
		{
			name:        "an old attempt does not",
			attempt:     now.Add(-2 * time.Minute),
			wantRefresh: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "usd-jpy")
			if tt.cached != 0 {
				if err := cache.Write(dir, cacheKey, tt.at, tt.cached); err != nil {
					t.Fatalf("seed: %v", err)
				}
			}
			if !tt.attempt.IsZero() {
				// Recorded the way the foreground records it, since that is the
				// only way in from outside the package.
				cache.ShouldAttempt(dir, tt.attempt, retryInterval)
			}

			rate, refresh := Lookup(dir, now)

			if rate != tt.wantRate {
				t.Errorf("rate = %v, want %v", rate, tt.wantRate)
			}
			if refresh != tt.wantRefresh {
				t.Errorf("refresh = %t, want %t", refresh, tt.wantRefresh)
			}
		})
	}
}

func TestRefresh(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		status int
		body   string
		want   float64 // zero means no record should be written
	}{
		{
			name:   "a rate is stored with the time it was fetched",
			status: http.StatusOK,
			body:   `{"amount":1.0,"base":"USD","date":"2026-08-31","rates":{"JPY":162.22}}`,
			want:   162.22,
		},
		{
			// Nothing is written, so the previous rate — however old — keeps
			// rendering rather than disappearing.
			name:   "a missing rate leaves the cache alone",
			status: http.StatusOK,
			body:   `{"amount":1.0,"base":"USD","rates":{}}`,
		},
		{
			name:   "an error response leaves the cache alone",
			status: http.StatusInternalServerError,
			body:   "nope",
		},
		{
			name:   "malformed json leaves the cache alone",
			status: http.StatusOK,
			body:   `{"rates":`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				w.WriteHeader(tt.status)
				if _, err := w.Write([]byte(tt.body)); err != nil {
					t.Errorf("write response: %v", err)
				}
			}))
			defer srv.Close()

			dir := filepath.Join(t.TempDir(), "usd-jpy")
			err := Refresh(t.Context(), srv.Client(), srv.URL, dir, now)

			rec, ok := cache.Read[float64](dir, cacheKey)
			if tt.want == 0 {
				// The rate keeps rendering either way; the error is what a
				// hand-run refresh has to say for itself.
				if err == nil {
					t.Error("Refresh reported no error")
				}
				if ok {
					t.Errorf("cache written as %v, want no record", rec.Value)
				}
				return
			}
			if err != nil {
				t.Fatalf("Refresh: %v", err)
			}
			if !ok {
				t.Fatal("no record written")
			}
			if rec.Value != tt.want {
				t.Errorf("rate = %v, want %v", rec.Value, tt.want)
			}
			if !rec.At.Equal(now) {
				t.Errorf("At = %v, want %v", rec.At, now)
			}
		})
	}
}

func TestRefreshSurvivesAnUnreachableServer(t *testing.T) {
	t.Parallel()

	srv := httptest.NewServer(http.HandlerFunc(func(http.ResponseWriter, *http.Request) {}))
	url := srv.URL
	srv.Close()

	dir := filepath.Join(t.TempDir(), "usd-jpy")
	if err := Refresh(t.Context(), http.DefaultClient, url, dir, now); err == nil {
		t.Error("Refresh reported no error")
	}

	if _, ok := cache.Read[float64](dir, cacheKey); ok {
		t.Error("a record was written for a failed fetch")
	}
}
