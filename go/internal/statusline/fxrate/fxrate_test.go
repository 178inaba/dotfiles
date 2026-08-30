package fxrate

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
)

const now = int64(1756600000)

func TestLookup(t *testing.T) {
	tests := []struct {
		name string
		// cached and attempt are written before the lookup; an empty string
		// means the file is absent.
		cached, attempt string

		wantRate    string
		wantRefresh bool
	}{
		{
			name:   "a fresh rate is used and nothing is fetched",
			cached: "1756600000\n162.22\n",

			wantRate: "162.22",
		},
		{
			name: "no cache renders nothing and starts a fetch",

			wantRefresh: true,
		},
		{
			// The whole point of the cache: an old rate keeps the cost visible
			// while a new one is fetched behind it.
			name:   "a stale rate is still rendered while it refreshes",
			cached: "1\n100.00\n",

			wantRate:    "100.00",
			wantRefresh: true,
		},
		{
			// A corrupt value must not reach the display, and it must not stop
			// the refresh that would replace it either.
			name:   "a malformed rate is discarded",
			cached: "1756600000\ngarbage\n",

			wantRefresh: true,
		},
		{
			name:   "a negative rate is malformed",
			cached: "1756600000\n-162.22\n",

			wantRefresh: true,
		},
		{
			// Without this an offline machine would start a fetch on every
			// redraw, five seconds apart, forever.
			name:    "a recent attempt suppresses the fetch",
			attempt: "1756599990\n",
		},
		{
			name:    "an old attempt does not",
			attempt: "1756500000\n",

			wantRefresh: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := filepath.Join(t.TempDir(), "usd-jpy")
			if tt.cached != "" {
				writeFile(t, p, tt.cached)
			}
			if tt.attempt != "" {
				writeFile(t, p+".attempt", tt.attempt)
			}

			rate, refresh := Lookup(p, now)

			if rate != tt.wantRate {
				t.Errorf("rate = %q, want %q", rate, tt.wantRate)
			}
			if refresh != tt.wantRefresh {
				t.Errorf("refresh = %t, want %t", refresh, tt.wantRefresh)
			}

			// Deciding to fetch has to be recorded before the fetch starts, or
			// the redraw five seconds later starts a second one.
			b, err := os.ReadFile(p + ".attempt")
			switch {
			case tt.wantRefresh && (err != nil || string(b) != "1756600000\n"):
				t.Errorf("attempt file = %q (err=%v), want %q", b, err, "1756600000\n")
			case !tt.wantRefresh && tt.attempt != "" && string(b) != tt.attempt:
				t.Errorf("attempt file = %q, want it left at %q", b, tt.attempt)
			}
		})
	}
}

func TestRefresh(t *testing.T) {
	tests := []struct {
		name    string
		status  int
		body    string
		want    string // the cache file contents, empty when none should exist
		wantErr bool
	}{
		{
			name:   "a rate is stored with the time it was fetched",
			status: http.StatusOK,
			body:   `{"amount":1.0,"base":"USD","date":"2026-08-31","rates":{"JPY":162.22}}`,
			want:   "1756600000\n162.22\n",
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
			srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				w.WriteHeader(tt.status)
				respond(t, w, tt.body)
			}))
			defer srv.Close()

			p := filepath.Join(t.TempDir(), "usd-jpy")
			Refresh(t.Context(), srv.Client(), srv.URL, p, now)

			b, err := os.ReadFile(p)
			if tt.want == "" {
				if err == nil {
					t.Errorf("cache written as %q, want no file", b)
				}
				return
			}
			if err != nil {
				t.Fatalf("ReadFile: %v", err)
			}
			if string(b) != tt.want {
				t.Errorf("cache = %q, want %q", b, tt.want)
			}
		})
	}
}

func TestRefreshSurvivesAnUnreachableServer(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(http.ResponseWriter, *http.Request) {}))
	url := srv.URL
	srv.Close()

	p := filepath.Join(t.TempDir(), "usd-jpy")
	Refresh(t.Context(), http.DefaultClient, url, p, now)

	if _, err := os.Stat(p); err == nil {
		t.Error("a cache file was written for a failed fetch")
	}
}

func writeFile(t *testing.T, name, body string) {
	t.Helper()
	if err := os.WriteFile(name, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
}

func respond(t *testing.T, w http.ResponseWriter, body string) {
	t.Helper()
	if _, err := w.Write([]byte(body)); err != nil {
		t.Fatalf("write response: %v", err)
	}
}
