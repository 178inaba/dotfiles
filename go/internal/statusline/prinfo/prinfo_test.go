package prinfo

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

var now = time.Date(2026, 8, 31, 12, 0, 0, 0, time.UTC)

const key = "/Users/x/repo:feat"

// fakeRunner answers by command and records what it was asked.
type fakeRunner struct {
	out   map[string]string
	fail  map[string]bool
	calls []string
}

func (f *fakeRunner) Run(_ context.Context, c runner.Command) ([]byte, error) {
	name := c.Name + " " + c.Args[0]
	f.calls = append(f.calls, name)
	if f.fail[name] {
		return nil, &runner.Error{Name: c.Name, Err: os.ErrInvalid}
	}
	return []byte(f.out[name]), nil
}

func TestLookup(t *testing.T) {
	t.Parallel()

	open := Info{Number: 123, State: StateNoReviewRequested, URL: "https://e/1"}

	tests := []struct {
		name string
		// seed is the record to write, with the key and time to write it under.
		seed     *Info
		seedKey  string
		at       time.Time
		attempt  time.Time
		wantInfo Info
		wantRef  bool
	}{
		{
			name: "a fresh record is used and gh is not asked",
			seed: &open, seedKey: key, at: now,
			wantInfo: open,
		},
		{
			name:    "no record renders nothing and starts a refresh",
			wantRef: true,
		},
		{
			// The badge stays put while the refresh runs, exactly as the
			// exchange rate does.
			name: "a stale record is still rendered",
			seed: &open, seedKey: key, at: now.Add(-time.Hour),
			wantInfo: open, wantRef: true,
		},
		{
			// "no pull request" is a real answer and is cached like any other,
			// which is what keeps gh from running on every redraw offline.
			name: "an empty record is a fresh answer",
			seed: &Info{}, seedKey: key, at: now,
		},
		{
			// Deep directories can share a cache file once the name is cut to
			// length; the key inside decides whether the record is ours.
			name: "a record for another key is discarded",
			seed: &open, seedKey: "/Users/x/other:main", at: now,
			wantRef: true,
		},
		{
			name:    "a recent attempt suppresses the refresh",
			attempt: now.Add(-10 * time.Second),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "pr")
			if tt.seed != nil {
				if err := cache.Write(dir, tt.seedKey, tt.at, *tt.seed); err != nil {
					t.Fatalf("seed: %v", err)
				}
			}
			if !tt.attempt.IsZero() {
				// Recorded the way the foreground records it, since that is the
				// only way in from outside the package.
				cache.ShouldAttempt(dir, tt.attempt, retryInterval)
			}

			info, refresh := Lookup(dir, key, now)

			if diff := cmp.Diff(tt.wantInfo, info); diff != "" {
				t.Errorf("Info mismatch (-want +got):\n%s", diff)
			}
			if refresh != tt.wantRef {
				t.Errorf("refresh = %t, want %t", refresh, tt.wantRef)
			}
		})
	}
}

func TestRefresh(t *testing.T) {
	t.Parallel()

	const prView = "gh pr"

	tests := []struct {
		name   string
		branch string
		out    map[string]string
		fail   map[string]bool
		want   Info
		wantGH bool
	}{
		{
			name:   "an open pull request awaiting review",
			branch: "feat",
			out: map[string]string{
				"git symbolic-ref": "origin/main\n",
				prView:             `{"number":123,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://e/123"}`,
			},
			want:   Info{Number: 123, State: StateNoReviewRequested, URL: "https://e/123"},
			wantGH: true,
		},
		{
			name:   "a draft outranks its review decision",
			branch: "feat",
			out: map[string]string{
				"git symbolic-ref": "origin/main\n",
				prView:             `{"number":126,"reviewDecision":"","state":"OPEN","isDraft":true,"url":"https://e/126"}`,
			},
			want:   Info{Number: 126, State: StateDraft, URL: "https://e/126"},
			wantGH: true,
		},
		{
			name:   "an approved pull request",
			branch: "feat",
			out: map[string]string{
				"git symbolic-ref": "origin/main\n",
				prView:             `{"number":124,"reviewDecision":"APPROVED","state":"OPEN","isDraft":false,"url":"https://e/124"}`,
			},
			want:   Info{Number: 124, State: StateApproved, URL: "https://e/124"},
			wantGH: true,
		},
		{
			// A merged pull request is history, not the current work.
			name:   "a merged pull request is not shown",
			branch: "feat",
			out: map[string]string{
				"git symbolic-ref": "origin/main\n",
				prView:             `{"number":127,"reviewDecision":"APPROVED","state":"MERGED","isDraft":false,"url":"https://e/127"}`,
			},
			wantGH: true,
		},
		{
			// The default branch may be the head of a release pull request, but
			// it is not a branch-specific context, so gh is never even asked.
			name:   "the default branch is skipped without asking gh",
			branch: "main",
			out:    map[string]string{"git symbolic-ref": "origin/main\n"},
		},
		{
			// A repository whose remote was added by hand has no origin/HEAD,
			// and gh knows the answer instead.
			name:   "gh supplies the default branch when origin/HEAD is missing",
			branch: "main",
			out:    map[string]string{"gh repo": `{"defaultBranchRef":{"name":"main"}}`},
			fail:   map[string]bool{"git symbolic-ref": true},
		},
		{
			// Neither source knows, so the badge is shown rather than hidden:
			// a badge too many beats a badge missing.
			name:   "an unknown default branch still shows the pull request",
			branch: "main",
			out: map[string]string{
				prView: `{"number":133,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://e/133"}`,
			},
			fail:   map[string]bool{"git symbolic-ref": true, "gh repo": true},
			want:   Info{Number: 133, State: StateNoReviewRequested, URL: "https://e/133"},
			wantGH: true,
		},
		{
			// gh reports no pull request, no network and no credentials with
			// the same exit status, and all three mean the same thing here. The
			// loop below requires a nil error for every row, this one included:
			// caching "no pull request" is the answer, not a failure to get one.
			name:   "a gh failure is cached as no pull request",
			branch: "feat",
			out:    map[string]string{"git symbolic-ref": "origin/main\n"},
			fail:   map[string]bool{prView: true},
			wantGH: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "pr")
			r := &fakeRunner{out: tt.out, fail: tt.fail}

			if err := Refresh(t.Context(), r, dir, key, tt.branch, now); err != nil {
				t.Fatalf("Refresh: %v", err)
			}

			rec, ok := cache.Read[Info](dir, key)
			if !ok {
				t.Fatal("no record written")
			}
			if diff := cmp.Diff(tt.want, rec.Value); diff != "" {
				t.Errorf("Info mismatch (-want +got):\n%s", diff)
			}

			asked := false
			for _, c := range r.calls {
				if c == prView {
					asked = true
				}
			}
			if asked != tt.wantGH {
				t.Errorf("gh pr view called = %t, want %t (calls: %v)", asked, tt.wantGH, r.calls)
			}
		})
	}
}
