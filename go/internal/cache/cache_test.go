package cache

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"
)

var now = time.Date(2026, 8, 31, 12, 0, 0, 0, time.UTC)

// value stands in for the things the status line caches.
type value struct {
	Segment string `json:"segment"`
	Count   int    `json:"count"`
}

func TestPath(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		base string
		key  []string
		want string
	}{
		{
			name: "the key becomes directories",
			base: "/c/statusline/git", key: []string{"/Users/x/repo"},
			want: "/c/statusline/git/Users/x/repo",
		},
		{
			name: "a branch is a further part of the path",
			base: "/c/statusline/pr", key: []string{"/Users/x/repo", "feature/99-a"},
			want: "/c/statusline/pr/Users/x/repo/feature/99-a",
		},
		{
			// The filesystem bounds each component, not the path, so a working
			// directory of any depth is kept whole.
			name: "depth is not a limit",
			base: "/c", key: []string{"/" + strings.Repeat("d/", 200) + "repo"},
			want: "/c/" + strings.Repeat("d/", 200) + "repo",
		},
		{
			// Rooting each part before cleaning it is what makes this total:
			// there is no key that names anything outside base.
			name: "a key cannot climb out of the base",
			base: "/c", key: []string{"/Users/x/../../../etc"},
			want: "/c/etc",
		},
		{
			name: "nor can a relative one",
			base: "/c", key: []string{"../../escape"},
			want: "/c/escape",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := Path(tt.base, tt.key...); got != tt.want {
				t.Errorf("Path(%q, %q) = %q, want %q", tt.base, tt.key, got, tt.want)
			}
		})
	}
}

func TestRoundTrip(t *testing.T) {
	t.Parallel()

	dir := filepath.Join(t.TempDir(), "Users", "x", "repo")
	want := value{Segment: " (main +1 ~1)", Count: 2}

	if err := Write(dir, "/Users/x/repo", now, want); err != nil {
		t.Fatalf("Write: %v", err)
	}

	rec, ok := Read[value](dir, "/Users/x/repo")
	if !ok {
		t.Fatal("Read reported no record")
	}
	if diff := cmp.Diff(want, rec.Value); diff != "" {
		t.Errorf("value mismatch (-want +got):\n%s", diff)
	}
	if !rec.At.Equal(now) {
		t.Errorf("At = %v, want %v", rec.At, now)
	}
}

func TestReadRejects(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// write puts an entry in place; nil means nothing is there at all.
		write func(t *testing.T, dir string)
	}{
		{name: "an entry that was never written"},
		{
			// A file left by an older version, or a machine that lost power
			// mid-write. Absent is the safe reading: it costs one
			// recomputation, where a partial parse could render nonsense.
			name: "a file that is not a record",
			write: func(t *testing.T, dir string) {
				if err := os.MkdirAll(dir, 0o755); err != nil {
					t.Fatalf("mkdir: %v", err)
				}
				if err := os.WriteFile(filepath.Join(dir, recordName), []byte("1756600000\nkey\n"), 0o644); err != nil {
					t.Fatalf("write: %v", err)
				}
			},
		},
		{
			// Parts are joined, so a directory and a branch can in principle
			// land where another pair would; one must not show the other's
			// state.
			name: "a record written for another key",
			write: func(t *testing.T, dir string) {
				if err := Write(dir, "other", now, value{Segment: "x"}); err != nil {
					t.Fatalf("Write: %v", err)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			dir := filepath.Join(t.TempDir(), "entry")
			if tt.write != nil {
				tt.write(t, dir)
			}
			if _, ok := Read[value](dir, "wanted"); ok {
				t.Error("Read accepted the record")
			}
		})
	}
}

func TestWriteLeavesNoTemporary(t *testing.T) {
	t.Parallel()

	dir := filepath.Join(t.TempDir(), "entry")
	if err := Write(dir, "k", now, value{Segment: "x"}); err != nil {
		t.Fatalf("Write: %v", err)
	}

	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	if len(entries) != 1 || entries[0].Name() != recordName {
		var names []string
		for _, e := range entries {
			names = append(names, e.Name())
		}
		t.Errorf("entry holds %v, want only %s", names, recordName)
	}
}

func TestFresh(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		at     time.Time
		maxAge time.Duration
		want   bool
	}{
		{name: "well inside", at: now.Add(-2 * time.Second), maxAge: 5 * time.Second, want: true},
		{name: "exactly at the limit", at: now.Add(-5 * time.Second), maxAge: 5 * time.Second, want: true},
		{name: "one past the limit", at: now.Add(-6 * time.Second), maxAge: 5 * time.Second},
		{
			// A clock that went backwards reads as fresh rather than starting a
			// stampede of refreshes after a time sync.
			name: "a future timestamp", at: now.Add(time.Hour), maxAge: 5 * time.Second, want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := Fresh(now, tt.at, tt.maxAge); got != tt.want {
				t.Errorf("Fresh = %t, want %t", got, tt.want)
			}
		})
	}
}

func TestShouldAttempt(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// last is when a previous attempt was recorded; zero means none.
		last time.Time
		want bool
	}{
		{name: "no previous attempt", want: true},
		// Without this an offline machine would start a refresh on every
		// redraw, five seconds apart, forever.
		{name: "a recent attempt", last: now.Add(-10 * time.Second)},
		{name: "an old attempt", last: now.Add(-2 * time.Minute), want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "entry")
			if !tt.last.IsZero() {
				if err := write(dir, attemptName, attemptKey, tt.last, struct{}{}); err != nil {
					t.Fatalf("seed: %v", err)
				}
			}

			if got := ShouldAttempt(dir, now, time.Minute); got != tt.want {
				t.Fatalf("ShouldAttempt = %t, want %t", got, tt.want)
			}

			// Deciding to refresh has to be recorded before the refresh starts,
			// or the redraw five seconds later starts a second one.
			rec, ok := read[struct{}](filepath.Join(dir, attemptName), attemptKey)
			switch {
			case tt.want && (!ok || !rec.At.Equal(now)):
				t.Errorf("attempt recorded at %v (present=%t), want %v", rec.At, ok, now)
			case !tt.want && !rec.At.Equal(tt.last):
				t.Errorf("attempt = %v, want it left at %v", rec.At, tt.last)
			}
		})
	}
}

// The throttle is a file of its own beside the record, because the process that
// decides to refresh is not the one that writes the result.
func TestShouldAttemptLeavesTheRecordAlone(t *testing.T) {
	t.Parallel()

	dir := filepath.Join(t.TempDir(), "entry")
	want := value{Segment: "x"}
	if err := Write(dir, "k", now, want); err != nil {
		t.Fatalf("Write: %v", err)
	}

	ShouldAttempt(dir, now, time.Minute)

	rec, ok := Read[value](dir, "k")
	if !ok {
		t.Fatal("the record is gone")
	}
	if diff := cmp.Diff(want, rec.Value); diff != "" {
		t.Errorf("value mismatch (-want +got):\n%s", diff)
	}
}
