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
		name      string
		base, key string
		want      string
	}{
		{
			name: "slashes become underscores",
			base: "/tmp/claude-statusline-git-cache", key: "/Users/x/repo",
			want: "/tmp/claude-statusline-git-cache-_Users_x_repo",
		},
		{
			name: "a branch in the key is flattened too",
			base: "/tmp/claude-statusline-pr-cache", key: "/Users/x/repo:feature/99-a",
			want: "/tmp/claude-statusline-pr-cache-_Users_x_repo:feature_99-a",
		},
		{
			// The whole path is cut at 200, base included, so two very deep
			// directories can share a file; the key stored inside it is what
			// tells them apart.
			name: "long paths are truncated whole",
			base: "/tmp/b", key: "/" + strings.Repeat("a", 300),
			want: ("/tmp/b-_" + strings.Repeat("a", 300))[:200],
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := Path(tt.base, tt.key); got != tt.want {
				t.Errorf("Path(%q, %q) = %q, want %q", tt.base, tt.key, got, tt.want)
			}
		})
	}
}

func TestPathTruncatesByCharacter(t *testing.T) {
	t.Parallel()

	// APFS rejects a file name that is not valid UTF-8, so a cut that split a
	// rune would produce a name that cannot be created at all and a cache that
	// silently never works.
	got := Path("/tmp/b", strings.Repeat("あ", 300))
	if n := len([]rune(got)); n != 200 {
		t.Errorf("length = %d characters, want 200", n)
	}
	if strings.ContainsRune(got, '�') {
		t.Errorf("Path produced an invalid rune: %q", got)
	}
}

func TestRoundTrip(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "cache")
	want := value{Segment: " (main +1 ~1)", Count: 2}

	if err := Write(path, "/Users/x/repo", now, want); err != nil {
		t.Fatalf("Write: %v", err)
	}

	rec, ok := Read[value](path, "/Users/x/repo")
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
		// write puts a file in place; nil means the file does not exist.
		write func(t *testing.T, path string)
	}{
		{name: "a missing file"},
		{
			// A file left by an older version, or a machine that lost power
			// mid-write. Absent is the safe reading: it costs one
			// recomputation, where a partial parse could render nonsense.
			name:  "a file that is not a record",
			write: func(t *testing.T, path string) { writeFile(t, path, "1756600000\nkey\nresult") },
		},
		{
			// Two deep directories can share a file once the name is cut to
			// length; one must not show the other's state.
			name: "a record written for another key",
			write: func(t *testing.T, path string) {
				if err := Write(path, "other", now, value{Segment: "x"}); err != nil {
					t.Fatalf("Write: %v", err)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			path := filepath.Join(t.TempDir(), "cache")
			if tt.write != nil {
				tt.write(t, path)
			}
			if _, ok := Read[value](path, "wanted"); ok {
				t.Error("Read accepted the record")
			}
		})
	}
}

func TestWriteLeavesNoTemporary(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	if err := Write(filepath.Join(dir, "cache"), "k", now, value{Segment: "x"}); err != nil {
		t.Fatalf("Write: %v", err)
	}

	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	if len(entries) != 1 || entries[0].Name() != "cache" {
		var names []string
		for _, e := range entries {
			names = append(names, e.Name())
		}
		t.Errorf("directory holds %v, want only the record", names)
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

			path := filepath.Join(t.TempDir(), "cache")
			if !tt.last.IsZero() {
				if err := Write(path+".attempt", attemptKey, tt.last, struct{}{}); err != nil {
					t.Fatalf("Write: %v", err)
				}
			}

			if got := ShouldAttempt(path, now, time.Minute); got != tt.want {
				t.Fatalf("ShouldAttempt = %t, want %t", got, tt.want)
			}

			// Deciding to refresh has to be recorded before the refresh starts,
			// or the redraw five seconds later starts a second one.
			rec, ok := Read[struct{}](path+".attempt", attemptKey)
			switch {
			case tt.want && (!ok || !rec.At.Equal(now)):
				t.Errorf("attempt recorded at %v (present=%t), want %v", rec.At, ok, now)
			case !tt.want && !rec.At.Equal(tt.last):
				t.Errorf("attempt = %v, want it left at %v", rec.At, tt.last)
			}
		})
	}
}

func writeFile(t *testing.T, name, body string) {
	t.Helper()
	if err := os.WriteFile(name, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
}
