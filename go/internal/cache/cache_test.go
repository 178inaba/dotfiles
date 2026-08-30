package cache

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestPath(t *testing.T) {
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
			// directories can share a file; the key stored inside the file is
			// what tells them apart.
			name: "long paths are truncated whole",
			base: "/tmp/b", key: "/" + strings.Repeat("a", 300),
			want: ("/tmp/b-_" + strings.Repeat("a", 300))[:200],
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := Path(tt.base, tt.key); got != tt.want {
				t.Errorf("Path(%q, %q) = %q, want %q", tt.base, tt.key, got, tt.want)
			}
		})
	}
}

func TestPathTruncatesByCharacter(t *testing.T) {
	// bash cuts a substring by character in a UTF-8 locale, which is the one
	// the status line runs in, so a multibyte path must not be cut mid-rune.
	got := Path("/tmp/b", strings.Repeat("あ", 300))
	if n := len([]rune(got)); n != 200 {
		t.Errorf("length = %d characters, want 200", n)
	}
	if strings.ContainsRune(got, '�') {
		t.Errorf("Path produced an invalid rune: %q", got)
	}
}

func TestKeyedRoundTrip(t *testing.T) {
	dir := t.TempDir()
	p := filepath.Join(dir, "git-cache-x")
	want := Keyed{At: 1756600000, Key: "/Users/x/repo", Result: " (main +1 ~1)"}

	if err := WriteKeyed(p, want); err != nil {
		t.Fatalf("WriteKeyed: %v", err)
	}

	// The three-line record ends without a newline. It is compared byte for
	// byte because the shell implementation wrote it with a bare printf and
	// anything reading it back has to agree.
	b, err := os.ReadFile(p)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if got, wantBytes := string(b), "1756600000\n/Users/x/repo\n (main +1 ~1)"; got != wantBytes {
		t.Errorf("file = %q, want %q", got, wantBytes)
	}

	got, ok := ReadKeyed(p)
	if !ok {
		t.Fatal("ReadKeyed reported no record")
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Keyed mismatch (-want +got):\n%s", diff)
	}
}

func TestWriteKeyedAtomicLeavesNoTemporary(t *testing.T) {
	dir := t.TempDir()
	p := filepath.Join(dir, "pr-cache-x")

	if err := WriteKeyedAtomic(p, Keyed{At: 1, Key: "k", Result: "123 NONE https://e/1"}); err != nil {
		t.Fatalf("WriteKeyedAtomic: %v", err)
	}

	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	if len(entries) != 1 || entries[0].Name() != filepath.Base(p) {
		var names []string
		for _, e := range entries {
			names = append(names, e.Name())
		}
		t.Errorf("directory holds %v, want only %q", names, filepath.Base(p))
	}

	// The shell wrote with a plain redirect, so the file is world readable and
	// not the 0600 a temporary file would default to.
	info, err := os.Stat(p)
	if err != nil {
		t.Fatalf("Stat: %v", err)
	}
	if got := info.Mode().Perm(); got != 0o644 {
		t.Errorf("mode = %o, want 644", got)
	}
}

func TestReadKeyedTolerance(t *testing.T) {
	tests := []struct {
		name string
		body string
		want Keyed
		ok   bool
	}{
		{
			name: "an empty result is a record",
			body: "5\nkey\n",
			want: Keyed{At: 5, Key: "key"},
			ok:   true,
		},
		{
			// A non-numeric timestamp is how a truncated or corrupt file reads,
			// and the shell treated it as no record rather than as time zero.
			name: "a non-numeric timestamp is no record",
			body: "nope\nkey\nresult",
		},
		{
			name: "a negative timestamp is no record",
			body: "-5\nkey\nresult",
		},
		{
			name: "a short file is no record",
			body: "5",
			want: Keyed{At: 5},
			ok:   true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := filepath.Join(t.TempDir(), "c")
			if err := os.WriteFile(p, []byte(tt.body), 0o644); err != nil {
				t.Fatalf("WriteFile: %v", err)
			}
			got, ok := ReadKeyed(p)
			if ok != tt.ok {
				t.Fatalf("ReadKeyed ok = %t, want %t", ok, tt.ok)
			}
			if diff := cmp.Diff(tt.want, got); diff != "" {
				t.Errorf("Keyed mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestReadKeyedMissingFile(t *testing.T) {
	if _, ok := ReadKeyed(filepath.Join(t.TempDir(), "absent")); ok {
		t.Error("ReadKeyed reported a record for a file that does not exist")
	}
}

func TestPairRoundTrip(t *testing.T) {
	p := filepath.Join(t.TempDir(), "usd-jpy")

	if err := WritePair(p, 1756600000, "162.22"); err != nil {
		t.Fatalf("WritePair: %v", err)
	}

	// Unlike the three-line record, this one ends with a newline.
	b, err := os.ReadFile(p)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if got, want := string(b), "1756600000\n162.22\n"; got != want {
		t.Errorf("file = %q, want %q", got, want)
	}

	at, value, ok := ReadPair(p)
	if !ok || at != 1756600000 || value != "162.22" {
		t.Errorf("ReadPair = (%d, %q, %t), want (1756600000, \"162.22\", true)", at, value, ok)
	}
}

func TestAttemptRoundTrip(t *testing.T) {
	p := filepath.Join(t.TempDir(), "usd-jpy.attempt")

	if err := WriteAttempt(p, 1756600000); err != nil {
		t.Fatalf("WriteAttempt: %v", err)
	}
	b, err := os.ReadFile(p)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if got, want := string(b), "1756600000\n"; got != want {
		t.Errorf("file = %q, want %q", got, want)
	}

	if at, ok := ReadAttempt(p); !ok || at != 1756600000 {
		t.Errorf("ReadAttempt = (%d, %t), want (1756600000, true)", at, ok)
	}
	if _, ok := ReadAttempt(filepath.Join(t.TempDir(), "absent")); ok {
		t.Error("ReadAttempt reported a time for a file that does not exist")
	}
}

func TestFresh(t *testing.T) {
	tests := []struct {
		name            string
		now, at, maxAge int64
		want            bool
	}{
		{name: "well inside", now: 100, at: 98, maxAge: 5, want: true},
		// The shell staleness test is a strict >, so exactly at the limit is
		// still fresh.
		{name: "exactly at the limit", now: 105, at: 100, maxAge: 5, want: true},
		{name: "one past the limit", now: 106, at: 100, maxAge: 5},
		{
			// A clock that went backwards makes the difference negative, which
			// reads as fresh. Preserved: the alternative is a stampede of
			// refreshes after a time sync.
			name: "a future timestamp", now: 100, at: 200, maxAge: 5, want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := Fresh(tt.now, tt.at, tt.maxAge); got != tt.want {
				t.Errorf("Fresh(%d, %d, %d) = %t, want %t", tt.now, tt.at, tt.maxAge, got, tt.want)
			}
		})
	}
}
