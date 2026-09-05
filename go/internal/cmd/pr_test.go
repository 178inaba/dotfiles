package cmd

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/pullrequest"
)

// TestStoreSeen is the writing half of the record: the bytes are the ones its
// declaration publishes, and the temporary file it went through is gone, so no
// reader finds a half-written record where a whole one is expected.
func TestStoreSeen(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	path := filepath.Join(dir, "5.json")
	if err := storeSeen(path, pullrequest.Seen{SeenAt: "2026-01-10T00:00:00Z"}); err != nil {
		t.Fatalf("storeSeen: %v", err)
	}

	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if !strings.Contains(string(b), `"seen_at": "2026-01-10T00:00:00Z"`) {
		t.Errorf("the record reads %s, want the instant under seen_at", b)
	}

	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	if len(entries) != 1 {
		t.Errorf("%s holds %d files, want only the record", dir, len(entries))
	}
}

// TestStateHome pins where a judged pull request is recorded. Not parallel,
// and here rather than in pullrequest, for the reason the clone workspace's
// equivalent is: t.Setenv changes the whole process, so the package that keeps
// the record takes the directory as a parameter and only this reader touches
// the environment.
func TestStateHome(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)

	t.Run("XDG_STATE_HOME wins", func(t *testing.T) {
		xdg := t.TempDir()
		t.Setenv("XDG_STATE_HOME", xdg)
		if got := stateHome(); got != xdg {
			t.Errorf("stateHome() = %q, want %q", got, xdg)
		}
	})

	t.Run("without it the home directory", func(t *testing.T) {
		t.Setenv("XDG_STATE_HOME", "")
		want := filepath.Join(home, ".local", "state")
		if got := stateHome(); got != want {
			t.Errorf("stateHome() = %q, want %q", got, want)
		}
	})
}
