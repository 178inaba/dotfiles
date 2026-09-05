package cmd

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
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

// contextDocument is the smallest document `ccx pr seen` can be given: what
// Context declares, and the two fields the command reads out of it.
func contextDocument(t *testing.T, fetchedAt string) string {
	t.Helper()

	path := filepath.Join(t.TempDir(), "pr-context-owner@repo-5.json")
	doc := fmt.Sprintf(`{"fetched_at":%q,
		"pending":{"since":null,"threads":[],"reviews":[],"comments":[]},
		"repo":"owner/repo","is_own_pr":true,
		"pr":{"number":5,"base_ref":"main","head_ref":"feature/x","head_oid":"abc123"},
		"review_threads":[]}`, fetchedAt)
	if err := os.WriteFile(path, []byte(doc), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	return path
}

// TestPRSeen drives the command the way a skill does: it records, it says
// where, and a second run against an older document is refused rather than
// moving the mark backwards.
func TestPRSeen(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)

	var out, errOut bytes.Buffer
	if code := run(t.Context(), []string{"pr", "seen", contextDocument(t, "2026-01-11T00:00:00Z")},
		strings.NewReader(""), &out, &errOut, selfbuild.State{}); code != 0 {
		t.Fatalf("`ccx pr seen` = %d, want 0: %s", code, errOut.String())
	}

	want := pullrequest.SeenPath(state, ghapi.Repo{Owner: "owner", Name: "repo"}, 5)
	if !strings.Contains(out.String(), want) || !strings.Contains(out.String(), "2026-01-11T00:00:00Z") {
		t.Errorf("`ccx pr seen` printed %s, want the path %s and the instant", out.String(), want)
	}
	if at := pullrequest.ReadSeen(state, ghapi.Repo{Owner: "owner", Name: "repo"}, 5); at == nil {
		t.Error("nothing was recorded")
	}

	out.Reset()
	errOut.Reset()
	if code := run(t.Context(), []string{"pr", "seen", contextDocument(t, "2026-01-10T00:00:00Z")},
		strings.NewReader(""), &out, &errOut, selfbuild.State{}); code == 0 {
		t.Error("`ccx pr seen` on an older document = 0, want a refusal")
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
