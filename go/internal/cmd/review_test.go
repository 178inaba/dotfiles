package cmd

import (
	"path/filepath"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/reviewprs"
)

// TestCloneOptionsDataHome pins where the review workspace goes. Not parallel,
// and the reason this lives here rather than in reviewprs: t.Setenv changes the
// whole process, so the package the clone is implemented in takes the directory
// as a parameter and only this thin reader touches the environment.
func TestCloneOptionsDataHome(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)

	t.Run("XDG_DATA_HOME wins", func(t *testing.T) {
		xdg := t.TempDir()
		t.Setenv("XDG_DATA_HOME", xdg)
		if got := cloneOptions().DataHome; got != xdg {
			t.Errorf("DataHome = %q, want %q", got, xdg)
		}
	})

	t.Run("without it the home directory", func(t *testing.T) {
		t.Setenv("XDG_DATA_HOME", "")
		want := filepath.Join(home, ".local", "share")
		if got := cloneOptions().DataHome; got != want {
			t.Errorf("DataHome = %q, want %q", got, want)
		}
	})

	t.Run("the host defaults to github.com", func(t *testing.T) {
		t.Setenv("GH_HOST", "")
		if got, want := cloneOptions().Host, "github.com"; got != want {
			t.Errorf("Host = %q, want %q", got, want)
		}
	})

	t.Run("GH_HOST is honoured", func(t *testing.T) {
		t.Setenv("GH_HOST", "github.example.com")
		if got, want := cloneOptions().Host, "github.example.com"; got != want {
			t.Errorf("Host = %q, want %q", got, want)
		}
	})
}

// TestClaimOptions pins where the holder of a claim comes from. Not parallel,
// for the reason TestCloneOptionsDataHome is not.
func TestClaimOptions(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())

	t.Run("inside Claude Code", func(t *testing.T) {
		t.Setenv("CLAUDE_PID", "4242")
		t.Setenv("CLAUDE_CODE_SESSION_ID", "session-1")
		got := claimOptions()
		if want := (reviewprs.Holder{PID: 4242, SessionID: "session-1"}); got.Holder != want {
			t.Errorf("Holder = %+v, want %+v", got.Holder, want)
		}
		if got.StateHome != stateHome() {
			t.Errorf("StateHome = %q, want %q", got.StateHome, stateHome())
		}
	})

	for _, pid := range []string{"", "not a pid", "-1"} {
		t.Run("CLAUDE_PID="+pid, func(t *testing.T) {
			t.Setenv("CLAUDE_PID", pid)
			t.Setenv("CLAUDE_CODE_SESSION_ID", "")
			if got := claimOptions().Holder; got != (reviewprs.Holder{}) {
				t.Errorf("Holder = %+v, want none", got)
			}
		})
	}
}
