package cmd

import (
	"path/filepath"
	"testing"
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
