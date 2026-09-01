// Package hooktest holds the fixtures more than one hook's tests need.
//
// It exists because Go cannot share a _test.go helper between packages, and
// the git isolation that keeps a fixture from reading the developer's own
// configuration is an invariant worth having one copy of: weakened in a second
// copy, it would fail only on the machine that has the unusual configuration.
package hooktest

import (
	"path/filepath"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
)

// OpenStore opens a state tree that closes itself when the test ends.
func OpenStore(t *testing.T, dir string) *state.Store {
	t.Helper()
	s, err := state.Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })
	return s
}

// Names lists a directory in the store, failing the test if it cannot be read.
func Names(t *testing.T, s *state.Store, dir string) []string {
	t.Helper()
	names, err := s.Names(dir)
	if err != nil {
		t.Fatalf("Names(%q): %v", dir, err)
	}
	return names
}

// InitRepo makes a repository with one commit in it.
func InitRepo(t *testing.T, dir string) {
	t.Helper()
	Write(t, filepath.Join(dir, "file.txt"), "x\n")
	Git(t, dir, "init", "-q")
	Git(t, dir, "config", "user.email", "test@example.com")
	Git(t, dir, "config", "user.name", "test")
	Git(t, dir, "add", ".")
	Git(t, dir, "commit", "-qm", "first")
}

// Git runs one git command in dir and fails the test if it does not succeed.
//
// gittest's, so that the isolation from the developer's own configuration has
// one implementation: weakened in one copy it would fail only on the machine
// that has the unusual setting.
func Git(t *testing.T, dir string, args ...string) {
	t.Helper()
	gittest.Run(t, dir, args...)
}

// Write creates a file and the directories above it.
func Write(t *testing.T, name, content string) {
	t.Helper()
	gittest.Write(t, name, content)
}

// SkipWithoutGit skips a test on a machine that has no git.
func SkipWithoutGit(t *testing.T) {
	t.Helper()
	gittest.SkipWithoutGit(t)
}
