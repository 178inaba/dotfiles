// Package statetest reads a hook's state tree from a test.
//
// A package of its own because Go cannot share a _test.go between packages,
// and both hooks that keep state — caffeinate's pid files and notify's
// subagent markers — assert on what they wrote through the same store.
package statetest

import (
	"testing"

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
