package state

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
)

const (
	marker    = "subagents/s1/a1"
	markerDir = "subagents/s1"
)

func TestOpenCreatesThePrivateRoot(t *testing.T) {
	t.Parallel()

	dir := filepath.Join(t.TempDir(), "ccx")
	s, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })

	info, err := os.Stat(dir)
	if err != nil {
		t.Fatalf("Stat: %v", err)
	}
	// The tree lives in world-writable /tmp, so nobody else may read a session
	// id out of it or drop a marker in.
	if got, want := info.Mode().Perm(), os.FileMode(0o700); got != want {
		t.Errorf("mode = %v, want %v", got, want)
	}
}

func TestCreateRemove(t *testing.T) {
	t.Parallel()
	s := open(t)

	if got := names(t, s, markerDir); len(got) != 0 {
		t.Fatalf("Names = %v, want none before Create", got)
	}
	if err := s.Create(marker); err != nil {
		t.Fatalf("Create: %v", err)
	}
	if got := names(t, s, markerDir); !slices.Equal(got, []string{"a1"}) {
		t.Errorf("Names = %v, want [a1]", got)
	}

	if err := s.Remove(marker); err != nil {
		t.Fatalf("Remove: %v", err)
	}
	if got := names(t, s, markerDir); len(got) != 0 {
		t.Errorf("Names = %v, want none after Remove", got)
	}
	// Removing what is not there is how every stop path begins.
	if err := s.Remove(marker); err != nil {
		t.Errorf("Remove of a missing file: %v", err)
	}
}

func TestListNames(t *testing.T) {
	t.Parallel()
	s := open(t)

	for _, name := range []string{marker, markerDir + "/a2", markerDir + "/a3"} {
		if err := s.Create(name); err != nil {
			t.Fatalf("Create(%s): %v", name, err)
		}
	}

	got := names(t, s, markerDir)
	slices.Sort(got)
	want := []string{"a1", "a2", "a3"}
	if !slices.Equal(got, want) {
		t.Errorf("Names = %v, want %v", got, want)
	}

	if got := names(t, s, "subagents/nothing"); len(got) != 0 {
		t.Errorf("Names of a missing directory = %v, want none", got)
	}
}

func TestRemoveAll(t *testing.T) {
	t.Parallel()
	s := open(t)

	if err := s.Create(marker); err != nil {
		t.Fatalf("Create: %v", err)
	}
	if err := s.RemoveAll(markerDir); err != nil {
		t.Fatalf("RemoveAll: %v", err)
	}
	if got := names(t, s, markerDir); len(got) != 0 {
		t.Errorf("Names = %v, want the directory to be gone", got)
	}
	if err := s.RemoveAll(markerDir); err != nil {
		t.Errorf("RemoveAll of a missing directory: %v", err)
	}
}

// TestSymlinkCannotEscape is why the store goes through os.Root: /tmp is
// world-writable, so anyone can leave a symlink where a hook is about to write.
func TestSymlinkCannotEscape(t *testing.T) {
	t.Parallel()

	dir := filepath.Join(t.TempDir(), "ccx")
	s, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })

	outside := filepath.Join(t.TempDir(), "outside")
	if err := os.MkdirAll(filepath.Join(dir, "subagents", "s1"), 0o700); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.Symlink(outside, filepath.Join(dir, "subagents", "s1", "a1")); err != nil {
		t.Fatalf("Symlink: %v", err)
	}

	if err := s.Create(marker); err == nil {
		t.Error("Create followed a symlink out of the root")
	}
	if _, err := os.Stat(outside); err == nil {
		t.Error("the file outside the root was created")
	}
}

func open(t *testing.T) *Store {
	t.Helper()
	return openAt(t, filepath.Join(t.TempDir(), "ccx"))
}

func openAt(t *testing.T, dir string) *Store {
	t.Helper()
	s, err := Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })
	return s
}

// names lists a directory, failing the test if it cannot be read.
func names(t *testing.T, s *Store, dir string) []string {
	t.Helper()
	got, err := s.Names(dir)
	if err != nil {
		t.Fatalf("Names(%q): %v", dir, err)
	}
	return got
}
