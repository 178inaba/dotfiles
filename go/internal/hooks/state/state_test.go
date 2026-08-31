package state

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
)

const (
	pidFile   = "caffeinate/s1.pid"
	doneFile  = "caffeinate/s1.done"
	otherFile = "caffeinate/s2.pid"
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
	// id out of it or drop a pid file in.
	if got, want := info.Mode().Perm(), os.FileMode(0o700); got != want {
		t.Errorf("mode = %v, want %v", got, want)
	}
}

func TestWriteReadRemove(t *testing.T) {
	t.Parallel()
	s := open(t)

	if _, ok := s.Read(pidFile); ok {
		t.Error("Read found a file that was never written")
	}
	if err := s.Write(pidFile, "4242"); err != nil {
		t.Fatalf("Write: %v", err)
	}
	got, ok := s.Read(pidFile)
	if !ok {
		t.Fatal("Read did not find the file just written")
	}
	if want := "4242"; got != want {
		t.Errorf("Read = %q, want %q", got, want)
	}

	if err := s.Remove(pidFile); err != nil {
		t.Fatalf("Remove: %v", err)
	}
	if _, ok := s.Read(pidFile); ok {
		t.Error("Read found the file after it was removed")
	}
	// Removing what is not there is how every stop path begins.
	if err := s.Remove(pidFile); err != nil {
		t.Errorf("Remove of a missing file: %v", err)
	}
}

func TestWriteStoresExactlyWhatItWasGiven(t *testing.T) {
	t.Parallel()
	root := filepath.Join(t.TempDir(), "ccx")
	s := openAt(t, root)

	// The value round-trips unchanged: what a hook writes is the format its
	// own reader defines, and the store adds nothing to it.
	if err := s.Write(marker, ""); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if got, ok := s.Read(marker); !ok || got != "" {
		t.Errorf("Read = %q, %t, want %q, true", got, ok, "")
	}

	raw, err := os.ReadFile(filepath.Join(root, marker))
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if len(raw) != 0 {
		t.Errorf("file contents = %q, want it empty", raw)
	}
}

func TestRename(t *testing.T) {
	t.Parallel()
	s := open(t)

	if err := s.Write(pidFile, "7"); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if err := s.Rename(pidFile, doneFile); err != nil {
		t.Fatalf("Rename: %v", err)
	}
	if _, ok := s.Read(pidFile); ok {
		t.Error("the running pid file survived the rename")
	}
	if got, ok := s.Read(doneFile); !ok || got != "7" {
		t.Errorf("Read(done) = %q, %t, want %q, true", got, ok, "7")
	}
}

func TestListNames(t *testing.T) {
	t.Parallel()
	s := open(t)

	for _, name := range []string{pidFile, doneFile, otherFile} {
		if err := s.Write(name, "1"); err != nil {
			t.Fatalf("Write(%s): %v", name, err)
		}
	}

	got := s.Names("caffeinate")
	slices.Sort(got)
	want := []string{"s1.done", "s1.pid", "s2.pid"}
	if !slices.Equal(got, want) {
		t.Errorf("Names = %v, want %v", got, want)
	}

	if got := s.Names("subagents/nothing"); len(got) != 0 {
		t.Errorf("Names of a missing directory = %v, want none", got)
	}
}

func TestRemoveAll(t *testing.T) {
	t.Parallel()
	s := open(t)

	if err := s.Write(marker, "1"); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if err := s.RemoveAll(markerDir); err != nil {
		t.Fatalf("RemoveAll: %v", err)
	}
	if got := s.Names(markerDir); len(got) != 0 {
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
	if err := os.MkdirAll(filepath.Join(dir, "caffeinate"), 0o700); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.Symlink(outside, filepath.Join(dir, "caffeinate", "s1.pid")); err != nil {
		t.Fatalf("Symlink: %v", err)
	}

	if err := s.Write(pidFile, "4242"); err == nil {
		t.Error("Write followed a symlink out of the root")
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
