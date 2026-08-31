package state

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func TestNames(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		got  string
		want string
	}{
		{"session pid", SessionPID("s1"), "caffeinate/s1.pid"},
		{"agent pid", AgentPID("s1", "a1"), "caffeinate/s1-a1.pid"},
		{"agent done", AgentDone("s1", "a1"), "caffeinate/s1-a1.done"},
		{"caffeinate directory", CaffeinateDir, "caffeinate"},
		{"marker", Marker("s1", "a1"), "subagents/s1/a1"},
		{"marker directory", MarkerDir("s1"), "subagents/s1"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if tt.got != tt.want {
				t.Errorf("name = %q, want %q", tt.got, tt.want)
			}
		})
	}
}

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

	if _, ok := s.Read(SessionPID("s1")); ok {
		t.Error("Read found a file that was never written")
	}
	if err := s.Write(SessionPID("s1"), "4242"); err != nil {
		t.Fatalf("Write: %v", err)
	}
	got, ok := s.Read(SessionPID("s1"))
	if !ok {
		t.Fatal("Read did not find the file just written")
	}
	if want := "4242"; got != want {
		t.Errorf("Read = %q, want %q", got, want)
	}

	if err := s.Remove(SessionPID("s1")); err != nil {
		t.Fatalf("Remove: %v", err)
	}
	if _, ok := s.Read(SessionPID("s1")); ok {
		t.Error("Read found the file after it was removed")
	}
	// Removing what is not there is how every stop path begins.
	if err := s.Remove(SessionPID("s1")); err != nil {
		t.Errorf("Remove of a missing file: %v", err)
	}
}

func TestWriteTrimsWhatReadReturns(t *testing.T) {
	t.Parallel()
	dir := filepath.Join(t.TempDir(), "ccx")
	s := openAt(t, dir)

	// The shell wrote its pid files with a trailing newline and read them back
	// with $(<file), which strips it.
	if err := s.Write(Marker("s1", "a1"), ""); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if got, _ := s.Read(Marker("s1", "a1")); got != "" {
		t.Errorf("Read = %q, want the empty marker to read as empty", got)
	}

	raw, err := os.ReadFile(filepath.Join(dir, Marker("s1", "a1")))
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if got, want := string(raw), "\n"; got != want {
		t.Errorf("file contents = %q, want %q", got, want)
	}
}

func TestRename(t *testing.T) {
	t.Parallel()
	s := open(t)

	if err := s.Write(AgentPID("s1", "a1"), "7"); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if err := s.Rename(AgentPID("s1", "a1"), AgentDone("s1", "a1")); err != nil {
		t.Fatalf("Rename: %v", err)
	}
	if _, ok := s.Read(AgentPID("s1", "a1")); ok {
		t.Error("the running pid file survived the rename")
	}
	if got, ok := s.Read(AgentDone("s1", "a1")); !ok || got != "7" {
		t.Errorf("Read(done) = %q, %t, want %q, true", got, ok, "7")
	}
}

func TestListNames(t *testing.T) {
	t.Parallel()
	s := open(t)

	for _, name := range []string{
		SessionPID("s1"),
		AgentPID("s1", "a1"),
		AgentDone("s1", "a2"),
		AgentPID("other", "a3"),
	} {
		if err := s.Write(name, "1"); err != nil {
			t.Fatalf("Write(%s): %v", name, err)
		}
	}

	got := s.Names(CaffeinateDir)
	slices.Sort(got)
	want := []string{"other-a3.pid", "s1-a1.pid", "s1-a2.done", "s1.pid"}
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

	if err := s.Write(Marker("s1", "a1"), "1"); err != nil {
		t.Fatalf("Write: %v", err)
	}
	if err := s.RemoveAll(MarkerDir("s1")); err != nil {
		t.Fatalf("RemoveAll: %v", err)
	}
	if got := s.Names(MarkerDir("s1")); len(got) != 0 {
		t.Errorf("Names = %v, want the directory to be gone", got)
	}
	if err := s.RemoveAll(MarkerDir("s1")); err != nil {
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
	if err := os.MkdirAll(filepath.Join(dir, CaffeinateDir), 0o700); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.Symlink(outside, filepath.Join(dir, CaffeinateDir, "s1.pid")); err != nil {
		t.Fatalf("Symlink: %v", err)
	}

	if err := s.Write(SessionPID("s1"), "4242"); err == nil {
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
