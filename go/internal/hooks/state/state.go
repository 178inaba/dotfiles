// Package state holds what the hooks have to remember between invocations: the
// process id of a running caffeinate, and a marker per live subagent.
//
// This is runtime state and not cached data, which is why it does not join the
// status line's caches under os.UserCacheDir. Deleting a cache costs the time
// to rebuild it; deleting a pid file leaks a caffeinate that keeps the machine
// awake, with nothing left to say which session it belonged to. It goes to
// /tmp, where macOS's dirhelper sweeps what a session that died without its
// stop hook left behind.
package state

import (
	"os"
	"path"
	"strings"
)

// Dir is where the state lives. Literally /tmp and not os.TempDir, which on
// macOS names a per-user directory under /var/folders that dirhelper does not
// sweep the same way.
const Dir = "/tmp/ccx"

// CaffeinateDir holds one pid file per running caffeinate.
const CaffeinateDir = "caffeinate"

// subagentDir holds one directory per session, holding one marker per subagent.
const subagentDir = "subagents"

// SessionPID names the pid file of the caffeinate held for a whole session.
func SessionPID(session string) string {
	return path.Join(CaffeinateDir, session+".pid")
}

// AgentPID names the pid file of the caffeinate held for one running subagent.
//
// The session and the agent are joined by a hyphen with nothing to tell them
// apart, so a session literally named "<other session>-<agent>" would collide
// with that agent's file. Claude Code issues both as fixed-length ids, which
// leaves no way to write one that is another with an agent appended.
func AgentPID(session, agent string) string {
	return path.Join(CaffeinateDir, session+"-"+agent+".pid")
}

// AgentDone names the same file after the subagent has finished and its
// caffeinate is waiting for the parent's next stop to collect it.
func AgentDone(session, agent string) string {
	return strings.TrimSuffix(AgentPID(session, agent), ".pid") + ".done"
}

// MarkerDir names the directory holding one session's subagent markers.
func MarkerDir(session string) string {
	return path.Join(subagentDir, session)
}

// Marker names the file whose existence says a subagent is running.
func Marker(session, agent string) string {
	return path.Join(MarkerDir(session), agent)
}

// Store is the state tree. Every path it is given is relative to the root and
// resolved by os.Root, so a symlink planted in world-writable /tmp cannot
// redirect a write out of the tree. What that does not protect against is a
// /tmp/ccx that already belongs to another account, which this accepts: the
// machine has one user.
type Store struct {
	root *os.Root
}

// Open creates the tree if it is not there and opens it. Callers must Close.
func Open(dir string) (*Store, error) {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, err
	}
	root, err := os.OpenRoot(dir)
	if err != nil {
		return nil, err
	}
	return &Store{root: root}, nil
}

// Close releases the tree.
func (s *Store) Close() error { return s.root.Close() }

// Read returns a file's contents without its trailing newline, and whether
// there was one to read. A file that cannot be read is a file that is not
// there: every caller's next step is the same either way.
func (s *Store) Read(name string) (string, bool) {
	b, err := s.root.ReadFile(name)
	if err != nil {
		return "", false
	}
	return strings.TrimRight(string(b), "\n"), true
}

// Write stores a line, creating the parent directory. The trailing newline is
// what the shell's printf wrote and what makes the files readable by hand.
func (s *Store) Write(name, line string) error {
	if dir := path.Dir(name); dir != "." {
		if err := s.root.MkdirAll(dir, 0o700); err != nil {
			return err
		}
	}
	return s.root.WriteFile(name, []byte(line+"\n"), 0o600)
}

// Remove deletes a file, and reports nothing for one that has already gone.
func (s *Store) Remove(name string) error {
	if err := s.root.Remove(name); err != nil && !os.IsNotExist(err) {
		return err
	}
	return nil
}

// RemoveAll deletes a directory and everything under it.
func (s *Store) RemoveAll(name string) error { return s.root.RemoveAll(name) }

// Rename moves a file within the tree.
func (s *Store) Rename(from, to string) error { return s.root.Rename(from, to) }

// Names lists the entries of a directory, in no particular order. A directory
// that is not there has no entries, which is the state every hook starts from.
func (s *Store) Names(dir string) []string {
	f, err := s.root.Open(dir)
	if err != nil {
		return nil
	}
	defer f.Close()

	entries, err := f.ReadDir(-1)
	if err != nil {
		return nil
	}
	names := make([]string, 0, len(entries))
	for _, e := range entries {
		names = append(names, e.Name())
	}
	return names
}
