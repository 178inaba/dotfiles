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
	"errors"
	"io/fs"
	"os"
	"path"
)

// Dir is where the state lives. Literally /tmp and not os.TempDir, which on
// macOS names a per-user directory under /var/folders that dirhelper does not
// sweep the same way.
const Dir = "/tmp/ccx"

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

// Read returns a file's contents, and whether there was one to read. A file
// that cannot be read is a file that is not there: every caller's next step is
// the same either way.
func (s *Store) Read(name string) (string, bool) {
	b, err := s.root.ReadFile(name)
	if err != nil {
		return "", false
	}
	return string(b), true
}

// Write stores a value, creating the parent directory.
func (s *Store) Write(name, value string) error {
	if dir := path.Dir(name); dir != "." {
		if err := s.root.MkdirAll(dir, 0o700); err != nil {
			return err
		}
	}
	return s.root.WriteFile(name, []byte(value), 0o600)
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

// Names lists the entries of a directory, in no particular order.
//
// A directory that is not there has no entries and no error: that is the state
// every hook starts from. A directory that is there and cannot be listed is a
// different thing, and what to do about it is the caller's to decide — folding
// the two together would have a stop hook collect nothing and say nothing.
func (s *Store) Names(dir string) ([]string, error) {
	f, err := s.root.Open(dir)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	defer f.Close()

	entries, err := f.ReadDir(-1)
	if err != nil {
		return nil, err
	}
	names := make([]string, 0, len(entries))
	for _, e := range entries {
		names = append(names, e.Name())
	}
	return names, nil
}
