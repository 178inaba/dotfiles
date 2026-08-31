// Package cache stores the short-lived state the status line would otherwise
// recompute on every redraw.
//
// One entry is one directory holding a record, and the record is a JSON
// document written whole and renamed into place, so a reader sees either the
// previous one or the new one. Anything that will not parse — a file left by an
// older version, a truncated write — is reported as absent, which costs one
// recomputation and never a garbled display.
package cache

import (
	"encoding/json/v2"
	"os"
	"path/filepath"
	"strconv"
	"time"
)

const (
	// recordName and attemptName are the two files an entry can hold. The
	// throttle is a file of its own because the process that decides to refresh
	// is not the one that writes the result: a single file would have the
	// foreground clobber what the background had just stored.
	recordName  = "record.json"
	attemptName = "attempt.json"
)

// Dir is the directory this module caches under.
//
// The user cache directory rather than /tmp, which is where the shell version
// kept these: /tmp is world-writable and shared between accounts, and a cache
// is what the cache directory is for.
func Dir() string {
	base, err := os.UserCacheDir()
	if err != nil {
		// Nowhere to derive a home directory from. Somewhere writable beats
		// scattering relative paths through whatever the working directory is.
		base = os.TempDir()
	}
	return filepath.Join(base, "ccx")
}

// Path is the entry for a key, whose parts are laid out as directories beneath
// base.
//
// Mirroring the key rather than flattening it into one name is what removes the
// need for a length limit: the filesystem bounds each component, not the path,
// so a working directory of any depth fits without being cut. Each part is
// rooted before it is cleaned, so no key can name anything outside base.
//
// Parts are joined, so a key can in principle be split two ways — a directory
// /a/b with branch c lands where /a would with branch b/c. The key recorded
// inside the entry is what tells those apart.
func Path(base string, key ...string) string {
	p := base
	for _, k := range key {
		p = filepath.Join(p, filepath.Clean("/"+k))
	}
	return p
}

// Record is one cached value: when it was written, what it was written for, and
// the value.
//
// Key is stored so that a reader who finds someone else's record treats it as
// absent rather than showing one working directory's state under another.
type Record[T any] struct {
	At    time.Time `json:"at"`
	Key   string    `json:"key"`
	Value T         `json:"value"`
}

// Read returns the record in an entry. The second value is false when there is
// none to use: a missing or unreadable file, or one written for another key.
func Read[T any](dir, key string) (Record[T], bool) {
	return read[T](filepath.Join(dir, recordName), key)
}

func read[T any](path, key string) (Record[T], bool) {
	b, err := os.ReadFile(path)
	if err != nil {
		return Record[T]{}, false
	}
	var r Record[T]
	if err := json.Unmarshal(b, &r); err != nil {
		return Record[T]{}, false
	}
	if r.Key != key {
		return Record[T]{}, false
	}
	return r, true
}

// Write stores a record through a temporary file and a rename.
//
// Always atomically: a background refresh writes while a redraw may be reading,
// and a torn read would show a half-built value rather than the previous one.
func Write[T any](dir, key string, at time.Time, value T) error {
	return write(dir, recordName, key, at, value)
}

func write[T any](dir, name, key string, at time.Time, value T) error {
	b, err := json.Marshal(Record[T]{At: at, Key: key, Value: value})
	if err != nil {
		return err
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}

	path := filepath.Join(dir, name)
	tmp := path + "." + strconv.Itoa(os.Getpid())
	if err := os.WriteFile(tmp, b, 0o644); err != nil {
		return err
	}
	if err := os.Rename(tmp, path); err != nil {
		os.Remove(tmp)
		return err
	}
	return nil
}

// Fresh reports whether a record written at is still within maxAge. A clock
// that jumped backwards makes everything look fresh rather than starting a
// stampede of refreshes.
func Fresh(now, at time.Time, maxAge time.Duration) bool {
	return now.Sub(at) <= maxAge
}

// attemptKey is the key of a throttle record, which has nothing to distinguish
// beyond the entry it lives in.
const attemptKey = "attempt"

// ShouldAttempt reports whether a refresh of an entry may start, and records
// the attempt when it may. Every stale-while-revalidate cache uses this, so the
// convention has one owner.
func ShouldAttempt(dir string, now time.Time, retry time.Duration) bool {
	if r, ok := read[struct{}](filepath.Join(dir, attemptName), attemptKey); ok && Fresh(now, r.At, retry) {
		return false
	}
	// Best effort: a write that fails only costs one duplicate refresh.
	_ = write(dir, attemptName, attemptKey, now, struct{}{})
	return true
}
