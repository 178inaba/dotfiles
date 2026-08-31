// Package cache stores the short-lived state the status line would otherwise
// recompute on every redraw.
//
// A record is one JSON document, written whole and renamed into place, so a
// reader sees either the previous record or the new one. Anything that will not
// parse — a file left by an older version, a truncated write — is reported as
// absent, which costs one recomputation and never a garbled display.
package cache

import (
	"encoding/json/v2"
	"os"
	"strconv"
	"strings"
	"time"
)

// maxPathLength caps the file name so a deep working directory cannot exceed
// the filesystem's limit. Two directories that collide after the cut share a
// file, which the key recorded inside it resolves.
const maxPathLength = 200

// Path is the cache file for a key: the base, the key with its slashes
// flattened, and the result cut to length.
//
// The cut counts characters rather than bytes. APFS rejects a name that is not
// valid UTF-8, so cutting a multibyte path mid-rune would produce a file that
// cannot be created at all and a cache that silently never works.
func Path(base, key string) string {
	p := base + "-" + strings.ReplaceAll(key, "/", "_")
	if r := []rune(p); len(r) > maxPathLength {
		return string(r[:maxPathLength])
	}
	return p
}

// Record is one cached value: when it was written, what it was written for, and
// the value.
//
// Key is stored because two directories can share a file once the name is cut
// to length. A reader that finds someone else's key treats the record as
// absent rather than showing one directory's state under another.
type Record[T any] struct {
	At    time.Time `json:"at"`
	Key   string    `json:"key"`
	Value T         `json:"value"`
}

// Read returns the record in a file. The second value is false when there is
// none to use: a missing or unreadable file, or one written for another key.
func Read[T any](path, key string) (Record[T], bool) {
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
func Write[T any](path, key string, at time.Time, value T) error {
	b, err := json.Marshal(Record[T]{At: at, Key: key, Value: value})
	if err != nil {
		return err
	}

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
// beyond the file it lives in.
const attemptKey = "attempt"

// ShouldAttempt reports whether a refresh of the record at path may start, and
// records the attempt when it may.
//
// The throttle is a file of its own beside the record, because the process that
// decides to refresh is not the one that writes the result: a single file would
// have the foreground clobber what the background had just stored. Every
// stale-while-revalidate cache uses this, so the convention has one owner.
func ShouldAttempt(path string, now time.Time, retry time.Duration) bool {
	attempt := path + ".attempt"
	if r, ok := Read[struct{}](attempt, attemptKey); ok && Fresh(now, r.At, retry) {
		return false
	}
	// Best effort: a write that fails only costs one duplicate refresh.
	_ = Write(attempt, attemptKey, now, struct{}{})
	return true
}
