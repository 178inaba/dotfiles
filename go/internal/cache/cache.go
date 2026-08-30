// Package cache reads and writes the cache files the ported tools keep under
// /tmp and ~/.cache.
//
// The files hold exactly the bytes the shell implementations wrote, down to
// which of them end with a newline. That matters because the two
// implementations shared them during the port, and because the records are
// compared by hand when something looks wrong.
package cache

import (
	"os"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/shellfmt"
)

// maxPathLength caps the file name so a deep working directory cannot exceed
// the filesystem's limit. Two directories that collide after the cut share a
// file, which the key recorded inside it resolves.
const maxPathLength = 200

// Path is the cache file for a key: the base, the key with its slashes
// flattened, and the result cut to length.
//
// The cut counts characters rather than bytes, which is what bash does in a
// UTF-8 locale. Under LC_ALL=C it would count bytes; that locale never applies
// here, and cutting a multibyte path mid-rune would be worse than the
// divergence.
func Path(base, key string) string {
	p := base + "-" + strings.ReplaceAll(key, "/", "_")
	if r := []rune(p); len(r) > maxPathLength {
		return string(r[:maxPathLength])
	}
	return p
}

// Keyed is the three-line record the git and pull request caches hold: when it
// was written, what it was written for, and the rendered fragment.
type Keyed struct {
	At     int64
	Key    string
	Result string
}

// ReadKeyed returns the record in a file. The second value is false when there
// is no usable one, which covers a missing file and a corrupt timestamp alike —
// the shell told those apart no more finely than this.
func ReadKeyed(path string) (Keyed, bool) {
	lines, err := lines(path, 3)
	if err != nil {
		return Keyed{}, false
	}
	at, ok := timestamp(lines[0])
	if !ok {
		return Keyed{}, false
	}
	return Keyed{At: at, Key: lines[1], Result: lines[2]}, true
}

// WriteKeyed writes the record in place, without a temporary file.
//
// The git cache is written this way because the shell was: a reader that
// catches a half-written file sees a corrupt timestamp, treats it as no record
// and recomputes, which costs one git invocation and nothing else.
func WriteKeyed(path string, k Keyed) error {
	return os.WriteFile(path, keyedBytes(k), 0o644)
}

// WriteKeyedAtomic writes the record through a temporary file and a rename.
//
// The pull request cache is written this way because a background refresh
// writes it while a render may be reading, and a torn read there would drop the
// badge for a whole refresh interval rather than for one tick.
func WriteKeyedAtomic(path string, k Keyed) error {
	return WriteAtomic(path, keyedBytes(k))
}

// keyedBytes is the record's on-disk form: three lines, no trailing newline.
func keyedBytes(k Keyed) []byte {
	return []byte(strconv.FormatInt(k.At, 10) + "\n" + k.Key + "\n" + k.Result)
}

// ReadPair returns the two-line record the exchange rate cache holds.
func ReadPair(path string) (int64, string, bool) {
	lines, err := lines(path, 2)
	if err != nil {
		return 0, "", false
	}
	at, ok := timestamp(lines[0])
	if !ok {
		return 0, "", false
	}
	return at, lines[1], true
}

// WritePair writes the exchange rate record, which unlike the three-line one
// ends with a newline.
func WritePair(path string, at int64, value string) error {
	return WriteAtomic(path, []byte(strconv.FormatInt(at, 10)+"\n"+value+"\n"))
}

// ReadAttempt returns when a refresh was last started.
func ReadAttempt(path string) (int64, bool) {
	lines, err := lines(path, 1)
	if err != nil {
		return 0, false
	}
	return timestamp(lines[0])
}

// WriteAttempt records that a refresh is starting. The foreground writes it
// before spawning the child, so a second render arriving while the first fetch
// is still in flight does not start another one.
func WriteAttempt(path string, at int64) error {
	return os.WriteFile(path, []byte(strconv.FormatInt(at, 10)+"\n"), 0o644)
}

// ShouldAttempt reports whether a refresh of the record at cachePath may start,
// and records the attempt when it may.
//
// The throttle lives beside the record as "<path>.attempt", which means two
// keys that collide after the name is cut to length share one throttle as well
// as one record. Every stale-while-revalidate cache uses this, so the suffix
// and the bookkeeping have one owner rather than one per caller.
func ShouldAttempt(cachePath string, now, retryInterval int64) bool {
	attemptPath := cachePath + ".attempt"
	if last, ok := ReadAttempt(attemptPath); ok && Fresh(now, last, retryInterval) {
		return false
	}
	// Best effort: a write that fails only costs one duplicate refresh.
	_ = WriteAttempt(attemptPath, now)
	return true
}

// Fresh reports whether a record written at is still within maxAge.
//
// The comparison is signed and the boundary is inclusive, both as the shell had
// them: a record exactly at its limit is fresh, and a clock that jumped
// backwards makes everything look fresh rather than starting a stampede of
// refreshes.
func Fresh(now, at, maxAge int64) bool {
	return now-at <= maxAge
}

// WriteAtomic writes through a sibling temporary file named after this process,
// as the shell did with $$, and renames it into place.
func WriteAtomic(path string, body []byte) error {
	tmp := path + "." + strconv.Itoa(os.Getpid())
	// 0644 rather than the 0600 a temporary file defaults to: the shell created
	// these with a plain redirect and the mode is part of the on-disk contract.
	if err := os.WriteFile(tmp, body, 0o644); err != nil {
		return err
	}
	if err := os.Rename(tmp, path); err != nil {
		os.Remove(tmp)
		return err
	}
	return nil
}

// lines reads the first n lines of a file the way a run of `read` calls would.
func lines(path string, n int) ([]string, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return shellfmt.Lines(string(b), n), nil
}

// timestamp accepts only what the shell's ^[0-9]+$ test accepted, so a corrupt
// or partially written record is no record at all.
func timestamp(s string) (int64, bool) {
	if !shellfmt.IsDigits(s) {
		return 0, false
	}
	at, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		return 0, false
	}
	return at, true
}
