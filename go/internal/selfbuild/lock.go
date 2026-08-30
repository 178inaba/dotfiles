package selfbuild

import (
	"os"
	"path/filepath"
	"syscall"
)

// lock serialises rebuilds across the processes that start together — a
// statusline tick and a handful of hooks can all notice the same stale binary
// within milliseconds of each other.
//
// The lock is never waited on. A process that does not get it carries on with
// the binary it has, because blocking would turn one slow build into a stall of
// everything that fired at the same moment; the next invocation picks the
// rebuild up if it is still needed.
func lock(d Deps) (func(), bool) {
	if err := os.MkdirAll(cacheDir(d), 0o755); err != nil {
		return nil, false
	}
	f, err := os.OpenFile(filepath.Join(cacheDir(d), "build.lock"), os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, false
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		return nil, false
	}
	return func() {
		// Closing the descriptor releases the flock; the file itself stays so
		// the next run locks the same inode.
		f.Close()
	}, true
}
