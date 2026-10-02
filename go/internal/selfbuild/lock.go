package selfbuild

import (
	"os"
	"path/filepath"

	"github.com/178inaba/dotfiles/go/internal/filelock"
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
	if err := os.MkdirAll(d.CacheDir, 0o755); err != nil {
		return nil, false
	}
	return filelock.TryLock(filepath.Join(d.CacheDir, "build.lock"))
}
