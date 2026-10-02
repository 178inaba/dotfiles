// Package filelock is an exclusive advisory lock that several ccx processes
// wait on in turn.
package filelock

import (
	"errors"
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

// Lock waits until it holds an exclusive flock on path, creating the file
// where it is missing, and returns the function that releases it.
//
// The file is never removed. Deleting a lock file while somebody waits on it
// would let the next caller create and lock a fresh inode beside the one still
// held, and the two would no longer exclude each other. A process that dies
// releases the lock with its descriptors, so nothing is left held by a caller
// that is gone.
func Lock(path string) (func(), error) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("failed to open the lock file %s: %w", path, err)
	}
	for {
		err = unix.Flock(int(f.Fd()), unix.LOCK_EX)
		// The Go runtime's own signals interrupt a blocking flock.
		if !errors.Is(err, unix.EINTR) {
			break
		}
	}
	if err != nil {
		f.Close()
		return nil, fmt.Errorf("failed to lock %s: %w", path, err)
	}
	return func() { f.Close() }, nil
}
