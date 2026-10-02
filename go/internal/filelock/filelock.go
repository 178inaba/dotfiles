// Package filelock is an exclusive advisory lock that several ccx processes
// wait on in turn.
package filelock

import (
	"errors"
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

// open opens the lock file, creating it where it is missing.
//
// The file is never removed. Deleting a lock file while somebody waits on it
// would let the next caller create and lock a fresh inode beside the one still
// held, and the two would no longer exclude each other. A process that dies
// releases the lock with its descriptors, so nothing is left held by a caller
// that is gone.
func open(path string) (*os.File, error) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("failed to open the lock file %s: %w", path, err)
	}
	return f, nil
}

// Lock waits until it holds an exclusive flock on path, creating the file
// where it is missing, and returns the function that releases it.
func Lock(path string) (func(), error) {
	f, err := open(path)
	if err != nil {
		return nil, err
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

// TryLock takes the exclusive flock on path only if nobody holds it, and
// reports whether it did.
func TryLock(path string) (func(), bool) {
	f, err := open(path)
	if err != nil {
		return nil, false
	}
	if err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB); err != nil {
		f.Close()
		return nil, false
	}
	return func() { f.Close() }, true
}
