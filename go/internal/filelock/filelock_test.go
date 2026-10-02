package filelock_test

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/178inaba/dotfiles/go/internal/filelock"
)

// TestLockWaitsForTheHolder pins the one property callers rely on: a second
// Lock of the same path does not return until the first is released, and the
// file outlives both so that the next caller locks the same inode.
func TestLockWaitsForTheHolder(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "x.lock")
	release, err := filelock.Lock(path)
	if err != nil {
		t.Fatalf("Lock: %v", err)
	}

	acquired := make(chan struct{})
	go func() {
		second, err := filelock.Lock(path)
		if err != nil {
			t.Errorf("second Lock: %v", err)
			close(acquired)
			return
		}
		close(acquired)
		second()
	}()

	select {
	case <-acquired:
		t.Fatal("the second Lock returned while the first was held")
	case <-time.After(100 * time.Millisecond):
	}

	release()
	select {
	case <-acquired:
	case <-time.After(5 * time.Second):
		t.Fatal("the second Lock did not return after the first was released")
	}

	if _, err := os.Stat(path); err != nil {
		t.Errorf("the lock file is gone after release: %v", err)
	}
}

func TestLockFailsWithoutItsDirectory(t *testing.T) {
	t.Parallel()

	if _, err := filelock.Lock(filepath.Join(t.TempDir(), "missing", "x.lock")); err == nil {
		t.Error("Lock succeeded in a directory that does not exist")
	}
}
