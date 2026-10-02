package atomicfile_test

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"golang.org/x/sys/unix"

	"github.com/178inaba/dotfiles/go/internal/atomicfile"
)

// writeString is the fill of a caller that has its bytes in hand.
func writeString(s string) func(*os.File) error {
	return func(f *os.File) error {
		_, err := f.WriteString(s)
		return err
	}
}

// names lists what dir holds, so that a temporary file left behind shows up as
// a name beside the destination.
func names(t *testing.T, dir string) []string {
	t.Helper()
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	var got []string
	for _, e := range entries {
		got = append(got, e.Name())
	}
	return got
}

func TestWrite(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		previous *string
	}{
		{name: "a new file"},
		{name: "a file that exists", previous: new("old")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := t.TempDir()
			path := filepath.Join(dir, "record.json")
			if tt.previous != nil {
				if err := os.WriteFile(path, []byte(*tt.previous), 0o600); err != nil {
					t.Fatalf("WriteFile: %v", err)
				}
			}

			if err := atomicfile.Write(path, 0o600, writeString("new")); err != nil {
				t.Fatalf("Write: %v", err)
			}

			b, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("ReadFile: %v", err)
			}
			if string(b) != "new" {
				t.Errorf("%s holds %q, want %q", path, b, "new")
			}
			if got := names(t, dir); len(got) != 1 || got[0] != "record.json" {
				t.Errorf("%s holds %v, want only record.json", dir, got)
			}
		})
	}
}

// TestWriteFailedFill is the half of the guarantee a reader relies on: a
// write that does not finish leaves the destination as it was.
func TestWriteFailedFill(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		previous *string
	}{
		{name: "no file before"},
		{name: "a file before", previous: new("old")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := t.TempDir()
			path := filepath.Join(dir, "record.json")
			if tt.previous != nil {
				if err := os.WriteFile(path, []byte(*tt.previous), 0o600); err != nil {
					t.Fatalf("WriteFile: %v", err)
				}
			}

			failed := errors.New("fill failed")
			err := atomicfile.Write(path, 0o600, func(f *os.File) error {
				if _, err := f.WriteString("partial"); err != nil {
					return err
				}
				return failed
			})
			if !errors.Is(err, failed) {
				t.Fatalf("Write error = %v, want the fill's error", err)
			}

			b, err := os.ReadFile(path)
			switch {
			case tt.previous == nil && !errors.Is(err, os.ErrNotExist):
				t.Errorf("%s exists (read error %v), want it still absent", path, err)
			case tt.previous != nil && string(b) != *tt.previous:
				t.Errorf("%s holds %q, want the previous %q", path, b, *tt.previous)
			}

			want := 0
			if tt.previous != nil {
				want = 1
			}
			if got := names(t, dir); len(got) != want {
				t.Errorf("%s holds %v, want no temporary file", dir, got)
			}
		})
	}
}

// TestWriteConcurrent is two writers in one process, which must not meet on
// the temporary file: the destination ends with one whole content.
func TestWriteConcurrent(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	path := filepath.Join(dir, "record.json")

	const writers = 20
	contents := make(map[string]bool, writers)
	var wg sync.WaitGroup
	errs := make(chan error, writers)
	for i := range writers {
		content := strings.Repeat(fmt.Sprint(i%10), 4096)
		contents[content] = true
		wg.Go(func() {
			errs <- atomicfile.Write(path, 0o600, writeString(content))
		})
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		if err != nil {
			t.Errorf("Write: %v", err)
		}
	}

	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if !contents[string(b)] {
		t.Errorf("%s holds %d bytes that no writer wrote whole", path, len(b))
	}
	if got := names(t, dir); len(got) != 1 {
		t.Errorf("%s holds %v, want only record.json", dir, got)
	}
}

// TestWriteMode pins the umask so that the mode is checked exactly: a helper
// that ignored perm and created every file 0600 would pass a subset check.
//
// Not parallel, because the umask belongs to the process: parallel tests are
// held until the sequential ones are done, so none of them creates a file
// under this umask.
func TestWriteMode(t *testing.T) {
	old := unix.Umask(0o022)
	t.Cleanup(func() { unix.Umask(old) })

	for _, perm := range []os.FileMode{0o600, 0o644, 0o666} {
		t.Run(perm.String(), func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "record.json")
			if err := atomicfile.Write(path, perm, writeString("x")); err != nil {
				t.Fatalf("Write: %v", err)
			}

			fi, err := os.Stat(path)
			if err != nil {
				t.Fatalf("Stat: %v", err)
			}
			if want := perm &^ 0o022; fi.Mode().Perm() != want {
				t.Errorf("mode = %v, want %v", fi.Mode().Perm(), want)
			}
		})
	}
}
