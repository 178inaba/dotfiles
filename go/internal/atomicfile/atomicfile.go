// Package atomicfile replaces a file so that a reader sees either the previous
// content or the new one, and never a torn file.
package atomicfile

import (
	"errors"
	"io/fs"
	"math/rand/v2"
	"os"
	"path/filepath"
	"strconv"
)

// Write replaces path with the file fill writes, through a temporary file
// beside it that is renamed onto path once fill has returned.
//
// fill gets the temporary file open for reading and writing; a caller whose
// content comes from another process can hand it f.Name(), and that process
// has to write into the file rather than replace it, since f is what is synced
// and what fill can read back. When fill fails, path is left as it was and the
// temporary file is removed.
//
// The file is created with perm masked by the umask, as os.WriteFile does.
// It is synced before the rename: without that, a crash after the rename can
// leave path empty on a file system that orders the rename before the data.
func Write(path string, perm os.FileMode, fill func(f *os.File) error) error {
	return write(path, perm, true, fill)
}

// WriteNoSync is Write without the sync, for a file whose loss costs nothing:
// a reader still never sees it torn, but a crash can leave it empty. The sync
// is a full flush of the device on darwin, which a file rewritten on every
// status line redraw should not pay.
func WriteNoSync(path string, perm os.FileMode, fill func(f *os.File) error) error {
	return write(path, perm, false, fill)
}

func write(path string, perm os.FileMode, sync bool, fill func(f *os.File) error) error {
	f, err := create(path, perm)
	if err != nil {
		return err
	}
	defer os.Remove(f.Name())

	err = fill(f)
	if err == nil && sync {
		err = f.Sync()
	}
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	if err != nil {
		return err
	}
	return os.Rename(f.Name(), path)
}

// create opens a new hidden file beside path under a name no other writer
// holds.
//
// Not os.CreateTemp, which creates every file 0600: opening with perm is what
// lets the kernel apply the umask, which the process cannot read without
// setting it for every other goroutine.
func create(path string, perm os.FileMode) (*os.File, error) {
	prefix := filepath.Join(filepath.Dir(path), "."+filepath.Base(path)+".")
	for range 10000 {
		f, err := os.OpenFile(prefix+strconv.FormatUint(rand.Uint64(), 10), os.O_RDWR|os.O_CREATE|os.O_EXCL, perm)
		if errors.Is(err, fs.ErrExist) {
			continue
		}
		return f, err
	}
	return nil, &fs.PathError{Op: "createtemp", Path: prefix + "*", Err: fs.ErrExist}
}
