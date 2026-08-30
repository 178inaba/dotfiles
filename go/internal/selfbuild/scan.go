package selfbuild

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io/fs"
	"path/filepath"
	"slices"
	"time"
)

// isStale reports whether anything under root is newer than the binary.
//
// This runs on every single invocation, so it is the one that has to be cheap:
// it stats each file and stops at the first one that is newer, and on the
// common path — where nothing is — that is all it does. Building the identity
// of the tree costs an allocation per file, so it waits for sourceSum, which
// only the stale path calls.
//
// Every file counts, not just .go ones: go.mod, go.sum and the linter config
// all change what a build produces. The cost is that anything dropped into the
// tree looks like a change — .DS_Store included, so opening the directory in
// Finder can trigger one rebuild — and that the check is only as cheap as the
// tree is small. A vendor directory or a large testdata corpus under go/ would
// put this on the wrong side of its budget.
func isStale(root string, binary time.Time) (bool, error) {
	stale := false
	err := filepath.WalkDir(root, func(_ string, e fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if e.IsDir() {
			return nil
		}
		info, err := e.Info()
		if err != nil {
			return err
		}
		if info.ModTime().After(binary) {
			stale = true
			return fs.SkipAll
		}
		return nil
	})
	if err != nil {
		return false, err
	}
	return stale, nil
}

// source is the full description of the tree, built only when a build is about
// to run: the identity a failure is recorded against, and the timestamp the
// freshly installed binary is stamped with.
type source struct {
	// sum changes with any edit, including one that only reverts, so a broken
	// tree is not rebuilt on every tick but a fixed one is retried at once.
	sum string
	// newest is what the build actually saw, so an edit that lands while it
	// runs stays newer than the binary it produced.
	newest time.Time
}

// scanSource walks the whole tree. It is the cold path: nothing calls it unless
// a build is about to happen, where go install dominates anyway.
func scanSource(root string) (source, error) {
	var s source
	var entries []string
	err := filepath.WalkDir(root, func(p string, e fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if e.IsDir() {
			return nil
		}
		info, err := e.Info()
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, p)
		if err != nil {
			return err
		}
		mod := info.ModTime()
		if mod.After(s.newest) {
			s.newest = mod
		}
		entries = append(entries, fmt.Sprintf("%s\x00%d\x00%d", rel, info.Size(), mod.UnixNano()))
		return nil
	})
	if err != nil {
		return source{}, err
	}

	slices.Sort(entries)
	h := sha256.New()
	for _, e := range entries {
		fmt.Fprintln(h, e)
	}
	s.sum = hex.EncodeToString(h.Sum(nil))
	return s, nil
}
