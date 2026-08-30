package selfbuild

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io/fs"
	"path/filepath"
	"sort"
	"time"
)

// scanned is the state of the source tree: what the staleness check compares
// against, and what identifies the tree for the build-failure record.
type scanned struct {
	newest time.Time
	// entries are "relative path\x00size\x00modification time", sorted, one per
	// file.
	entries []string
}

// scan walks the source tree. It is on the path of every invocation, so it does
// no more than stat each of a few dozen files.
//
// Every file counts, not just .go ones: go.mod, go.sum and the linter config
// all change what a build produces. The cost is that anything dropped into the
// tree looks like a change — .DS_Store included, so opening the directory in
// Finder can trigger one rebuild.
func scan(root string) (scanned, error) {
	var s scanned
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
		mod := info.ModTime()
		if mod.After(s.newest) {
			s.newest = mod
		}
		rel, err := filepath.Rel(root, p)
		if err != nil {
			return err
		}
		s.entries = append(s.entries, fmt.Sprintf("%s\x00%d\x00%d", rel, info.Size(), mod.UnixNano()))
		return nil
	})
	if err != nil {
		return scanned{}, err
	}
	return s, nil
}

// sum identifies this exact source state. A build failure is recorded against
// it so the same broken tree is not rebuilt on every tick, while any edit —
// including one that only reverts — produces a different sum and earns a retry.
func (s scanned) sum() string {
	sort.Strings(s.entries)
	h := sha256.New()
	for _, e := range s.entries {
		fmt.Fprintln(h, e)
	}
	return hex.EncodeToString(h.Sum(nil))
}
