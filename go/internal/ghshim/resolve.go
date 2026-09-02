package ghshim

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
)

// ErrNoRealGH is returned when neither GH_BIN nor PATH leads to a gh that is
// not this program.
var ErrNoRealGH = errors.New("ghshim: no real gh on GH_BIN or PATH")

// Real finds the gh this program stands in front of. ghBin is GH_BIN, pathList
// is PATH and selfDir is this executable's directory, resolved; nothing is read
// from the environment here, so a test can lay out a PATH of its own.
//
// GH_BIN is the one test-stub variable the port keeps, because finding the real
// gh is what this program does, which makes it an input rather than a seam. One
// that points back here — an exported GH_BIN=gh — would recurse, so it is
// passed over.
//
// Directories are compared resolved: after stow ~/.local/shims is a symlink
// into the repository, so a shim started through the repository's own path
// would otherwise fail to recognise itself.
func Real(ghBin, pathList, selfDir string) (string, error) {
	if ghBin != "" {
		// command -v: a name holding a separator is a path, anything else is
		// looked up on PATH.
		if resolved := lookPath(ghBin, pathList); resolved != "" {
			// An unresolvable directory is not this one, which is the reading
			// the shell arrived at: GH_BIN was set deliberately, so it is taken
			// at its word unless it demonstrably points back here.
			if dir, err := resolvedDir(resolved); err != nil || dir != selfDir {
				return resolved, nil
			}
		}
	}

	for _, dir := range filepath.SplitList(pathList) {
		if dir == "" {
			continue
		}
		candidate := filepath.Join(dir, "gh")
		if !executable(candidate) {
			continue
		}
		// Unlike GH_BIN above, a candidate that cannot be placed is passed
		// over: PATH is walked over whatever it happens to hold.
		if resolved, err := resolvedDir(candidate); err != nil || resolved == selfDir {
			continue
		}
		return candidate, nil
	}
	return "", ErrNoRealGH
}

// lookPath resolves one name the way command -v does.
func lookPath(name, pathList string) string {
	if strings.ContainsRune(name, filepath.Separator) {
		if executable(name) {
			return name
		}
		return ""
	}
	for _, dir := range filepath.SplitList(pathList) {
		if dir == "" {
			continue
		}
		if candidate := filepath.Join(dir, name); executable(candidate) {
			return candidate
		}
	}
	return ""
}

// executable reports whether path is a regular file this process could run.
func executable(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.Mode().IsRegular() && info.Mode().Perm()&0o111 != 0
}

// resolvedDir is the directory of path, made absolute and with its symlinks
// resolved. The file itself is not followed, as the shell's cd -P on the
// dirname did not.
//
// Absolute first: a relative path resolves to a relative one and would never
// equal selfDir, so a GH_BIN of ./gh would fail to be recognised as this
// program and hand off to itself for ever.
func resolvedDir(path string) (string, error) {
	abs, err := filepath.Abs(path)
	if err != nil {
		return "", err
	}
	return filepath.EvalSymlinks(filepath.Dir(abs))
}
