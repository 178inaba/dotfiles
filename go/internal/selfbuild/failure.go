package selfbuild

import (
	"os"
	"path/filepath"
	"strings"
)

// failure is the record of a build that did not succeed.
type failure struct {
	sum        string
	firstError string
}

// cacheDir is ~/.cache/ccx, shared by every binary this module installs.
func cacheDir(d Deps) string {
	return filepath.Join(d.Home, ".cache", "ccx")
}

func failurePath(d Deps) string {
	return filepath.Join(cacheDir(d), "build-failed")
}

// readFailure returns the recorded failure, if there is a readable one.
func readFailure(d Deps) (failure, bool) {
	b, err := os.ReadFile(failurePath(d))
	if err != nil {
		return failure{}, false
	}
	sum, rest, _ := strings.Cut(strings.TrimRight(string(b), "\n"), "\n")
	if sum == "" {
		return failure{}, false
	}
	return failure{sum: sum, firstError: rest}, true
}

// writeFailure records that this source state does not build, so later
// invocations can skip the rebuild and still report the breakage.
func writeFailure(d Deps, sum, firstError string) {
	if err := os.MkdirAll(cacheDir(d), 0o755); err != nil {
		return
	}
	// A partial record would read as a mismatched sum and cost one wasted
	// rebuild, so this is written whole or not at all.
	body := []byte(sum + "\n" + strings.ReplaceAll(firstError, "\n", " ") + "\n")
	tmp := failurePath(d) + ".tmp"
	if err := os.WriteFile(tmp, body, 0o644); err != nil {
		return
	}
	if err := os.Rename(tmp, failurePath(d)); err != nil {
		os.Remove(tmp)
	}
}

// removeFailure clears the record after a successful build.
func removeFailure(d Deps) {
	os.Remove(failurePath(d))
}
