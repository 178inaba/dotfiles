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
	// A partial record reads as a mismatched sum and costs one wasted rebuild,
	// which is why this file is not worth writing atomically: it is only ever
	// written under the build lock, and the failure mode is benign.
	body := []byte(sum + "\n" + strings.ReplaceAll(firstError, "\n", " ") + "\n")
	// Best effort: without a record the next invocation retries the build,
	// which is the safe direction.
	_ = os.WriteFile(failurePath(d), body, 0o644)
}

func removeFailure(d Deps) {
	os.Remove(failurePath(d))
}
