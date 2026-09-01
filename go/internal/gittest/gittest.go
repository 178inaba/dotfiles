// Package gittest builds throwaway git repositories for tests.
//
// The hooks have their own copy of this in internal/hooks/hooktest, which grew
// with the fixtures the hooks need; this one serves the commands ported from
// the shell scripts, whose fixtures are worktrees and remotes rather than
// sessions and transcripts.
package gittest

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// Run runs one git command in dir and fails the test if it does not succeed.
func Run(t *testing.T, dir string, args ...string) string {
	t.Helper()

	cmd := exec.Command("git", args...)
	cmd.Dir = dir
	// The developer's own configuration must not reach a fixture: a global
	// init.defaultBranch or a commit template changes what git does. Set on the
	// command rather than with t.Setenv, so that these tests stay parallel.
	cmd.Env = append(os.Environ(), "GIT_CONFIG_GLOBAL="+os.DevNull, "GIT_CONFIG_SYSTEM="+os.DevNull)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v in %s: %v\n%s", args, dir, err, out)
	}
	return string(out)
}

// Write creates a file and the directories above it.
func Write(t *testing.T, name, content string) {
	t.Helper()

	if err := os.MkdirAll(filepath.Dir(name), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.WriteFile(name, []byte(content), 0o644); err != nil {
		t.Fatalf("WriteFile(%q): %v", name, err)
	}
}

// SkipWithoutGit skips a test on a machine that has no git.
func SkipWithoutGit(t *testing.T) {
	t.Helper()

	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}
}
