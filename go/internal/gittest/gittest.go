// Package gittest builds throwaway git repositories for tests. Every command
// runs through Run, which is where the isolation from the developer's own
// configuration lives.
package gittest

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// Init makes a repository at dir with an identity to commit as, and returns
// dir. Extra arguments go to `git init` — --bare for an origin to push to.
func Init(t *testing.T, dir string, args ...string) string {
	t.Helper()

	Run(t, t.TempDir(), append(append([]string{"init", "-q"}, args...), dir)...)
	identify(t, dir)
	return dir
}

// InitWithCommit makes a repository at dir with one commit in it, and returns
// dir. Wherever a fixture needs a head to hang worktrees off rather than
// remote-tracking refs, which is what an empty repository cannot give it.
func InitWithCommit(t *testing.T, dir string) string {
	t.Helper()

	Init(t, dir)
	Write(t, filepath.Join(dir, "file.txt"), "x\n")
	Run(t, dir, "add", ".")
	Run(t, dir, "commit", "-qm", "first")
	return dir
}

// Clone copies bare to dir, with an identity to commit as, and returns dir.
//
// A clone rather than an init wherever the fixture needs remote-tracking refs,
// which is most of them: a worktree, a freshness check and a cleanup all
// compare against origin.
func Clone(t *testing.T, bare, dir string) string {
	t.Helper()

	Run(t, t.TempDir(), "clone", "-q", bare, dir)
	identify(t, dir)
	return dir
}

// identify gives a repository somebody to commit as, since the developer's own
// configuration is shut out.
func identify(t *testing.T, dir string) {
	t.Helper()

	Run(t, dir, "config", "user.email", "test@example.com")
	Run(t, dir, "config", "user.name", "test")
}

// Rev resolves a revision to its commit.
func Rev(t *testing.T, dir, rev string) string {
	t.Helper()

	return strings.TrimSpace(Run(t, dir, "rev-parse", rev))
}

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
