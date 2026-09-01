package gitstate_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/statusline/gitstate"
)

// These run real git in throwaway repositories, which the table tests over
// captured output cannot replace: they are what proves the parser still agrees
// with whatever git is installed. The shell suite this replaces worked the same
// way, and its own comments record a bug that only real git surfaced.
func TestSegmentAgainstRealGit(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}

	// The user's own git configuration must not reach these fixtures: a global
	// status.showUntrackedFiles would change the output the parser is being
	// validated against.
	t.Setenv("GIT_CONFIG_GLOBAL", os.DevNull)
	t.Setenv("GIT_CONFIG_SYSTEM", os.DevNull)

	root := t.TempDir()
	origin := filepath.Join(root, "origin.git")
	run(t, root, "init", "-q", "--bare", "-b", "main", origin)

	// solo has a remote but was never cloned, so it has no origin/HEAD.
	solo := filepath.Join(root, "solo")
	run(t, root, "init", "-q", "-b", "main", solo)
	writeFile(t, filepath.Join(solo, "tracked.txt"), "a\n")
	run(t, solo, "add", "tracked.txt")
	commit(t, solo, "first")

	t.Run("a branch that was never pushed", func(t *testing.T) {
		if got, want := segment(t, solo), "(main ↑∅)"; got != want {
			t.Errorf("segment = %q, want %q", got, want)
		}
	})

	run(t, solo, "remote", "add", "origin", origin)
	run(t, solo, "push", "-q", "-u", "origin", "main")
	commit(t, solo, "second", "--allow-empty")
	writeFile(t, filepath.Join(solo, "tracked.txt"), "a\nb\n")
	writeFile(t, filepath.Join(solo, "staged.txt"), "s\n")
	run(t, solo, "add", "staged.txt")
	writeFile(t, filepath.Join(solo, "untracked.txt"), "u\n")

	t.Run("staged, modified and ahead, with the untracked file ignored", func(t *testing.T) {
		if got, want := segment(t, solo), "(main +1 ~1 ↑1)"; got != want {
			t.Errorf("segment = %q, want %q", got, want)
		}
	})

	clone := filepath.Join(root, "clone")
	run(t, root, "clone", "-q", origin, clone)

	t.Run("in sync with its upstream", func(t *testing.T) {
		if got, want := segment(t, clone), "(main)"; got != want {
			t.Errorf("segment = %q, want %q", got, want)
		}
	})

	commit(t, clone, "third", "--allow-empty")
	run(t, clone, "push", "-q", "origin", "main")
	run(t, clone, "reset", "-q", "--hard", "HEAD~1")

	t.Run("behind its upstream", func(t *testing.T) {
		if got, want := segment(t, clone), "(main ↓1)"; got != want {
			t.Errorf("segment = %q, want %q", got, want)
		}
	})

	run(t, clone, "switch", "-q", "--detach", "HEAD")

	t.Run("detached", func(t *testing.T) {
		if got, want := segment(t, clone), "()"; got != want {
			t.Errorf("segment = %q, want %q", got, want)
		}
	})

	conflicted := filepath.Join(root, "conflicted")
	run(t, root, "init", "-q", "-b", "main", conflicted)
	writeFile(t, filepath.Join(conflicted, "cf"), "base\n")
	run(t, conflicted, "add", "cf")
	commit(t, conflicted, "first")
	run(t, conflicted, "switch", "-qc", "side")
	writeFile(t, filepath.Join(conflicted, "cf"), "side\n")
	commit(t, conflicted, "side", "-a")
	run(t, conflicted, "switch", "-q", "main")
	writeFile(t, filepath.Join(conflicted, "cf"), "main\n")
	commit(t, conflicted, "main", "-a")
	// The merge is expected to fail, and it has to carry an identity: git 2.43
	// checks the committer before touching the working tree, so without one it
	// stops before the conflict exists and the fixture is silently empty. Only
	// a machine with no git identity — a CI runner — would notice.
	merge(t, conflicted)

	t.Run("a conflict counts on both sides", func(t *testing.T) {
		if got, want := segment(t, conflicted), "(main +1 ~1 ↑∅)"; got != want {
			t.Errorf("segment = %q, want %q", got, want)
		}
	})

	t.Run("outside a repository there is no segment", func(t *testing.T) {
		plain := t.TempDir()
		if _, err := runner.Git(t.Context(), runner.Exec{}, plain, gitstate.StatusArgs()...); err == nil {
			t.Error("git status succeeded outside a repository")
		}
	})
}

// segment runs the invocation production runs, with the arguments taken from
// the package rather than retyped: the whole point of these tests is that the
// parser agrees with the output of that exact command.
func segment(t *testing.T, dir string) string {
	t.Helper()
	out, err := runner.Git(t.Context(), runner.Exec{}, dir, gitstate.StatusArgs()...)
	if err != nil {
		t.Fatalf("git status in %s: %v", dir, err)
	}
	return gitstate.Parse(out).Segment()
}

// identity is passed on every commit so the fixtures build on a machine with no
// git configuration of its own.
var identity = []string{
	"-c", "user.email=test@example.com",
	"-c", "user.name=test",
	"-c", "commit.gpgsign=false",
}

func commit(t *testing.T, dir, message string, extra ...string) {
	t.Helper()
	run(t, dir, slices.Concat(identity, []string{"commit", "-q", "-m", message}, extra)...)
}

func merge(t *testing.T, dir string) {
	t.Helper()
	args := slices.Concat(identity, []string{"merge", "-q", "side"})
	// A conflicting merge exits non-zero, which is the point of the fixture.
	_, _ = runner.Git(t.Context(), runner.Exec{}, dir, args...)
}

// run builds a fixture with one git command.
func run(t *testing.T, dir string, args ...string) {
	t.Helper()
	if _, err := runner.Git(t.Context(), runner.Exec{}, dir, args...); err != nil {
		t.Fatalf("git %s: %v: %s", strings.Join(args, " "), err, runner.Stderr(err))
	}
}

func writeFile(t *testing.T, name, body string) {
	t.Helper()
	if err := os.WriteFile(name, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
}
