package gitlock_test

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"sync"
	"testing"
	"time"

	"github.com/178inaba/dotfiles/go/internal/gitlock"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// overlapRunner hands the common-dir lookup to real git, and holds every other
// command open long enough for a second one to overlap it if nothing stops it.
type overlapRunner struct {
	mu     sync.Mutex
	active int
	most   int
	ran    [][]string
	err    error
}

func (o *overlapRunner) Run(ctx context.Context, c runner.Command) ([]byte, error) {
	if slices.Contains(c.Args, "--git-common-dir") {
		return runner.Exec{}.Run(ctx, c)
	}
	o.mu.Lock()
	o.active++
	o.most = max(o.most, o.active)
	o.ran = append(o.ran, c.Args)
	o.mu.Unlock()

	time.Sleep(50 * time.Millisecond)

	o.mu.Lock()
	o.active--
	o.mu.Unlock()
	return nil, o.err
}

// TestGitSerialisesAcrossWorktrees runs a fetch, a config write and a branch
// deletion at once, from a repository and two of its linked worktrees: all
// three share one common git directory, so they must queue on one lock there.
func TestGitSerialisesAcrossWorktrees(t *testing.T) {
	t.Parallel()

	repo := gittest.InitWithCommit(t, filepath.Join(t.TempDir(), "repo"))
	dirs := []string{repo}
	for _, name := range []string{"a", "b"} {
		wt := filepath.Join(t.TempDir(), name)
		gittest.Run(t, repo, "worktree", "add", "-q", "-b", name, wt)
		dirs = append(dirs, wt)
	}
	lockFile := filepath.Join(repo, ".git", "ccx.lock")
	if _, err := os.Stat(lockFile); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the lock file exists before any command: %v", err)
	}

	commands := [][]string{{"fetch", "origin", "main"}, {"config", "branch.a.remote", "origin"}, {"branch", "-D", "gone"}}
	r := &overlapRunner{}
	var wg sync.WaitGroup
	for i, dir := range dirs {
		wg.Go(func() {
			if _, err := gitlock.Git(t.Context(), r, dir, commands[i]...); err != nil {
				t.Errorf("Git in %s: %v", dir, err)
			}
		})
	}
	wg.Wait()

	if r.most != 1 {
		t.Errorf("%d commands ran at once, want 1", r.most)
	}
	for i, dir := range dirs {
		want := append([]string{"-C", dir}, commands[i]...)
		if !slices.ContainsFunc(r.ran, func(got []string) bool { return slices.Equal(got, want) }) {
			t.Errorf("nothing ran as %v; ran %v", want, r.ran)
		}
	}
	info, err := os.Stat(lockFile)
	if err != nil {
		t.Fatalf("the lock file was not created in the common git directory: %v", err)
	}

	// A later command locks the same file rather than a new one.
	if _, err := gitlock.Git(t.Context(), r, dirs[1], "fetch"); err != nil {
		t.Fatalf("Git: %v", err)
	}
	again, err := os.Stat(lockFile)
	if err != nil || !os.SameFile(info, again) {
		t.Errorf("the lock file was replaced between commands (err %v)", err)
	}
}

func TestGitReturnsTheCommandError(t *testing.T) {
	t.Parallel()

	repo := gittest.InitWithCommit(t, filepath.Join(t.TempDir(), "repo"))
	offline := errors.New("offline")
	if _, err := gitlock.Git(t.Context(), &overlapRunner{err: offline}, repo, "fetch"); !errors.Is(err, offline) {
		t.Errorf("Git = %v, want %v", err, offline)
	}
}

func TestGitOutsideARepository(t *testing.T) {
	t.Parallel()

	r := &overlapRunner{}
	if _, err := gitlock.Git(t.Context(), r, t.TempDir(), "fetch"); err == nil {
		t.Error("Git succeeded outside a repository")
	}
	if len(r.ran) != 0 {
		t.Errorf("ran outside a repository: %v", r.ran)
	}
}
