package gitfetch_test

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"sync"
	"testing"
	"time"

	"github.com/178inaba/dotfiles/go/internal/gitfetch"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// overlapRunner hands every git command but fetch to real git, and holds each
// fetch open long enough for a second one to overlap it if nothing stops it.
type overlapRunner struct {
	mu      sync.Mutex
	active  int
	most    int
	fetches [][]string
	err     error
}

func (o *overlapRunner) Run(ctx context.Context, c runner.Command) ([]byte, error) {
	if !slices.Contains(c.Args, "fetch") {
		return runner.Exec{}.Run(ctx, c)
	}
	o.mu.Lock()
	o.active++
	o.most = max(o.most, o.active)
	o.fetches = append(o.fetches, c.Args)
	o.mu.Unlock()

	time.Sleep(50 * time.Millisecond)

	o.mu.Lock()
	o.active--
	o.mu.Unlock()
	return nil, o.err
}

// TestFetchSerialisesAcrossWorktrees runs fetches from a repository and from
// two of its linked worktrees at once: all three share one common git
// directory, so they must queue on one lock there.
func TestFetchSerialisesAcrossWorktrees(t *testing.T) {
	t.Parallel()

	repo := gittest.InitWithCommit(t, filepath.Join(t.TempDir(), "repo"))
	dirs := []string{repo}
	for _, name := range []string{"a", "b"} {
		wt := filepath.Join(t.TempDir(), name)
		gittest.Run(t, repo, "worktree", "add", "-q", "-b", name, wt)
		dirs = append(dirs, wt)
	}
	lockFile := filepath.Join(repo, ".git", "ccx-fetch.lock")
	if _, err := os.Stat(lockFile); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the lock file exists before any fetch: %v", err)
	}

	r := &overlapRunner{}
	var wg sync.WaitGroup
	for _, dir := range dirs {
		wg.Go(func() {
			if _, err := gitfetch.Fetch(t.Context(), r, dir, "origin", "main"); err != nil {
				t.Errorf("Fetch in %s: %v", dir, err)
			}
		})
	}
	wg.Wait()

	if r.most != 1 {
		t.Errorf("%d fetches ran at once, want 1", r.most)
	}
	if len(r.fetches) != len(dirs) {
		t.Errorf("%d fetches ran, want %d", len(r.fetches), len(dirs))
	}
	for _, dir := range dirs {
		want := []string{"-C", dir, "fetch", "origin", "main"}
		if !slices.ContainsFunc(r.fetches, func(got []string) bool { return slices.Equal(got, want) }) {
			t.Errorf("no fetch ran as %v; ran %v", want, r.fetches)
		}
	}
	info, err := os.Stat(lockFile)
	if err != nil {
		t.Fatalf("the lock file was not created in the common git directory: %v", err)
	}

	// A later fetch locks the same file rather than a new one.
	if _, err := gitfetch.Fetch(t.Context(), r, dirs[1]); err != nil {
		t.Fatalf("Fetch: %v", err)
	}
	again, err := os.Stat(lockFile)
	if err != nil || !os.SameFile(info, again) {
		t.Errorf("the lock file was replaced between fetches (err %v)", err)
	}
}

func TestFetchReturnsTheFetchError(t *testing.T) {
	t.Parallel()

	repo := gittest.InitWithCommit(t, filepath.Join(t.TempDir(), "repo"))
	offline := errors.New("offline")
	if _, err := gitfetch.Fetch(t.Context(), &overlapRunner{err: offline}, repo); !errors.Is(err, offline) {
		t.Errorf("Fetch = %v, want %v", err, offline)
	}
}

func TestFetchOutsideARepository(t *testing.T) {
	t.Parallel()

	r := &overlapRunner{}
	if _, err := gitfetch.Fetch(t.Context(), r, t.TempDir()); err == nil {
		t.Error("Fetch succeeded outside a repository")
	}
	if len(r.fetches) != 0 {
		t.Errorf("fetched outside a repository: %v", r.fetches)
	}
}
