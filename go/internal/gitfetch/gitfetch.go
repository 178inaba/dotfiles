// Package gitfetch runs git fetch one at a time per repository.
//
// A fetch writes remote-tracking refs, and git takes a lock on each ref it
// writes: two fetches that update the same ref at once — the default branch, a
// pull request's base, or every stale ref under --prune — leave one of them
// failing on that lock. Every ccx fetch goes through here, so that two
// subagents working in one repository, or in two worktrees of it, queue
// instead.
package gitfetch

import (
	"context"
	"fmt"
	"path/filepath"

	"github.com/178inaba/dotfiles/go/internal/filelock"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// lockName is the lock file in the common git directory. A name git itself
// does not use, inside .git so that it never shows up in git status.
const lockName = "ccx-fetch.lock"

// Fetch runs `git -C dir fetch <args...>` holding the fetch lock of dir's
// repository, waiting for whoever holds it first.
//
// The lock lives in the common git directory, which a linked worktree shares
// with its main worktree, so fetches from any worktree of one repository
// exclude each other. The error from the fetch itself is returned as it came,
// so a caller can still read what git said.
func Fetch(ctx context.Context, r runner.Runner, dir string, args ...string) ([]byte, error) {
	common, err := runner.Git(ctx, r, dir, "rev-parse", "--path-format=absolute", "--git-common-dir")
	if err != nil {
		return nil, fmt.Errorf("failed to find the git directory of %s: %w", dir, err)
	}
	release, err := filelock.Lock(filepath.Join(common, lockName))
	if err != nil {
		return nil, err
	}
	defer release()
	return r.Run(ctx, runner.Command{Name: "git", Args: append([]string{"-C", dir, "fetch"}, args...)})
}
