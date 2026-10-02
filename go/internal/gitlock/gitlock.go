// Package gitlock runs the git commands ccx writes a repository's shared state
// with one at a time per repository.
//
// git locks what it writes — each ref, packed-refs, .git/config — and gives up
// at once when another process holds the lock. Two subagents working in one
// repository, or in two worktrees of it, would otherwise fail on each other:
// two fetches updating the same remote-tracking ref, two checkouts writing
// their branches' tracking into .git/config, a branch deletion rewriting
// packed-refs under a fetch. Every such command ccx runs goes through here, so
// they queue instead.
package gitlock

import (
	"context"
	"fmt"
	"path/filepath"

	"github.com/178inaba/dotfiles/go/internal/filelock"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// lockName is the lock file in the common git directory. A name git itself
// does not use, inside .git so that it never shows up in git status.
const lockName = "ccx.lock"

// Git runs `git -C dir <args...>` as runner.Git does, holding the lock of
// dir's repository and waiting for whoever holds it first.
//
// The lock lives in the common git directory, which a linked worktree shares
// with its main worktree, so commands from any worktree of one repository
// exclude each other. It is not reentrant: a caller already inside Git must not
// call it again, or it waits on itself.
func Git(ctx context.Context, r runner.Runner, dir string, args ...string) (string, error) {
	common, err := runner.GitCommonDir(ctx, r, dir)
	if err != nil {
		return "", fmt.Errorf("failed to find the git directory of %s: %w", dir, err)
	}
	release, err := filelock.Lock(filepath.Join(common, lockName))
	if err != nil {
		return "", err
	}
	defer release()
	return runner.Git(ctx, r, dir, args...)
}
