package worktree

import (
	"context"
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// The two commands that bring a checkout up to a pull request's head share what
// follows: what counts as dirty, and how far a synchronisation may go on its
// own. One definition, because a checkout that one command called safe to
// fast-forward and the other called dirty would be a difference nobody could
// explain.

// isDirty reports whether dir has changes that a fast-forward would disturb.
//
// Untracked files are not among them. They are not git's to move, and the
// scratch files a review leaves behind would otherwise stop every
// synchronisation. A fast-forward that would overwrite one still fails, which
// is where that shows up instead.
func isDirty(ctx context.Context, r runner.Runner, dir string) (bool, error) {
	out, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"-C", dir, "status", "--porcelain"}})
	if err != nil {
		return false, fmt.Errorf("read the status of %s: %w", dir, err)
	}
	for line := range strings.SplitSeq(strings.TrimRight(string(out), "\n"), "\n") {
		if line != "" && !strings.HasPrefix(line, "??") {
			return true, nil
		}
	}
	return false, nil
}

// fastForwardOrDirty brings dir up to target when it can be done without
// touching anybody's work, and answers with which of the two happened.
//
// Fast-forward only. A merge or a rebase would be this command deciding what to
// do with commits it did not make, and the caller stops and asks instead.
func fastForwardOrDirty(ctx context.Context, r runner.Runner, dir, target string) (Freshness, error) {
	dirty, err := isDirty(ctx, r, dir)
	if err != nil {
		return "", err
	}
	if dirty {
		return FreshnessBehindDirty, nil
	}
	if _, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", dir, "merge", "--ff-only", "-q", target},
	}); err != nil {
		return "", fmt.Errorf("fast-forward merge to %s failed (untracked file collision?)", target)
	}
	return FreshnessSynced, nil
}

// isAncestor reports whether one commit is reachable from another, which is how
// behind and ahead are told apart.
func isAncestor(ctx context.Context, r runner.Runner, dir, ancestor, descendant string) bool {
	_, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", dir, "merge-base", "--is-ancestor", ancestor, descendant},
	})
	return err == nil
}
