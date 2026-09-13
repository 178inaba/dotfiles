package worktree

import (
	"context"
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// What counts as uncommitted work, and how far a synchronisation may go on its
// own, is defined here once. The two commands that bring a checkout up to a
// pull request's head share it, because a checkout that one called safe to
// fast-forward and the other called dirty would be a difference nobody could
// explain; the checks that must not move the checkout read the same status.

// statusLines is what `git status --porcelain` printed, one entry per line.
func statusLines(ctx context.Context, r runner.Runner, dir string) ([]string, error) {
	out, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"-C", dir, "status", "--porcelain"}})
	if err != nil {
		return nil, fmt.Errorf("read the status of %s: %w", dir, err)
	}
	trimmed := strings.TrimRight(string(out), "\n")
	if trimmed == "" {
		return nil, nil
	}
	return strings.Split(trimmed, "\n"), nil
}

// isDirty reports whether dir has changes that a fast-forward would disturb.
//
// Untracked files are not among them. They are not git's to move, and the
// scratch files a review leaves behind would otherwise stop every
// synchronisation. A fast-forward that would overwrite one still fails, which
// is where that shows up instead.
func isDirty(ctx context.Context, r runner.Runner, dir string) (bool, error) {
	lines, err := statusLines(ctx, r, dir)
	if err != nil {
		return false, err
	}
	for _, line := range lines {
		if !strings.HasPrefix(line, "??") {
			return true, nil
		}
	}
	return false, nil
}

// IsClean reports whether dir has nothing uncommitted at all, untracked files
// included.
//
// Two definitions of dirty on purpose. isDirty leaves untracked files out
// because they are not git's to move and the scratch files a review leaves
// behind must not stop a synchronisation. This one counts them, because a file
// that was never added is exactly a fix that exists only locally, which is
// what a pull request must not be declared ready over.
func IsClean(ctx context.Context, r runner.Runner, dir string) (bool, error) {
	lines, err := statusLines(ctx, r, dir)
	if err != nil {
		return false, err
	}
	return len(lines) == 0, nil
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

// IsAncestor reports whether one commit is reachable from another, which is how
// behind and ahead are told apart.
//
// A commit the repository does not hold answers false rather than raising:
// `git merge-base --is-ancestor` exits 128 on a name it cannot resolve, and to
// a caller a commit that is not there is one nothing is reachable from. Which
// of the two a caller passes is therefore load-bearing whenever one of them may
// be absent.
func IsAncestor(ctx context.Context, r runner.Runner, dir, ancestor, descendant string) bool {
	_, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", dir, "merge-base", "--is-ancestor", ancestor, descendant},
	})
	return err == nil
}
