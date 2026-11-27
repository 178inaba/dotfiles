package worktree

import (
	"context"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// CleanupReport is what one run of `ccx worktree cleanup` did, whole enough
// that a report of the run needs nothing else.
type CleanupReport struct {
	// GitHub could not be reached, so the judgement fell back to
	// what git alone knows and no pull request was consulted.
	Degraded      bool    `json:"degraded"`
	DefaultBranch string  `json:"default_branch"`
	Removed       Removed `json:"removed"`
	// Judged finished and left alone, each with why. is_current is the one a
	// caller can act on: leave it and run the command again.
	Skipped  []Skipped `json:"skipped"`
	Failures []Failure `json:"failures"`
	// Worktrees on no branch, reported rather than judged: with no branch there
	// is nothing to compare against a pull request.
	Detached []string `json:"detached"`
	Warnings []string `json:"warnings"`
}

// Cleanup removes the worktrees and branches whose work is finished.
//
// What the caller stands in is reported as is_current rather than removed:
// leaving it is a step only the caller can take — a session leaves a worktree
// through the harness, not through git — so the command runs again once it has.
//
// It exists as a command rather than as a loop the model writes each time for
// two reasons. The shell that runs those loops is zsh, where assigning to a
// variable named path destroys PATH, and a candidate field called path invites
// exactly that. And the choice between -d and -D belongs somewhere it can be
// tested.
func Cleanup(ctx context.Context, r runner.Runner, c *ghapi.Client, dir string) (CleanupReport, error) {
	col, err := Collect(ctx, r, c, dir)
	if err != nil {
		return CleanupReport{}, err
	}
	out := CleanupReport{
		Degraded:      col.Degraded,
		DefaultBranch: col.DefaultBranch,
		Removed:       Removed{Worktrees: []RemovedWorktree{}, Branches: []RemovedBranch{}},
		Skipped:       col.Skipped,
		Failures:      []Failure{},
		Detached:      col.Detached,
		Warnings:      col.Warnings,
	}

	var todo Candidates
	for _, w := range col.Candidates.Worktrees {
		if w.IsCurrent {
			out.Skipped = append(out.Skipped, Skipped{
				Type: KindWorktree, Target: w.Path, Branch: w.Branch, Reason: SkipCurrent, Detail: currentDetail(w.Detail),
			})
			continue
		}
		todo.Worktrees = append(todo.Worktrees, w)
	}
	for _, b := range col.Candidates.Branches {
		if b.IsCurrent {
			out.Skipped = append(out.Skipped, Skipped{
				Type: KindBranch, Target: b.Branch, Reason: SkipCurrent, Detail: currentDetail(b.Detail),
			})
			continue
		}
		todo.Branches = append(todo.Branches, b)
	}
	// Delete reads the process table before anything else, which is wasted
	// when there is nothing to remove.
	if len(todo.Worktrees)+len(todo.Branches) == 0 {
		return out, nil
	}

	deleted, err := Delete(ctx, r, dir, todo)
	if err != nil {
		return CleanupReport{}, err
	}
	out.Removed, out.Failures = deleted.Removed, deleted.Failures
	return out, nil
}

// currentDetail keeps the judgement behind an is_current skip, which is the
// only line a caller who cannot leave gets to read about it.
func currentDetail(judged string) string {
	return skipDetails[SkipCurrent] + ": " + judged
}
