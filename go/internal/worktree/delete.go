package worktree

import (
	"context"
	"fmt"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Failure is one deletion that did not happen, and why.
type Failure struct {
	Type   TargetKind `json:"type"`
	Target string     `json:"target"`
	Error  string     `json:"error"`
}

// Removed is what is gone, each with the verdict and detail it went on, so
// that a report of the run needs nothing but this output.
type Removed struct {
	Worktrees []RemovedWorktree `json:"worktrees"`
	// Only the candidates that had no worktree of their own: a worktree's
	// branch is reported on the worktree.
	Branches []RemovedBranch `json:"branches"`
}

// RemovedWorktree is a worktree that was removed.
type RemovedWorktree struct {
	Path   string `json:"path"`
	Branch string `json:"branch"`
	// False when the worktree went and its branch did not; why is under
	// failures.
	BranchRemoved bool    `json:"branch_removed"`
	Verdict       Verdict `json:"verdict"`
	Detail        string  `json:"detail"`
}

// RemovedBranch is a branch candidate that was deleted.
type RemovedBranch struct {
	Branch  string  `json:"branch"`
	Verdict Verdict `json:"verdict"`
	Detail  string  `json:"detail"`
}

// Deletion is the outcome of one pass.
type Deletion struct {
	Removed  Removed   `json:"removed"`
	Failures []Failure `json:"failures"`
}

// Delete removes the given worktrees and branches.
//
// A failure of one is recorded and the rest go on, because the list is one
// batch: stopping at the first refusal would leave the caller to work out which
// half happened. Only a broken premise — no repository, no lsof — fails the
// call itself.
func Delete(ctx context.Context, r runner.Runner, dir string, candidates Candidates) (Deletion, error) {
	if _, err := runner.Git(ctx, r, dir, "rev-parse", "--git-dir"); err != nil {
		return Deletion{}, fmt.Errorf("not a git repository")
	}
	// git will remove a worktree somebody is sitting in — it succeeds, and
	// every command that process runs afterwards fails. Checked here as well as
	// in Collect because judging asks GitHub about every branch, which takes
	// long enough for somebody to have entered it since.
	table, err := loadCWDTable(ctx, r)
	if err != nil {
		return Deletion{}, err
	}

	d := &deleter{r: r, dir: dir}
	d.out.Removed = Removed{Worktrees: []RemovedWorktree{}, Branches: []RemovedBranch{}}
	d.out.Failures = []Failure{}

	for _, wt := range candidates.Worktrees {
		if holders := table.holders(wt.Path); holders != "" {
			d.out.Failures = append(d.out.Failures, Failure{
				Type: KindWorktree, Target: wt.Path, Error: "refusing to remove: in use by " + holders,
			})
			continue
		}
		if _, err := runner.Git(ctx, d.r, d.dir, "worktree", "remove", wt.Path); err != nil {
			d.out.Failures = append(d.out.Failures, Failure{
				Type: KindWorktree, Target: wt.Path, Error: runner.Message(err),
			})
			continue
		}
		// Only once the worktree is gone: a branch checked out in one cannot be
		// deleted, and this ordering is what makes the pair work.
		branchRemoved := d.deleteBranch(ctx, wt.Branch, wt.Verdict, wt.HeadOID)
		d.out.Removed.Worktrees = append(d.out.Removed.Worktrees, RemovedWorktree{
			Path: wt.Path, Branch: wt.Branch, BranchRemoved: branchRemoved, Verdict: wt.Verdict, Detail: wt.Detail,
		})
	}
	for _, br := range candidates.Branches {
		if d.deleteBranch(ctx, br.Branch, br.Verdict, br.HeadOID) {
			d.out.Removed.Branches = append(d.out.Removed.Branches, RemovedBranch{
				Branch: br.Branch, Verdict: br.Verdict, Detail: br.Detail,
			})
		}
	}
	return d.out, nil
}

type deleter struct {
	r   runner.Runner
	dir string
	out Deletion
}

// deleteBranch removes a branch, with the flag its verdict has earned, and
// reports whether it went; a refusal is recorded under failures.
//
// -d only for merged_no_pr, where git's own merge check is what the verdict
// rests on and stays as a second opinion. -D for the pull request verdicts,
// whose head was checked against the pull request's, and only after checking
// it again: judging asks GitHub about every branch, and a commit made in the
// meantime would be deleted with nothing to restore it from. A closed pull
// request needs the exact head; a merged one needs the branch contained in the
// head that merged, since a squash or rebase merge leaves the branch out of the
// default branch for good.
func (d *deleter) deleteBranch(ctx context.Context, branch string, verdict Verdict, headOID string) bool {
	flag := "-d"
	if verdict.verifiedByPRHead() {
		flag = "-D"
		if !prHeadHolds(ctx, d.r, d.dir, verdict, branch, headOID) {
			current, _ := runner.Git(ctx, d.r, d.dir, "rev-parse", "refs/heads/"+branch)
			rule, want := "no longer matches the verified PR head", or(headOID, "<missing>")
			if verdict == VerdictPRMerged {
				rule, want = "is no longer contained in the merged PR head", "an ancestor of "+want
			}
			d.refuse(branch, fmt.Sprintf("refusing -D: branch head %s (expected %s, got %s)",
				rule, want, or(current, "<unresolved>")))
			return false
		}
	}
	if _, err := runner.Git(ctx, d.r, d.dir, "branch", flag, branch); err != nil {
		d.refuse(branch, runner.Message(err))
		return false
	}
	return true
}

func (d *deleter) refuse(branch, reason string) {
	d.out.Failures = append(d.out.Failures, Failure{Type: KindBranch, Target: branch, Error: reason})
}

func or(value, fallback string) string {
	if value == "" {
		return fallback
	}
	return value
}
