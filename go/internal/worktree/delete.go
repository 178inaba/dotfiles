package worktree

import (
	"context"
	"fmt"

	"github.com/178inaba/dotfiles/go/internal/contract"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Failure is one deletion that did not happen, and why.
type Failure struct {
	Type   TargetKind `json:"type"`
	Target string     `json:"target"`
	Error  string     `json:"error"`
}

// Removed is what is gone.
type Removed struct {
	Worktrees []string `json:"worktrees"`
	Branches  []string `json:"branches"`
}

// Deletion is the outcome of one pass.
type Deletion struct {
	Removed  Removed   `json:"removed"`
	Failures []Failure `json:"failures"`
}

// DeleteInput is the document `ccx worktree delete` reads from standard input.
//
// What `ccx worktree collect` wrote, minus whatever the person or the model
// took out of it. The two commands are separate so that a person sees the list
// before anything is deleted, and this document is the boundary approval
// passes through.
type DeleteInput struct {
	// The approved lists, in the shape collect printed them. A document
	// without the field is a failure rather than an empty list quietly
	// deleting nothing; an empty object is an approved list of nothing.
	Candidates *Candidates `json:"candidates" contract:"required"`
}

// ParseCandidates reads the approved candidates.
func ParseCandidates(b []byte) (Candidates, error) {
	var wire DeleteInput
	if err := contract.Unmarshal(b, &wire, "stdin JSON"); err != nil {
		return Candidates{}, err
	}
	return *wire.Candidates, nil
}

// Delete removes the approved worktrees and branches.
//
// A failure of one is recorded and the rest go on, because the list is a batch
// a person approved: stopping at the first refusal would leave them to work out
// which half happened. Only a broken premise — no repository, no lsof, no
// candidates — fails the command itself.
//
// It exists as a command rather than as a loop the model writes each time for
// two reasons. The shell that runs those loops is zsh, where assigning to a
// variable named path destroys PATH, and a candidate field called path invites
// exactly that. And the choice between -d and -D belongs somewhere it can be
// tested.
func Delete(ctx context.Context, r runner.Runner, dir string, candidates Candidates) (Deletion, error) {
	if _, err := runner.Git(ctx, r, dir, "rev-parse", "--git-dir"); err != nil {
		return Deletion{}, fmt.Errorf("not a git repository")
	}
	// git will remove a worktree somebody is sitting in — it succeeds, and
	// every command that process runs afterwards fails. Checked here as well as
	// in Collect because approval takes time, and somebody may have entered it
	// since.
	table, err := loadCWDTable(ctx, r)
	if err != nil {
		return Deletion{}, err
	}

	d := &deleter{r: r, dir: dir}
	d.out.Removed = Removed{Worktrees: []string{}, Branches: []string{}}
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
		d.out.Removed.Worktrees = append(d.out.Removed.Worktrees, wt.Path)
		// Only once the worktree is gone: a branch checked out in one cannot be
		// deleted, and this ordering is what makes the pair work.
		d.deleteBranch(ctx, wt.Branch, wt.Verdict, wt.HeadOID)
	}
	for _, br := range candidates.Branches {
		d.deleteBranch(ctx, br.Branch, br.Verdict, br.HeadOID)
	}
	return d.out, nil
}

type deleter struct {
	r   runner.Runner
	dir string
	out Deletion
}

// deleteBranch removes a branch, with the flag its verdict has earned.
//
// -d only for merged_no_pr, where git's own merge check is what the verdict
// rests on and stays as a second opinion. -D for the pull request verdicts,
// whose head was checked against the pull request's, and only after checking
// it again: approval takes time, and a commit made in between would be deleted
// with nothing to restore it from. A closed pull request needs the exact head;
// a merged one needs the branch contained in the head that merged, since a
// squash or rebase merge leaves the branch out of the default branch for good.
func (d *deleter) deleteBranch(ctx context.Context, branch string, verdict Verdict, headOID string) {
	flag := "-d"
	switch verdict {
	case VerdictPRClosed:
		flag = "-D"
		current, _ := runner.Git(ctx, d.r, d.dir, "rev-parse", "refs/heads/"+branch)
		if headOID == "" || current != headOID {
			d.refuse(branch, fmt.Sprintf(
				"refusing -D: branch head no longer matches verified PR head (expected %s, got %s)",
				or(headOID, "<missing>"), or(current, "<unresolved>")))
			return
		}
	case VerdictPRMerged:
		flag = "-D"
		// The empty check is explicit so the refusal names the missing head;
		// IsAncestor would refuse it too, but without saying why.
		if headOID == "" || !IsAncestor(ctx, d.r, d.dir, "refs/heads/"+branch, headOID) {
			current, _ := runner.Git(ctx, d.r, d.dir, "rev-parse", "refs/heads/"+branch)
			d.refuse(branch, fmt.Sprintf(
				"refusing -D: branch head is no longer contained in the merged PR head (expected an ancestor of %s, got %s)",
				or(headOID, "<missing>"), or(current, "<unresolved>")))
			return
		}
	}
	if _, err := runner.Git(ctx, d.r, d.dir, "branch", flag, branch); err != nil {
		d.refuse(branch, runner.Message(err))
		return
	}
	d.out.Removed.Branches = append(d.out.Removed.Branches, branch)
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
