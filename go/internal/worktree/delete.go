package worktree

import (
	"context"
	"encoding/json/v2"
	"errors"
	"fmt"
	"strings"

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

// ParseCandidates reads the approved candidates.
//
// What Collect wrote, minus whatever the person or the model took out of it.
// The two commands are separate so that a person sees the list before anything
// is deleted, and this is the boundary that approval passes through.
func ParseCandidates(b []byte) (Candidates, error) {
	var wire struct {
		// A pointer, so that a document without the field is a failure rather
		// than an empty list quietly deleting nothing.
		Candidates *Candidates `json:"candidates"`
	}
	if err := json.Unmarshal(b, &wire); err != nil {
		return Candidates{}, errors.New("invalid JSON on stdin")
	}
	if wire.Candidates == nil {
		return Candidates{}, errors.New("stdin JSON missing .candidates")
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
	if _, err := run(ctx, r, dir, "rev-parse", "--git-dir"); err != nil {
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

	d := &deleter{r: r, dir: dir, table: table}
	d.out.Removed = Removed{Worktrees: []string{}, Branches: []string{}}
	d.out.Failures = []Failure{}

	for _, wt := range candidates.Worktrees {
		if holders := table.holders(wt.Path); holders != "" {
			d.fail(KindWorktree, wt.Path, "refusing to remove: in use by "+holders)
			continue
		}
		if err := d.git(ctx, "worktree", "remove", wt.Path); err != nil {
			d.fail(KindWorktree, wt.Path, err.Error())
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
	r     runner.Runner
	dir   string
	table cwdTable
	out   Deletion
}

func (d *deleter) fail(kind TargetKind, target, message string) {
	d.out.Failures = append(d.out.Failures, Failure{Type: kind, Target: target, Error: message})
}

// git runs one git command and turns a failure into what git said about it,
// which is what reaches the caller as the reason.
func (d *deleter) git(ctx context.Context, args ...string) error {
	if _, err := d.r.Run(ctx, runner.Command{Name: "git", Args: append([]string{"-C", d.dir}, args...)}); err != nil {
		if message := strings.TrimSpace(string(runner.Stderr(err))); message != "" {
			return errors.New(message)
		}
		return err
	}
	return nil
}

// deleteBranch removes a branch, with the flag its verdict has earned.
//
// -d for everything but a closed pull request, so that git's own merge check
// stays as a second opinion. -D only where the head was matched against the
// pull request's, and only after matching it again: approval takes time, and a
// commit made in between would be deleted with nothing to restore it from.
func (d *deleter) deleteBranch(ctx context.Context, branch string, verdict Verdict, headOID string) {
	flag := "-d"
	if verdict == VerdictPRClosed {
		flag = "-D"
		current, _ := run(ctx, d.r, d.dir, "rev-parse", "refs/heads/"+branch)
		if headOID == "" || current != headOID {
			d.fail(KindBranch, branch, fmt.Sprintf(
				"refusing -D: branch head no longer matches verified PR head (expected %s, got %s)",
				or(headOID, "<missing>"), or(current, "<unresolved>")))
			return
		}
	}
	if err := d.git(ctx, "branch", flag, branch); err != nil {
		d.fail(KindBranch, branch, err.Error())
		return
	}
	d.out.Removed.Branches = append(d.out.Removed.Branches, branch)
}

func or(value, fallback string) string {
	if value == "" {
		return fallback
	}
	return value
}
