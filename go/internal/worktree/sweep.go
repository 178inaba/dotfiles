package worktree

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// The names the harness gives an isolated agent's worktree and branch, which
// are the whole of what this command recognises. Both are required of a
// worktree candidate: an issue-handle worktree sits in the same directory
// under a branch of its own, and a worktree on an agent branch may sit
// somewhere the harness never puts one. Should the harness rename either, the
// sweep finds nothing, which is the failure mode wanted — it never removes
// what it did not recognise.
const (
	agentDirPrefix    = "agent-"
	agentBranchPrefix = "worktree-agent-"
)

// KeptReason is why a candidate was left in place.
//
// Its own set rather than the one /cleanup-merged skips by: every constant of
// a set reaches the help of any command that prints it, and neither of these
// can arise there.
type KeptReason string

const (
	// KeptLocked is a worktree git holds a lock on, which is an agent still
	// running — in this session or in another. The harness locks a worktree
	// while its agent runs and unlocks the one it leaves behind, so the lock
	// is the whole distinction and no process table has to be consulted for
	// it. Nothing here ever unlocks anything, so a lock left by a session that
	// crashed is reported every run until a person removes it.
	KeptLocked KeptReason = "locked"
	// KeptInUseByProcess is a leftover some process has as its working
	// directory — a shell somebody opened there, a command still running from
	// a finished agent. Removing it would kill every later command of that
	// process. This is the second guard, for what the lock does not cover.
	KeptInUseByProcess KeptReason = "in_use_by_process"
	// KeptBranchBeyondHead is a branch carrying a commit the working
	// directory's own head does not, which is the one thing standing between
	// an agent's own work and a deletion that skips git's merged check. It is
	// reported again by every later sweep, which is the reminder it is kept
	// as.
	KeptBranchBeyondHead KeptReason = "branch_beyond_head"
)

// What the reasons that need no particulars of their own look like in the list
// a person reads, in the language the rule that runs this speaks.
const (
	lockedWithoutReason = "実行中のエージェントがロック中（理由なし）"
	beyondHead          = "HEAD に含まれない commit を持つ"
)

// Swept is one thing the sweep removed.
type Swept struct {
	Type TargetKind `json:"type"`
	// Path is absent for a branch that had no worktree of its own.
	Path   string `json:"path,omitzero"`
	Branch string `json:"branch"`
	// Whether the branch was deleted as well. Always true for a branch that
	// had no worktree, which reaches this list only once it is gone; false for
	// a worktree whose branch was kept or could not be deleted, and the branch
	// then says which under kept or failures.
	BranchDeleted bool `json:"branch_deleted"`
}

// Kept is one candidate the sweep left in place, and why.
type Kept struct {
	Type TargetKind `json:"type"`
	// Target is the worktree's path, or the branch's name.
	Target string `json:"target"`
	// Branch is absent for a bare branch, whose name is already the target.
	Branch string     `json:"branch,omitzero"`
	Reason KeptReason `json:"reason"`
	// For a person reading the list, and written in the language
	// the rest of this rule speaks.
	Detail string `json:"detail"`
	// Head is the commit the branch is at, for the one reason that is about a
	// branch's commits and absent for the rest.
	Head string `json:"head,omitzero"`
}

// SweepReport is the outcome of one pass.
type SweepReport struct {
	Removed []Swept `json:"removed"`
	Kept    []Kept  `json:"kept"`
	// A git command that did not succeed, with what git said about it.
	Failures []Failure `json:"failures"`
}

// Sweep removes the worktrees the harness's isolated agents left behind.
//
// The unattended counterpart of Collect and Delete: nothing is asked of the
// caller between finding and removing, because this runs in the middle of
// another procedure. Its safety is in the checks instead — the harness's lock
// tells a running agent from a leftover, the working-directory table refuses
// one somebody is standing in, and a branch goes only where the working
// directory's own head already holds its commits.
//
// It runs against the repository of dir rather than resolving the main
// worktree, since that head is what a branch is measured against.
func Sweep(ctx context.Context, r runner.Runner, dir string) (SweepReport, error) {
	if _, err := runner.Git(ctx, r, dir, "rev-parse", "--git-dir"); err != nil {
		return SweepReport{}, fmt.Errorf("not a git repository")
	}
	entries, err := List(ctx, r, dir)
	if err != nil {
		return SweepReport{}, err
	}
	// Read once, before anything is removed: lsof costs time proportional to
	// the number of processes, and a failure to read it has to stop the run
	// rather than read as "nothing is in use".
	table, err := loadCWDTable(ctx, r)
	if err != nil {
		return SweepReport{}, err
	}

	s := &sweeper{r: r, dir: dir, table: table}
	s.out = SweepReport{Removed: []Swept{}, Kept: []Kept{}, Failures: []Failure{}}

	// The candidates are settled from this one listing, before any removal.
	// Asking again afterwards would find the branch of a worktree just removed
	// checked out nowhere, put it through the branch-only path, and record a
	// second deletion of it as a failure.
	checkedOut := make(map[string]bool)
	var worktrees []Entry
	for _, e := range entries {
		if e.Branch != "" {
			checkedOut[e.Branch] = true
		}
		if !e.Main && isAgentWorktree(e) {
			worktrees = append(worktrees, e)
		}
	}
	branches, _ := runner.Git(ctx, r, dir, "branch", "--list", agentBranchPrefix+"*", "--format=%(refname:lstrip=2)")

	for _, e := range worktrees {
		s.sweepWorktree(ctx, e)
	}
	for branch := range strings.SplitSeq(branches, "\n") {
		if branch == "" || checkedOut[branch] {
			continue
		}
		if s.deleteBranch(ctx, branch) {
			s.out.Removed = append(s.out.Removed, Swept{Type: KindBranch, Branch: branch, BranchDeleted: true})
		}
	}
	return s.out, nil
}

// isAgentWorktree says whether a worktree is named the way the harness names
// an isolated agent's, in both its path and its branch.
//
// The path is matched on its last three segments, which holds for an agent
// launched from the main worktree and for one launched from a linked worktree
// — measured for issue #251, both land directly under the main worktree's
// .claude/worktrees — and would hold for a nested placement too. The two
// directory names are spelled out here rather than taken from the constant
// that says where ccx puts its own worktrees: they name the same place today,
// but this is a match on what the harness does, and a rename on the ccx side
// should not silently change what a sweep recognises.
func isAgentWorktree(e Entry) bool {
	dir := filepath.Dir(e.Path)
	return strings.HasPrefix(e.Branch, agentBranchPrefix) &&
		strings.HasPrefix(filepath.Base(e.Path), agentDirPrefix) &&
		filepath.Base(dir) == "worktrees" && filepath.Base(filepath.Dir(dir)) == ".claude"
}

type sweeper struct {
	r     runner.Runner
	dir   string
	table cwdTable
	out   SweepReport
}

// sweepWorktree removes one leftover and the branch it was on, or says why it
// is being left alone.
func (s *sweeper) sweepWorktree(ctx context.Context, e Entry) {
	if e.Locked {
		detail := lockedWithoutReason
		if e.LockReason != "" {
			detail = "実行中のエージェントがロック中: " + e.LockReason
		}
		s.keep(Kept{Type: KindWorktree, Target: e.Path, Branch: e.Branch, Reason: KeptLocked, Detail: detail})
		return
	}
	if holders := s.table.holders(e.Path); holders != "" {
		s.keep(Kept{
			Type: KindWorktree, Target: e.Path, Branch: e.Branch,
			Reason: KeptInUseByProcess, Detail: "使用中のプロセスあり: " + holders,
		})
		return
	}
	// --force because these are dirty by nature, which is what Delete
	// deliberately will not do and one reason this is a command of its own.
	// Once rather than twice: git refuses a locked worktree to a single
	// --force, which is a second net under the check above should the harness
	// lock one between the listing and here.
	if _, err := runner.Git(ctx, s.r, s.dir, "worktree", "remove", "--force", e.Path); err != nil {
		s.out.Failures = append(s.out.Failures, Failure{
			Type: KindWorktree, Target: e.Path, Error: runner.Message(err),
		})
		return
	}
	// Only once the worktree is gone: a branch checked out in one cannot be
	// deleted, whatever it is deleted with.
	s.out.Removed = append(s.out.Removed, Swept{
		Type: KindWorktree, Path: e.Path, Branch: e.Branch,
		BranchDeleted: s.deleteBranch(ctx, e.Branch),
	})
}

// deleteBranch deletes a branch the working directory's head already holds,
// keeps one it does not, and says which of those happened.
//
// It records nothing about a worktree: what became of the branch is all it
// knows, and each caller adds that to the removal it is itself reporting.
//
// -D rather than -d because these branches were never merged anywhere; the
// ancestor test is what stands in for git's own check, and it is exact: a
// branch at or behind this head has no commit that deleting it would be the
// end of.
func (s *sweeper) deleteBranch(ctx context.Context, branch string) bool {
	// refs/heads/ spelled out: git resolves a tag before a branch of the same
	// name, and the tag's commit would be compared instead.
	if !IsAncestor(ctx, s.r, s.dir, "refs/heads/"+branch, "HEAD") {
		head, _ := runner.Git(ctx, s.r, s.dir, "rev-parse", "refs/heads/"+branch)
		s.keep(Kept{
			Type: KindBranch, Target: branch, Reason: KeptBranchBeyondHead,
			Detail: beyondHead, Head: head,
		})
		return false
	}
	if _, err := runner.Git(ctx, s.r, s.dir, "branch", "-D", branch); err != nil {
		s.out.Failures = append(s.out.Failures, Failure{
			Type: KindBranch, Target: branch, Error: runner.Message(err),
		})
		return false
	}
	return true
}

func (s *sweeper) keep(k Kept) {
	s.out.Kept = append(s.out.Kept, k)
}
