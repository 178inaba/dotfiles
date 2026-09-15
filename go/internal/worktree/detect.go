package worktree

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// DetectStatus is what became of the worktree an issue already had.
type DetectStatus string

const (
	// DetectNone is an issue with no worktree of its own.
	DetectNone DetectStatus = "none"
	// DetectRemoved is a leftover that held nothing but what the base holds
	// and files git ignores, and is gone along with its branch.
	DetectRemoved DetectStatus = "removed"
	// DetectKept is a worktree that failed one of the checks and was left
	// exactly as it was; whether to throw it away is a question for the person.
	DetectKept DetectStatus = "kept"
)

// LeftoverReason is the check a kept worktree failed.
//
// Its own set rather than the one the sweep keeps by: every constant of a set
// reaches the help of any command that prints it, and most of these cannot
// arise in a sweep, whose own commit check cannot arise here.
type LeftoverReason string

const (
	// LeftoverLocked is a worktree git holds a lock on, which somebody took on
	// purpose.
	LeftoverLocked LeftoverReason = "locked"
	// LeftoverInUseByProcess is a worktree some process has as its working
	// directory. Removing it would kill every later command of that process.
	LeftoverInUseByProcess LeftoverReason = "in_use_by_process"
	// LeftoverDirty is a worktree with anything uncommitted, untracked files
	// included. Files git ignores are not looked at.
	LeftoverDirty LeftoverReason = "dirty"
	// LeftoverBeyondStartRef is a branch carrying a commit the ref a new
	// worktree would start from does not: origin/<base> where it exists, the
	// local base otherwise.
	LeftoverBeyondStartRef LeftoverReason = "beyond_start_ref"
	// LeftoverOnOrigin is a branch of the same name on origin, which is work
	// somebody pushed.
	LeftoverOnOrigin LeftoverReason = "on_origin"
	// LeftoverOriginUnreachable is an origin that could not be asked — no
	// remote, no network. An unanswered question is not taken as a no.
	LeftoverOriginUnreachable LeftoverReason = "origin_unreachable"
)

// Detection is the outcome of checking an issue for a worktree it already has.
type Detection struct {
	Status DetectStatus `json:"status"`
	// worktree_path and branch are null when there was no worktree, rather
	// than empty strings: the caller branches on status and reads these only
	// after.
	Path   *string `json:"worktree_path"`
	Branch *string `json:"branch"`
	// Reason is null unless the worktree was kept.
	Reason *LeftoverReason `json:"reason"`
}

// Detect finds the worktree of an issue among the repository's linked ones and
// removes it when it is a leftover, before a new one is created for the issue.
//
// Two namings match. The current one is <type>/<issue>-<slug>; the other is
// what EnterWorktree(name:) produced, whose worktrees are still on disk. Only
// the first match is looked at.
//
// A leftover is one where every check passes: no lock, no process standing in
// it, nothing uncommitted, no commit beyond the start ref, and no branch of
// that name on origin. What that leaves is a worktree whose tracked and
// untracked files are all on the base, so removing it loses nothing but the
// files git ignores. Every check is made before anything is removed.
//
// The main worktree is never the answer, even when it has the branch checked
// out: that is the repository itself.
func Detect(ctx context.Context, r runner.Runner, root string, issue int, base string) (Detection, error) {
	entries, err := List(ctx, r, root)
	if err != nil {
		return Detection{}, err
	}

	// The number is bounded on both sides so that 42 does not answer for 142.
	current := regexp.MustCompile(fmt.Sprintf(`^[a-z]+/%d-`, issue))
	legacy := regexp.MustCompile(fmt.Sprintf(`^worktree-[a-z]+-%d-`, issue))
	for _, e := range entries {
		if e.Main || e.Branch == "" {
			continue
		}
		if current.MatchString(e.Branch) || legacy.MatchString(e.Branch) {
			return removeLeftover(ctx, r, root, e, base)
		}
	}
	return Detection{Status: DetectNone}, nil
}

// removeLeftover checks one worktree and removes it if it passes, or says
// which check it failed.
func removeLeftover(ctx context.Context, r runner.Runner, root string, e Entry, base string) (Detection, error) {
	reason, err := leftoverReason(ctx, r, root, e, base)
	if err != nil {
		return Detection{}, err
	}
	if reason != "" {
		return Detection{Status: DetectKept, Path: &e.Path, Branch: &e.Branch, Reason: &reason}, nil
	}

	// Without --force: the checks above end with a question put over the
	// network, and anything written into the tree meanwhile makes git refuse
	// rather than go with it.
	if _, err := runner.Git(ctx, r, root, "worktree", "remove", e.Path); err != nil {
		return Detection{}, fmt.Errorf("git worktree remove failed for %s: %s", e.Path, runner.Message(err))
	}
	// Only once the worktree is gone: a branch checked out in one cannot be
	// deleted. -D because the branch was never merged anywhere; the commit
	// check stands in for git's own.
	if _, err := runner.Git(ctx, r, root, "branch", "-D", e.Branch); err != nil {
		return Detection{}, fmt.Errorf("git branch -D failed for %s: %s", e.Branch, runner.Message(err))
	}
	return Detection{Status: DetectRemoved, Path: &e.Path, Branch: &e.Branch}, nil
}

// leftoverReason is the first check a worktree fails, or empty when it passes
// them all. A check that could not be made at all is an error, except the one
// put to origin, which is a reason of its own.
func leftoverReason(ctx context.Context, r runner.Runner, root string, e Entry, base string) (LeftoverReason, error) {
	if e.Locked {
		return LeftoverLocked, nil
	}

	// Read only once a worktree was found, so that an issue with none does not
	// depend on lsof.
	table, err := loadCWDTable(ctx, r)
	if err != nil {
		return "", err
	}
	if table.holders(e.Path) != "" {
		return LeftoverInUseByProcess, nil
	}

	clean, err := IsClean(ctx, r, e.Path)
	if err != nil {
		return "", err
	}
	if !clean {
		return LeftoverDirty, nil
	}

	startRef, err := resolveStartRef(ctx, r, root, base)
	if err != nil {
		return "", err
	}
	if !IsAncestor(ctx, r, root, "refs/heads/"+e.Branch, startRef) {
		return LeftoverBeyondStartRef, nil
	}

	// Last, being the one check that may go over the network.
	ref := "refs/heads/" + e.Branch
	out, err := runner.Git(ctx, r, root, "ls-remote", "--heads", "origin", ref)
	if err != nil {
		return LeftoverOriginUnreachable, nil
	}
	for line := range strings.SplitSeq(out, "\n") {
		// <sha>\t<ref>; the pattern git was given matches on trailing path
		// components, so the name is compared whole.
		if _, name, _ := strings.Cut(line, "\t"); name == ref {
			return LeftoverOnOrigin, nil
		}
	}
	return "", nil
}
