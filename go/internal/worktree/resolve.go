package worktree

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// ResolveStatus is whether the caller may go on.
//
// Everything but ResolveOK is a stop with somebody's work at stake, and none of
// them is resolved here: throwing away an uncommitted change or a local commit
// is a decision for the person, not for the command that found it.
type ResolveStatus string

const (
	// ResolveOK is a worktree at the pull request's head, ready to be entered.
	ResolveOK ResolveStatus = "ok"
	// ResolveBehindDirty is a checkout with uncommitted changes in it, which
	// cannot be moved to the head.
	ResolveBehindDirty ResolveStatus = "behind_dirty"
	// ResolveDiverged is a checkout carrying commits the remote has never
	// seen.
	ResolveDiverged ResolveStatus = "diverged"
	// ResolveEvacuationDirty is the main repository sitting on the pull
	// request's branch with changes in it, so it cannot be moved off to make
	// room for the worktree.
	ResolveEvacuationDirty ResolveStatus = "evacuation_dirty"
)

// Action is what the caller should do next.
type Action string

const (
	// ActionEnterExisting names a worktree that is already there.
	ActionEnterExisting Action = "enter_existing"
	// ActionCreate says one has to be made first.
	ActionCreate Action = "create"
)

// Resolution is where a pull request's worktree is, or what has to happen for
// one to exist.
type Resolution struct {
	Status ResolveStatus `json:"status"`
	Action Action        `json:"action"`
	// PRNumber and HeadRef are the pull request as resolved, so that a caller
	// that left the number out learns which one it got.
	PRNumber int    `json:"pr_number"`
	HeadRef  string `json:"head_ref"`
	// WorktreeName is the head branch with its slashes flattened, since one
	// directory name has to stand for a branch that may be nested.
	WorktreeName string `json:"worktree_name"`
	// Path is null unless an existing worktree was found; there is nowhere to
	// point at until Checkout has made one.
	Path      *string  `json:"worktree_path"`
	Evacuated bool     `json:"evacuated"`
	Synced    bool     `json:"synced"`
	Warnings  []string `json:"warnings"`
}

// Resolve finds the worktree for a pull request, or prepares for one to be
// made.
//
// Two outcomes, and the caller picks its next move from the action rather than
// from the status: an existing worktree is entered, and its absence means
// Checkout comes next. What this does not do is switch the session — whether
// that is EnterWorktree or a cd depends on session state no command can see.
//
// The main worktree is never the answer even when it has the branch checked
// out. That case is what evacuation is for: it moves out of the way so the
// worktree can have the branch instead.
func Resolve(ctx context.Context, r runner.Runner, c *ghapi.Client, repo ghapi.Repo, dir string, number int) (Resolution, error) {
	pr, err := resolvePR(ctx, r, c, repo, dir, number)
	if err != nil {
		return Resolution{}, err
	}

	out := Resolution{
		PRNumber:     pr.Number,
		HeadRef:      pr.HeadRefName,
		WorktreeName: strings.ReplaceAll(pr.HeadRefName, "/", "-"),
	}

	entries, err := List(ctx, r, dir)
	if err != nil {
		return Resolution{}, err
	}
	if found := linkedWorktreeOn(entries, pr.HeadRefName); found != "" {
		if err := fetchHead(ctx, r, dir, pr.HeadRefName); err != nil {
			return Resolution{}, err
		}
		status, synced, err := syncWithOrigin(ctx, r, found, pr.HeadRefName)
		if err != nil {
			return Resolution{}, err
		}
		out.Status, out.Action, out.Path, out.Synced = status, ActionEnterExisting, &found, synced
		return out, nil
	}

	out.Action = ActionCreate
	if len(entries) == 0 {
		return Resolution{}, fmt.Errorf("not inside a git repository")
	}
	main := entries[0].Path
	if entries[0].Branch == pr.HeadRefName {
		dirty, err := isDirty(ctx, r, main)
		if err != nil {
			return Resolution{}, err
		}
		if dirty {
			out.Status = ResolveEvacuationDirty
			return out, nil
		}
		if err := evacuate(ctx, r, c, repo, dir, main); err != nil {
			return Resolution{}, err
		}
		out.Evacuated = true
	}

	if err := fetchHead(ctx, r, dir, pr.HeadRefName); err != nil {
		return Resolution{}, err
	}
	out.Status = ResolveOK
	return out, nil
}

// resolvePR settles which pull request is meant, from a number or from the
// branch checked out here.
//
// The two failures are reported apart because the remedies are: one is a
// number that names nothing, the other is a branch with no pull request, where
// naming a number explicitly is the way out.
func resolvePR(ctx context.Context, r runner.Runner, c *ghapi.Client, repo ghapi.Repo, dir string, number int) (ghapi.PullRequest, error) {
	if number == 0 {
		pr, err := c.PullRequestForCurrentBranch(ctx, r, dir, repo)
		if err != nil {
			return ghapi.PullRequest{}, fmt.Errorf(
				"could not infer the PR from the current branch (no PR, unauthenticated, or network error); pass <pr-number> explicitly")
		}
		return pr, nil
	}
	pr, err := c.PullRequest(ctx, repo, number)
	if err != nil || pr.HeadRefName == "" {
		return ghapi.PullRequest{}, fmt.Errorf(
			"failed to get the head branch of PR #%d (not found, unauthenticated, or network error)", number)
	}
	return pr, nil
}

// linkedWorktreeOn returns the linked worktree that has branch checked out.
//
// The main worktree is skipped deliberately: counting it would make evacuation
// unreachable, and a session sent into it would be working in the repository
// itself.
func linkedWorktreeOn(entries []Entry, branch string) string {
	for _, e := range entries {
		if !e.Main && e.Branch == branch {
			return e.Path
		}
	}
	return ""
}

// evacuate moves the main repository off the pull request's branch, so that the
// worktree can have it: git allows one checkout of a branch at a time.
func evacuate(ctx context.Context, r runner.Runner, c *ghapi.Client, repo ghapi.Repo, dir, main string) error {
	// origin/HEAD first, and the API only where a repository has none —
	// evacuation has to land somewhere real, so this one cannot assume main.
	branch := DefaultBranch(ctx, r, dir)
	if branch == "" {
		var err error
		if branch, err = c.DefaultBranch(ctx, repo); err != nil {
			branch = ""
		}
	}
	if branch == "" {
		return fmt.Errorf("failed to determine default branch for main repository evacuation")
	}
	if _, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"-C", main, "switch", "-q", branch}}); err != nil {
		return fmt.Errorf("failed to switch main repository to %s: %v", branch, err)
	}
	return nil
}

// fetchHead updates the remote-tracking ref the worktree will be built on.
func fetchHead(ctx context.Context, r runner.Runner, dir, headRef string) error {
	if _, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"-C", dir, "fetch", "-q", "origin", headRef}}); err != nil {
		return fmt.Errorf(
			"git fetch origin %s failed (network issue, or fork PR whose head branch is not on origin — out of scope)", headRef)
	}
	return nil
}

// CheckedOut is a worktree made for a pull request.
type CheckedOut struct {
	Status ResolveStatus `json:"status"`
	Path   string        `json:"worktree_path"`
	Synced bool          `json:"synced"`
	// CopiedFiles counts what .worktreeinclude brought in, the same way
	// EnterWorktree would have.
	CopiedFiles int      `json:"copied_files"`
	Warnings    []string `json:"warnings"`
}

// Checkout makes a worktree at the pull request's head branch.
//
// Detached first and switched afterwards, which looks roundabout and is not:
// Resolve's fetch creates refs/remotes/origin/<head>, and `git worktree add`
// does not do the remote-branch guessing `git switch` does — so adding the
// branch directly fails the first time round.
//
// A stopping status still leaves the worktree behind. It exists by then, and
// somebody may well want to work in it as it stands; deciding otherwise would
// mean deleting a checkout on their behalf.
func Checkout(ctx context.Context, r runner.Runner, root, name, headRef string) (CheckedOut, error) {
	if !hasRef(ctx, r, root, "refs/remotes/origin/"+headRef) {
		return CheckedOut{}, fmt.Errorf(
			"origin/%s not found locally; run the resolve subcommand first (it fetches the head branch)", headRef)
	}

	path := filepath.Join(root, worktreesUnder, name)
	if _, err := r.Run(ctx, runner.Command{
		Name: "git", Args: []string{"-C", root, "worktree", "add", "-q", "--detach", path},
	}); err != nil {
		return CheckedOut{}, fmt.Errorf("git worktree add failed for %s: %v", path, err)
	}
	if _, err := r.Run(ctx, runner.Command{
		Name: "git", Args: []string{"-C", path, "switch", "-q", headRef},
	}); err != nil {
		return CheckedOut{}, fmt.Errorf("git switch %s failed inside %s: %v", headRef, path, err)
	}

	status, synced, err := syncWithOrigin(ctx, r, path, headRef)
	if err != nil {
		return CheckedOut{}, err
	}
	copied, warnings, err := copyWorktreeInclude(ctx, r, root, path)
	if err != nil {
		return CheckedOut{}, err
	}
	return CheckedOut{Status: status, Path: path, Synced: synced, CopiedFiles: copied, Warnings: warnings}, nil
}

// syncWithOrigin classifies how a branch stands against its remote and
// fast-forwards where that is safe.
//
// Ahead comes first: a local commit the remote has never seen is the one thing
// here that cannot be reconstructed, so its presence stops everything, even
// when the branch is behind as well.
func syncWithOrigin(ctx context.Context, r runner.Runner, dir, branch string) (ResolveStatus, bool, error) {
	counts, err := runner.Git(ctx, r, dir, "rev-list", "--left-right", "--count", branch+"...origin/"+branch)
	if err != nil {
		return "", false, fmt.Errorf("failed to compare %s with origin/%s", branch, branch)
	}
	ahead, behind, ok := strings.Cut(counts, "\t")
	if !ok {
		return "", false, fmt.Errorf("failed to compare %s with origin/%s", branch, branch)
	}

	switch {
	case strings.TrimSpace(ahead) != "0":
		return ResolveDiverged, false, nil
	case strings.TrimSpace(behind) == "0":
		return ResolveOK, false, nil
	}

	freshness, err := fastForwardOrDirty(ctx, r, dir, "origin/"+branch)
	if err != nil {
		return "", false, err
	}
	if freshness == FreshnessBehindDirty {
		return ResolveBehindDirty, false, nil
	}
	return ResolveOK, true, nil
}
