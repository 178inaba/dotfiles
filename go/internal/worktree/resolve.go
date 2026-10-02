package worktree

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/gitfetch"
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
	// ResolveDiverged is a checkout carrying commits the pull request's head
	// has never seen.
	ResolveDiverged ResolveStatus = "diverged"
	// ResolveEvacuationDirty is the main repository sitting on the pull
	// request's branch with changes in it, so it cannot be moved off to make
	// room for the worktree.
	ResolveEvacuationDirty ResolveStatus = "evacuation_dirty"
)

// Action is what the caller should do next.
type Action string

const (
	// ActionEnterExisting is a worktree that is already there.
	ActionEnterExisting Action = "enter_existing"
	// ActionCreate says one has to be made first.
	ActionCreate Action = "create"
)

// Resolution is where a pull request's worktree is, or what has to happen for
// one to exist.
type Resolution struct {
	Status ResolveStatus `json:"status"`
	Action Action        `json:"action"`
	// pr_number and head_ref are the pull request as resolved, so that a caller
	// that left the number out learns which one it got.
	PRNumber int    `json:"pr_number"`
	HeadRef  string `json:"head_ref"`
	// The branch the worktree is on: head_ref for a pull request whose
	// head is in this repository, <owner>/<head_ref> for one from a fork,
	// owner being the fork's, so that two forks' branches of the same name
	// do not share one, and pr-<number>/<head_ref> for one whose fork has
	// been deleted, which leaves no owner to name it after.
	LocalBranch string `json:"local_branch"`
	// local_branch with its slashes flattened, since one directory name
	// has to stand for a branch that may be nested.
	WorktreeName string `json:"worktree_name"`
	// Path is null unless an existing worktree was found; there is nowhere to
	// point at until Checkout has made one.
	Path      *string  `json:"worktree_path"`
	Evacuated bool     `json:"evacuated"`
	Synced    bool     `json:"synced"`
	Warnings  []string `json:"warnings"`
}

// LocalBranch is the branch a checkout of pr is on.
//
// The head branch's own name for a pull request whose head is in this
// repository, and <owner>/<head_ref> for one from a fork: two forks may both
// push a patch-1, and the worktree search and the branch check identify a
// checkout by nothing but its branch name. A fork that has been deleted leaves
// no owner to name it after, so its pull request's number stands in:
// pr-<n>/<head_ref>.
func LocalBranch(pr ghapi.PullRequest) string {
	switch {
	case !pr.IsCrossRepository:
		return pr.HeadRefName
	case pr.HeadRepository == nil:
		return fmt.Sprintf("pr-%d/%s", pr.Number, pr.HeadRefName)
	default:
		return pr.HeadRepository.Owner + "/" + pr.HeadRefName
	}
}

// worktreeName is the directory a worktree on branch is made in: the branch
// with its slashes flattened, since one directory name has to stand for a
// branch that may be nested.
func worktreeName(branch string) string {
	return strings.ReplaceAll(branch, "/", "-")
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
func Resolve(ctx context.Context, r runner.Runner, c *ghapi.Client, repo ghapi.Repo, dir string, number int, remote ghapi.RemoteOptions) (Resolution, error) {
	pr, err := resolvePR(ctx, r, c, repo, dir, number)
	if err != nil {
		return Resolution{}, err
	}
	local := LocalBranch(pr)

	out := Resolution{
		PRNumber:     pr.Number,
		HeadRef:      pr.HeadRefName,
		LocalBranch:  local,
		WorktreeName: worktreeName(local),
	}

	entries, err := List(ctx, r, dir)
	if err != nil {
		return Resolution{}, err
	}
	if found := linkedWorktreeOn(entries, local); found != "" {
		if err := FetchPullHead(ctx, r, dir, pr.Number, pr.HeadRefOid); err != nil {
			return Resolution{}, err
		}
		status, synced, err := syncWithHead(ctx, r, found, local, pr.HeadRefOid)
		if err != nil {
			return Resolution{}, err
		}
		warnings, err := setTracking(ctx, r, found, local, pr, remote)
		if err != nil {
			return Resolution{}, err
		}
		out.Status, out.Action, out.Path, out.Synced = status, ActionEnterExisting, &found, synced
		out.Warnings = append(out.Warnings, warnings...)
		return out, nil
	}

	out.Action = ActionCreate
	if len(entries) == 0 {
		return Resolution{}, fmt.Errorf("not inside a git repository")
	}
	main := entries[0].Path
	if entries[0].Branch == local {
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

// setTracking writes the branch config a pull request's local branch gets, in
// the form `gh pr checkout` writes it.
//
// For a head in this repository that is origin/<head_ref> as the upstream,
// fetched here since nothing else needs it, and left unset with a warning
// where the head branch is no longer on origin. For a fork it is the fork's url
// as both remote and pushRemote, with <head_ref> to merge; for a fork that has
// been deleted, origin with refs/pull/<n>/head to merge. Either way the local
// branch is named differently from what it merges, which git refuses to push
// under push.default=simple — where no upstream at all would let
// push.autoSetupRemote create the local branch on the base repository.
func setTracking(ctx context.Context, r runner.Runner, dir, branch string, pr ghapi.PullRequest, remote ghapi.RemoteOptions) ([]string, error) {
	if !pr.IsCrossRepository {
		if _, err := gitfetch.Fetch(ctx, r, dir, "-q", "origin", pr.HeadRefName); err != nil {
			return []string{fmt.Sprintf(
				"%s is not on origin any more, so %s was left without an upstream", pr.HeadRefName, branch)}, nil
		}
		if _, err := runner.Git(ctx, r, dir, "branch", "-q", "--set-upstream-to=origin/"+pr.HeadRefName, branch); err != nil {
			return nil, fmt.Errorf("failed to set the upstream of %s to origin/%s: %v", branch, pr.HeadRefName, err)
		}
		return nil, nil
	}

	url, merge := "origin", pullRef(pr.Number)
	if pr.HeadRepository != nil {
		var err error
		if url, err = ghapi.RepoURL(remote, *pr.HeadRepository); err != nil {
			return nil, err
		}
		merge = "refs/heads/" + pr.HeadRefName
	}
	for _, kv := range [][2]string{{"remote", url}, {"pushRemote", url}, {"merge", merge}} {
		key := "branch." + branch + "." + kv[0]
		if _, err := runner.Git(ctx, r, dir, "config", key, kv[1]); err != nil {
			return nil, fmt.Errorf("failed to set %s: %v", key, err)
		}
	}
	return nil, nil
}

// CheckedOut is a worktree made for a pull request.
type CheckedOut struct {
	Status ResolveStatus `json:"status"`
	Path   string        `json:"worktree_path"`
	Synced bool          `json:"synced"`
	// How many files .worktreeinclude brought in, the same way the harness
	// would have.
	CopiedFiles int      `json:"copied_files"`
	Warnings    []string `json:"warnings"`
}

// Checkout makes a worktree for a pull request, on its local branch at its
// head.
//
// The pull request is read again rather than taken from Resolve, so that the
// local branch is derived in one place and nothing derived is copied between
// the two commands. A local branch of that name that already exists — left by
// the evacuation of the main repository, or by an earlier review — is checked
// out as it stands and synchronised like an existing worktree: creating it
// again would fail, and forcing it would discard the author's unpushed commits.
//
// A stopping status still leaves the worktree behind. It exists by then, and
// somebody may well want to work in it as it stands; deciding otherwise would
// mean deleting a checkout on their behalf.
func Checkout(ctx context.Context, r runner.Runner, c *ghapi.Client, repo ghapi.Repo, root string, number int, remote ghapi.RemoteOptions) (CheckedOut, error) {
	pr, err := resolvePR(ctx, r, c, repo, root, number)
	if err != nil {
		return CheckedOut{}, err
	}
	local := LocalBranch(pr)
	if err := FetchPullHead(ctx, r, root, pr.Number, pr.HeadRefOid); err != nil {
		return CheckedOut{}, err
	}

	path := filepath.Join(root, worktreesUnder, worktreeName(local))
	if _, err := r.Run(ctx, runner.Command{
		Name: "git", Args: []string{"-C", root, "worktree", "add", "-q", "--detach", path},
	}); err != nil {
		return CheckedOut{}, fmt.Errorf("git worktree add failed for %s: %v", path, err)
	}
	switchArgs := []string{"-C", path, "switch", "-q", local}
	if !hasRef(ctx, r, root, "refs/heads/"+local) {
		switchArgs = []string{"-C", path, "switch", "-q", "-c", local, pr.HeadRefOid}
	}
	if _, err := r.Run(ctx, runner.Command{Name: "git", Args: switchArgs}); err != nil {
		return CheckedOut{}, fmt.Errorf("git switch %s failed inside %s: %v", local, path, err)
	}

	status, synced, err := syncWithHead(ctx, r, path, local, pr.HeadRefOid)
	if err != nil {
		return CheckedOut{}, err
	}
	warnings, err := setTracking(ctx, r, path, local, pr, remote)
	if err != nil {
		return CheckedOut{}, err
	}
	copied, copyWarnings, err := copyWorktreeInclude(ctx, r, root, path)
	if err != nil {
		return CheckedOut{}, err
	}
	return CheckedOut{
		Status: status, Path: path, Synced: synced, CopiedFiles: copied, Warnings: append(warnings, copyWarnings...),
	}, nil
}

// syncWithHead classifies how a branch stands against the pull request's head
// and fast-forwards where that is safe.
//
// Ahead comes first: a local commit the head has never seen is the one thing
// here that cannot be reconstructed, so its presence stops everything, even
// when the branch is behind as well.
func syncWithHead(ctx context.Context, r runner.Runner, dir, branch, headOID string) (ResolveStatus, bool, error) {
	counts, err := runner.Git(ctx, r, dir, "rev-list", "--left-right", "--count", branch+"..."+headOID)
	if err != nil {
		return "", false, fmt.Errorf("failed to compare %s with %s", branch, headOID)
	}
	ahead, behind, ok := strings.Cut(counts, "\t")
	if !ok {
		return "", false, fmt.Errorf("failed to compare %s with %s", branch, headOID)
	}

	switch {
	case strings.TrimSpace(ahead) != "0":
		return ResolveDiverged, false, nil
	case strings.TrimSpace(behind) == "0":
		return ResolveOK, false, nil
	}

	freshness, err := fastForwardOrDirty(ctx, r, dir, headOID)
	if err != nil {
		return "", false, err
	}
	if freshness == FreshnessBehindDirty {
		return ResolveBehindDirty, false, nil
	}
	return ResolveOK, true, nil
}
