package worktree

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// worktreesUnder is where a repository keeps the worktrees these commands make.
const worktreesUnder = ".claude/worktrees"

// CreateStatus is how far Create got.
type CreateStatus string

const (
	// CreateOK is a worktree that now exists and did not before.
	CreateOK CreateStatus = "ok"
	// CreateBranchExists is a stopping condition rather than a failure: the
	// branch may be the remains of earlier work on the same issue, and whether
	// to throw that away is a question for the person, not for a command that
	// would answer it by deleting.
	CreateBranchExists CreateStatus = "branch_exists"
	// CreatePathExists is the same stopping condition for the directory.
	CreatePathExists CreateStatus = "path_exists"
)

// Created is the outcome of making a worktree.
type Created struct {
	Status CreateStatus `json:"status"`
	Path   *string      `json:"worktree_path"`
	Branch string       `json:"branch"`
	// The ref the worktree was branched from, null unless one was.
	StartRef    *string  `json:"start_ref"`
	CopiedFiles int      `json:"copied_files"`
	Warnings    []string `json:"warnings"`
}

// Create makes a worktree for branch, started from base, under the
// repository's worktree directory.
//
// The main worktree's head and working tree are left alone, which is why this
// exists at all: EnterWorktree(name:) cannot be given a base branch, and
// reaching one through it meant moving the main tree's head — no longer
// something that can be undone since Claude Code 2.1.222 isolated worktrees.
//
// Nothing is fetched. The skill that calls this fetched already, and a fetch
// that failed is why the fallback to a local base exists.
func Create(ctx context.Context, r runner.Runner, root, name, branch, base string) (Created, error) {
	if hasRef(ctx, r, root, "refs/heads/"+branch) {
		return Created{Status: CreateBranchExists, Branch: branch}, nil
	}

	path := filepath.Join(root, worktreesUnder, name)
	// Lstat rather than Stat, so that a dangling symlink at the path counts as
	// something already being there — git worktree add would refuse it too.
	if _, err := os.Lstat(path); err == nil {
		return Created{Status: CreatePathExists, Branch: branch}, nil
	}

	startRef, warnings, err := startRefFor(ctx, r, root, base)
	if err != nil {
		return Created{}, err
	}

	if _, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", root, "worktree", "add", "--quiet", path, "-b", branch, startRef},
	}); err != nil {
		return Created{}, fmt.Errorf("git worktree add failed for %s: %v", path, err)
	}

	copied, copyWarnings, err := copyWorktreeInclude(ctx, r, root, path)
	if err != nil {
		return Created{}, err
	}
	return Created{
		Status:      CreateOK,
		Path:        &path,
		Branch:      branch,
		StartRef:    &startRef,
		CopiedFiles: copied,
		Warnings:    append(warnings, copyWarnings...),
	}, nil
}

// startRefFor picks what the new worktree starts from.
//
// The remote-tracking ref first, because that is what everyone else's work is
// based on. Both ways of departing from it are worth saying out loud: falling
// back to a local branch means the fetch that should have happened did not, and
// a local branch ahead of the remote means commits the author has not pushed
// will be missing from the worktree.
func startRefFor(ctx context.Context, r runner.Runner, root, base string) (string, []string, error) {
	full, err := resolveStartRef(ctx, r, root, base)
	if err != nil {
		return "", nil, err
	}
	local := "refs/heads/" + base
	if full == local {
		return base, []string{fmt.Sprintf("origin/%s not found; started from local branch %s", base, base)}, nil
	}

	var warnings []string
	if hasRef(ctx, r, root, local) && !IsAncestor(ctx, r, root, local, full) {
		warnings = append(warnings, fmt.Sprintf(
			"local branch %s has commits not on origin/%s; worktree starts from origin/%s", base, base, base))
	}
	return "origin/" + base, warnings, nil
}

// resolveStartRef is the ref a worktree started from base would start from,
// spelled out in full: git resolves a tag before a branch of the same name, and
// a comparison against the short name would take the tag's commit instead.
func resolveStartRef(ctx context.Context, r runner.Runner, root, base string) (string, error) {
	for _, ref := range []string{"refs/remotes/origin/" + base, "refs/heads/" + base} {
		if hasRef(ctx, r, root, ref) {
			return ref, nil
		}
	}
	return "", fmt.Errorf("base branch not found: neither origin/%s nor %s exists", base, base)
}

// hasRef reports whether a ref exists.
//
// git answers with its exit status alone, so a git that could not run at all
// reads as the ref being absent, which the caller then reports as a base
// branch that is not there.
func hasRef(ctx context.Context, r runner.Runner, root, ref string) bool {
	_, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", root, "show-ref", "--verify", "--quiet", ref},
	})
	return err == nil
}
