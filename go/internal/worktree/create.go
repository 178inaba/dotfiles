package worktree

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"regexp"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// worktreesUnder is where a repository keeps the worktrees these commands make.
const worktreesUnder = ".claude/worktrees"

// Detection is whether an issue already has a worktree, which is how a skill
// tells starting from resuming.
type Detection struct {
	Found bool `json:"found"`
	// Path and Branch are null when nothing was found, rather than empty
	// strings: the caller branches on found and reads these only after.
	Path   *string `json:"worktree_path"`
	Branch *string `json:"branch"`
}

// Detect finds the worktree of an issue among the repository's linked ones.
//
// Two namings match. The current one is <type>/<issue>-<slug>; the other is
// what EnterWorktree(name:) produced before these commands took over creating
// worktrees, and it stays because the worktrees it made are still on disk and
// resuming into one is the whole point of asking.
//
// The main worktree is never the answer, even when it has the branch checked
// out: a session resumed into it would be working in the repository itself.
func Detect(ctx context.Context, r runner.Runner, root string, issue int) (Detection, error) {
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
			return Detection{Found: true, Path: &e.Path, Branch: &e.Branch}, nil
		}
	}
	return Detection{}, nil
}

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
	// StartRef is the ref the worktree was branched from, null unless one was.
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
	remote := hasRef(ctx, r, root, "refs/remotes/origin/"+base)
	local := hasRef(ctx, r, root, "refs/heads/"+base)

	switch {
	case remote:
		var warnings []string
		if local && !isAncestor(ctx, r, root, "refs/heads/"+base, "refs/remotes/origin/"+base) {
			warnings = append(warnings, fmt.Sprintf(
				"local branch %s has commits not on origin/%s; worktree starts from origin/%s", base, base, base))
		}
		return "origin/" + base, warnings, nil
	case local:
		return base, []string{fmt.Sprintf("origin/%s not found; started from local branch %s", base, base)}, nil
	default:
		return "", nil, fmt.Errorf("base branch not found: neither origin/%s nor %s exists", base, base)
	}
}

// hasRef reports whether a ref exists.
//
// git answers with its exit status alone, so a git that could not run at all
// reads as the ref being absent — which is what the shell did too, and what the
// caller then reports as a base branch that is not there.
func hasRef(ctx context.Context, r runner.Runner, root, ref string) bool {
	_, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", root, "show-ref", "--verify", "--quiet", ref},
	})
	return err == nil
}
