package cmd

import (
	"fmt"
	"io"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// newWorktreeCmd builds `ccx worktree`, the git plumbing the skills that work
// in a worktree share.
//
// Every one of these runs from anywhere inside the repository and resolves the
// main worktree itself, because a skill may be running in a worktree already.
func newWorktreeCmd(deps Deps) *cobra.Command {
	c := newParentCmd("worktree", "Create and resolve the worktrees the skills work in")
	c.AddCommand(worktreeDetectCmd(deps), worktreeCreateCmd(deps), worktreeResolveCmd(deps), worktreeCheckoutCmd(deps),
		worktreeCollectCmd(deps), worktreeDeleteCmd(deps), worktreeSweepCmd(deps))
	return c
}

func worktreeDetectCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "detect <issue-number>",
		Short: "Find the worktree an issue is already being worked on in",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			issue, err := issueNumber(args[0])
			if err != nil {
				return err
			}
			root, err := worktree.MainRoot(c.Context(), runner.Exec{}, deps.Dir)
			if err != nil {
				return silent(err)
			}
			found, err := worktree.Detect(c.Context(), runner.Exec{}, root, issue)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), found))
		},
	}
}

func worktreeCreateCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "create <worktree-name> <branch> <base-branch>",
		Short: "Create a worktree for a new branch off a base branch",
		Args:  cobra.ExactArgs(3),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			root, err := worktree.MainRoot(c.Context(), runner.Exec{}, deps.Dir)
			if err != nil {
				return silent(err)
			}
			created, err := worktree.Create(c.Context(), runner.Exec{}, root, args[0], args[1], args[2])
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), created))
		},
	}
}

// worktreeResolveCmd builds `ccx worktree resolve`, the first half of the
// worktree-resolution procedure /deep-review and /review-response run for
// --worktree. The second half is worktreeCheckoutCmd; switching the session is
// the caller's, because no command can see the session's state.
func worktreeResolveCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "resolve [<pr-number>]",
		Short: "Find the worktree for a pull request, or prepare to make one",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			// Zero means no number was given, and the pull request is inferred
			// from the branch checked out here.
			number := 0
			if len(args) == 1 {
				var err error
				if number, err = issueNumber(args[0]); err != nil {
					return fmt.Errorf("invalid pr number: %s", args[0])
				}
			}

			client, err := deps.NewClient()
			if err != nil {
				return silent(err)
			}
			repo, err := targetRepo(c.Context(), client, "", deps.Dir)
			if err != nil {
				return silent(err)
			}

			resolved, err := worktree.Resolve(c.Context(), runner.Exec{}, client, repo, deps.Dir, number)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), resolved))
		},
	}
}

func worktreeCheckoutCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "checkout <worktree-name> <head-ref>",
		Short: "Make a worktree at a pull request's head branch",
		Args:  cobra.ExactArgs(2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			root, err := worktree.MainRoot(c.Context(), runner.Exec{}, deps.Dir)
			if err != nil {
				return silent(err)
			}
			checked, err := worktree.Checkout(c.Context(), runner.Exec{}, root, args[0], args[1])
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), checked))
		},
	}
}

// worktreeCollectCmd builds `ccx worktree collect`, the first half of
// /cleanup-merged. It deletes nothing: the list goes to a person for approval,
// and worktreeDeleteCmd takes back whatever survives that.
func worktreeCollectCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "collect",
		Short: "List the worktrees and branches whose work is finished",
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			reportBuild(c, deps.Build)
			client, err := deps.NewClient()
			if err != nil {
				return silent(err)
			}
			collected, err := worktree.Collect(c.Context(), runner.Exec{}, client, deps.Dir)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), collected))
		},
	}
}

// worktreeSweepCmd builds `ccx worktree sweep`, the unattended counterpart of
// the pair above: it runs in the middle of another procedure, so nothing is
// asked of the caller between finding and removing.
func worktreeSweepCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "sweep",
		Short: "Remove the worktrees the harness's isolated agents left behind",
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			reportBuild(c, deps.Build)
			swept, err := worktree.Sweep(c.Context(), runner.Exec{}, deps.Dir)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), swept))
		},
	}
}

func worktreeDeleteCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "delete",
		Short: "Delete the approved worktrees and branches read from standard input",
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			reportBuild(c, deps.Build)
			in, err := io.ReadAll(c.InOrStdin())
			if err != nil {
				return silent(err)
			}
			candidates, err := worktree.ParseCandidates(in)
			if err != nil {
				return silent(err)
			}
			deleted, err := worktree.Delete(c.Context(), runner.Exec{}, deps.Dir, candidates)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), deleted))
		},
	}
}
