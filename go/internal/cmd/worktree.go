package cmd

import (
	"fmt"
	"io"
	"os"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/ghapi"

	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// newWorktreeCmd builds `ccx worktree`, the git plumbing the skills that work
// in a worktree share.
//
// Every one of these runs from anywhere inside the repository and resolves the
// main worktree itself, because a skill may be running in a worktree already.
func newWorktreeCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("worktree", "Create and resolve the worktrees the skills work in")
	c.AddCommand(worktreeDetectCmd(build), worktreeCreateCmd(build), worktreeResolveCmd(build), worktreeCheckoutCmd(build),
		worktreeCollectCmd(build), worktreeDeleteCmd(build))
	return c
}

// mainRoot is the main worktree of the repository the command was started in.
// The working directory is resolved here rather than left for git to assume, so
// that every directory these commands ask git about is one they named.
func mainRoot(c *cobra.Command) (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	return worktree.MainRoot(c.Context(), runner.Exec{}, dir)
}

func worktreeDetectCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "detect <issue-number>",
		Short: "Find the worktree an issue is already being worked on in",
		Long:  longFor("worktree detect"),
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			issue, err := issueNumber(args[0])
			if err != nil {
				return err
			}
			root, err := mainRoot(c)
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

func worktreeCreateCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "create <worktree-name> <branch> <base-branch>",
		Short: "Create a worktree for a new branch off a base branch",
		Long:  longFor("worktree create"),
		Args:  cobra.ExactArgs(3),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			root, err := mainRoot(c)
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
func worktreeResolveCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "resolve [<pr-number>]",
		Short: "Find the worktree for a pull request, or prepare to make one",
		Long:  longFor("worktree resolve"),
		Args:  cobra.MaximumNArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			// Zero means no number was given, and the pull request is inferred
			// from the branch checked out here.
			number := 0
			if len(args) == 1 {
				var err error
				if number, err = issueNumber(args[0]); err != nil {
					return fmt.Errorf("invalid pr number: %s", args[0])
				}
			}

			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			repo, err := targetRepo(c.Context(), client, "")
			if err != nil {
				return silent(err)
			}

			resolved, err := worktree.Resolve(c.Context(), runner.Exec{}, client, repo, ".", number)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), resolved))
		},
	}
}

func worktreeCheckoutCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "checkout <worktree-name> <head-ref>",
		Short: "Make a worktree at a pull request's head branch",
		Long:  longFor("worktree checkout"),
		Args:  cobra.ExactArgs(2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			root, err := mainRoot(c)
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
func worktreeCollectCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "collect",
		Short: "List the worktrees and branches whose work is finished",
		Long:  longFor("worktree collect"),
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			reportBuild(c, build)
			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			collected, err := worktree.Collect(c.Context(), runner.Exec{}, client, ".")
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), collected))
		},
	}
}

func worktreeDeleteCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "delete",
		Short: "Delete the approved worktrees and branches read from standard input",
		Long:  longFor("worktree delete"),
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			reportBuild(c, build)
			in, err := io.ReadAll(c.InOrStdin())
			if err != nil {
				return silent(err)
			}
			candidates, err := worktree.ParseCandidates(in)
			if err != nil {
				return silent(err)
			}
			deleted, err := worktree.Delete(c.Context(), runner.Exec{}, ".", candidates)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), deleted))
		},
	}
}
