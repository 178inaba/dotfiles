package cmd

import (
	"github.com/spf13/cobra"

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
	c.AddCommand(worktreeDetectCmd(build), worktreeCreateCmd(build))
	return c
}

func worktreeDetectCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "detect <issue-number>",
		Short: "Find the worktree an issue is already being worked on in",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			issue, err := issueNumber(args[0])
			if err != nil {
				return err
			}
			root, err := worktree.MainRoot(c.Context(), runner.Exec{}, "")
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
		Args:  cobra.ExactArgs(3),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			root, err := worktree.MainRoot(c.Context(), runner.Exec{}, "")
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
