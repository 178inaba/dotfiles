package cmd

import (
	"fmt"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// newPRCmd builds `ccx pr`, the commands that work from a pull request.
func newPRCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("pr", "Read and act on a pull request")
	c.AddCommand(prFreshnessCmd(build))
	return c
}

// prFreshnessCmd builds `ccx pr freshness`, the guard /deep-review and
// /review-response run before they read a diff or apply a fix.
func prFreshnessCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "freshness <pr-context.json>",
		Short: "Compare the checkout here with the pull request's head",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			content, err := readFile(args[0], "pr context file")
			if err != nil {
				return silent(err)
			}
			pr, err := worktree.ParsePullRequest([]byte(content))
			if err != nil {
				// The package reports the field without naming the file,
				// because the path is the caller's; a message that says which
				// file is missing a field is the useful one.
				return silent(fmt.Errorf("%v in %s", err, args[0]))
			}

			// The working directory, which is the checkout the caller means:
			// these run inside the worktree being reviewed in.
			report, err := worktree.CheckFreshness(c.Context(), runner.Exec{}, ".", pr)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), report))
		},
	}
}
