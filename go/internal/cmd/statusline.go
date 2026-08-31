package cmd

import (
	"time"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/statusline"
)

// newStatuslineCmd renders the status line from the payload on standard input.
// It never fails; see statusline.Run. It reports a build failure on every
// render rather than once, which is what selfbuild.State.Failed is for.
func newStatuslineCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "statusline",
		Short: "Render the Claude Code status line",
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			err := statusline.Run(c.Context(), statusline.Default(),
				c.InOrStdin(), c.OutOrStdout(), build.FirstError)
			if err != nil {
				return silent(err)
			}
			return nil
		},
	}
}

// newRefreshCmds are the detached children a redraw starts. They are hidden:
// running one by hand does nothing useful, and the status line's own contract
// is what defines their arguments.
func newRefreshCmds() []*cobra.Command {
	var (
		now                       int64
		cachePath, cacheKey, head string
	)

	fx := &cobra.Command{
		Use:    statusline.RefreshFXCommandName,
		Short:  "Fetch the dollar to yen rate into the status line cache",
		Hidden: true,
		Args:   cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			statusline.RefreshFX(c.Context(), cachePath, time.Unix(now, 0))
			return nil
		},
	}
	pr := &cobra.Command{
		Use:    statusline.RefreshPRCommandName,
		Short:  "Fetch the current branch's pull request into the status line cache",
		Hidden: true,
		Args:   cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			statusline.RefreshPR(c.Context(), runner.Exec{}, cachePath, cacheKey, head, time.Unix(now, 0))
			return nil
		},
	}

	for _, c := range []*cobra.Command{fx, pr} {
		c.Flags().Int64Var(&now, statusline.FlagNow, 0, "unix time the refresh was started for")
		c.Flags().StringVar(&cachePath, statusline.FlagCache, "", "cache file to write")
	}
	pr.Flags().StringVar(&cacheKey, statusline.FlagKey, "", "cache key the record belongs to")
	pr.Flags().StringVar(&head, statusline.FlagBranch, "", "branch to look the pull request up for")

	return []*cobra.Command{fx, pr}
}
