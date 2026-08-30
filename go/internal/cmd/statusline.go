package cmd

import (
	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/statusline"
)

// newStatuslineCmd renders the status line from the payload on standard input.
//
// It never fails. Every source it draws on is optional, and Claude Code redraws
// it every few seconds, so an error exit would be noise the user cannot act on;
// a missing segment says as much as a message would.
func newStatuslineCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "statusline",
		Short: "Render the Claude Code status line",
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			cfg := statusline.Default()
			cfg.BuildError = build.FirstError
			if err := statusline.Run(c.Context(), cfg, c.InOrStdin(), c.OutOrStdout()); err != nil {
				return Silent(err)
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
			statusline.RefreshFX(c.Context(), cachePath, now)
			return nil
		},
	}
	pr := &cobra.Command{
		Use:    statusline.RefreshPRCommandName,
		Short:  "Fetch the current branch's pull request into the status line cache",
		Hidden: true,
		Args:   cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			statusline.RefreshPR(c.Context(), statusline.Default(), cachePath, cacheKey, head, now)
			return nil
		},
	}

	for _, c := range []*cobra.Command{fx, pr} {
		// The parent passes what it computed rather than letting the child work
		// it out again: the cache path is cut to a fixed length, and two
		// derivations of that could disagree.
		c.Flags().Int64Var(&now, "now", 0, "unix time the refresh was started for")
		c.Flags().StringVar(&cachePath, "cache", "", "cache file to write")
	}
	pr.Flags().StringVar(&cacheKey, "key", "", "cache key the record belongs to")
	pr.Flags().StringVar(&head, "branch", "", "branch to look the pull request up for")

	return []*cobra.Command{fx, pr}
}
