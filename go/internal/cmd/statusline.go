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
			return silent(statusline.Run(c.Context(), statusline.Default(),
				c.InOrStdin(), c.OutOrStdout(), build.FirstError))
		},
	}
}

// newRefreshCmds are the detached children a redraw starts. They are hidden:
// the status line's own contract is what defines their arguments. A failure is
// reported the ordinary way, which reaches nobody when the child is detached
// and is the only account of what went wrong when one is run by hand.
func newRefreshCmds() []*cobra.Command {
	var (
		now                           int64
		cacheDir, cacheKey, head, dir string
	)

	fx := &cobra.Command{
		Use:    statusline.RefreshFXCommandName,
		Short:  "Fetch the dollar to yen rate into the status line cache",
		Hidden: true,
		Args:   cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return silent(statusline.RefreshFX(c.Context(), cacheDir, time.Unix(now, 0)))
		},
	}
	pr := &cobra.Command{
		Use:    statusline.RefreshPRCommandName,
		Short:  "Fetch the current branch's pull request into the status line cache",
		Hidden: true,
		Args:   cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return silent(statusline.RefreshPR(c.Context(), runner.Exec{}, cacheDir, cacheKey, head, dir, time.Unix(now, 0)))
		},
	}

	for _, c := range []*cobra.Command{fx, pr} {
		c.Flags().Int64Var(&now, statusline.FlagNow, 0, "unix time the refresh was started for")
		c.Flags().StringVar(&cacheDir, statusline.FlagCache, "", "cache entry to write")
	}
	pr.Flags().StringVar(&cacheKey, statusline.FlagKey, "", "cache key the record belongs to")
	pr.Flags().StringVar(&head, statusline.FlagBranch, "", "branch to look the pull request up for")
	pr.Flags().StringVar(&dir, statusline.FlagDir, "", "repository directory the badge is about")

	return []*cobra.Command{fx, pr}
}
