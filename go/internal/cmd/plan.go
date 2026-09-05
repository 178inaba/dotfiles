package cmd

import (
	"os"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/plandocs"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

func newPlanCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("plan", "Read what a plan has to be drafted against")
	c.AddCommand(newPlanDocsCmd(build))
	return c
}

// newPlanDocsCmd builds `ccx plan docs`, which lists the documents a planner
// reads before drafting.
//
// It takes no arguments and runs from anywhere inside the repository: the
// instruction files a session has in context are decided by where it was
// started, and the walk reads that off the working directory upwards.
func newPlanDocsCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "docs",
		Short: "List the documents a plan has to be drafted against",
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			reportBuild(c, build)

			root, err := os.Getwd()
			if err != nil {
				return silent(err)
			}
			home, err := os.UserHomeDir()
			if err != nil {
				return silent(err)
			}

			collection, err := plandocs.Collect(root, home)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), collection))
		},
	}
}
