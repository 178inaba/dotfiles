package cmd

import (
	"os"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/plandocs"
)

func newPlanCmd(deps Deps) *cobra.Command {
	c := newParentCmd("plan", "Read what a plan has to be drafted against")
	c.AddCommand(newPlanRulesCmd(deps), newPlanCheckCmd(deps))
	return c
}

// newPlanCheckCmd builds `ccx plan check`, which reads a drafted plan against
// the repository it is a plan for.
//
// Exactly one argument, unlike the rules command beside it: that one takes the
// subjects of a pattern match, of which none is a legitimate run, while this
// one reads a file and there is no plan to read without being told which.
//
// The findings leave through the exit status as well as the output, because
// the caller is a skill told to fix them and run again — a loop that reads a
// number rather than the answer it just printed.
func newPlanCheckCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "check <plan-file>",
		Short: "Check a plan's paths, names and recorded commands",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)

			home, err := os.UserHomeDir()
			if err != nil {
				return silent(err)
			}

			checked, err := plandocs.Check(args[0], deps.Dir, home)
			if err != nil {
				return silent(err)
			}
			if err := renderJSON(c.OutOrStdout(), checked); err != nil {
				return silent(err)
			}
			if len(checked.MissingPaths)+len(checked.UnresolvedSymbols)+len(checked.UnrecordedCommands) > 0 {
				return exitCode(planCheckFindings.code)
			}
			return nil
		},
	}
}

// newPlanRulesCmd builds `ccx plan rules`, which lists what the harness loaded
// at launch and the path-scoped rules a planner reads before drafting.
//
// The arguments are the paths the task touches, and they are patterns' subjects
// rather than files to open, so any number of them is a legitimate run: none
// asks only what was loaded, and cobra has no validator for that.
//
// It runs from anywhere inside the repository: the instruction files a session
// has in context are decided by where it was started, and Collect reads that
// off the checkout it was given upwards.
func newPlanRulesCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "rules [<path>...]",
		Short: "List the loaded instruction files and the rules the given paths bring in",
		Args:  cobra.ArbitraryArgs,
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)

			home, err := os.UserHomeDir()
			if err != nil {
				return silent(err)
			}

			collection, err := plandocs.Collect(deps.Dir, home, args...)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), collection))
		},
	}
}
