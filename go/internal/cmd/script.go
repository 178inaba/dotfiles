package cmd

import (
	"fmt"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// reportBuild says that the module no longer compiles, on the one invocation
// that tried to rebuild it.
//
// Standard error, and only standard error. The hooks put the same news in a
// systemMessage on their standard output when they exit 0, because Claude Code
// parses it; these subcommands promise standard output that is JSON and nothing
// else, and their caller pipes it to jq. Once per failure rather than once per
// run; see selfbuild.State.JustFailed.
func reportBuild(c *cobra.Command, build selfbuild.State) {
	if !build.JustFailed {
		return
	}
	fmt.Fprintf(c.ErrOrStderr(),
		"ccx: the Go module does not build, so this ran the previously installed binary: %s\n",
		build.FirstError)
}
