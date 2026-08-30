// Package cmd is the ccx command tree.
//
// ccx is one dispatcher for every Claude Code extension in this repository:
// the statusline, and later the hooks and the shared scripts. One binary rather
// than one per command, because a couple of dozen Go binaries would cost more
// than a hundred megabytes and as many names on PATH.
package cmd

import (
	"errors"
	"fmt"
	"io"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// silentError marks a failure as the command's own rather than a misuse of the
// command line, so it is reported without the usage text a typo deserves.
type silentError struct{ error }

// Silent wraps err so Execute prints it on its own.
func Silent(err error) error { return silentError{err} }

// Execute runs the tree and returns the process exit status.
//
// The self-rebuild check runs first, and before anything reads stdin: it may
// replace this process, and the replacement inherits the argv but not bytes
// already taken off the pipe.
func Execute(args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	return run(args, stdin, stdout, stderr, selfbuild.Run(selfbuild.NewDeps(args)))
}

// run is Execute without the self-rebuild check, so tests can drive the tree
// without the filesystem underneath it.
func run(args []string, stdin io.Reader, stdout, stderr io.Writer, build selfbuild.State) int {
	root := newRootCmd(build)
	root.SetArgs(args)
	root.SetIn(stdin)
	root.SetOut(stdout)
	root.SetErr(stderr)

	cmd, err := root.ExecuteC()
	if err == nil {
		return 0
	}
	fmt.Fprintf(stderr, "ccx: %v\n", err)
	// Usage is printed here rather than by cobra, which sends it to the out
	// stream — the same stream a subcommand renders on.
	if _, silent := errors.AsType[silentError](err); !silent {
		fmt.Fprint(stderr, cmd.UsageString())
	}
	return 1
}

// newRootCmd builds the command tree. build is the self-rebuild outcome, which
// each subcommand reports in whatever way suits its own output contract.
func newRootCmd(build selfbuild.State) *cobra.Command {
	root := &cobra.Command{
		Use:   "ccx",
		Short: "Claude Code extensions for this dotfiles repository",
		// Errors and usage are printed once, centrally, in Execute.
		SilenceUsage:  true,
		SilenceErrors: true,
		RunE: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return c.Help()
			}
			// Without a RunE, cobra treats a stray argument on a command that
			// cannot run as a request for help and exits 0, which would let a
			// mistyped subcommand pass for a successful one in a hook.
			return fmt.Errorf("unknown command %q for %q", args[0], c.CommandPath())
		},
	}
	root.AddCommand(newStatuslineCmd(build))
	root.AddCommand(newRefreshCmds()...)
	return root
}
