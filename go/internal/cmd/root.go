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

// Execute runs the tree and returns the process exit status. The self-rebuild
// check runs first, before anything reads stdin; see selfbuild.Run.
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
	root := newParentCmd("ccx", "Claude Code extensions for this dotfiles repository")
	// Errors and usage are printed once, centrally, in run.
	root.SilenceUsage = true
	root.SilenceErrors = true

	root.AddCommand(newStatuslineCmd(build))
	root.AddCommand(newRefreshCmds()...)
	return root
}

// newParentCmd builds a command that only groups others.
//
// Every such command has to be built this way. cobra checks whether a command
// is runnable before it validates the arguments, so a parent with no RunE
// treats a mistyped subcommand as a request for help and exits 0 — and a hook
// whose name is misspelled in settings.json exiting 0 reads as "allow".
func newParentCmd(use, short string) *cobra.Command {
	return &cobra.Command{
		Use:   use,
		Short: short,
		RunE: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return c.Help()
			}
			return fmt.Errorf("unknown command %q for %q", args[0], c.CommandPath())
		},
	}
}
