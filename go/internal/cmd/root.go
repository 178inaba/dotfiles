// Package cmd is the ccx command tree.
//
// ccx is one dispatcher for every Claude Code extension in this repository:
// the statusline, and later the hooks and the shared scripts. One binary rather
// than one per command, because a couple of dozen Go binaries would cost more
// than a hundred megabytes and as many names on PATH.
package cmd

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// silentError marks a failure as the command's own rather than a misuse of the
// command line, so it is reported without the usage text a typo deserves.
type silentError struct{ error }

// Unwrap keeps errors.Is and errors.As working through the wrapper.
func (e silentError) Unwrap() error { return e.error }

// silent wraps err so run prints it on its own. A nil stays nil: wrapping one
// would produce a non-nil error interface around nothing, and every command
// that succeeded would report a failure with no message.
func silent(err error) error {
	if err == nil {
		return nil
	}
	return silentError{err}
}

// Deps is what a command is given rather than what it reaches for: the
// self-build outcome it reports, the checkout it acts on, and the way to a
// GitHub client.
//
// Every constructor takes the whole of it, including the ones that need no
// client today, so that a command which later does needs nothing rethreaded to
// reach GitHub. A helper that runs rather than builds takes the field it uses:
// reportBuild, buildFailure and runHook report the build, and handing them a
// client they have no business reaching would be the wider signature, not the
// consistent one.
type Deps struct {
	// Build is the self-rebuild outcome, which each subcommand reports in
	// whatever way suits its own output contract.
	Build selfbuild.State
	// NewClient is a constructor rather than a client, for the reason
	// prinfo.Refresh documents: building one costs an exec that a run never
	// reaching GitHub should not pay. The commands call it where they used to
	// construct one, which is to say late.
	NewClient func() (*ghapi.Client, error)
	// Dir is the checkout the command acts on, carried for the reason
	// ghapi.PullRequestForCurrentBranch documents of its own dir — a default
	// of the process's directory is one a test forgets to override — and with
	// the half this package adds: overriding it moves every test at once.
	// Always absolute, which is what plan docs needs to walk up from it.
	//
	// The process's checkout rather than the session's. A command handed a
	// payload has the better answer in it — hooks.Payload.Dir, the status
	// line's workspace — and takes the directory from there.
	Dir string
}

// Execute runs the tree and returns the process exit status. The self-rebuild
// check runs first, before anything reads stdin; see selfbuild.Run.
//
// The one place a client is constructed, and the one place this package asks
// the process where it is running. Every other one goes through Deps, which is
// what lets a test put ghapitest's client and a temporary repository in front
// of a command; see TestOnlyExecuteBuildsTheClient. The status line keeps a
// seam of its own for the payload that arrives without a directory, in
// statusline.Config.
//
// A directory that cannot be read stops the run here rather than travelling as
// an empty one. Empty is not a failure anywhere downstream: git reads it as
// "do not change directory" and plan docs walks up from the process's own,
// which is the ambient answer this field exists to stop giving.
func Execute(ctx context.Context, args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	build := selfbuild.Run(ctx, selfbuild.NewDeps(args))
	dir, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(stderr, "ccx: %v\n", err)
		return 1
	}
	return run(ctx, args, stdin, stdout, stderr, Deps{
		Build:     build,
		NewClient: func() (*ghapi.Client, error) { return ghapi.New(ghapi.Options{}) },
		Dir:       dir,
	})
}

// run is Execute without the self-rebuild check, so tests can drive the tree
// without the filesystem underneath it and without the process's own directory
// standing in for the checkout.
func run(ctx context.Context, args []string, stdin io.Reader, stdout, stderr io.Writer, deps Deps) int {
	root := newRootCmd(deps)
	root.SetArgs(args)
	root.SetIn(stdin)
	root.SetOut(stdout)
	root.SetErr(stderr)

	cmd, err := root.ExecuteContextC(ctx)
	if err == nil {
		return 0
	}
	// A status a subcommand chose, not a failure: it has already written
	// whatever its own contract calls for.
	if code, ok := errors.AsType[exitCode](err); ok {
		return int(code)
	}
	fmt.Fprintf(stderr, "ccx: %v\n", err)
	// Usage is printed here rather than by cobra, which sends it to the out
	// stream — the same stream a subcommand renders on.
	if _, silent := errors.AsType[silentError](err); !silent {
		fmt.Fprint(stderr, cmd.UsageString())
	}
	return 1
}

// newRootCmd builds the command tree, handing every subcommand the same deps.
func newRootCmd(deps Deps) *cobra.Command {
	root := newParentCmd("ccx", "Claude Code extensions for this dotfiles repository")
	// Errors and usage are printed once, centrally, in run.
	root.SilenceUsage = true
	root.SilenceErrors = true

	root.AddCommand(newStatuslineCmd(deps))
	root.AddCommand(newHookCmd(deps))
	root.AddCommand(newPRCmd(deps))
	root.AddCommand(newIssueCmd(deps))
	root.AddCommand(newPlanCmd(deps))
	root.AddCommand(newReviewCmd(deps))
	root.AddCommand(newWorktreeCmd(deps))
	root.AddCommand(newSkillCmd(deps))
	root.AddCommand(newRefreshCmds(deps)...)

	// Rendered when help is asked for, not when the tree is built: all
	// twenty-one at construction was 77% of the time and 96% of the
	// allocations of building it, paid by every ccx process — the hooks run on
	// every tool call — for text almost never printed. Cobra reads Long only
	// on the help path.
	def := root.HelpFunc()
	root.SetHelpFunc(func(c *cobra.Command, args []string) {
		c.Long = longFor(commandPath(c))
		def(c, args)
	})
	return root
}

// commandPath is a command's key in the contract table.
func commandPath(c *cobra.Command) string {
	return strings.TrimPrefix(c.CommandPath(), "ccx ")
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
