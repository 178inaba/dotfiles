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
// self-build outcome it reports, and the way to a GitHub client.
//
// Every constructor takes the whole of it, including the ones that need no
// client today, so that a command which later does needs nothing rethreaded to
// reach GitHub.
type Deps struct {
	// Build is the self-rebuild outcome, which each subcommand reports in
	// whatever way suits its own output contract.
	Build selfbuild.State
	// NewClient is a constructor rather than a client for the reason
	// prinfo.Refresh documents: ghapi.New resolves go-gh's options and, for a
	// token in the system keyring, execs `gh auth token` — a cost a run that
	// never reaches GitHub should not pay. The commands call it where they
	// used to construct one, which is to say late.
	NewClient func() (*ghapi.Client, error)
}

// Execute runs the tree and returns the process exit status. The self-rebuild
// check runs first, before anything reads stdin; see selfbuild.Run.
//
// The one place a client is constructed. Every other one goes through Deps,
// which is what lets a test put ghapitest's client in front of a command; see
// TestOnlyExecuteBuildsTheClient.
func Execute(ctx context.Context, args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	return run(ctx, args, stdin, stdout, stderr, Deps{
		Build:     selfbuild.Run(ctx, selfbuild.NewDeps(args)),
		NewClient: func() (*ghapi.Client, error) { return ghapi.New(ghapi.Options{}) },
	})
}

// run is Execute without the self-rebuild check, so tests can drive the tree
// without the filesystem underneath it.
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
