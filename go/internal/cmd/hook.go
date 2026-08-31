package cmd

import (
	"context"
	"encoding/json/v2"
	"fmt"
	"io"
	"strconv"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/terminalbell"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// hook is one entry in settings.json.
//
// There is no error return. A hook that cannot do its job has already decided
// what that means for the event it was asked about — a guard that cannot read
// its input lets the call through, a check that cannot run blocks — so a second
// channel would only leave the caller guessing which of the two answers wins.
// Errors inside a hook are ordinary Go errors; Run is where they become one of
// these.
type hook interface {
	Run(ctx context.Context, in hooks.Payload, stderr io.Writer) hooks.Result
}

// exitCode is a status a subcommand reached rather than a failure it suffered.
// run turns it into the process exit status and prints nothing: a hook that
// blocks has already written its own message, and usage text would bury it.
type exitCode int

func (c exitCode) Error() string { return "exit status " + strconv.Itoa(int(c)) }

// newHookCmd builds `ccx hook`. Every hook is a subcommand of it, so a name
// that settings.json got wrong is a cobra error rather than an exit 0.
func newHookCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("hook", "Run a Claude Code hook")
	c.AddCommand(
		leafHookCmd("terminal-bell", "Ring the terminal bell", build,
			func(*cobra.Command) hook { return terminalbell.New() }),
	)
	return c
}

// leafHookCmd wires one hook into the tree. The hook is built at run time
// rather than passed in, so that a hook with flags can read the values cobra
// parsed into the closure the caller registered them on.
func leafHookCmd(use, short string, build selfbuild.State, make func(*cobra.Command) hook) *cobra.Command {
	return &cobra.Command{
		Use:   use,
		Short: short,
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runHook(c.Context(), make(c), build,
				c.InOrStdin(), c.OutOrStdout(), c.ErrOrStderr())
		},
	}
}

// runHook reads the payload, runs the hook and reports the outcome on whichever
// stream the exit status has Claude Code reading.
//
// Standard input is read here rather than earlier because the self-rebuild may
// re-exec this process, and the replacement inherits the pipe only as far as
// nothing has consumed it; see selfbuild.Run.
func runHook(ctx context.Context, h hook, build selfbuild.State, stdin io.Reader, stdout, stderr io.Writer) error {
	in, _ := io.ReadAll(stdin)
	result := h.Run(ctx, hooks.Parse(in), stderr)

	// Only on the invocation that ran the build. A broken tree otherwise
	// produces one message per hook per tool call until somebody fixes it.
	if build.JustFailed {
		if result.Decision == hooks.Allow {
			result.Directive.SystemMessage = join(result.Directive.SystemMessage, buildFailure(build))
		} else {
			fmt.Fprintln(stderr, buildFailure(build))
		}
	}

	if !result.Directive.IsEmpty() {
		b, err := json.Marshal(result.Directive)
		if err != nil {
			return silent(err)
		}
		fmt.Fprintf(stdout, "%s\n", b)
	}
	if result.Decision != hooks.Allow {
		return exitCode(result.Decision)
	}
	return nil
}

// buildFailure is what a hook says about a module that no longer compiles. It
// says which binary ran, because the answer the hook just gave came from the
// previous build and may not be the one the current source would give.
func buildFailure(build selfbuild.State) string {
	return "ccx: the Go module does not build, so this ran the previously installed binary: " + build.FirstError
}

func join(existing, added string) string {
	if existing == "" {
		return added
	}
	return existing + "\n" + added
}
