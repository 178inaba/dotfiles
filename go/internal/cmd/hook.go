package cmd

import (
	"context"
	"encoding/json/v2"
	"fmt"
	"io"
	"strconv"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/caffeinate"
	"github.com/178inaba/dotfiles/go/internal/hooks/idlenotify"
	"github.com/178inaba/dotfiles/go/internal/hooks/slacknotify"
	"github.com/178inaba/dotfiles/go/internal/hooks/subagents"
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
		leafHookCmd("start-caffeinate", "Hold the machine awake while Claude Code works", build,
			func(*cobra.Command) hook { return caffeinate.NewStart(caffeinate.Default()) }),
		stopCaffeinateCmd(build),
		leafHookCmd("idle-notify", "Notify unless a subagent is still running", build,
			func(*cobra.Command) hook { return idlenotify.New(idlenotify.Default()) }),
		leafHookCmd("slack-notify", "Post the notification to Slack", build,
			func(*cobra.Command) hook { return slacknotify.New(slacknotify.Default()) }),
		subagentTrackerCmd(build),
		leafHookCmd("terminal-bell", "Ring the terminal bell", build,
			func(*cobra.Command) hook { return terminalbell.New() }),
	)
	return c
}

// stopCaffeinateCmd is the stop half, registered on four events with two
// flags between them. Neither flag is the ordinary end of a turn, which is why
// the mode with no flag is the one that stops the session's own caffeinate.
func stopCaffeinateCmd(build selfbuild.State) *cobra.Command {
	var agentDone, force bool
	c := leafHookCmd("stop-caffeinate", "Let the machine sleep again", build,
		func(*cobra.Command) hook {
			mode := caffeinate.Session
			switch {
			case agentDone:
				mode = caffeinate.AgentDone
			case force:
				mode = caffeinate.Force
			}
			return caffeinate.NewStop(caffeinate.Default(), mode)
		})
	c.Flags().BoolVar(&agentDone, "agent-done", false, "a subagent has finished")
	c.Flags().BoolVar(&force, "force", false, "the session has ended")
	c.MarkFlagsMutuallyExclusive("agent-done", "force")
	return c
}

// subagentTrackerCmd is the one hook registered on three different events, one
// flag each. cobra rejects a combination rather than picking one, so an entry
// in settings.json that asks for two of them is a startup error and not a
// marker quietly written for the wrong event.
func subagentTrackerCmd(build selfbuild.State) *cobra.Command {
	var start, stop, sessionEnd bool
	c := leafHookCmd("subagent-tracker", "Track which subagents are running", build,
		func(*cobra.Command) hook {
			mode := subagents.None
			switch {
			case start:
				mode = subagents.Start
			case stop:
				mode = subagents.Stop
			case sessionEnd:
				mode = subagents.SessionEnd
			}
			return subagents.New(subagents.Default(), mode)
		})
	c.Flags().BoolVar(&start, "start", false, "a subagent has started")
	c.Flags().BoolVar(&stop, "stop", false, "a subagent has finished")
	c.Flags().BoolVar(&sessionEnd, "session-end", false, "the session has ended")
	c.MarkFlagsMutuallyExclusive("start", "stop", "session-end")
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
