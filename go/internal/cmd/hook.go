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
	"github.com/178inaba/dotfiles/go/internal/hooks/noopwait"
	"github.com/178inaba/dotfiles/go/internal/hooks/skillcheck"
	"github.com/178inaba/dotfiles/go/internal/hooks/slacknotify"
	"github.com/178inaba/dotfiles/go/internal/hooks/subagents"
	"github.com/178inaba/dotfiles/go/internal/hooks/terminalbell"
	"github.com/178inaba/dotfiles/go/internal/hooks/worktreeguard"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// hook is one entry in settings.json. Errors inside a hook are ordinary Go
// errors; Run is where they become a hooks.Result, whose single answer is the
// whole of what a hook has to say.
type hook interface {
	Run(ctx context.Context, in hooks.Payload) hooks.Result
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
			func() hook { return caffeinate.NewStart(caffeinate.Default()) }),
		stopCaffeinateCmd(build),
		leafHookCmd("idle-notify", "Notify unless a subagent is still running", build,
			func() hook { return idlenotify.New(idlenotify.Default()) }),
		leafHookCmd("no-op-wait-guard", "Block a Bash call whose only purpose is to wait", build,
			func() hook { return noopwait.New() }),
		leafHookCmd("skill-frontmatter-check", "Check a SKILL.md that was just saved", build,
			func() hook { return skillcheck.New() }),
		leafHookCmd("slack-notify", "Post the notification to Slack", build,
			func() hook { return slacknotify.New(slacknotify.Default()) }),
		subagentTrackerCmd(build),
		leafHookCmd("worktree-edit-guard", "Block an edit that leaves the current worktree", build,
			func() hook { return worktreeguard.New(runner.Exec{}) }),
		leafHookCmd("terminal-bell", "Ring the terminal bell", build,
			func() hook { return terminalbell.New() }),
	)
	return c
}

// stopCaffeinateCmd is the stop half, registered on four events with two
// flags between them. Neither flag is the ordinary end of a turn, which is why
// the mode with no flag is the one that stops the session's own caffeinate.
func stopCaffeinateCmd(build selfbuild.State) *cobra.Command {
	var agentDone, force bool
	c := leafHookCmd("stop-caffeinate", "Let the machine sleep again", build,
		func() hook {
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
// flag each. cobra rejects a wrong number of them rather than picking one, so
// an entry in settings.json that asks for two, or for none, is a startup error
// and not a marker quietly written for the wrong event.
func subagentTrackerCmd(build selfbuild.State) *cobra.Command {
	var start, stop, sessionEnd bool
	c := leafHookCmd("subagent-tracker", "Track which subagents are running", build,
		func() hook {
			mode := subagents.Start
			switch {
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
	// And one is required. A registration that lost its flag would otherwise
	// track nothing and exit 0, and the only symptom would be idle-notify
	// falling silent — the failure this pair exists to prevent.
	c.MarkFlagsOneRequired("start", "stop", "session-end")
	return c
}

// leafHookCmd wires one hook into the tree. The hook is built at run time
// rather than passed in, so that one with flags sees the values cobra has by
// then parsed into the variables its registration closed over — and so that
// eight discarded subcommands construct no dependencies.
func leafHookCmd(use, short string, build selfbuild.State, newHook func() hook) *cobra.Command {
	return &cobra.Command{
		Use:   use,
		Short: short,
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runHook(c.Context(), newHook(), build,
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
	// A read that fails leaves the same empty payload as input that said
	// nothing, which every hook already handles; see hooks.Parse.
	in, _ := io.ReadAll(stdin)
	result := h.Run(ctx, hooks.Parse(in))

	// Only on the invocation that ran the build; see selfbuild.State.JustFailed.
	// Which channel it goes to follows the decision, because Claude Code only
	// parses the standard output of a hook that exited 0.
	if build.JustFailed {
		if result.Decision == hooks.Allow {
			result.Directive.SystemMessage = join(result.Directive.SystemMessage, buildFailure(build))
		} else {
			result.Message += buildFailure(build) + "\n"
		}
	}

	if result.Message != "" {
		fmt.Fprint(stderr, result.Message)
	}
	if !result.Directive.IsEmpty() {
		// Two strings and two tags: there is no value of Directive that fails
		// to marshal, so this is not a third thing that can go wrong.
		b, _ := json.Marshal(result.Directive)
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
