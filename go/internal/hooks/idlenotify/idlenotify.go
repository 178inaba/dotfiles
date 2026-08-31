// Package idlenotify notifies the user that a session is waiting for them.
//
// Claude Code raises idle_prompt whenever a turn ends with no work left,
// including the turn a parent ends to wait for a subagent it started. That one
// resumes on its own when the agent finishes, so there is nothing for a human
// to do and no reason to interrupt them. This hook is the guard: it notifies
// only when no subagent of the session is running.
//
// Being unable to tell notifies anyway. The failure the user forgives is a
// notification too many; the one they do not is a session waiting in silence.
package idlenotify

import (
	"context"
	"fmt"
	"io"
	"strconv"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/slacknotify"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
	"github.com/178inaba/dotfiles/go/internal/hooks/terminalbell"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	// soundBin plays the notification sound. Ghostty rings no bell of its own,
	// so the terminalSequence lights the tmux tab and this is what is audible.
	soundBin = "/usr/bin/afplay"
	// soundFile is macOS's own notification sound.
	soundFile = "/System/Library/Sounds/Ping.aiff"
)

// Deps are the seams. Use Default for the real ones.
type Deps struct {
	// Dir is the state tree; see state.Dir.
	Dir       string
	Runner    runner.Runner
	Signaller runner.Signaller
	Slack     slacknotify.Deps
}

// Default wires the real implementations.
func Default() Deps {
	return Deps{
		Dir:       state.Dir,
		Runner:    runner.Exec{},
		Signaller: runner.Exec{},
		Slack:     slacknotify.Default(),
	}
}

// Hook notifies unless a subagent is running.
type Hook struct{ deps Deps }

// New returns the hook.
func New(d Deps) Hook { return Hook{deps: d} }

// Run implements the hook contract.
//
// It always allows, and a Slack failure is reported through the directive
// rather than the exit status, because Claude Code reads the standard output of
// a hook that exited 0 and no other — exiting non-zero to report the failed
// post would take the bell down with it, which is what the shell version did.
func (h Hook) Run(ctx context.Context, in hooks.Payload, _ io.Writer) hooks.Result {
	if h.busy(in.SessionID) {
		return hooks.Result{}
	}

	// Best effort: a machine with no sound is still owed the bell and the post.
	_, _ = h.deps.Runner.Run(ctx, runner.Command{Name: soundBin, Args: []string{soundFile}})

	directive := terminalbell.Ring()
	if err := slacknotify.Post(ctx, h.deps.Slack, in); err != nil {
		directive.SystemMessage = fmt.Sprintf("ccx: the Slack notification was not delivered: %v", err)
	}
	return hooks.Result{Directive: directive}
}

// busy reports whether the session has a subagent still running.
func (h Hook) busy(session string) bool {
	s, err := state.Open(h.deps.Dir)
	if err != nil {
		// No way to check is a reason to notify, not to stay quiet.
		return false
	}
	defer s.Close()

	for _, agent := range s.Names(state.MarkerDir(session)) {
		watched, ok := s.Read(state.Marker(session, agent))
		if !ok {
			continue
		}
		// A marker recording no pid is one whose writer could not identify
		// Claude Code, so there is nothing to check and it counts as running.
		if watched == "" {
			return true
		}
		pid, err := strconv.Atoi(watched)
		if err != nil || h.deps.Signaller.Alive(pid) {
			return true
		}
		// A marker whose process has gone is what a session that crashed left
		// behind, and honouring it would silence this session for good.
	}
	return false
}
