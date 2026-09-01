package notify

import (
	"context"

	"github.com/178inaba/dotfiles/go/internal/hooks"
)

const (
	// soundBin plays the notification sound. Ghostty rings no bell of its own,
	// so the terminalSequence lights the tmux tab and this is what is audible.
	// Named absolutely: a hook's PATH is whatever sh -c inherited.
	soundBin = "/usr/bin/afplay"
	// soundFile is macOS's own notification sound.
	soundFile = "/System/Library/Sounds/Ping.aiff"
)

// Idle notifies unless a subagent is running.
//
// Claude Code raises idle_prompt whenever a turn ends with no work left,
// including the turn a parent ends to wait for a subagent it started. That one
// resumes on its own when the agent finishes, so there is nothing for a human
// to do and no reason to interrupt them. This hook is the guard: it notifies
// only when no subagent of the session is running.
//
// Being unable to tell notifies anyway. The failure the user forgives is a
// notification too many; the one they do not is a session waiting in silence.
type Idle struct{ deps Deps }

// NewIdle returns the hook.
func NewIdle(d Deps) Idle { return Idle{deps: d} }

// Run implements the hook contract.
//
// It always allows, and a Slack failure is reported through the directive
// rather than the decision, because Claude Code reads the standard output of a
// hook that exited 0 and no other — reporting the failed post as a status would
// take the bell down with it, which is what the shell version did.
func (h Idle) Run(ctx context.Context, in hooks.Payload) hooks.Result {
	if busy(h.deps, in.SessionID) {
		return hooks.Result{}
	}

	// Detached, not run: the sound is a second and a half long, and waiting for
	// it would hold the bell and the post back by that much. Best effort, since
	// a machine with no sound is still owed both.
	_, _ = h.deps.Sound.Detach(soundBin, soundFile)

	directive := ring()
	if err := post(ctx, h.deps, in); err != nil {
		directive.SystemMessage = undelivered(err)
	}
	return hooks.Result{Directive: directive}
}
