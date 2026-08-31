// Package terminalbell rings the terminal bell.
//
// Hooks run in a session with no controlling terminal (Claude Code v2.1.139+),
// so a hook cannot write \a to /dev/tty and its standard output does not reach
// the terminal either. The terminalSequence field (v2.1.141+) has Claude Code
// write it instead. tmux takes the BEL as a reason to light the window's tab;
// Ghostty rings nothing by default, which is why the sound is a separate hook.
package terminalbell

import (
	"context"

	"github.com/178inaba/dotfiles/go/internal/hooks"
)

// bel is the character a terminal takes as a bell.
const bel = "\a"

// Hook rings the bell.
type Hook struct{}

// New returns the hook.
func New() Hook { return Hook{} }

// Run implements the hook contract. It reads no input and cannot fail.
func (Hook) Run(context.Context, hooks.Payload) hooks.Result {
	return hooks.Result{Directive: Ring()}
}

// Ring is the directive on its own, for a caller that rings the bell as one
// step of something larger.
func Ring() hooks.Directive {
	return hooks.Directive{TerminalSequence: bel}
}
