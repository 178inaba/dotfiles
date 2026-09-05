// Package noopwait blocks a Bash call whose only purpose is to wait.
//
// What it prevents: instead of ending the turn to wait for a background
// command, task or subagent, repeating `echo idle1` … `echo idle159` every few
// seconds. One session did it 541 times. It wastes tokens, fills the
// transcript, and defeats idle-notify, whose guard assumes a parent that is
// waiting has ended its turn.
//
// It blocks the first call rather than the tenth. There is no thinking inside
// the loop — after the first no-op every iteration is a reflex that increments
// a counter — so the only point at which ending the turn is still an option is
// the first one. That is also why this keeps no state.
package noopwait

import (
	"context"
	"fmt"
	"regexp"
	"strings"
	"sync"

	"github.com/178inaba/dotfiles/go/internal/hooks"
)

// noOpWait matches a command that waits and observes nothing.
//
// It is anchored at both ends, and that is what keeps the pattern honest
// without a list of exceptions: a pipe, a redirection, a boolean operator, a
// dollar sign, a backquote or any semicolon other than the one after a leading
// sleep is simply not in the character set, so a polling loop that runs a real
// command falls outside the match on its own.
//
// The duration takes a fractional part and an s, m or h suffix, since `sleep 5m`
// is the same wait spelled differently. The token is a short bare word, quoted
// or not — quoting a word does not stop it being a no-op. A trailing semicolon,
// and no-ops of other shapes such as pwd or git status, are out of scope: if
// the shape changes, the answer is to report it to the harness rather than to
// chase it here.
//
// Compiled on first use rather than at init: this is the largest package
// initialiser in the binary, and every ccx invocation — ten hooks and the
// status line — would otherwise pay for it to answer one hook's question about
// Bash calls.
var noOpWait = sync.OnceValue(func() *regexp.Regexp {
	const (
		duration = `([0-9]+(\.[0-9]+)?|\.[0-9]+)[smh]?`
		word     = `[A-Za-z0-9_-]{1,24}`
	)
	token := `(` + word + `|'` + word + `'|"` + word + `")`
	noOp := `((echo|printf)( ` + token + `)?|true|:)`
	return regexp.MustCompile(`^(sleep ` + duration + `( ?; ?` + noOp + `)?|` + noOp + `)$`)
})

// message is what the model is told instead. Blocking without saying what to do
// instead only moves the loop to another no-op.
//
// English, like every other message this module writes and like the two other
// guards a blocked tool call can produce. The reader is the model, not the
// user.
const message = `Blocked: this command does nothing but wait for something else to finish.

  command: %s

Fix: end the turn and wait there instead. A background command, task or subagent
sends a notification when it finishes, and the session resumes from that on its
own.

Reaching for a different no-op — pwd, git status, true — is the same busy-wait
and not a way round this. If you need the result within this turn, run the work
in the foreground rather than the background, and bound the wait with the Bash
tool's timeout parameter (up to 600000ms).
`

// Hook is the guard.
type Hook struct{}

// New returns the hook.
func New() Hook { return Hook{} }

// Run implements the hook contract.
func (Hook) Run(_ context.Context, in hooks.Payload) hooks.Result {
	if in.ToolName != "Bash" || in.Command == "" {
		return hooks.Result{}
	}
	// Runs of whitespace collapse to one space, newlines included: an echo on
	// its own line in a longer script still leaves the other lines to fail the
	// anchors, while splitting `sleep 1;` from `echo w` across a newline stops
	// being a way around them.
	if !noOpWait().MatchString(strings.Join(strings.Fields(in.Command), " ")) {
		return hooks.Result{}
	}

	return hooks.Result{Decision: hooks.Block, Message: fmt.Sprintf(message, in.Command)}
}
