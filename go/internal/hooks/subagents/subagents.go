// Package subagents keeps one marker per running subagent, which is how
// idle-notify tells a session waiting for a human from one waiting for an agent
// it started.
//
// The caffeinate pid files are not reused for this. They belong to sleep
// suppression, whose lifecycle can be changed for reasons that have nothing to
// do with notifications, and a leftover pid file would push the notification
// towards silence — the direction that loses the notification the user needed.
package subagents

import (
	"context"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Mode is the event the tracker was registered on. The zero value does
// nothing, which is what the shell's case statement did with an argument it did
// not recognise.
type Mode int

const (
	// None is a registration with no flag.
	None Mode = iota
	// Start records that a subagent is running.
	Start
	// Stop forgets one that has finished.
	Stop
	// SessionEnd forgets the whole session.
	SessionEnd
)

// Deps are the seams. Use Default for the real ones.
type Deps struct {
	// Dir is the state tree; see state.Dir.
	Dir     string
	Runner  runner.Runner
	Getppid func() int
}

// Default wires the real implementations.
func Default() Deps {
	return Deps{Dir: state.Dir, Runner: runner.Exec{}, Getppid: os.Getppid}
}

// Hook manages the markers.
type Hook struct {
	deps Deps
	mode Mode
}

// New returns the hook for one mode.
func New(d Deps, mode Mode) Hook { return Hook{deps: d, mode: mode} }

// Run implements the hook contract.
func (h Hook) Run(ctx context.Context, in hooks.Payload, stderr io.Writer) hooks.Result {
	// Every mode but SessionEnd is about one agent, and an event that names
	// none is not about a subagent at all.
	if h.mode == None || (h.mode != SessionEnd && in.AgentID == "") {
		return hooks.Result{}
	}

	s, err := state.Open(h.deps.Dir)
	if err != nil {
		fmt.Fprintf(stderr, "ccx: the subagent markers are unreachable: %v\n", err)
		return hooks.Result{Decision: hooks.Fail}
	}
	defer s.Close()

	switch h.mode {
	case Start:
		err = s.Write(state.Marker(in.SessionID, in.AgentID), h.watched(ctx))
	case Stop:
		err = s.Remove(state.Marker(in.SessionID, in.AgentID))
	case SessionEnd:
		err = s.RemoveAll(state.MarkerDir(in.SessionID))
	}
	if err != nil {
		fmt.Fprintf(stderr, "ccx: the subagent marker was not updated: %v\n", err)
		return hooks.Result{Decision: hooks.Fail}
	}
	return hooks.Result{}
}

// watched is the process a marker records, so that idle-notify can tell a
// running subagent from the residue of a session that crashed.
//
// Empty when the parent is not Claude Code itself, which reads as "cannot
// check" rather than "gone": a marker nobody can verify keeps the session
// quiet, and silence is the failure the user forgives.
func (h Hook) watched(ctx context.Context) string {
	pid := h.deps.Getppid()
	out, err := h.deps.Runner.Run(ctx, runner.Command{
		Name: "ps", Args: []string{"-o", "comm=", "-p", strconv.Itoa(pid)},
	})
	if err != nil {
		return ""
	}
	switch strings.TrimSpace(strings.ReplaceAll(string(out), " ", "")) {
	case "claude", "node":
		return strconv.Itoa(pid)
	}
	return ""
}
