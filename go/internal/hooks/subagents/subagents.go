// Package subagents keeps one marker per running subagent, which is how
// idle-notify tells a session waiting for a human from one waiting for an agent
// it started.
//
// The caffeinate pid files are not reused for this. They belong to sleep
// suppression, whose lifecycle can be changed for reasons that have nothing to
// do with notifications, and a leftover pid file would push the notification
// towards silence — the direction that loses the notification the user needed.
//
// A marker records the pid of Claude Code itself, or nothing when the writer
// could not identify it. Busy is the only reader of that format, so the two
// halves stay here together.
package subagents

import (
	"context"
	"fmt"
	"os"
	"path"
	"strconv"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// dir holds one directory per session, holding one marker per subagent.
const dir = "subagents"

// markerDir names the directory holding one session's markers.
func markerDir(session string) string { return path.Join(dir, session) }

// marker names the file whose existence says a subagent is running.
func marker(session, agent string) string { return path.Join(markerDir(session), agent) }

// Mode is the event the tracker was registered on.
type Mode int

const (
	// Start records that a subagent is running.
	Start Mode = iota
	// Stop forgets one that has finished.
	Stop
	// SessionEnd forgets the whole session.
	SessionEnd
)

// Deps are the seams. Use Default for the real ones.
type Deps struct {
	// Dir is the state tree; see state.Dir.
	Dir       string
	Runner    runner.Runner
	Signaller runner.Signaller
	Getppid   func() int
}

// Default wires the real implementations.
func Default() Deps {
	return Deps{Dir: state.Dir, Runner: runner.Exec{}, Signaller: runner.Exec{}, Getppid: os.Getppid}
}

// Hook manages the markers.
type Hook struct {
	deps Deps
	mode Mode
}

// New returns the hook for one mode.
func New(d Deps, mode Mode) Hook { return Hook{deps: d, mode: mode} }

// Run implements the hook contract.
func (h Hook) Run(ctx context.Context, in hooks.Payload) hooks.Result {
	// Start and Stop are about one agent, and an event that names none is not
	// about a subagent at all.
	if h.mode != SessionEnd && in.AgentID == "" {
		return hooks.Result{}
	}

	s, err := state.Open(h.deps.Dir)
	if err != nil {
		return failed(err)
	}
	defer s.Close()

	switch h.mode {
	case Start:
		err = s.Write(marker(in.SessionID, in.AgentID), h.watched(ctx))
	case Stop:
		err = s.Remove(marker(in.SessionID, in.AgentID))
	case SessionEnd:
		err = s.RemoveAll(markerDir(in.SessionID))
	}
	if err != nil {
		return failed(err)
	}
	return hooks.Result{}
}

// watched is what a marker records, so that Busy can tell a running subagent
// from the residue of a session that crashed.
//
// Empty when the parent is not Claude Code itself, which Busy reads as "cannot
// check" rather than "gone".
func (h Hook) watched(ctx context.Context) string {
	pid := h.deps.Getppid()
	if !hooks.IsClaude(ctx, h.deps.Runner, pid) {
		return ""
	}
	return strconv.Itoa(pid)
}

// Busy reports whether the session has a subagent still running.
//
// Anything it cannot answer counts as not busy. This decides whether the user
// is notified, and the failure they forgive is one notification too many.
func Busy(d Deps, session string) bool {
	s, err := state.Open(d.Dir)
	if err != nil {
		return false
	}
	defer s.Close()

	// Discarded deliberately: Busy answers a question about notifying, and
	// anything it cannot determine already counts as not busy.
	markers, _ := s.Names(markerDir(session))
	for _, agent := range markers {
		watched, ok := s.Read(marker(session, agent))
		if !ok {
			continue
		}
		// A marker recording no pid is one whose writer could not identify
		// Claude Code, so there is nothing to check and it counts as running.
		if watched == "" {
			return true
		}
		pid, err := strconv.Atoi(watched)
		if err != nil || d.Signaller.Alive(pid) {
			return true
		}
		// A marker whose process has gone is what a session that crashed left
		// behind, and honouring it would silence this session for good.
	}
	return false
}

func failed(err error) hooks.Result {
	return hooks.Result{
		Decision: hooks.Fail,
		Message:  fmt.Sprintf("ccx: the subagent marker was not updated: %v\n", err),
	}
}
