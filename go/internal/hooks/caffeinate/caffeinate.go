// Package caffeinate holds macOS awake while Claude Code is working.
//
// One caffeinate per session, renewed on every tool call, plus one per running
// subagent. The renewal is what makes the suppression safe: an Escape, an API
// error or a crash never reaches the stop hook, so instead of trusting that it
// will, the process is started with a lease and simply stops being renewed.
// Thirty minutes is far longer than a tool call's ten-minute ceiling plus
// thinking, so an active session never expires under its own feet.
//
// Both hooks live here because they share the pid files: one writes them and
// the other is the only thing that reads them.
package caffeinate

import (
	"context"
	"fmt"
	"io"
	"os"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	// bin is named absolutely because a hook's PATH is whatever sh -c
	// inherited, and because this same string identifies the process again in
	// ps output when something comes to stop it.
	bin = "/usr/bin/caffeinate"
	// lease is how long a caffeinate outlives the last thing that renewed it.
	lease = 30 * time.Minute
	// bridgeEnv is set while the session is driven through Remote Control.
	bridgeEnv = "CLAUDE_CODE_BRIDGE_SESSION_ID"
)

// Deps are the seams. Use Default for the real ones.
type Deps struct {
	// Dir is the state tree; see state.Dir.
	Dir       string
	Runner    runner.Runner
	Detacher  runner.Detacher
	Signaller runner.Signaller
	Getppid   func() int
	Getenv    func(string) string
}

// Default wires the real implementations.
func Default() Deps {
	return Deps{
		Dir: state.Dir, Runner: runner.Exec{}, Detacher: runner.Exec{},
		Signaller: runner.Exec{}, Getppid: os.Getppid, Getenv: os.Getenv,
	}
}

// Start begins or renews the suppression.
type Start struct{ deps Deps }

// NewStart returns the hook registered on the events that mean work is
// happening.
func NewStart(d Deps) Start { return Start{deps: d} }

// Run implements the hook contract.
func (s Start) Run(ctx context.Context, in hooks.Payload, stderr io.Writer) hooks.Result {
	store, err := state.Open(s.deps.Dir)
	if err != nil {
		return failed(stderr, err)
	}
	defer store.Close()

	name := state.SessionPID(in.SessionID)
	if in.AgentID != "" {
		name = state.AgentPID(in.SessionID, in.AgentID)
	}

	// Renewal is a replacement: the old process holds the remainder of its own
	// lease and nothing can extend it in place.
	if pid, ok := store.Read(name); ok {
		terminate(ctx, s.deps, pid)
	}

	pid, err := s.deps.Detacher.Detach(bin, s.args(ctx, in)...)
	if err != nil {
		return failed(stderr, err)
	}
	if err := store.Write(name, strconv.Itoa(pid)); err != nil {
		return failed(stderr, err)
	}
	return hooks.Result{}
}

// args is how this caffeinate should be run.
func (s Start) args(ctx context.Context, in hooks.Payload) []string {
	args := []string{"-di"}

	// -w ties the caffeinate to Claude Code itself, so a crash or a SIGKILL
	// takes it down too. Only when the parent really is Claude Code: pointing
	// it at a short-lived parent would end the suppression immediately.
	if pid := s.deps.Getppid(); s.isClaude(ctx, pid) {
		args = append(args, "-w", strconv.Itoa(pid))
	}

	// A session driven through Remote Control holds sleep off with no lease:
	// the host sleeping drops the remote session about ten minutes later, and
	// waiting for a reply from a phone looks exactly like being idle. A
	// subagent is leased regardless, since the session's own caffeinate is
	// already covering that case.
	if in.AgentID != "" || s.deps.Getenv(bridgeEnv) == "" {
		args = append(args, "-t", strconv.Itoa(int(lease.Seconds())))
	}
	return args
}

// isClaude reports whether a process is Claude Code itself.
func (s Start) isClaude(ctx context.Context, pid int) bool {
	out, err := s.deps.Runner.Run(ctx, runner.Command{
		Name: "ps", Args: []string{"-o", "comm=", "-p", strconv.Itoa(pid)},
	})
	if err != nil {
		return false
	}
	switch strings.TrimSpace(strings.ReplaceAll(string(out), " ", "")) {
	case "claude", "node":
		return true
	}
	return false
}

// Mode is which of the events a stop hook was registered on.
type Mode int

const (
	// Session is the end of a turn, a permission prompt or an idle prompt: the
	// session's own caffeinate goes, and finished subagents are collected.
	Session Mode = iota
	// AgentDone is a subagent finishing. It marks rather than kills; see Run.
	AgentDone
	// Force is the end of the session, which takes everything.
	Force
)

// Stop ends the suppression.
type Stop struct {
	deps Deps
	mode Mode
}

// NewStop returns the hook for one mode.
func NewStop(d Deps, mode Mode) Stop { return Stop{deps: d, mode: mode} }

// Run implements the hook contract.
func (s Stop) Run(ctx context.Context, in hooks.Payload, stderr io.Writer) hooks.Result {
	store, err := state.Open(s.deps.Dir)
	if err != nil {
		return failed(stderr, err)
	}
	defer store.Close()

	if s.mode == AgentDone {
		// Marked, not killed. The parent is still reading the agent's result,
		// and the machine sleeping in the middle of that is the gap this
		// closes; the parent's next Stop collects it.
		if in.AgentID == "" {
			return hooks.Result{}
		}
		from := state.AgentPID(in.SessionID, in.AgentID)
		if _, ok := store.Read(from); ok {
			if err := store.Rename(from, state.AgentDone(in.SessionID, in.AgentID)); err != nil {
				return failed(stderr, err)
			}
		}
		return hooks.Result{}
	}

	// The Remote Control exception, and the one thing that overrides it: at
	// the end of the session there is no reply left to wait for.
	if s.mode == Force || s.deps.Getenv(bridgeEnv) == "" {
		s.collect(ctx, store, state.SessionPID(in.SessionID))
	}

	// Finished subagents are collected either way: their work is over, so
	// holding sleep off for them protects nothing.
	suffixes := []string{".done"}
	if s.mode == Force {
		suffixes = append(suffixes, ".pid")
	}
	for _, suffix := range suffixes {
		for _, name := range s.agentFiles(store, in.SessionID, suffix) {
			s.collect(ctx, store, name)
		}
	}
	return hooks.Result{}
}

// agentFiles lists one session's per-agent pid files, sorted so that what a
// stop does is the same from one run to the next.
func (s Stop) agentFiles(store *state.Store, session, suffix string) []string {
	prefix := session + "-"
	var found []string
	for _, name := range store.Names(state.CaffeinateDir) {
		if strings.HasPrefix(name, prefix) && strings.HasSuffix(name, suffix) {
			found = append(found, state.CaffeinateDir+"/"+name)
		}
	}
	slices.Sort(found)
	return found
}

// collect drops a pid file and stops the process it named.
//
// The file goes first, so that a kill that fails leaves no record pointing at
// a process nobody will try again to stop.
func (s Stop) collect(ctx context.Context, store *state.Store, name string) {
	pid, ok := store.Read(name)
	if !ok {
		return
	}
	// Best effort: the file is on its way out either way, and a removal that
	// fails costs one stale record that dirhelper will sweep.
	_ = store.Remove(name)
	terminate(ctx, s.deps, pid)
}

// terminate stops a process, but only once ps has confirmed it really is the
// caffeinate this pid file was written for. A lease that expired on its own
// leaves the file behind, and by the time anything reads it the number may name
// something else entirely.
func terminate(ctx context.Context, d Deps, pid string) {
	if pid == "" {
		return
	}
	out, err := d.Runner.Run(ctx, runner.Command{
		Name: "ps", Args: []string{"-o", "command=", "-p", pid},
	})
	if err != nil || !strings.Contains(string(out), bin) {
		return
	}
	n, err := strconv.Atoi(pid)
	if err != nil {
		return
	}
	// Best effort: the process may have exited between the check and here.
	_ = d.Signaller.Terminate(n)
}

func failed(stderr io.Writer, err error) hooks.Result {
	fmt.Fprintf(stderr, "ccx: the sleep suppression was not updated: %v\n", err)
	return hooks.Result{Decision: hooks.Fail}
}
