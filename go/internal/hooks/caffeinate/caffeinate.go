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
	"os"
	"path"
	"slices"
	"strconv"
	"strings"
	"sync"
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

	// dir holds one pid file per running caffeinate, and running is the suffix
	// they carry until the subagent that owns one finishes; see Stop.Run.
	dir     = "caffeinate"
	running = ".pid"
	done    = ".done"
)

// sessionPID names the pid file of the caffeinate held for a whole session.
func sessionPID(session string) string { return path.Join(dir, session+running) }

// agentPID names the pid file of the caffeinate held for one running subagent.
//
// The session and the agent are joined by a hyphen with nothing to tell them
// apart, so a session literally named "<other session>-<agent>" would collide
// with that agent's file. Claude Code issues both as fixed-length ids, which
// leaves no way to write one that is another with an agent appended.
func agentPID(session, agent string) string {
	return path.Join(dir, session+"-"+agent+running)
}

// agentDone names the same file after the subagent has finished.
func agentDone(session, agent string) string {
	return strings.TrimSuffix(agentPID(session, agent), running) + done
}

// Proc is everything these hooks need of the machine's processes.
//
// Implementations must be safe for concurrent use: Start asks two independent
// questions of it at once, since a fork costs more than everything else the
// hook does put together.
type Proc interface {
	runner.Runner
	runner.Detacher
	runner.Signaller
}

// Deps are the seams. Use Default for the real ones.
type Deps struct {
	// Dir is the state tree; see state.Dir.
	Dir     string
	Proc    Proc
	Getppid func() int
	Getenv  func(string) string
}

// Default wires the real implementations.
func Default() Deps {
	return Deps{Dir: state.Dir, Proc: runner.Exec{}, Getppid: os.Getppid, Getenv: os.Getenv}
}

// Start begins or renews the suppression.
type Start struct{ deps Deps }

// NewStart returns the hook registered on the events that mean work is
// happening.
func NewStart(d Deps) Start { return Start{deps: d} }

// Run implements the hook contract.
func (s Start) Run(ctx context.Context, in hooks.Payload) hooks.Result {
	store, err := state.Open(s.deps.Dir)
	if err != nil {
		return failed(err)
	}
	defer store.Close()

	name := sessionPID(in.SessionID)
	if in.AgentID != "" {
		name = agentPID(in.SessionID, in.AgentID)
	}
	old, hadOld := store.Read(name)

	// The two ps invocations ask independent questions, so they wait together.
	var args []string
	var wg sync.WaitGroup
	wg.Go(func() { args = s.args(ctx, in) })
	// Renewal is a replacement: the old process holds the remainder of its own
	// lease and nothing can extend it in place.
	if hadOld {
		if pid, ok := parsePID(old); ok {
			terminate(ctx, s.deps, pid)
		}
	}
	wg.Wait()

	pid, err := s.deps.Proc.Detach(bin, args...)
	if err != nil {
		return failed(err)
	}
	if err := store.Write(name, strconv.Itoa(pid)); err != nil {
		return failed(err)
	}
	return hooks.Result{}
}

// args is how this caffeinate should be run.
func (s Start) args(ctx context.Context, in hooks.Payload) []string {
	args := []string{"-di"}

	// -w ties the caffeinate to Claude Code itself, so a crash or a SIGKILL
	// takes it down too. Only when the parent really is Claude Code: pointing
	// it at a short-lived parent would end the suppression immediately.
	if pid := s.deps.Getppid(); hooks.IsClaude(ctx, s.deps.Proc, pid) {
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
func (s Stop) Run(ctx context.Context, in hooks.Payload) hooks.Result {
	// Decided before the state tree is opened: an event that names no agent
	// gives this nothing to do, and creating a directory to discover that is
	// work for nothing on a hook that runs constantly.
	if s.mode == AgentDone && in.AgentID == "" {
		return hooks.Result{}
	}

	store, err := state.Open(s.deps.Dir)
	if err != nil {
		return failed(err)
	}
	defer store.Close()

	if s.mode == AgentDone {
		// Marked, not killed. The parent is still reading the agent's result,
		// and the machine sleeping in the middle of that is the gap this
		// closes; the parent's next Stop collects it.
		from := agentPID(in.SessionID, in.AgentID)
		if _, ok := store.Read(from); ok {
			if err := store.Rename(from, agentDone(in.SessionID, in.AgentID)); err != nil {
				return failed(err)
			}
		}
		return hooks.Result{}
	}

	// The Remote Control exception, and the one thing that overrides it: at
	// the end of the session there is no reply left to wait for.
	if s.mode == Force || s.deps.Getenv(bridgeEnv) == "" {
		s.collect(ctx, store, sessionPID(in.SessionID))
	}

	// Finished subagents are collected either way: their work is over, so
	// holding sleep off for them protects nothing. The ones still running keep
	// theirs unless the session itself is ending.
	suffixes := []string{done}
	if s.mode == Force {
		suffixes = append(suffixes, running)
	}
	for _, name := range agentFiles(store, in.SessionID, suffixes) {
		s.collect(ctx, store, name)
	}
	return hooks.Result{}
}

// agentFiles lists one session's per-agent pid files, in suffix order and
// sorted within it, so that what a stop does is the same from one run to the
// next. The directory is read once however many suffixes are asked for.
func agentFiles(store *state.Store, session string, suffixes []string) []string {
	prefix := session + "-"
	// Discarded deliberately: a listing that fails leaves the pid files behind
	// for the next stop, and their leases expire regardless. Reporting it would
	// turn a stop hook that has nothing to collect into one that failed.
	entries, _ := store.Names(dir)

	var found []string
	for _, suffix := range suffixes {
		var batch []string
		for _, name := range entries {
			if strings.HasPrefix(name, prefix) && strings.HasSuffix(name, suffix) {
				batch = append(batch, path.Join(dir, name))
			}
		}
		slices.Sort(batch)
		found = append(found, batch...)
	}
	return found
}

// collect drops a pid file and stops the process it named.
//
// The file goes first, so that a kill that fails leaves no record pointing at
// a process nobody will try again to stop.
func (s Stop) collect(ctx context.Context, store *state.Store, name string) {
	raw, ok := store.Read(name)
	if !ok {
		return
	}
	// Best effort: the file is on its way out either way, and a removal that
	// fails costs one stale record that dirhelper will sweep. It goes even when
	// the contents make no sense, since no later run could act on them either.
	_ = store.Remove(name)
	if pid, ok := parsePID(raw); ok {
		terminate(ctx, s.deps, pid)
	}
}

// parsePID reads what a pid file holds. Nothing but this package writes these,
// so a value that is not a number is a truncated write rather than a format to
// accommodate — and there is no process to go looking for.
func parsePID(raw string) (int, bool) {
	pid, err := strconv.Atoi(strings.TrimSpace(raw))
	return pid, err == nil
}

// terminate stops a process, but only once ps has confirmed it really is the
// caffeinate this pid file was written for. A lease that expired on its own
// leaves the file behind, and by the time anything reads it the number may name
// something else entirely.
func terminate(ctx context.Context, d Deps, pid int) {
	out, err := d.Proc.Run(ctx, runner.Command{
		Name: "ps", Args: []string{"-o", "command=", "-p", strconv.Itoa(pid)},
	})
	if err != nil || !strings.Contains(string(out), bin) {
		return
	}
	// Best effort: the process may have exited between the check and here.
	_ = d.Proc.Terminate(pid)
}

func failed(err error) hooks.Result {
	return hooks.Result{
		Decision: hooks.Fail,
		Message:  fmt.Sprintf("ccx: the sleep suppression was not updated: %v\n", err),
	}
}
