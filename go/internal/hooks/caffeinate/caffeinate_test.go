package caffeinate

import (
	"context"
	"fmt"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"sync"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	session  = "s1"
	agent    = "a1"
	started  = 9000 // what the fake Detacher hands back
	claudePI = 4242 // the parent, when it is Claude Code
)

func TestDefaultUsesTheStateTree(t *testing.T) {
	t.Parallel()
	// The pid directory was an environment seam; production wiring is now the
	// only thing that says where the files go.
	if got, want := Default().Dir, state.Dir; got != want {
		t.Errorf("Dir = %q, want %q", got, want)
	}
}

func TestStart(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		in   hooks.Payload
		// seed is the pid file already there, and running holds the pids ps
		// reports as caffeinate.
		seed    map[string]string
		running []int
		parent  string
		bridge  bool

		wantArgs   []string
		wantFile   string
		wantKilled []int
	}{
		{
			name: "the first prompt of a session starts one",
			in:   hooks.Payload{SessionID: session},
			// No -w: the parent is not Claude Code, and tying the lifetime to
			// a short-lived parent would end the suppression at once.
			wantArgs: []string{"-di", "-t", "1800"},
			wantFile: state.SessionPID(session),
		},
		{
			// Every tool call renews the lease, which is what makes an Escape
			// or an API error expire rather than suppress sleep for ever.
			name: "a renewal replaces the process that was running",
			in:   hooks.Payload{SessionID: session},
			seed: map[string]string{state.SessionPID(session): "111"}, running: []int{111},
			wantArgs: []string{"-di", "-t", "1800"}, wantFile: state.SessionPID(session),
			wantKilled: []int{111},
		},
		{
			// The lease expires on its own, and the pid file outlives it. By
			// then the number may belong to something else entirely.
			name:     "a pid that is no longer caffeinate is left alone",
			in:       hooks.Payload{SessionID: session},
			seed:     map[string]string{state.SessionPID(session): "111"},
			wantArgs: []string{"-di", "-t", "1800"}, wantFile: state.SessionPID(session),
		},
		{
			name:     "a Claude Code parent ties the lifetime to the session",
			in:       hooks.Payload{SessionID: session},
			parent:   "claude",
			wantArgs: []string{"-di", "-w", strconv.Itoa(claudePI), "-t", "1800"},
			wantFile: state.SessionPID(session),
		},
		{
			name:     "a node parent counts as Claude Code too",
			in:       hooks.Payload{SessionID: session},
			parent:   "node",
			wantArgs: []string{"-di", "-w", strconv.Itoa(claudePI), "-t", "1800"},
			wantFile: state.SessionPID(session),
		},
		{
			// A host that sleeps drops the remote session about ten minutes
			// later, so a session being driven from a phone holds sleep off
			// for as long as it is connected.
			name: "Remote Control leaves the session's caffeinate with no lease",
			in:   hooks.Payload{SessionID: session}, bridge: true,
			wantArgs: []string{"-di"}, wantFile: state.SessionPID(session),
		},
		{
			// A subagent runs on past the parent's Stop, so it holds its own.
			// The session's caffeinate already covers the Remote Control case,
			// so this one is always leased.
			name: "a subagent gets its own leased caffeinate even under Remote Control",
			in:   hooks.Payload{SessionID: session, AgentID: agent}, bridge: true,
			wantArgs: []string{"-di", "-t", "1800"}, wantFile: state.AgentPID(session, agent),
		},
		{
			name:     "a payload with no session still has somewhere to write",
			in:       hooks.Payload{SessionID: "unknown"},
			wantArgs: []string{"-di", "-t", "1800"}, wantFile: state.SessionPID("unknown"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			seed(t, dir, tt.seed)
			p := &fakeProc{parent: tt.parent, running: tt.running}

			var stderr strings.Builder
			got := NewStart(deps(dir, p, tt.bridge)).Run(t.Context(), tt.in, &stderr)
			if got.Decision != hooks.Allow || stderr.Len() != 0 {
				t.Fatalf("Decision = %d, stderr = %q, want %d and nothing", got.Decision, stderr.String(), hooks.Allow)
			}

			if !slices.Equal(p.detached, tt.wantArgs) {
				t.Errorf("caffeinate args = %v, want %v", p.detached, tt.wantArgs)
			}
			if !slices.Equal(p.killed, tt.wantKilled) {
				t.Errorf("killed = %v, want %v", p.killed, tt.wantKilled)
			}
			if pid, ok := open(t, dir).Read(tt.wantFile); !ok || pid != strconv.Itoa(started) {
				t.Errorf("%s = %q, %t, want %q, true", tt.wantFile, pid, ok, strconv.Itoa(started))
			}
		})
	}
}

func TestStop(t *testing.T) {
	t.Parallel()

	var (
		sessionFile = state.SessionPID(session)
		liveFile    = state.AgentPID(session, "live")
		doneFile    = state.AgentDone(session, "done")
		otherFile   = state.SessionPID("other")
	)
	// One session with its own caffeinate, one subagent still running and one
	// that has finished, plus another session that must not be touched.
	full := map[string]string{sessionFile: "11", liveFile: "22", doneFile: "33", otherFile: "44"}
	allRunning := []int{11, 22, 33, 44}

	tests := []struct {
		name   string
		mode   Mode
		in     hooks.Payload
		seed   map[string]string
		bridge bool

		wantKilled []int
		wantLeft   []string
	}{
		{
			name: "the turn ends, so the session's own caffeinate goes",
			mode: Session, in: hooks.Payload{SessionID: session},
			seed: full, wantKilled: []int{11, 33},
			// The running subagent keeps its own: it is still working, and
			// letting the machine sleep would freeze it.
			wantLeft: []string{"other.pid", "s1-live.pid"},
		},
		{
			name: "stopping what was never started is not an error",
			mode: Session, in: hooks.Payload{SessionID: session},
		},
		{
			// The lease expires on its own and the pid file outlives it, by
			// which time the number may belong to something else entirely.
			name: "a pid that is no longer caffeinate is dropped, not killed",
			mode: Session, in: hooks.Payload{SessionID: session},
			seed: map[string]string{sessionFile: "99"},
		},
		{
			name: "Remote Control keeps the session awake but still collects finished agents",
			mode: Session, in: hooks.Payload{SessionID: session}, bridge: true,
			seed: full, wantKilled: []int{33},
			wantLeft: []string{"other.pid", "s1-live.pid", "s1.pid"},
		},
		{
			// The session is over, so the Remote Control exception has nothing
			// left to protect.
			name: "the end of the session overrides Remote Control and takes everything",
			mode: Force, in: hooks.Payload{SessionID: session}, bridge: true,
			seed: full, wantKilled: []int{11, 33, 22},
			wantLeft: []string{"other.pid"},
		},
		{
			// The parent is still reading the agent's result, so the machine
			// must not sleep yet; the parent's next Stop collects this.
			name: "a finished subagent is marked rather than killed",
			mode: AgentDone, in: hooks.Payload{SessionID: session, AgentID: "live"},
			seed: full, wantLeft: []string{"other.pid", "s1-done.done", "s1-live.done", "s1.pid"},
		},
		{
			name: "marking an agent that was never started is not an error",
			mode: AgentDone, in: hooks.Payload{SessionID: session, AgentID: "ghost"},
			seed: full, wantLeft: []string{"other.pid", "s1-done.done", "s1-live.pid", "s1.pid"},
		},
		{
			name: "an event that names no agent has nothing to mark",
			mode: AgentDone, in: hooks.Payload{SessionID: session},
			seed: full, wantLeft: []string{"other.pid", "s1-done.done", "s1-live.pid", "s1.pid"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			seed(t, dir, tt.seed)
			p := &fakeProc{running: allRunning}

			var stderr strings.Builder
			got := NewStop(deps(dir, p, tt.bridge), tt.mode).Run(t.Context(), tt.in, &stderr)
			if got.Decision != hooks.Allow || stderr.Len() != 0 {
				t.Fatalf("Decision = %d, stderr = %q, want %d and nothing", got.Decision, stderr.String(), hooks.Allow)
			}

			if !slices.Equal(p.killed, tt.wantKilled) {
				t.Errorf("killed = %v, want %v", p.killed, tt.wantKilled)
			}
			left := open(t, dir).Names(state.CaffeinateDir)
			slices.Sort(left)
			if !slices.Equal(left, tt.wantLeft) {
				t.Errorf("pid files left = %v, want %v", left, tt.wantLeft)
			}
		})
	}
}

func deps(dir string, p *fakeProc, bridge bool) Deps {
	return Deps{
		Dir: dir, Runner: p, Detacher: p, Signaller: p,
		Getppid: func() int { return claudePI },
		Getenv: func(string) string {
			if bridge {
				return "bridge-1"
			}
			return ""
		},
	}
}

func seed(t *testing.T, dir string, files map[string]string) {
	t.Helper()
	s := open(t, dir)
	for name, pid := range files {
		if err := s.Write(name, pid); err != nil {
			t.Fatalf("Write(%s): %v", name, err)
		}
	}
}

func open(t *testing.T, dir string) *state.Store {
	t.Helper()
	s, err := state.Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })
	return s
}

// fakeProc stands in for ps, for starting a caffeinate and for signalling one.
type fakeProc struct {
	// parent is what ps calls this process's parent.
	parent string
	// running holds the pids ps reports as a caffeinate.
	running []int

	mu       sync.Mutex
	detached []string
	killed   []int
}

func (f *fakeProc) Run(_ context.Context, c runner.Command) ([]byte, error) {
	pid, err := strconv.Atoi(c.Args[len(c.Args)-1])
	if err != nil {
		return nil, fmt.Errorf("ps: %q is not a pid", c.Args[len(c.Args)-1])
	}
	// -o comm= names the parent, -o command= identifies a pid as caffeinate.
	if slices.Contains(c.Args, "comm=") {
		if f.parent == "" {
			return nil, fmt.Errorf("ps: no such process")
		}
		return []byte(f.parent + "\n"), nil
	}
	if !slices.Contains(f.running, pid) {
		return nil, fmt.Errorf("ps: no such process")
	}
	return []byte(bin + " -di -t 1800\n"), nil
}

func (f *fakeProc) Detach(_ string, args ...string) (int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.detached = args
	return started, nil
}

func (f *fakeProc) Terminate(pid int) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.killed = append(f.killed, pid)
	return nil
}

func (f *fakeProc) Alive(int) bool { return true }
