package subagents

import (
	"context"
	"path/filepath"
	"slices"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/hooktest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// The pids fakeSignaller reports on.
const (
	livePID = "111"
	deadPID = "222"
)

func TestRun(t *testing.T) {
	t.Parallel()

	const session = "s1"
	started := []string{"a1", "a2"}

	tests := []struct {
		name string
		mode Mode
		in   hooks.Payload
		// parent is what ps says this process's parent is called.
		parent string

		wantMarkers []string
		wantWatched string
	}{
		{
			name: "start records the agent",
			mode: Start, in: hooks.Payload{SessionID: session, AgentID: "a3"},
			wantMarkers: []string{"a1", "a2", "a3"},
		},
		{
			// The marker records Claude Code's own pid so that idle-notify can
			// spot the residue of a session that crashed.
			name: "start records the watched process when the parent is Claude Code",
			mode: Start, in: hooks.Payload{SessionID: session, AgentID: "a3"},
			parent:      "claude",
			wantMarkers: []string{"a1", "a2", "a3"}, wantWatched: "4242",
		},
		{
			name: "start records the watched process when the parent is node",
			mode: Start, in: hooks.Payload{SessionID: session, AgentID: "a3"},
			parent:      "node",
			wantMarkers: []string{"a1", "a2", "a3"}, wantWatched: "4242",
		},
		{
			// Nothing to verify beats a pid that is not Claude Code's: an
			// unverifiable marker keeps the session quiet.
			name: "start records nothing when the parent is something else",
			mode: Start, in: hooks.Payload{SessionID: session, AgentID: "a3"},
			parent:      "bash",
			wantMarkers: []string{"a1", "a2", "a3"}, wantWatched: "",
		},
		{
			name: "start without an agent does nothing",
			mode: Start, in: hooks.Payload{SessionID: session},
			wantMarkers: started,
		},
		{
			name: "stop forgets one agent and leaves its siblings",
			mode: Stop, in: hooks.Payload{SessionID: session, AgentID: "a1"},
			wantMarkers: []string{"a2"},
		},
		{
			name: "stop of an agent that was never started is not an error",
			mode: Stop, in: hooks.Payload{SessionID: session, AgentID: "a9"},
			wantMarkers: started,
		},
		{
			name: "stop without an agent does nothing",
			mode: Stop, in: hooks.Payload{SessionID: session},
			wantMarkers: started,
		},
		{
			name: "session end forgets the whole session",
			mode: SessionEnd, in: hooks.Payload{SessionID: session},
			wantMarkers: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			seed(t, dir, session, started)

			h := New(Deps{
				Dir:     dir,
				Runner:  fixedRunner{out: tt.parent},
				Getppid: func() int { return 4242 },
			}, tt.mode)

			if got, want := h.Run(t.Context(), tt.in), (hooks.Result{}); got != want {
				t.Fatalf("Run() = %+v, want %+v", got, want)
			}

			s := hooktest.OpenStore(t, dir)
			got := hooktest.Names(t, s, markerDir(session))
			slices.Sort(got)
			if !slices.Equal(got, tt.wantMarkers) {
				t.Errorf("markers = %v, want %v", got, tt.wantMarkers)
			}
			if tt.mode == Start && tt.in.AgentID != "" {
				if watched, _ := s.Read(marker(session, tt.in.AgentID)); watched != tt.wantWatched {
					t.Errorf("marker contents = %q, want %q", watched, tt.wantWatched)
				}
			}
		})
	}
}

func seed(t *testing.T, dir, session string, agents []string) {
	t.Helper()
	s := hooktest.OpenStore(t, dir)
	for _, a := range agents {
		if err := s.Write(marker(session, a), ""); err != nil {
			t.Fatalf("Write(%s): %v", a, err)
		}
	}
}

// fixedRunner answers ps with one name, or fails when it has none.
type fixedRunner struct{ out string }

func (f fixedRunner) Run(context.Context, runner.Command) ([]byte, error) {
	if f.out == "" {
		return nil, &runner.Error{Name: "ps", Err: context.Canceled}
	}
	return []byte(f.out + "\n"), nil
}

func TestBusy(t *testing.T) {
	t.Parallel()

	const session = "s1"
	tests := []struct {
		name string
		// markers is agent id to the pid it records.
		markers map[string]string
		want    bool
	}{
		{name: "no markers at all", markers: nil},
		{name: "a running agent", markers: map[string]string{"a1": livePID}, want: true},
		{
			// A marker whose process is gone is what a crash leaves behind.
			// Honouring it would keep the session quiet for good.
			name: "the residue of a crashed session", markers: map[string]string{"a1": deadPID},
		},
		{
			name: "a marker recording no pid", markers: map[string]string{"a1": ""}, want: true,
		},
		{
			name:    "one running agent among stale ones",
			markers: map[string]string{"a1": deadPID, "a2": livePID}, want: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			s := hooktest.OpenStore(t, dir)
			for agent, pid := range tt.markers {
				if err := s.Write(marker(session, agent), pid); err != nil {
					t.Fatalf("Write(%s): %v", agent, err)
				}
			}

			d := Deps{Dir: dir, Signaller: fakeSignaller{}}
			if got := Busy(d, session); got != tt.want {
				t.Errorf("Busy = %t, want %t", got, tt.want)
			}
		})
	}
}

// fakeSignaller knows one live process.
type fakeSignaller struct{}

func (fakeSignaller) Terminate(int) error { return nil }
func (fakeSignaller) Alive(pid int) bool  { return pid == 111 }
