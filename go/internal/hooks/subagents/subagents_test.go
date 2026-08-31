package subagents

import (
	"context"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
	"github.com/178inaba/dotfiles/go/internal/runner"
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
		{
			// A registration with no flag, and the payload a broken input
			// produces: neither is about a subagent.
			name: "no mode does nothing",
			mode: None, in: hooks.Payload{SessionID: session, AgentID: "a1"},
			wantMarkers: started,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			seed(t, dir, session, started)

			var stderr strings.Builder
			h := New(Deps{
				Dir:     dir,
				Runner:  fixedRunner{out: tt.parent},
				Getppid: func() int { return 4242 },
			}, tt.mode)

			if got := h.Run(t.Context(), tt.in, &stderr); got.Decision != hooks.Allow {
				t.Fatalf("Decision = %d, want %d (stderr=%q)", got.Decision, hooks.Allow, stderr.String())
			}
			if stderr.Len() != 0 {
				t.Errorf("stderr = %q, want empty", stderr.String())
			}

			s := open(t, dir)
			got := s.Names(state.MarkerDir(session))
			slices.Sort(got)
			if !slices.Equal(got, tt.wantMarkers) {
				t.Errorf("markers = %v, want %v", got, tt.wantMarkers)
			}
			if tt.mode == Start && tt.in.AgentID != "" {
				if watched, _ := s.Read(state.Marker(session, tt.in.AgentID)); watched != tt.wantWatched {
					t.Errorf("marker contents = %q, want %q", watched, tt.wantWatched)
				}
			}
		})
	}
}

func seed(t *testing.T, dir, session string, agents []string) {
	t.Helper()
	s := open(t, dir)
	for _, a := range agents {
		if err := s.Write(state.Marker(session, a), ""); err != nil {
			t.Fatalf("Write(%s): %v", a, err)
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

// fixedRunner answers ps with one name, or fails when it has none.
type fixedRunner struct{ out string }

func (f fixedRunner) Run(context.Context, runner.Command) ([]byte, error) {
	if f.out == "" {
		return nil, &runner.Error{Name: "ps", Err: context.Canceled}
	}
	return []byte(f.out + "\n"), nil
}
