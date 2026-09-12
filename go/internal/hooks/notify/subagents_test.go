package notify

import (
	"path/filepath"
	"slices"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
)

func TestTrackerRun(t *testing.T) {
	t.Parallel()

	started := []string{"a1", "a2"}

	tests := []struct {
		name        string
		mode        Mode
		in          hooks.Payload
		wantMarkers []string
	}{
		{
			name: "start records the agent",
			mode: Start, in: hooks.Payload{SessionID: session, AgentID: "a3"},
			wantMarkers: []string{"a1", "a2", "a3"},
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
			name: "session start purges whatever a previous run left under the id",
			mode: SessionStart, in: hooks.Payload{SessionID: session, Source: "startup"},
			wantMarkers: nil,
		},
		{
			name: "session start purges on resume, which reuses the session id",
			mode: SessionStart, in: hooks.Payload{SessionID: session, Source: "resume"},
			wantMarkers: nil,
		},
		{
			// Compaction happens mid-session, and a background subagent survives
			// it: purging here would forget one that is still running.
			name: "session start leaves the markers when the source is a compaction",
			mode: SessionStart, in: hooks.Payload{SessionID: session, Source: "compact"},
			wantMarkers: started,
		},
		{
			// Only "compact" is excluded, rather than an allowlist of the known
			// sources: a source Claude Code adds later purges by default instead
			// of silently stopping, which is the direction that risks one extra
			// notification rather than a marker that never gets forgotten.
			name: "session start purges for a source this rule does not recognize",
			mode: SessionStart, in: hooks.Payload{SessionID: session, Source: "future"},
			wantMarkers: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			seed(t, dir, session, started)

			h := NewTracker(Deps{Dir: dir}, tt.mode)

			if got, want := h.Run(t.Context(), tt.in), (hooks.Result{}); got != want {
				t.Fatalf("Run() = %+v, want %+v", got, want)
			}

			s := openStore(t, dir)
			got := names(t, s, markerDir(session))
			slices.Sort(got)
			if !slices.Equal(got, tt.wantMarkers) {
				t.Errorf("markers = %v, want %v", got, tt.wantMarkers)
			}
		})
	}
}

func seed(t *testing.T, dir, session string, agents []string) {
	t.Helper()
	s := openStore(t, dir)
	for _, a := range agents {
		if err := s.Create(marker(session, a)); err != nil {
			t.Fatalf("Create(%s): %v", a, err)
		}
	}
}

func TestBusy(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		agents []string
		want   bool
	}{
		{name: "no subagents recorded"},
		{name: "a subagent recorded", agents: []string{"a1"}, want: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			s := openStore(t, dir)
			for _, agent := range tt.agents {
				if err := s.Create(marker(session, agent)); err != nil {
					t.Fatalf("Create(%s): %v", agent, err)
				}
			}

			d := Deps{Dir: dir}
			if got := busy(d, session); got != tt.want {
				t.Errorf("busy = %t, want %t", got, tt.want)
			}
		})
	}
}

// openStore opens a state tree that closes itself when the test ends.
func openStore(t *testing.T, dir string) *state.Store {
	t.Helper()
	s, err := state.Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })
	return s
}

// names lists a directory in the store, failing the test if it cannot be read.
func names(t *testing.T, s *state.Store, dir string) []string {
	t.Helper()
	names, err := s.Names(dir)
	if err != nil {
		t.Fatalf("Names(%q): %v", dir, err)
	}
	return names
}
