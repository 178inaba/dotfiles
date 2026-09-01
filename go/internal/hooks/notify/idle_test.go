package notify

import (
	"net/http"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/state/statetest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// session is the id every test in this package keeps its markers under.
const session = "s1"

func payload() hooks.Payload {
	return hooks.Payload{
		SessionID: session, Message: "hello",
		NotificationType: "idle_prompt", Dir: "/r/proj",
	}
}

func TestIdleRun(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		in   hooks.Payload
		// agent is a subagent to record as running, if any.
		agent bool

		wantSound bool
		wantSlack string
		wantBell  bool
	}{
		{
			name:      "nothing is running, so the human is the one waiting",
			in:        payload(),
			wantSound: true, wantSlack: "[proj] (idle_prompt) hello", wantBell: true,
		},
		{
			// The parent is only waiting for the agent it started, and will be
			// woken by its completion; there is nothing here for a human.
			// Which markers count as running is subagents' own test.
			name: "a running subagent keeps the session quiet",
			in:   payload(), agent: true,
		},
		{
			// What the dispatcher hands over when the input would not parse.
			// Notifying is the direction that cannot lose a prompt, and an
			// empty message is nothing for Slack to carry.
			name:      "an unreadable payload still rings",
			in:        hooks.Payload{SessionID: "unknown"},
			wantSound: true, wantBell: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			if tt.agent {
				s := statetest.OpenStore(t, dir)
				// A marker recording nothing is the plainest "still running".
				if err := s.Write(marker(session, "a1"), ""); err != nil {
					t.Fatalf("Write: %v", err)
				}
			}
			srv := newWebhook(t, http.StatusOK)
			sound := &recordingDetacher{}

			got := NewIdle(deps(dir, srv, sound)).Run(t.Context(), tt.in)

			if got.Decision != hooks.Allow {
				t.Errorf("Decision = %d, want %d", got.Decision, hooks.Allow)
			}
			if played := sound.played(); played != tt.wantSound {
				t.Errorf("sound played = %t, want %t", played, tt.wantSound)
			}
			if posts := srv.Posts(); tt.wantSlack == "" {
				if len(posts) != 0 {
					t.Errorf("posted %q, want nothing", posts)
				}
			} else if len(posts) != 1 || posts[0] != tt.wantSlack {
				t.Errorf("posted %q, want [%q]", posts, tt.wantSlack)
			}
			if rang := got.Directive.TerminalSequence != ""; rang != tt.wantBell {
				t.Errorf("bell = %t, want %t", rang, tt.wantBell)
			}
			if got.Directive.SystemMessage != "" {
				t.Errorf("SystemMessage = %q, want none", got.Directive.SystemMessage)
			}
		})
	}
}

// TestIdleRunRingsTheBellWhenSlackFails is the hole this port closes. The shell
// piped into slack-notify.sh under `set -euo pipefail`, so a refused post
// aborted before terminal-bell.sh ran: a Slack outage took the bell with it.
func TestIdleRunRingsTheBellWhenSlackFails(t *testing.T) {
	t.Parallel()

	dir := filepath.Join(t.TempDir(), "ccx")
	srv := newWebhook(t, http.StatusForbidden)
	got := NewIdle(deps(dir, srv, &recordingDetacher{})).Run(t.Context(), payload())

	if got.Decision != hooks.Allow {
		// Anything else and Claude Code stops reading the directive, which is
		// the only way a hook can ring the bell at all.
		t.Errorf("Decision = %d, want %d", got.Decision, hooks.Allow)
	}
	if got.Directive.TerminalSequence == "" {
		t.Error("the bell did not ring")
	}
	if !strings.Contains(got.Directive.SystemMessage, "403") {
		t.Errorf("SystemMessage = %q, want the Slack status in it", got.Directive.SystemMessage)
	}
}

func deps(dir string, srv *webhook, sound runner.Detacher) Deps {
	return Deps{
		Dir:       dir,
		Sound:     sound,
		Client:    srv.Client(),
		Runner:    fixedRunner{toplevel: "/r/proj", common: "/r/proj/.git"},
		Signaller: fakeSignaller{},
		Getenv:    func(string) string { return srv.URL },
	}
}

// recordingDetacher remembers whether the sound was started.
type recordingDetacher struct {
	mu      sync.Mutex
	started bool
}

func (r *recordingDetacher) Detach(string, ...string) (int, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.started = true
	return 1, nil
}

func (r *recordingDetacher) played() bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.started
}
