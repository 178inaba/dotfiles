package idlenotify

import (
	"context"
	"encoding/json/v2"
	"io"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/hooks/slacknotify"
	"github.com/178inaba/dotfiles/go/internal/hooks/state"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	session   = "s1"
	livePID   = "111"
	deadPID   = "222"
	wantSlack = "[proj] (idle_prompt) hello"
)

func payload() hooks.Payload {
	return hooks.Payload{
		SessionID: session, Message: "hello",
		NotificationType: "idle_prompt", Dir: "/r/proj",
	}
}

func TestRun(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		in   hooks.Payload
		// markers is agent id to the pid it records, "" for one that records
		// nothing.
		markers map[string]string

		wantSound bool
		wantSlack string
		wantBell  bool
	}{
		{
			name:      "nothing is running, so the human is the one waiting",
			in:        payload(),
			wantSound: true, wantSlack: wantSlack, wantBell: true,
		},
		{
			// The parent is only waiting for the agent it started, and will be
			// woken by its completion; there is nothing here for a human.
			name: "a running subagent keeps the session quiet",
			in:   payload(), markers: map[string]string{"a1": livePID},
		},
		{
			// A marker whose process is gone is what a crash leaves behind.
			// Honouring it would silence the session for good.
			name: "the residue of a crashed session does not silence anything",
			in:   payload(), markers: map[string]string{"a1": deadPID},
			wantSound: true, wantSlack: wantSlack, wantBell: true,
		},
		{
			name: "a marker that records no pid counts as running",
			in:   payload(), markers: map[string]string{"a1": ""},
		},
		{
			name: "one running agent among stale ones is enough",
			in:   payload(), markers: map[string]string{"a1": deadPID, "a2": livePID},
		},
		{
			// What the dispatcher hands over when the input would not parse.
			// Notifying is the direction that cannot lose a prompt.
			name:      "an unreadable payload still notifies",
			in:        hooks.Payload{SessionID: "unknown"},
			wantSound: true, wantBell: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "ccx")
			seed(t, dir, tt.markers)
			srv := newWebhook(t, http.StatusOK)
			sound := &recordingRunner{}

			var stderr strings.Builder
			got := New(deps(dir, srv, sound)).Run(t.Context(), tt.in, &stderr)

			if got.Decision != hooks.Allow {
				t.Errorf("Decision = %d, want %d", got.Decision, hooks.Allow)
			}
			if stderr.Len() != 0 {
				t.Errorf("stderr = %q, want empty", stderr.String())
			}
			if played := sound.played(); played != tt.wantSound {
				t.Errorf("afplay run = %t, want %t", played, tt.wantSound)
			}
			if posts := srv.posts(); tt.wantSlack == "" {
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

// TestRunRingsTheBellWhenSlackFails is the hole this port closes. The shell
// piped into slack-notify.sh under `set -euo pipefail`, so a refused post
// aborted before terminal-bell.sh ran: a Slack outage took the bell with it.
func TestRunRingsTheBellWhenSlackFails(t *testing.T) {
	t.Parallel()

	dir := filepath.Join(t.TempDir(), "ccx")
	seed(t, dir, nil)
	srv := newWebhook(t, http.StatusForbidden)
	sound := &recordingRunner{}

	var stderr strings.Builder
	got := New(deps(dir, srv, sound)).Run(t.Context(), payload(), &stderr)

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

func deps(dir string, srv *webhook, sound runner.Runner) Deps {
	return Deps{
		Dir:       dir,
		Runner:    sound,
		Signaller: fakeSignaller{},
		Slack: slacknotify.Deps{
			Client: srv.Client(),
			Runner: gitRunner{},
			Getenv: func(string) string { return srv.URL },
		},
	}
}

func seed(t *testing.T, dir string, markers map[string]string) {
	t.Helper()
	s, err := state.Open(dir)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })
	for agent, pid := range markers {
		if err := s.Write(state.Marker(session, agent), pid); err != nil {
			t.Fatalf("Write(%s): %v", agent, err)
		}
	}
}

// fakeSignaller knows one live process.
type fakeSignaller struct{}

func (fakeSignaller) Terminate(int) error { return nil }
func (fakeSignaller) Alive(pid int) bool  { return pid == 111 }

// recordingRunner remembers whether the sound was played.
type recordingRunner struct {
	mu   sync.Mutex
	runs []string
}

func (r *recordingRunner) Run(_ context.Context, c runner.Command) ([]byte, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.runs = append(r.runs, c.Name)
	return nil, nil
}

func (r *recordingRunner) played() bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.runs) > 0
}

// gitRunner answers the project label's rev-parse.
type gitRunner struct{}

func (gitRunner) Run(context.Context, runner.Command) ([]byte, error) {
	return []byte("/r/proj\n/r/proj/.git\n"), nil
}

// webhook records what was posted to it.
type webhook struct {
	*httptest.Server
	mu   sync.Mutex
	text []string
}

func newWebhook(t *testing.T, status int) *webhook {
	t.Helper()
	w := &webhook{}
	w.Server = httptest.NewServer(http.HandlerFunc(func(rw http.ResponseWriter, r *http.Request) {
		var body struct {
			Text string `json:"text"`
		}
		if err := json.UnmarshalRead(io.LimitReader(r.Body, 1<<16), &body); err != nil {
			rw.WriteHeader(http.StatusBadRequest)
			return
		}
		w.mu.Lock()
		w.text = append(w.text, body.Text)
		w.mu.Unlock()
		rw.WriteHeader(status)
	}))
	t.Cleanup(w.Close)
	return w
}

func (w *webhook) posts() []string {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.text
}
