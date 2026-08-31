package slacknotify

import (
	"context"
	"encoding/json/v2"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

func TestLabel(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		dir      string
		toplevel string
		common   string
		want     string
	}{
		{
			name: "the main tree is the repository",
			dir:  "/r/myrepo", toplevel: "/r/myrepo", common: "/r/myrepo/.git",
			want: "myrepo",
		},
		{
			name: "a subdirectory is still the repository",
			dir:  "/r/myrepo/api", toplevel: "/r/myrepo", common: "/r/myrepo/.git",
			want: "myrepo",
		},
		{
			// In a worktree the directory name alone says nothing about which
			// project it belongs to, which is what this whole label is for.
			name:     "a worktree names the repository it belongs to",
			dir:      "/r/myrepo/.claude/worktrees/feature-x",
			toplevel: "/r/myrepo/.claude/worktrees/feature-x", common: "/r/myrepo/.git",
			want: "myrepo:feature-x",
		},
		{
			name:     "a worktree keeps a slash in its name",
			dir:      "/r/myrepo/.claude/worktrees/feat/nested",
			toplevel: "/r/myrepo/.claude/worktrees/feat/nested", common: "/r/myrepo/.git",
			want: "myrepo:feat/nested",
		},
		{
			name: "a worktree outside the usual place falls back to its own name",
			dir:  "/elsewhere/manual-wt", toplevel: "/elsewhere/manual-wt", common: "/r/myrepo/.git",
			want: "myrepo:manual-wt",
		},
		{
			// A bare repository's common directory is the repository itself,
			// so the same expression has to cope with the .git being a suffix
			// rather than a directory of its own.
			name: "a bare repository loses its .git suffix",
			dir:  "/r/bare-wt", toplevel: "/r/bare-wt", common: "/r/bare.git",
			want: "bare:bare-wt",
		},
		{
			name: "no git output at all falls back to the directory name",
			dir:  "/somewhere/outside", toplevel: "", common: "",
			want: "outside",
		},
		{
			name: "no directory is no label",
			dir:  "", toplevel: "", common: "",
			want: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := label(tt.dir, tt.toplevel, tt.common); got != tt.want {
				t.Errorf("label = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestRun(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		webhook string
		in      hooks.Payload
		want    hooks.Decision
		// text is the message body expected at the webhook; empty means the
		// webhook must not be called at all.
		text string
	}{
		{
			name: "a notification is posted with its project and type",
			in:   hooks.Payload{Message: "hello", NotificationType: "idle_prompt", Dir: "/r/myrepo"},
			text: "[myrepo] (idle_prompt) hello",
		},
		{
			name: "a notification with no type gets the generic one",
			in:   hooks.Payload{Message: "hello", Dir: "/r/myrepo"},
			text: "[myrepo] (notification) hello",
		},
		{
			// The permission and idle events both carry a message; anything
			// else reaching this hook has nothing to say.
			name: "a payload with no message posts nothing",
			in:   hooks.Payload{NotificationType: "idle_prompt", Dir: "/r/myrepo"},
		},
		{
			name:    "no webhook configured posts nothing",
			webhook: "-",
			in:      hooks.Payload{Message: "hello", Dir: "/r/myrepo"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			srv := newWebhook(t, http.StatusOK)
			webhook := srv.URL
			if tt.webhook == "-" {
				webhook = ""
			}

			var stderr strings.Builder
			h := New(Deps{
				Client: srv.Client(),
				Runner: fixedRunner{toplevel: "/r/myrepo", common: "/r/myrepo/.git"},
				Getenv: func(string) string { return webhook },
			})
			if got := h.Run(t.Context(), tt.in, &stderr); got.Decision != tt.want {
				t.Errorf("Decision = %d, want %d (stderr=%q)", got.Decision, tt.want, stderr.String())
			}

			if tt.text == "" {
				if got := srv.posts(); len(got) != 0 {
					t.Errorf("posted %q, want nothing", got)
				}
				return
			}
			got := srv.posts()
			if len(got) != 1 {
				t.Fatalf("posted %d times, want once", len(got))
			}
			if got[0] != tt.text {
				t.Errorf("text = %q, want %q", got[0], tt.text)
			}
		})
	}
}

// TestRunReportsARefusedWebhook is the hole this port closes: curl ran without
// -f, so a webhook answering 403 dropped every notification without a word.
func TestRunReportsARefusedWebhook(t *testing.T) {
	t.Parallel()

	srv := newWebhook(t, http.StatusForbidden)
	var stderr strings.Builder
	h := New(Deps{
		Client: srv.Client(),
		Runner: fixedRunner{},
		Getenv: func(string) string { return srv.URL },
	})

	got := h.Run(t.Context(), hooks.Payload{Message: "hello"}, &stderr)
	if got.Decision != hooks.Fail {
		t.Errorf("Decision = %d, want %d", got.Decision, hooks.Fail)
	}
	if !strings.Contains(stderr.String(), "403") {
		t.Errorf("stderr does not name the status:\n%s", stderr.String())
	}
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

// fixedRunner answers every git invocation with the same two paths, or with a
// failure when it has none.
type fixedRunner struct{ toplevel, common string }

func (f fixedRunner) Run(context.Context, runner.Command) ([]byte, error) {
	if f.toplevel == "" {
		return nil, &runner.Error{Name: "git", Err: context.Canceled}
	}
	return []byte(f.toplevel + "\n" + f.common + "\n"), nil
}
