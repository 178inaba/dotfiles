package herdrsummary

import (
	"context"
	"encoding/json/v2"
	"errors"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const pane = "w1:p1"

// now is the fixed clock; its Unix milliseconds are the --seq every report
// must carry.
var now = time.UnixMilli(1700000000123)

const seq = "1700000000123"

// call is what the fake saw of one command.
type call struct {
	Name  string
	Args  []string
	Stdin string
	// Deadline is how long the command had left when it started; zero when
	// its context had no deadline.
	Deadline time.Duration
}

// fakeRunner answers herdr and claude by name and records every call.
type fakeRunner struct {
	paneGet    string
	paneGetErr error
	model      string
	modelErr   error
	reportErr  error
	calls      []call
}

func (f *fakeRunner) Run(ctx context.Context, c runner.Command) ([]byte, error) {
	got := call{Name: c.Name, Args: c.Args, Stdin: string(c.Stdin)}
	if d, ok := ctx.Deadline(); ok {
		got.Deadline = time.Until(d)
	}
	f.calls = append(f.calls, got)

	switch {
	case c.Name == "herdr" && len(c.Args) > 1 && c.Args[1] == "get":
		return []byte(f.paneGet), f.paneGetErr
	case c.Name == "herdr":
		return nil, f.reportErr
	case c.Name == "claude":
		return []byte(f.model), f.modelErr
	}
	return nil, errors.New("unexpected command " + c.Name)
}

func (f *fakeRunner) named(name string) []call {
	var out []call
	for _, c := range f.calls {
		if c.Name == name {
			out = append(out, c)
		}
	}
	return out
}

func run(t *testing.T, f *fakeRunner, paneID string, mode Mode, in hooks.Payload) {
	t.Helper()
	d := Deps{
		Runner: f,
		Getenv: func(key string) string {
			if key == "HERDR_PANE_ID" {
				return paneID
			}
			return ""
		},
		Now: func() time.Time { return now },
	}
	// Every path, failures included, ends in an empty result: the hook never
	// blocks or alters the prompt and never says anything to the session.
	if got := New(d, mode).Run(t.Context(), in); got != (hooks.Result{}) {
		t.Errorf("Run() = %+v, want an empty result", got)
	}
}

func paneWithSummary(summary string) string {
	return `{"id":"cli:pane:get","result":{"pane":{"pane_id":"w1:p1","tokens":{"summary":"` + summary + `"}},"type":"pane_info"}}`
}

const paneWithoutTokens = `{"id":"cli:pane:get","result":{"pane":{"pane_id":"w1:p1"},"type":"pane_info"}}`

func modelSays(continues bool, summary string) string {
	b, _ := json.Marshal(map[string]any{
		"type": "result", "is_error": false,
		"structured_output": map[string]any{"continues": continues, "summary": summary},
	})
	return string(b)
}

func report(token ...string) call {
	return call{Name: "herdr", Args: append([]string{
		"pane", "report-metadata", pane, "--source", "claude-summary", "--seq", seq,
	}, token...)}
}

func TestPromptPublishesTheSummary(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		paneGet  string
		wantPrev string
	}{
		{
			// A pane nothing has reported a summary for has no tokens key.
			name:     "a pane without a summary yet",
			paneGet:  paneWithoutTokens,
			wantPrev: "",
		},
		{
			name:     "the previous summary is handed to the model",
			paneGet:  paneWithSummary("READMEの誤字修正"),
			wantPrev: "READMEの誤字修正",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			f := &fakeRunner{paneGet: tt.paneGet, model: modelSays(false, "herdr の作業要約表示")}
			run(t, f, pane, Prompt, hooks.Payload{Prompt: "/issue-handle 391"})

			if diff := cmp.Diff([]call{{Name: "herdr", Args: []string{"pane", "get", pane}}}, f.named("herdr")[:1]); diff != "" {
				t.Errorf("pane get mismatch (-want +got):\n%s", diff)
			}

			models := f.named("claude")
			if len(models) != 1 {
				t.Fatalf("model calls = %d, want 1", len(models))
			}
			var input struct {
				PreviousSummary string `json:"previous_summary"`
				Instruction     string `json:"instruction"`
			}
			if err := json.Unmarshal([]byte(models[0].Stdin), &input); err != nil {
				t.Fatalf("model stdin %q is not JSON: %v", models[0].Stdin, err)
			}
			if input.PreviousSummary != tt.wantPrev || input.Instruction != "/issue-handle 391" {
				t.Errorf("model input = %+v, want previous %q and the prompt", input, tt.wantPrev)
			}

			want := []call{report("--token", "summary=herdr の作業要約表示")}
			if diff := cmp.Diff(want, f.named("herdr")[1:]); diff != "" {
				t.Errorf("report mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// The child must not run this hook again on its own UserPromptSubmit, must not
// leave a session behind in /resume, and is bounded by the hook itself because
// Claude Code does not time out an async hook.
func TestModelCall(t *testing.T) {
	t.Parallel()

	f := &fakeRunner{paneGet: paneWithoutTokens, model: modelSays(false, "x")}
	run(t, f, pane, Prompt, hooks.Payload{Prompt: "p"})

	models := f.named("claude")
	if len(models) != 1 {
		t.Fatalf("model calls = %d, want 1", len(models))
	}
	m := models[0]
	for _, want := range [][]string{
		{"-p"},
		{"--model", "haiku"},
		{"--settings", `{"disableAllHooks": true}`},
		{"--no-session-persistence"},
	} {
		if !containsRun(m.Args, want) {
			t.Errorf("model args %q lack %q", m.Args, want)
		}
	}
	if m.Deadline <= 0 || m.Deadline > 60*time.Second {
		t.Errorf("model call deadline = %v, want within 60s", m.Deadline)
	}
}

func containsRun(args, want []string) bool {
	for i := 0; i+len(want) <= len(args); i++ {
		if cmp.Equal(args[i:i+len(want)], want) {
			return true
		}
	}
	return false
}

func TestPromptPublishesNothing(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		f          *fakeRunner
		prompt     string
		wantModels int
	}{
		{
			// Re-sending the previous summary under this prompt's newer seq
			// could roll the row back past a summary still being generated.
			name:       "a follow-up that continues the work",
			f:          &fakeRunner{paneGet: paneWithSummary("READMEの誤字修正"), model: modelSays(true, "READMEの誤字修正")},
			prompt:     "続けて",
			wantModels: 1,
		},
		{
			name:       "pane get fails",
			f:          &fakeRunner{paneGetErr: errors.New("no server")},
			prompt:     "p",
			wantModels: 0,
		},
		{
			name:       "pane get prints something unreadable",
			f:          &fakeRunner{paneGet: "not json"},
			prompt:     "p",
			wantModels: 0,
		},
		{
			name:       "the model call fails",
			f:          &fakeRunner{paneGet: paneWithoutTokens, modelErr: errors.New("killed")},
			prompt:     "p",
			wantModels: 1,
		},
		{
			name:       "the model reports an error",
			f:          &fakeRunner{paneGet: paneWithoutTokens, model: `{"type":"result","is_error":true,"result":"overloaded"}`},
			prompt:     "p",
			wantModels: 1,
		},
		{
			name:       "the model answers without structured output",
			f:          &fakeRunner{paneGet: paneWithoutTokens, model: `{"type":"result","is_error":false,"result":"hi"}`},
			prompt:     "p",
			wantModels: 1,
		},
		{
			name:       "the model answers with an empty summary",
			f:          &fakeRunner{paneGet: paneWithoutTokens, model: modelSays(false, " \n ")},
			prompt:     "p",
			wantModels: 1,
		},
		{
			name:       "the model output is not JSON",
			f:          &fakeRunner{paneGet: paneWithoutTokens, model: "oops"},
			prompt:     "p",
			wantModels: 1,
		},
		{
			name:       "a payload without a prompt",
			f:          &fakeRunner{paneGet: paneWithoutTokens, model: modelSays(false, "x")},
			prompt:     "",
			wantModels: 0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			run(t, tt.f, pane, Prompt, hooks.Payload{Prompt: tt.prompt})

			if got := len(tt.f.named("claude")); got != tt.wantModels {
				t.Errorf("model calls = %d, want %d", got, tt.wantModels)
			}
			for _, c := range tt.f.named("herdr") {
				if c.Args[1] == "report-metadata" {
					t.Errorf("reported %q, want nothing published", c.Args)
				}
			}
		})
	}
}

func TestSummaryIsOneLine(t *testing.T) {
	t.Parallel()

	f := &fakeRunner{paneGet: paneWithoutTokens, model: modelSays(false, "  herdr の\n作業要約  ")}
	run(t, f, pane, Prompt, hooks.Payload{Prompt: "p"})

	want := []call{report("--token", "summary=herdr の 作業要約")}
	if diff := cmp.Diff(want, f.named("herdr")[1:]); diff != "" {
		t.Errorf("report mismatch (-want +got):\n%s", diff)
	}
}

func TestClear(t *testing.T) {
	t.Parallel()

	for _, reportErr := range []error{nil, errors.New("no server")} {
		f := &fakeRunner{reportErr: reportErr}
		run(t, f, pane, Clear, hooks.Payload{})

		if diff := cmp.Diff([]call{report("--clear-token", "summary")}, f.calls); diff != "" {
			t.Errorf("reportErr=%v: calls mismatch (-want +got):\n%s", reportErr, diff)
		}
	}
}

func TestOutsideHerdrDoesNothing(t *testing.T) {
	t.Parallel()

	for _, mode := range []Mode{Prompt, Clear} {
		f := &fakeRunner{paneGet: paneWithoutTokens, model: modelSays(false, "x")}
		run(t, f, "", mode, hooks.Payload{Prompt: "p"})

		if len(f.calls) != 0 {
			t.Errorf("mode %d: calls = %+v, want none", mode, f.calls)
		}
	}
}
