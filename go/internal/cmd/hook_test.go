package cmd

import (
	"bytes"
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// stub is a hook whose answer the test chooses.
type stub struct {
	result hooks.Result
	// got is the payload the dispatcher handed over.
	got hooks.Payload
}

func (s *stub) Run(_ context.Context, p hooks.Payload) hooks.Result {
	s.got = p
	return s.result
}

func TestRunHook(t *testing.T) {
	t.Parallel()

	const compileError = "./x.go:1:1: syntax error"
	failed := selfbuild.State{Failed: true, JustFailed: true, FirstError: compileError}

	tests := []struct {
		name   string
		hook   stub
		build  selfbuild.State
		stdin  string
		want   error
		stdout string
		stderr []string
		// bareStdout says nothing at all may be written, which is what a hook
		// with no directive owes the pipe Claude Code reads.
		bareStdout bool
		bareStderr bool
	}{
		{
			name:       "a hook with nothing to say writes nothing",
			hook:       stub{result: hooks.Result{Decision: hooks.Allow}},
			bareStdout: true,
			bareStderr: true,
		},
		{
			name: "a directive is written as one line of JSON",
			hook: stub{result: hooks.Result{
				Directive: hooks.Directive{TerminalSequence: "\a"},
			}},
			stdout:     "{\"terminalSequence\":\"\\u0007\"}\n",
			bareStderr: true,
		},
		{
			name:       "blocking exits 2 with only the hook's own message",
			hook:       stub{result: hooks.Result{Decision: hooks.Block, Message: "Blocked: no.\n"}},
			want:       exitCode(hooks.Block),
			stderr:     []string{"Blocked: no.\n"},
			bareStdout: true,
		},
		{
			name:       "failing exits 1",
			hook:       stub{result: hooks.Result{Decision: hooks.Fail, Message: "webhook refused\n"}},
			want:       exitCode(hooks.Fail),
			stderr:     []string{"webhook refused\n"},
			bareStdout: true,
		},
		{
			name:       "a build that just failed reaches an allowing hook through its directive",
			hook:       stub{result: hooks.Result{Decision: hooks.Allow}},
			build:      failed,
			stdout:     compileError,
			bareStderr: true,
		},
		{
			// The hook's own message is the one the user asked for; the build
			// failure is appended rather than substituted for it.
			name: "a build failure joins a message the hook already had",
			hook: stub{result: hooks.Result{
				Directive: hooks.Directive{SystemMessage: "slack refused the post"},
			}},
			build:      failed,
			stdout:     "slack refused the post",
			bareStderr: true,
		},
		{
			name:       "a build that just failed reaches a blocking hook through stderr",
			hook:       stub{result: hooks.Result{Decision: hooks.Block, Message: "Blocked: no.\n"}},
			build:      failed,
			want:       exitCode(hooks.Block),
			stderr:     []string{"Blocked: no.\n", compileError},
			bareStdout: true,
		},
		{
			// Reported once, on the invocation that ran the build. Later starts
			// in the same source state say nothing at all.
			name:       "a build that failed earlier is not reported again",
			hook:       stub{result: hooks.Result{Decision: hooks.Allow}},
			build:      selfbuild.State{Failed: true, FirstError: compileError},
			bareStdout: true,
			bareStderr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stdout, stderr bytes.Buffer
			err := runHook(t.Context(), &tt.hook, tt.build, strings.NewReader(tt.stdin), &stdout, &stderr)

			if !errors.Is(err, tt.want) {
				t.Errorf("runHook = %v, want %v", err, tt.want)
			}
			if tt.stdout != "" && !strings.Contains(stdout.String(), tt.stdout) {
				t.Errorf("stdout does not contain %q:\n%s", tt.stdout, stdout.String())
			}
			for _, want := range tt.stderr {
				if !strings.Contains(stderr.String(), want) {
					t.Errorf("stderr does not contain %q:\n%s", want, stderr.String())
				}
			}
			if tt.bareStdout && stdout.Len() != 0 {
				t.Errorf("stdout = %q, want empty", stdout.String())
			}
			if tt.bareStderr && stderr.Len() != 0 {
				t.Errorf("stderr = %q, want empty", stderr.String())
			}
		})
	}
}

func TestRunHookParsesStdin(t *testing.T) {
	t.Parallel()

	var h stub
	var stdout, stderr bytes.Buffer
	in := strings.NewReader(`{"session_id":"s/1","tool_name":"Bash"}`)
	if err := runHook(t.Context(), &h, selfbuild.State{}, in, &stdout, &stderr); err != nil {
		t.Fatalf("runHook: %v", err)
	}

	// Sanitised on the way in, so no hook has to remember to do it.
	if got, want := h.got.SessionID, "s1"; got != want {
		t.Errorf("SessionID = %q, want %q", got, want)
	}
	if got, want := h.got.ToolName, "Bash"; got != want {
		t.Errorf("ToolName = %q, want %q", got, want)
	}
}

func TestHookCommand(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		args []string

		wantCode   int
		wantStdout string
		wantStderr []string
		bareStdout bool
		bareStderr bool
	}{
		{
			name:       "terminal-bell rings through the directive",
			args:       []string{"hook", "terminal-bell"},
			wantStdout: "{\"terminalSequence\":\"\\u0007\"}\n",
			bareStderr: true,
		},
		{
			// A hook whose name is misspelled in settings.json must not exit 0:
			// that reads as "allow".
			name:       "an unknown hook fails with usage",
			args:       []string{"hook", "bogus"},
			wantCode:   1,
			wantStderr: []string{`unknown command "bogus"`, "Usage:"},
			bareStdout: true,
		},
		{
			name:       "a hook asked for two events at once fails",
			args:       []string{"hook", "subagent-tracker", "--start", "--stop"},
			wantCode:   1,
			wantStderr: []string{"were all set", "Usage:"},
			bareStdout: true,
		},
		{
			// A registration that lost its flag would otherwise track nothing
			// and exit 0, and the only symptom would be idle-notify going
			// quiet — the failure the tracker exists to prevent.
			name:       "a hook asked for no event at all fails",
			args:       []string{"hook", "subagent-tracker"},
			wantCode:   1,
			wantStderr: []string{"at least one of the flags", "Usage:"},
			bareStdout: true,
		},
		{
			name:       "the hook list is on the root command",
			args:       []string{"--help"},
			wantStdout: "hook",
			bareStderr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stdout, stderr bytes.Buffer
			code := run(t.Context(), tt.args, strings.NewReader(""), &stdout, &stderr, selfbuild.State{})

			if code != tt.wantCode {
				t.Errorf("exit code = %d, want %d (stdout=%q stderr=%q)", code, tt.wantCode, stdout.String(), stderr.String())
			}
			if tt.wantStdout != "" && !strings.Contains(stdout.String(), tt.wantStdout) {
				t.Errorf("stdout does not contain %q:\n%s", tt.wantStdout, stdout.String())
			}
			for _, want := range tt.wantStderr {
				if !strings.Contains(stderr.String(), want) {
					t.Errorf("stderr does not contain %q:\n%s", want, stderr.String())
				}
			}
			if tt.bareStdout && stdout.Len() != 0 {
				t.Errorf("stdout = %q, want empty", stdout.String())
			}
			if tt.bareStderr && stderr.Len() != 0 {
				t.Errorf("stderr = %q, want empty", stderr.String())
			}
		})
	}
}
