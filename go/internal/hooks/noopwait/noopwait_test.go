package noopwait

import (
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
)

func TestRunAllows(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		in   hooks.Payload
	}{
		{"a tool that is not Bash", hooks.Payload{ToolName: "Edit", Command: "echo waiting"}},
		{"no command at all", hooks.Payload{ToolName: "Bash"}},
		{
			// The shape this guard must never touch: a wait that observes
			// something and can therefore end.
			"a polling loop that runs a real command",
			bash(`until ! gh pr checks 13 -R o/r 2>&1 | grep -q pending; do sleep 5; done`),
		},
		{"a sleep followed by a real command", bash("sleep 5; gh pr checks 13 -R o/r")},
		{"an echo that writes a file", bash(`echo "$x" > f`)},
		{"an echo of two words", bash("echo a b")},
		{"an echo into a pipe", bash("echo ok | cat")},
		{"a loop that ends with an echo", bash("for i in $(seq 1 60); do [ -f x ] && exit 0; sleep 1; done; echo TIMEOUT")},
		{"a sleep joined by && rather than a leading ;", bash("sleep 1 && echo w")},
		{"an echo at the end of a script", bash("set -e\nmake build\necho done")},
		{"pwd, which is a no-op of another shape", bash("pwd")},
		{"git status, which is a no-op of another shape", bash("git status")},
		{"a token past the length limit", bash("echo abcdefghijklmnopqrstuvwxy")},
		{"a trailing semicolon, which is another shape", bash("echo idle1;")},
		{"a sleep with no duration", bash("sleep")},
		{"an echo with a flag", bash("echo -n ok")},
		{
			// What the dispatcher hands over when the input would not parse.
			// A guard that cannot read its input lets the call through.
			"an unreadable payload",
			hooks.Payload{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			if got := New().Run(t.Context(), tt.in); got.Decision != hooks.Allow || got.Message != "" {
				t.Errorf("Result = %+v, want an allow with no message", got)
			}
		})
	}
}

func TestRunBlocks(t *testing.T) {
	t.Parallel()

	commands := []string{
		"echo idle12",
		"echo w7",
		"echo waiting",
		"echo ok",
		"echo waiting-for-plan-agent",
		"printf ok",
		"echo",
		"true",
		":",
		"sleep 2",
		"sleep 5m",
		"sleep 30s",
		"sleep 1; echo waiting",
		"sleep 1; echo done",
		"sleep 1 ; true",
		"sleep 1;echo w",
		"sleep 0.5; echo w",
		"sleep .5; true",
		`echo "ok"`,
		"echo 'ok'",
		"  echo\t  idle1  ",
		"sleep 1;\necho waiting",
		"echo abcdefghijklmnopqrstuvwx",
	}

	for _, command := range commands {
		t.Run(command, func(t *testing.T) {
			t.Parallel()

			got := New().Run(t.Context(), bash(command))
			if got.Decision != hooks.Block {
				t.Errorf("Decision = %d, want %d", got.Decision, hooks.Block)
			}
			if !strings.Contains(got.Message, command) {
				t.Errorf("message does not quote the command:\n%s", got.Message)
			}
		})
	}
}

// TestBlockMessage pins the guidance, which is the whole value of blocking:
// the model has to be told what to do instead, or it reaches for another no-op.
func TestBlockMessage(t *testing.T) {
	t.Parallel()

	got := New().Run(t.Context(), bash("echo waiting"))

	for _, want := range []string{
		"end the turn",
		"the same busy-wait",
		"timeout parameter",
	} {
		if !strings.Contains(got.Message, want) {
			t.Errorf("message does not contain %q:\n%s", want, got.Message)
		}
	}
}

func bash(command string) hooks.Payload {
	return hooks.Payload{ToolName: "Bash", Command: command}
}
