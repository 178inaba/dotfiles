package skillcheck

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const scriptPath = "/r/claude/.claude/skills/skill-authoring/scripts/check-skill-frontmatter.sh"

func TestRunAllows(t *testing.T) {
	t.Parallel()

	skill := "/r/claude/.claude/skills/clean/SKILL.md"
	tests := []struct {
		name string
		in   hooks.Payload
	}{
		{"a clean SKILL.md", edit(skill)},
		{"a tool that edits nothing", hooks.Payload{ToolName: "Bash", Command: "ls", Dir: "/r"}},
		{"an edit with no path", hooks.Payload{ToolName: "Edit", Dir: "/r"}},
		{"a file that is not a SKILL.md", edit("/r/README.md")},
		{
			// The basename has to match exactly; a name that merely ends with
			// it is a different file.
			name: "a file whose name only ends with SKILL.md", in: edit("/r/NOT-SKILL.md"),
		},
		{
			// What the dispatcher hands over when the input would not parse.
			name: "an unreadable payload", in: hooks.Payload{},
		},
		{"a relative path with nothing to resolve it against", relative("skills/x/SKILL.md", "")},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stderr strings.Builder
			got := New(deps(&fakeRunner{out: `{"violations":[]}`})).Run(t.Context(), tt.in, &stderr)
			if got.Decision != hooks.Allow {
				t.Errorf("Decision = %d, want %d", got.Decision, hooks.Allow)
			}
			if stderr.Len() != 0 {
				t.Errorf("stderr = %q, want empty", stderr.String())
			}
		})
	}
}

func TestRunReportsViolations(t *testing.T) {
	t.Parallel()

	const target = "/r/claude/.claude/skills/seqhint/SKILL.md"
	tests := []struct {
		name string
		out  string
		want []string
	}{
		{
			name: "an unquoted flow value names the key and the line",
			out:  `{"violations":[{"type":"unquoted_flow","file":"seqhint/SKILL.md","key":"argument-hint","line":4}]}`,
			want: []string{"unquoted_flow", "argument-hint", "line 4"},
		},
		{
			name: "frontmatter that will not parse relays the parser's message",
			out:  `{"violations":[{"type":"invalid_yaml","file":"x/SKILL.md","message":"could not find expected ':'"}]}`,
			want: []string{"invalid_yaml", "could not find expected ':'"},
		},
		{
			name: "a missing field names the field",
			out:  `{"violations":[{"type":"missing_field","file":"x/SKILL.md","field":"description"}]}`,
			want: []string{"missing_field", "description"},
		},
		{
			name: "a name that does not match its directory shows both",
			out:  `{"violations":[{"type":"name_mismatch","file":"x/SKILL.md","expected":"mismatched","actual":"something-else"}]}`,
			want: []string{"name_mismatch", "mismatched", "something-else"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stderr strings.Builder
			got := New(deps(&fakeRunner{out: tt.out})).Run(t.Context(), edit(target), &stderr)
			if got.Decision != hooks.Block {
				t.Errorf("Decision = %d, want %d", got.Decision, hooks.Block)
			}
			// The payload's own path, not the checker's <skill>/SKILL.md: that
			// form does not say where in the repository the file is.
			for _, want := range append(tt.want, target, "Re-check with:", scriptPath) {
				if !strings.Contains(stderr.String(), want) {
					t.Errorf("stderr does not contain %q:\n%s", want, stderr.String())
				}
			}
		})
	}
}

func TestRunResolvesARelativePath(t *testing.T) {
	t.Parallel()

	r := &fakeRunner{out: `{"violations":[]}`}
	in := relative("skills/x/SKILL.md", "/r/claude/.claude")
	if got := New(deps(r)).Run(t.Context(), in, &strings.Builder{}); got.Decision != hooks.Allow {
		t.Fatalf("Decision = %d, want %d", got.Decision, hooks.Allow)
	}
	// Left relative, the checker would look for it from wherever the hook
	// happened to be started and report a file that is not there.
	if want := "/r/claude/.claude/skills/x/SKILL.md"; !strings.Contains(strings.Join(r.args, " "), want) {
		t.Errorf("checker args = %v, want the path resolved to %q", r.args, want)
	}
}

// TestRunBlocksWhenTheCheckCannotRun is the fail-closed half: a check that did
// not happen must not read as a check that passed.
func TestRunBlocksWhenTheCheckCannotRun(t *testing.T) {
	t.Parallel()

	const target = "/r/claude/.claude/skills/x/SKILL.md"
	tests := []struct {
		name    string
		deps    Deps
		want    []string
		notWant string
	}{
		{
			name: "the checker refuses to run",
			deps: deps(&fakeRunner{err: "yq is required\n"}),
			want: []string{"was not checked", "yq is required", "Re-check with:", scriptPath},
		},
		{
			// Nothing to name means no command to suggest, so the guidance is
			// left out rather than printed with a hole in it.
			name: "the repository cannot be located",
			deps: Deps{
				Runner: &fakeRunner{out: `{"violations":[]}`},
				Script: func() (string, bool) { return "", false },
			},
			want: []string{"was not checked"}, notWant: "Re-check with:",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stderr strings.Builder
			got := New(tt.deps).Run(t.Context(), edit(target), &stderr)
			if got.Decision != hooks.Block {
				t.Errorf("Decision = %d, want %d", got.Decision, hooks.Block)
			}
			for _, want := range tt.want {
				if !strings.Contains(stderr.String(), want) {
					t.Errorf("stderr does not contain %q:\n%s", want, stderr.String())
				}
			}
			if tt.notWant != "" && strings.Contains(stderr.String(), tt.notWant) {
				t.Errorf("stderr contains %q:\n%s", tt.notWant, stderr.String())
			}
		})
	}
}

func TestShellQuote(t *testing.T) {
	t.Parallel()

	// The suggested command has to be runnable as printed, and an ordinary
	// path has to stay readable.
	tests := []struct{ in, want string }{
		{"/r/a-b/SKILL.md", "/r/a-b/SKILL.md"},
		{"/r/with space/SKILL.md", `'/r/with space/SKILL.md'`},
		{"/r/it's/SKILL.md", `'/r/it'\''s/SKILL.md'`},
	}
	for _, tt := range tests {
		if got := shellQuote(tt.in); got != tt.want {
			t.Errorf("shellQuote(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func deps(r runner.Runner) Deps {
	return Deps{Runner: r, Script: func() (string, bool) { return scriptPath, true }}
}

func edit(target string) hooks.Payload {
	return hooks.Payload{ToolName: "Edit", FilePath: target, Dir: filepath.Dir(target)}
}

func relative(target, dir string) hooks.Payload {
	return hooks.Payload{ToolName: "Write", FilePath: target, Dir: dir}
}

// fakeRunner stands in for the checker.
type fakeRunner struct {
	out  string
	err  string
	args []string
}

func (f *fakeRunner) Run(_ context.Context, c runner.Command) ([]byte, error) {
	f.args = c.Args
	if f.err != "" {
		return nil, &runner.Error{Name: c.Name, Err: context.Canceled, Stderr: []byte(f.err)}
	}
	return []byte(f.out), nil
}
