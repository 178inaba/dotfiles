package skillcheck

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/skill"
)

// writeSkillBody puts a SKILL.md with the given frontmatter and body under a
// directory named for the skill, and returns its path.
func writeSkillBody(t *testing.T, name, body string, lines ...string) string {
	t.Helper()

	target := filepath.Join(t.TempDir(), name, "SKILL.md")
	if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	content := "---\n" + strings.Join(lines, "\n") + "\n---\n\n" + body
	if err := os.WriteFile(target, []byte(content), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	return target
}

// writeSkill puts a SKILL.md with the given frontmatter and an ordinary small
// body under a directory named for the skill, and returns its path.
func writeSkill(t *testing.T, skill string, lines ...string) string {
	t.Helper()
	return writeSkillBody(t, skill, "# /"+skill+"\n", lines...)
}

func TestRunAllows(t *testing.T) {
	t.Parallel()

	clean := writeSkill(t, "clean", "name: clean", "description: a clean skill", `argument-hint: "[--yes]"`)
	tests := []struct {
		name string
		in   hooks.Payload
	}{
		{"a clean SKILL.md", edit(clean)},
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

			got := New().Run(t.Context(), tt.in)
			// The clean skill's body is small — well under either guide — so
			// this also covers the size report saying nothing about it.
			if got.Decision != hooks.Allow || got.Message != "" || !got.Directive.IsEmpty() {
				t.Errorf("Result = %+v, want an empty allow", got)
			}
		})
	}
}

func TestRunReportsViolations(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// skill is the directory name and lines its frontmatter.
		skill string
		lines []string
		want  []string
	}{
		{
			// The violation that went unnoticed in two files and is the reason
			// this hook exists.
			name: "an unquoted flow value names the key and the line", skill: "seqhint",
			lines: []string{"name: seqhint", "description: a skill", "argument-hint: [--yes]"},
			want:  []string{"unquoted_flow", "argument-hint", "line 4"},
		},
		{
			name: "frontmatter that will not parse relays the parser's message", skill: "badyaml",
			lines: []string{"name: badyaml", "description: a skill", "argument-hint: [<a>] [--b]"},
			want:  []string{"invalid_yaml"},
		},
		{
			name: "a missing field names the field", skill: "nodesc",
			lines: []string{"name: nodesc"},
			want:  []string{"missing_field", "description"},
		},
		{
			name: "a name that does not match its directory shows both", skill: "mismatched",
			lines: []string{"name: something-else", "description: a skill"},
			want:  []string{"name_mismatch", "mismatched", "something-else"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			target := writeSkill(t, tt.skill, tt.lines...)
			got := New().Run(t.Context(), edit(target))
			if got.Decision != hooks.Block {
				t.Fatalf("Decision = %d, want %d (message=%q)", got.Decision, hooks.Block, got.Message)
			}
			// The payload's own path, not the <skill>/SKILL.md form the check
			// reports: that one does not say where in the repository it is.
			for _, want := range append(tt.want, target, "Re-check with:", "ccx skill frontmatter") {
				if !strings.Contains(got.Message, want) {
					t.Errorf("message does not contain %q:\n%s", want, got.Message)
				}
			}
		})
	}
}

func TestRunResolvesARelativePath(t *testing.T) {
	t.Parallel()

	target := writeSkill(t, "x", "name: something-else", "description: a skill")
	dir := filepath.Dir(filepath.Dir(target))

	// Left relative, the check would look for the file from wherever the hook
	// happened to be started and report one that is not there.
	got := New().Run(t.Context(), relative(filepath.Join("x", "SKILL.md"), dir))
	if got.Decision != hooks.Block {
		t.Fatalf("Decision = %d, want %d", got.Decision, hooks.Block)
	}
	if !strings.Contains(got.Message, target) {
		t.Errorf("message does not name the resolved path %q:\n%s", target, got.Message)
	}
}

// TestRunBlocksWhenTheCheckCannotRun is the fail-closed half: a check that did
// not happen must not read as a check that passed.
func TestRunBlocksWhenTheCheckCannotRun(t *testing.T) {
	t.Parallel()

	target := filepath.Join(t.TempDir(), "ghost", "SKILL.md")
	got := New().Run(t.Context(), edit(target))
	if got.Decision != hooks.Block {
		t.Fatalf("Decision = %d, want %d", got.Decision, hooks.Block)
	}
	for _, want := range []string{"was not checked", "Re-check with:"} {
		if !strings.Contains(got.Message, want) {
			t.Errorf("message does not contain %q:\n%s", want, got.Message)
		}
	}
}

// TestRunReportsSize is the second check this hook makes once the frontmatter
// is clean: a body over the guide that applies to it is reported through the
// directive rather than blocked, since the guide is a target to split
// against and not a limit.
func TestRunReportsSize(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		// want is substrings every one is expected in additionalContext; nil
		// means the hook says nothing about the body at all.
		want []string
	}{
		{
			// mostly_non_ascii gates character_guide: derived from Japanese,
			// it does not hold for an English body, however many characters
			// it has.
			name: "over the character guide, ASCII",
			body: strings.Repeat("a", skill.CharacterGuide+1),
		},
		{
			name: "over the character guide, mostly Japanese",
			body: strings.Repeat("あ", skill.CharacterGuide+1) + "\n",
			want: []string{"characters", "ccx skill size"},
		},
		{
			name: "over the line guide, ASCII",
			body: strings.Repeat("a\n", skill.LineGuide+1),
			want: []string{"lines", "ccx skill size"},
		},
		{
			// Both guides over at once, mostly Japanese: the report mentions
			// both rather than only the first one it finds.
			name: "over both guides, mostly Japanese",
			body: strings.Repeat("あ\n", skill.CharacterGuide/2+1),
			want: []string{"lines", "characters", "ccx skill size"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			target := writeSkillBody(t, "sized", tc.body, "name: sized", "description: a skill")
			got := New().Run(t.Context(), edit(target))

			if got.Decision != hooks.Allow || got.Message != "" {
				t.Fatalf("Result = %+v, want an allow with no message", got)
			}
			if tc.want == nil {
				if !got.Directive.IsEmpty() {
					t.Errorf("Directive = %+v, want empty", got.Directive)
				}
				return
			}
			if got.Directive.HookSpecificOutput.HookEventName != "PostToolUse" {
				t.Errorf("hookEventName = %q, want PostToolUse", got.Directive.HookSpecificOutput.HookEventName)
			}
			ctx := got.Directive.HookSpecificOutput.AdditionalContext
			for _, want := range append(tc.want, target) {
				if !strings.Contains(ctx, want) {
					t.Errorf("additionalContext does not contain %q:\n%s", want, ctx)
				}
			}
		})
	}
}

// TestRunReportsOnlyTheFrontmatterViolation is what keeps the two checks from
// talking over each other: a directive is only read from an exit 0, so a
// size report beside a block would never reach anyone anyway, but the hook
// should not build one it cannot use.
func TestRunReportsOnlyTheFrontmatterViolation(t *testing.T) {
	t.Parallel()

	target := writeSkillBody(t, "seqhint", strings.Repeat("a\n", skill.LineGuide+1),
		"name: seqhint", "description: a skill", "argument-hint: [--yes]")
	got := New().Run(t.Context(), edit(target))

	if got.Decision != hooks.Block {
		t.Fatalf("Decision = %d, want %d", got.Decision, hooks.Block)
	}
	if !got.Directive.IsEmpty() {
		t.Errorf("Directive = %+v, want empty", got.Directive)
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

func edit(target string) hooks.Payload {
	return hooks.Payload{ToolName: "Edit", FilePath: target, Dir: filepath.Dir(target)}
}

func relative(target, dir string) hooks.Payload {
	return hooks.Payload{ToolName: "Write", FilePath: target, Dir: dir}
}
