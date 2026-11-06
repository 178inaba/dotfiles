package findinglist

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
)

// The records below are the shapes Claude Code writes, kept as raw JSON so
// that a test reads like the file the guard reads.

func toolUse(id, name, input string) string {
	return fmt.Sprintf(`{"type":"assistant","message":{"role":"assistant","content":[{"type":"tool_use","id":%q,"name":%q,"input":%s}]}}`, id, name, input)
}

// launch is the Skill call the invariant is about.
func launch(id string) string {
	return toolUse(id, "Skill", `{"skill":"finding-severity"}`)
}

func skillCall(id, skill string) string {
	return toolUse(id, "Skill", fmt.Sprintf(`{"skill":%q}`, skill))
}

func agentCall(id, subagentType string) string {
	return toolUse(id, "Agent", fmt.Sprintf(`{"subagent_type":%q,"prompt":"review"}`, subagentType))
}

// reviewAgent is how /simplify launches its reviewers: no subagent_type, only a
// description.
func reviewAgent(id, description string) string {
	return toolUse(id, "Agent", fmt.Sprintf(`{"description":%q,"isolation":"worktree","prompt":"review"}`, description))
}

func text(s string) string {
	return fmt.Sprintf(`{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":%q}]}}`, s)
}

// list is the assistant text that pairs a launch: the heading at the start of
// a line, after whatever the model wrote before it.
var list = text("Triage done.\n\n" + Heading + "\n\n#### 対応する指摘\n- a.go: x")

// skillBody is the expanded SKILL.md. It quotes the heading inside a fence, and
// that is not the model writing a list.
var skillBody = `{"type":"user","isMeta":true,"message":{"role":"user","content":[{"type":"text","text":"Base directory for this skill: /x/finding-severity\n\n` + "```markdown\\n" + Heading + "\\n```" + `"}]}}`

func transcript(t *testing.T, lines ...string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "session.jsonl")
	if err := os.WriteFile(path, []byte(strings.Join(lines, "\n")+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func bash(command, path string) hooks.Payload {
	return hooks.Payload{ToolName: "Bash", Command: command, TranscriptPath: path}
}

const (
	createPR = "gh pr create --draft --base main -R o/r --title t --body-file b.md"
	ready    = "ccx pr ready 12"
)

func TestRunBlocks(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		command string
		lines   []string
		// count is how many launches the message says lack a list, and after
		// is the tool call it names as the one before the most recent of them.
		count int
		after string
	}{
		{
			name:    "a launch with no list, at PR creation",
			command: createPR,
			lines:   []string{skillCall("s1", "simplify"), launch("l1"), skillBody, text("対応方針を決めて適用します。")},
			count:   1, after: "Skill (simplify)",
		},
		{
			name:    "a launch with no list, at ready",
			command: ready,
			lines:   []string{skillCall("s1", "simplify"), launch("l1"), skillBody, text("対応方針を決めて適用します。")},
			count:   1, after: "Skill (simplify)",
		},
		{
			name:    "the second of two launches has no list",
			command: ready,
			lines: []string{
				skillCall("s1", "simplify"), launch("l1"), list,
				agentCall("a1", "independent-reviewer"), launch("l2"), text("applying"),
			},
			count: 1, after: "Agent (independent-reviewer)",
		},
		{
			// The list has to come before the next launch: one list after both
			// pairs the second and leaves the first open.
			name:    "one list after two launches pairs only the second",
			command: ready,
			lines: []string{
				skillCall("s1", "simplify"), launch("l1"), text("applying"),
				agentCall("a1", "independent-reviewer"), launch("l2"), list,
			},
			count: 1, after: "Skill (simplify)",
		},
		{
			name:    "a launch after an agent with only a description",
			command: createPR,
			lines:   []string{reviewAgent("a1", "Efficiency review"), launch("l1"), text("applying")},
			count:   1, after: "Agent (Efficiency review)",
		},
		{
			name:    "the heading only in the expanded skill body",
			command: createPR,
			lines:   []string{launch("l1"), skillBody},
			count:   1, after: "the start of the transcript",
		},
		{
			name:    "the heading only inside a tool call's input",
			command: createPR,
			lines: []string{
				launch("l1"),
				toolUse("w1", "Write", fmt.Sprintf(`{"file_path":"list.md","content":%q}`, Heading+"\n")),
			},
			count: 1, after: "the start of the transcript",
		},
		{
			name:    "the heading in the middle of a line",
			command: createPR,
			lines:   []string{launch("l1"), text("I will write " + Heading + " later")},
			count:   1, after: "the start of the transcript",
		},
		{
			// The line is skipped and the rest is still judged.
			name:    "a malformed line among records with an unpaired launch",
			command: createPR,
			lines:   []string{launch("l1"), `{"type":"assistant","message":`, text("applying")},
			count:   1, after: "the start of the transcript",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got := New().Run(t.Context(), bash(tt.command, transcript(t, tt.lines...)))
			if got.Decision != hooks.Block {
				t.Fatalf("Decision = %v, want Block (message %q)", got.Decision, got.Message)
			}
			for _, want := range []string{
				fmt.Sprintf("launches without a list: %d", tt.count),
				"the most recent came right after: " + tt.after,
				Heading,
				"Issue 側の修正が要る指摘",
				"run the blocked command again",
			} {
				if !strings.Contains(got.Message, want) {
					t.Errorf("message lacks %q:\n%s", want, got.Message)
				}
			}
			// issue-conflict.md exists under one launcher only; the other
			// launcher's run would be sent to a file it does not have.
			if strings.Contains(got.Message, "/") || strings.Contains(got.Message, "issue-conflict") {
				t.Errorf("message names a path or issue-conflict.md:\n%s", got.Message)
			}
			if !got.Directive.IsEmpty() {
				t.Errorf("Directive = %+v, want empty: exit 2 carries none", got.Directive)
			}
		})
	}
}

func TestRunAllows(t *testing.T) {
	t.Parallel()

	missing := filepath.Join(t.TempDir(), "no-such.jsonl")
	unpaired := transcript(t, launch("l1"), text("applying"))

	tests := []struct {
		name string
		in   hooks.Payload
	}{
		{"a launch followed by its list", bash(createPR, transcript(t, launch("l1"), skillBody, list))},
		{"two launches each followed by a list", bash(ready, transcript(t, launch("l1"), list, launch("l2"), list))},
		{"no finding-severity launch at all", bash(createPR, transcript(t, skillCall("s1", "simplify"), text("done")))},
		{"a malformed line among records with a paired launch", bash(createPR, transcript(t, launch("l1"), `not json`, list))},
		{
			// The tail of a transcript may have no newline yet; a complete one
			// still counts.
			"a list on a last line with no newline",
			bash(createPR, func() string {
				path := filepath.Join(t.TempDir(), "s.jsonl")
				if err := os.WriteFile(path, []byte(launch("l1")+"\n"+list), 0o600); err != nil {
					t.Fatal(err)
				}
				return path
			}()),
		},
		// Outside the gate the transcript is never read: a missing file allows.
		{"gh pr view", bash("gh pr view --json isDraft", missing)},
		{"git commit", bash("git commit -m x", missing)},
		{"gh pr ready, which is not a gate", bash("gh pr ready 12", missing)},
		{"ccx pr ready --help", bash("ccx pr ready --help", unpaired)},
		{"gh pr create --help", bash("gh pr create --help", unpaired)},
		{"a tool that is not Bash", hooks.Payload{ToolName: "Edit", Command: createPR, TranscriptPath: unpaired}},
		{
			// A subagent's call is judged against no transcript: the session
			// that owes the list is not the one making it.
			"a call inside a subagent",
			hooks.Payload{ToolName: "Bash", AgentID: "a1", Command: createPR, TranscriptPath: unpaired},
		},
		// Fail open on anything the hook cannot read.
		{"no transcript path", bash(createPR, "")},
		{"a transcript that does not exist", bash(createPR, missing)},
		{"an unreadable payload", hooks.Payload{}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got := New().Run(t.Context(), tt.in)
			if got.Decision != hooks.Allow || got.Message != "" || !got.Directive.IsEmpty() {
				t.Errorf("Run() = %+v, want an empty allow", got)
			}
		})
	}
}

// TestHeadingMatchesTheSkill pins the heading to the skill that owns it. The
// test runs from its own package directory, so the only way the path misses is
// that it is written wrong, and a skip would hide that for good.
func TestHeadingMatchesTheSkill(t *testing.T) {
	path := filepath.Join("..", "..", "..", "..", "claude", ".claude", "skills", "finding-severity", "SKILL.md")
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile(%q): %v", path, err)
	}
	if !startsALine(string(b), Heading) {
		t.Errorf("%s does not start a line with %q, so the guard no longer recognises the list the skill asks for", path, Heading)
	}
}
