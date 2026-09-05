package issuehandle

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
)

// The records below are the shapes Claude Code actually writes, kept as raw
// JSON so that the test reads like the file the guard reads.
const (
	invoke      = `{"type":"user","message":{"role":"user","content":"<command-message>issue-handle</command-message>\n<command-name>/issue-handle</command-name>\n<command-args>190</command-args>"}}`
	invokeDraft = `{"type":"user","message":{"role":"user","content":"<command-message>issue-draft</command-message>\n<command-name>/issue-draft</command-name>\n<command-args></command-args>"}}`
	invokeModel = `{"type":"user","message":{"role":"user","content":"<command-message>model</command-message>\n<command-name>/model</command-name>\n<command-args></command-args>"}}`

	// skillBody is what makes a typed command a skill launch, and stdout is
	// what a built-in leaves behind instead.
	skillBody = `{"type":"user","isMeta":true,"message":{"role":"user","content":[{"type":"text","text":"Base directory for this skill: /Users/x/.claude/skills/issue-handle\n\n# /issue-handle"}]}}`
	stdout    = `{"type":"user","message":{"role":"user","content":"<local-command-stdout>Set model to Opus</local-command-stdout>"}}`
)

func toolUse(id, name, input string) string {
	return fmt.Sprintf(`{"type":"assistant","message":{"role":"assistant","content":[{"type":"tool_use","id":%q,"name":%q,"input":%s}]}}`, id, name, input)
}

func bash(id, command string) string {
	return toolUse(id, "Bash", fmt.Sprintf(`{"command":%q}`, command))
}

// result carries no is_error at all, which is one of the two shapes a success
// arrives in; okResult is the other.
func result(id string) string {
	return fmt.Sprintf(`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":%q,"content":"done"}]}}`, id)
}

func okResult(id string) string {
	return fmt.Sprintf(`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":%q,"is_error":false,"content":"done"}]}}`, id)
}

func errResult(id string) string {
	return fmt.Sprintf(`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":%q,"is_error":true,"content":"rejected"}]}}`, id)
}

// planned is the prefix every active run shares: the typed launch, the skill
// body that makes it one, and an approved plan.
func planned() []string {
	return []string{invoke, skillBody, toolUse("p1", "ExitPlanMode", `{}`), result("p1")}
}

func withPR() []string {
	return append(planned(),
		bash("b1", "git commit -m x"), okResult("b1"),
		bash("b2", "gh pr create --draft -R o/r"), okResult("b2"))
}

func withReview() []string {
	return append(withPR(),
		toolUse("a1", "Agent", `{"subagent_type":"independent-reviewer","description":"Independent review"}`))
}

// transcript writes the records as one JSONL file and returns its path.
func transcript(t *testing.T, records ...string) string {
	t.Helper()
	p := filepath.Join(t.TempDir(), "session.jsonl")
	if err := os.WriteFile(p, []byte(strings.Join(records, "\n")+"\n"), 0o600); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	return p
}

// stopping is the Stop input of a turn ending with nothing in flight, which is
// the only input the guard ever judges.
func stopping(path string) hooks.Payload {
	return hooks.Payload{TranscriptPath: path, BackgroundTasks: new(0)}
}

func TestRunBlocks(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		records []string
		want    string
	}{
		{
			name:    "a run that has committed but not created the pull request",
			records: append(planned(), bash("b1", "git commit -m x"), okResult("b1")),
			want:    "step 6",
		},
		{
			name:    "a pull request nobody has reviewed",
			records: withPR(),
			want:    "step 7-1",
		},
		{
			name:    "a review that has not been answered",
			records: withReview(),
			want:    "step 7-2",
		},
		{
			// The draft check of step 6 runs this, and undoing ready is the
			// opposite of finishing.
			name:    "undoing ready is not reaching it",
			records: append(withReview(), bash("b9", "gh pr ready --undo 7 -R o/r"), okResult("b9")),
			want:    "step 7-2",
		},
		{
			name:    "a pull request whose creation failed",
			records: append(planned(), bash("b2", "gh pr create --draft -R o/r"), errResult("b2")),
			want:    "step 6",
		},
		{
			// A built-in command is not a skill: no body follows it, so it
			// leaves the run it interrupted running.
			name:    "a built-in command does not end the run",
			records: append(withPR(), invokeModel, stdout),
			want:    "step 7-1",
		},
		{
			// Only the last launch is judged, so a finished run before it says
			// nothing about this one.
			name: "a second run after a finished one",
			records: append(append(withReview(),
				bash("b8", "gh pr ready 7 -R o/r"), okResult("b8")),
				planned()...),
			want: "step 6",
		},
		{
			// A tool result of a megabyte is ordinary here, and a scan that
			// stopped at one would miss everything after it.
			name:    "a record far longer than a scanner's default buffer",
			records: append(planned(), longResult(), bash("b1", "git commit -m x"), okResult("b1")),
			want:    "step 6",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got := New().Run(t.Context(), stopping(transcript(t, tt.records...)))
			if got.Decision != hooks.Allow {
				t.Errorf("Decision = %v, want Allow: the refusal travels in the directive", got.Decision)
			}
			if got.Directive.StopDecision != "block" {
				t.Fatalf("StopDecision = %q, want %q", got.Directive.StopDecision, "block")
			}
			if !strings.Contains(got.Directive.Reason, tt.want) {
				t.Errorf("Reason does not name %q:\n%s", tt.want, got.Directive.Reason)
			}
			if got.Directive.SystemMessage == "" {
				t.Error("blocking said nothing to the user")
			}
			if got.Message != "" {
				t.Errorf("Message = %q, want nothing on standard error", got.Message)
			}
		})
	}
}

// TestRunBlocksOnceAReasonSaysHowToStopAnyway is the escape every block leaves
// open: an intentional stop costs one extra round rather than being trapped.
func TestRunBlocksOnceAReasonSaysHowToStopAnyway(t *testing.T) {
	t.Parallel()

	got := New().Run(t.Context(), stopping(transcript(t, withPR()...)))
	if !strings.Contains(got.Directive.Reason, "stop again") {
		t.Errorf("Reason does not say what an intentional stop should do:\n%s", got.Directive.Reason)
	}
}

func TestRunAllows(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		in   func(t *testing.T) hooks.Payload
	}{
		{
			name: "a run that marked the pull request ready",
			in: func(t *testing.T) hooks.Payload {
				return stopping(transcript(t, append(withReview(),
					bash("b8", "gh pr ready 7 -R o/r && gh pr view 7 -R o/r --json isDraft"), okResult("b8"))...))
			},
		},
		{
			// The second block of a turn would be blocking on a condition this
			// hook's own block cannot resolve.
			name: "a turn already held open by a stop hook",
			in: func(t *testing.T) hooks.Payload {
				p := stopping(transcript(t, withPR()...))
				p.StopHookActive = true
				return p
			},
		},
		{
			// Ending the turn to wait for the /simplify agents or a background
			// command is the behaviour no-op-wait-guard steers towards.
			name: "a turn ending with work still in flight",
			in: func(t *testing.T) hooks.Payload {
				p := stopping(transcript(t, withPR()...))
				p.BackgroundTasks = new(2)
				return p
			},
		},
		{
			// An absent array is the registry not answering, which is not the
			// same as nothing running.
			name: "a payload that does not say what is in flight",
			in: func(t *testing.T) hooks.Payload {
				p := stopping(transcript(t, withPR()...))
				p.BackgroundTasks = nil
				return p
			},
		},
		{
			name: "a plan the user rejected",
			in: func(t *testing.T) hooks.Payload {
				return stopping(transcript(t,
					invoke, skillBody, toolUse("p1", "ExitPlanMode", `{}`), errResult("p1")))
			},
		},
		{
			name: "a plan not yet put to the user",
			in: func(t *testing.T) hooks.Payload {
				return stopping(transcript(t, invoke, skillBody))
			},
		},
		{
			name: "another skill the user launched afterwards",
			in: func(t *testing.T) hooks.Payload {
				return stopping(transcript(t, append(withPR(), invokeDraft, skillBody)...))
			},
		},
		{
			name: "a transcript with no issue-handle launch in it",
			in: func(t *testing.T) hooks.Payload {
				return stopping(transcript(t, invokeDraft, skillBody,
					toolUse("p1", "ExitPlanMode", `{}`), result("p1")))
			},
		},
		{
			name: "a payload naming no transcript",
			in:   func(*testing.T) hooks.Payload { return hooks.Payload{BackgroundTasks: new(0)} },
		},
		{
			name: "a transcript that is not there",
			in: func(t *testing.T) hooks.Payload {
				return stopping(filepath.Join(t.TempDir(), "gone.jsonl"))
			},
		},
		{
			name: "a transcript with a line that will not parse",
			in: func(t *testing.T) hooks.Payload {
				return stopping(transcript(t, append(withPR(), "not json at all")...))
			},
		},
		{
			name: "a session still in plan mode",
			in: func(t *testing.T) hooks.Payload {
				p := stopping(transcript(t, withPR()...))
				p.PermissionMode = "plan"
				return p
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got := New().Run(t.Context(), tt.in(t))
			if got.Decision != hooks.Allow || got.Message != "" || !got.Directive.IsEmpty() {
				t.Errorf("Result = %+v, want an allow with nothing to say", got)
			}
		})
	}
}

// TestRunReadsATranscriptStillBeingWritten covers the tail of a file Claude
// Code is appending to: the last line may be half a record, and treating that
// as malformed would switch the guard off exactly when it is needed.
func TestRunReadsATranscriptStillBeingWritten(t *testing.T) {
	t.Parallel()

	p := filepath.Join(t.TempDir(), "session.jsonl")
	records := strings.Join(withPR(), "\n") + "\n" + `{"type":"assist`
	if err := os.WriteFile(p, []byte(records), 0o600); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	if got := New().Run(t.Context(), stopping(p)); got.Directive.StopDecision != "block" {
		t.Errorf("StopDecision = %q, want a block: a half-written tail is not a broken transcript",
			got.Directive.StopDecision)
	}
}

// longResult is a tool result past the 64KB a bufio.Scanner reads by default.
func longResult() string {
	return fmt.Sprintf(`{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"long","content":%q}]}}`,
		strings.Repeat("x", 200_000))
}
