// Package findinglist refuses to create or ready a pull request while a
// finding-severity launch in the session has not produced its disposition
// list.
//
// What it prevents: finding-severity ends in a list under one heading with
// three buckets, and the third — findings whose fix contradicts the Issue —
// is the only way such a finding reaches the user. A run that launches the
// skill and never writes the list skips those findings without asking. The
// skill is in context when that happens; its output is what gets skipped.
// That is the layer noopwait records for wording: the model has the
// instruction and ignores it with some probability, so the fix is a refusal
// the harness enforces. Whether the list exists is a deterministic question
// about the transcript; which bucket a finding belongs in is a judgment, and
// this guard never makes it.
//
// It runs on PreToolUse for the two commands at which undisposed findings
// leave the session — creating the pull request and marking it ready — and
// not on Stop. A Stop guard cannot tell a premature stop from a turn that
// ends to wait for background agents or to ask the user, and blocks both of
// those. Neither applies here: a run about to publish is not waiting and not
// asking.
//
// The judgment comes from the transcript rather than from a file the skill
// writes, because writing that file would depend on the model running a
// command, which is the failure being fixed. The invariant is keyed on the
// finding-severity launch rather than on the skill that launched it: every
// launcher requires the list, so no launch detection and no step numbering
// is needed.
//
// Every question it cannot answer allows the call: no transcript, an
// unreadable one, no launch in it, or a call made inside a subagent — the
// session that owes the list is not the one making that call. A line that is
// not JSON is skipped and the rest still judged. Two known limits fall on the
// blocking side. The gate matches the command as a substring, so a grep or a
// commit message that merely mentions one of the two commands is judged too.
// And a list the model has just written may not be flushed to the transcript
// yet when the gate runs; that costs one extra block, and the retry passes.
package findinglist

import (
	"bufio"
	"context"
	"encoding/json/jsontext"
	"encoding/json/v2"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/hooks"
)

// Heading opens the list finding-severity asks for. The skill owns it; a test
// pins this copy to the skill's text.
const Heading = "### 指摘事項の対応判断"

const skill = "finding-severity"

// gates are the commands at which findings leave the session. --help on
// either publishes nothing, and issue-handle sends the run to ccx pr ready
// --help for how to read its output.
var gates = []string{"gh pr create", "ccx pr ready"}

// message is what the model is told. It names no path: the file a launcher
// reads for the Issue bucket exists under one launcher only.
const message = `Blocked: a finding-severity launch in this session has no disposition list.

  launches without a list: %d
  the most recent came right after: %s

Fix, before this command:
  1. Write the list for that launch, in finding-severity's format, as your own
     text: the heading "%s" at the start of a line, then its three buckets.
  2. If "Issue 側の修正が要る指摘" is not empty, handle it as the skill that
     launched finding-severity prescribes, before acting on those findings.
  3. Then run the blocked command again.
`

// Hook is the guard.
type Hook struct{}

// New returns the hook.
func New() Hook { return Hook{} }

// Run implements the hook contract.
func (Hook) Run(_ context.Context, in hooks.Payload) hooks.Result {
	if in.ToolName != "Bash" || in.AgentID != "" || !gated(in.Command) || in.TranscriptPath == "" {
		return hooks.Result{}
	}

	r, err := scan(in.TranscriptPath)
	if err != nil || r.missing == 0 {
		return hooks.Result{}
	}
	return hooks.Result{Decision: hooks.Block, Message: fmt.Sprintf(message, r.missing, r.last, Heading)}
}

func gated(command string) bool {
	if strings.Contains(command, "--help") {
		return false
	}
	for _, g := range gates {
		if strings.Contains(command, g) {
			return true
		}
	}
	return false
}

// tally is what a scan found.
type tally struct {
	missing int
	// last describes the tool call made just before the most recent launch
	// that has no list, so that the model can find it without the file.
	last string
}

// record is a transcript line, read only as far as this guard needs it.
type record struct {
	Type    string `json:"type"`
	Message struct {
		// Content is a string on a prompt and an array of blocks on a tool
		// call or a reply, so it arrives raw and is decoded once its shape
		// is known.
		Content jsontext.Value `json:"content"`
	} `json:"message"`
}

// block is one entry of an array content.
type block struct {
	Type  string `json:"type"`
	Name  string `json:"name"`
	Text  string `json:"text"`
	Input struct {
		Skill        string `json:"skill"`
		SubagentType string `json:"subagent_type"`
		Description  string `json:"description"`
	} `json:"input"`
}

// scan reads the transcript once from the top. A session transcript reaches
// tens of megabytes, so nothing is held but the current line and the pairing
// state.
func scan(path string) (tally, error) {
	f, err := os.Open(path)
	if err != nil {
		return tally{}, err
	}
	defer f.Close()

	var (
		t    tally
		open bool
		// prev describes the latest tool call, and cur the one before the
		// launch that is open.
		prev = "the start of the transcript"
		cur  string
	)

	// bufio.Reader rather than bufio.Scanner: a single record of more than a
	// megabyte is ordinary here, and a scan that stopped at a buffer limit
	// would miss every later list.
	lines := bufio.NewReader(f)
	for {
		line, err := lines.ReadBytes('\n')
		if err != nil && !errors.Is(err, io.EOF) {
			return tally{}, err
		}

		var rec record
		// A line that does not decode is skipped rather than ending the scan:
		// giving up there would let one broken record hide every launch
		// before it. A tail with no newline is a write in progress and lands
		// here too unless it is already whole.
		if json.Unmarshal(line, &rec) == nil && rec.Type == "assistant" {
			for _, b := range blocks(rec) {
				switch {
				case b.Type == "tool_use" && b.Name == "Skill" && b.Input.Skill == skill:
					if open {
						t.missing++
						t.last = cur
					}
					open, cur = true, prev
				case b.Type == "tool_use":
					prev = describe(b)
				case b.Type == "text" && open && startsALine(b.Text, Heading):
					open = false
				}
			}
		}

		if err != nil {
			if open {
				t.missing++
				t.last = cur
			}
			return t, nil
		}
	}
}

// describe names a tool call the way the model would look for it. An agent
// launched without a type — the reviewers a quality pass fans out — is told
// apart by its description instead.
func describe(b block) string {
	switch {
	case b.Name == "Agent" && b.Input.SubagentType != "":
		return fmt.Sprintf("Agent (%s)", b.Input.SubagentType)
	case b.Name == "Agent" && b.Input.Description != "":
		return fmt.Sprintf("Agent (%s)", b.Input.Description)
	case b.Name == "Skill" && b.Input.Skill != "":
		return fmt.Sprintf("Skill (%s)", b.Input.Skill)
	}
	return b.Name
}

// blocks reads an array content, and nothing at all from any other shape.
func blocks(rec record) []block {
	if rec.Message.Content.Kind() != '[' {
		return nil
	}
	var bs []block
	if err := json.Unmarshal(rec.Message.Content, &bs); err != nil {
		return nil
	}
	return bs
}

// startsALine reports whether some line of s opens with prefix. The skill
// body quotes the heading inside a fence too, but that record is a user
// record and never reaches here.
func startsALine(s, prefix string) bool {
	for line := range strings.Lines(s) {
		if strings.HasPrefix(line, prefix) {
			return true
		}
	}
	return false
}
