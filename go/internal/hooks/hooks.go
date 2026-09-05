// Package hooks is the protocol every ccx hook speaks: the JSON Claude Code
// writes to a hook's standard input, and the exit status, message and JSON
// directive it reads back.
//
// The ten hooks live in packages beneath this one, cut by what they do rather
// than by where Claude Code calls them from: notify holds the four that decide
// and deliver a notification, caffeinate the two that hold the machine awake,
// one package each for the three that inspect a tool call, and one for the
// guard that judges the end of a turn instead. What every one of them has in
// common is only this contract; the dispatcher in internal/cmd declares the
// interface that binds them together, because it is the one that consumes it.
package hooks

import (
	"context"
	"encoding/json/v2"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// unknownSession stands in for a payload that names no session, so that a
// hook's state has somewhere to go rather than a path with a hole in it.
const unknownSession = "unknown"

// Payload is the hook input, with the two ids already reduced to what may
// appear in a file name.
type Payload struct {
	// SessionID is unknownSession when the input names no session.
	SessionID string
	// AgentID is empty outside a subagent event.
	AgentID  string
	ToolName string
	// Dir is the session's working directory, the payload's cwd.
	Dir              string
	Message          string
	NotificationType string
	// Command is what a Bash tool call is about to run.
	Command string
	// FilePath is what an edit is about to write. NotebookEdit names it
	// notebook_path and the others file_path; no hook cares which key it came
	// from, only what is being edited.
	FilePath string
	// TranscriptPath names the session's transcript. Claude Code writes it
	// with a leading ~ on the Stop event, so it is not ready to be opened.
	TranscriptPath string
	// StopHookActive is true when the turn is already carrying on because a
	// Stop hook refused its end. It is how a guard avoids blocking twice on a
	// condition its own block will never resolve.
	StopHookActive bool
	// PermissionMode is the mode the session is in, "plan" among them.
	PermissionMode string
	// BackgroundTasks counts the tasks still in flight, and is nil when the
	// input gave no answer: the array is absent when Claude Code could not
	// reach the task registry, which is not the same as nothing running.
	BackgroundTasks *int
}

// wire is the input as it arrives. Keeping it separate from Payload is what
// lets Parse hand back ids that are already safe to build a path from, without
// a raw field beside it for somebody to reach for by mistake.
type wire struct {
	SessionID        string `json:"session_id"`
	AgentID          string `json:"agent_id"`
	ToolName         string `json:"tool_name"`
	CWD              string `json:"cwd"`
	Message          string `json:"message"`
	NotificationType string `json:"notification_type"`
	ToolInput        struct {
		Command      string `json:"command"`
		FilePath     string `json:"file_path"`
		NotebookPath string `json:"notebook_path"`
	} `json:"tool_input"`
	TranscriptPath string `json:"transcript_path"`
	StopHookActive bool   `json:"stop_hook_active"`
	PermissionMode string `json:"permission_mode"`
	// BackgroundTasks is a pointer so that an absent array stays distinct from
	// an empty one; only its length is read, so what the entries hold is left
	// to the harness to describe.
	BackgroundTasks *[]any `json:"background_tasks"`
}

// Parse reads the hook input.
//
// Input it cannot read produces the same payload as input that said nothing,
// because that is the state the shell hooks were left in when jq failed: every
// field empty, and each hook falling through to whichever of blocking nothing
// or notifying anyway it had chosen as its safe direction. encoding/json/v2
// turns away two inputs jq accepted — duplicate object keys and invalid UTF-8 —
// which lands them on that same path.
func Parse(in []byte) Payload {
	var w wire
	if err := json.Unmarshal(in, &w); err != nil {
		w = wire{}
	}

	p := Payload{
		SessionID:        fileSafe(w.SessionID),
		AgentID:          fileSafe(w.AgentID),
		ToolName:         w.ToolName,
		Dir:              w.CWD,
		Message:          w.Message,
		NotificationType: w.NotificationType,
		Command:          w.ToolInput.Command,
		FilePath:         w.ToolInput.FilePath,
		TranscriptPath:   w.TranscriptPath,
		StopHookActive:   w.StopHookActive,
		PermissionMode:   w.PermissionMode,
	}
	if w.BackgroundTasks != nil {
		n := len(*w.BackgroundTasks)
		p.BackgroundTasks = &n
	}
	if p.SessionID == "" {
		p.SessionID = unknownSession
	}
	if p.FilePath == "" {
		p.FilePath = w.ToolInput.NotebookPath
	}
	return p
}

// fileSafe drops everything an id may not contribute to a file name.
//
// Both ids become path components under the state directory, so anything else
// is removed rather than escaped — this is where a session id of "../.." stops
// being able to name a file outside it.
func fileSafe(id string) string {
	return strings.Map(func(r rune) rune {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9', r == '-':
			return r
		}
		return -1
	}, id)
}

// IsEditTool reports whether a tool call writes a file. The three names are
// here rather than in each hook that asks, for the same reason Payload folds
// file_path and notebook_path into one field: it is one idea.
func IsEditTool(name string) bool {
	switch name {
	case "Edit", "Write", "NotebookEdit":
		return true
	}
	return false
}

// Decision is the exit status a hook reached.
type Decision int

const (
	// Allow lets the event proceed.
	Allow Decision = 0
	// Fail says the hook could not do its job. Claude Code shows the message
	// and carries on.
	Fail Decision = 1
	// Block stops the tool call and shows the message to the model. It is a
	// decision, not a failure.
	Block Decision = 2
)

// Result is everything a hook has to say, and the whole of what it returns.
type Result struct {
	Decision Decision
	// Message goes to standard error, which is the channel Claude Code reads
	// for any exit status. It carries its own trailing newline.
	Message string
	// Directive is only read when the hook exits 0; Claude Code does not parse
	// the standard output of one that did not.
	Directive Directive
}

// Directive is the JSON object a hook may write to standard output.
//
// Returning it as a value rather than writing the bytes is what lets the
// dispatcher add a system message to whatever a hook produced without decoding
// its output again.
type Directive struct {
	// TerminalSequence is written to the terminal on the hook's behalf; see
	// the notify package for why that is the only way to ring a bell.
	TerminalSequence string `json:"terminalSequence,omitempty"`
	// SystemMessage is shown to the user.
	SystemMessage string `json:"systemMessage,omitempty"`
	// StopDecision is "block" when a Stop hook refuses the end of a turn.
	// It is not Result.Decision, which stays Allow: Claude Code reads this
	// object only from a hook that exited 0, so the refusal travels here
	// rather than in the exit status.
	StopDecision string `json:"decision,omitempty"`
	// Reason tells the model why it has to carry on, and Claude Code requires
	// it wherever StopDecision is set.
	Reason string `json:"reason,omitempty"`
}

// IsEmpty reports whether there is nothing here worth writing.
func (d Directive) IsEmpty() bool { return d == Directive{} }

// IsClaude reports whether a process is Claude Code itself.
//
// Two hooks ask: caffeinate, to tie a suppression's lifetime to the session,
// and notify, to record something a later reader can verify. Both would
// break in the same way if the answer drifted — Claude Code has already been
// both names once — so the rule has one owner.
func IsClaude(ctx context.Context, r runner.Runner, pid int) bool {
	out, err := r.Run(ctx, runner.Command{
		Name: "ps", Args: []string{"-o", "comm=", "-p", strconv.Itoa(pid)},
	})
	if err != nil {
		return false
	}
	switch strings.TrimSpace(string(out)) {
	case "claude", "node":
		return true
	}
	return false
}
