// Package hooks is the protocol every ccx hook speaks: the JSON Claude Code
// writes to a hook's standard input, and the exit status and JSON directive it
// reads back.
//
// The nine hooks live in packages beneath this one. What they have in common is
// only this contract; the dispatcher in internal/cmd declares the interface
// that binds them together, because it is the one that consumes it.
package hooks

import (
	"encoding/json/v2"
	"regexp"
)

// unknownSession stands in for a payload that names no session, so that a
// hook's state has somewhere to go rather than a path with a hole in it.
const unknownSession = "unknown"

// idAllowed is every character an id may contribute to a file name. Both ids
// become path components under the state directory, so anything else is
// dropped rather than escaped — this is where a session id of "../.." stops
// being able to name a file outside it.
var idAllowed = regexp.MustCompile(`[^A-Za-z0-9-]`)

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
}

// wire is the input as it arrives. Keeping it separate from Payload is what
// lets Parse hand back ids that are already safe to build a path from, without
// a raw field beside them for somebody to reach for by mistake.
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
		SessionID:        idAllowed.ReplaceAllString(w.SessionID, ""),
		AgentID:          idAllowed.ReplaceAllString(w.AgentID, ""),
		ToolName:         w.ToolName,
		Dir:              w.CWD,
		Message:          w.Message,
		NotificationType: w.NotificationType,
		Command:          w.ToolInput.Command,
		FilePath:         w.ToolInput.FilePath,
	}
	if p.SessionID == "" {
		p.SessionID = unknownSession
	}
	if p.FilePath == "" {
		p.FilePath = w.ToolInput.NotebookPath
	}
	return p
}

// Decision is the exit status a hook reached, and the whole of what it returns:
// a hook that cannot do its job has already chosen which of these that means,
// so there is no second error channel for a caller to weigh against this one.
type Decision int

const (
	// Allow lets the event proceed.
	Allow Decision = 0
	// Fail says the hook could not do its job. Claude Code shows the hook's
	// standard error and carries on.
	Fail Decision = 1
	// Block stops the tool call and shows the hook's standard error to the
	// model. It is a decision, not a failure.
	Block Decision = 2
)

// Result is what a hook decided and what it wants written to standard output.
type Result struct {
	Decision Decision
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
	// TerminalSequence is written to the terminal on the hook's behalf. Hooks
	// run in a session with no controlling terminal, so this is the only way
	// one can ring a bell.
	TerminalSequence string `json:"terminalSequence,omitempty"`
	// SystemMessage is shown to the user.
	SystemMessage string `json:"systemMessage,omitempty"`
}

// IsEmpty reports whether there is nothing here worth writing.
func (d Directive) IsEmpty() bool { return d == Directive{} }
