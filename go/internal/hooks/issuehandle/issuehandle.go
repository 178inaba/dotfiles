// Package issuehandle refuses the end of a turn while an /issue-handle run is
// still short of a Ready pull request.
//
// What it prevents: the run stopping after the /simplify summary with "次は PR
// 作成です" and waiting for the user to type 「続けて」 before the PR is created
// and the independent review runs. Measured over every transcript under
// ~/.claude/projects on 2026-09-05, this happened in about 7 of the 73
// issue-handle sessions that reached /simplify after the wording fix that was
// supposed to stop it — roughly one run in ten.
//
// Wording is the wrong layer, as it was for noopwait. In every stopped session
// the rule saying the summary is not the end of the turn was in context, no
// compaction had dropped it, and no hook intervened; sessions that ran through
// wrote the same summary and merely had a tool call in the same response. The
// model is not missing the instruction, it is ignoring it with some
// probability, so the fix has to be a refusal the harness enforces.
//
// It blocks at most once per turn: an input whose stop_hook_active is set is
// allowed through. An intentional stop — an escalation, a question only the
// user can answer — therefore costs one extra round rather than being trapped,
// and no condition this hook cannot resolve can hold a turn open. Claude Code
// caps consecutive continuations at eight in any case; this sits well inside
// that.
//
// The judgment comes from the transcript rather than from a state file the
// skill writes, because writing that file would depend on the model executing
// a command, which is the failure mode being fixed. Deriving it from git or gh
// state instead cannot tell "before PR creation" from "not an issue-handle run
// at all".
//
// A command the user typed is a record whose content is a string of the
// <command-message>, <command-name> and <command-args> tags alone. Whether it
// launched a skill is decided by the record after it: an expanded SKILL.md body
// arrives with isMeta set, while a built-in has no body and leaves a
// <local-command-stdout> record, and a launch that failed leaves
// <local-command-stderr>. The tags are read without depending on their order,
// because the two cases are written differently — a launch puts
// <command-message> first, and a built-in or a failed launch puts
// <command-name> first — and a rule that leant on that would be deciding by
// accident. The origin field is not usable either: real transcripts carry
// origin.kind "human" on some launches of the same skill and not others.
//
// Every question it cannot answer allows the turn to end: no transcript, an
// unreadable or malformed one, no launch to judge, plan mode, or background
// work still in flight. Two known limits fall the same way. A Bash call that
// merely mentions gh pr ready or gh pr create — a grep, a commit message —
// advances the stage, since this matches a token rather than parsing a shell.
// And because the transcript is written asynchronously, a gh pr ready whose
// result has not been flushed yet costs one extra block, which the next Stop
// allows through on stop_hook_active.
package issuehandle

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

// launch is the command whose run this guard holds open, and reviewer is the
// subagent type that stands for the review of step 7-1.
const (
	launch   = "/issue-handle"
	reviewer = "independent-reviewer"
)

// step is the one the run has yet to take, named as the skill names it.
type step struct {
	number string
	what   string
}

var (
	createPR  = step{"6", "push the branch and create the draft pull request"}
	askReview = step{"7-1", "launch the independent-reviewer subagent for the review"}
	answerIt  = step{"7-2 / 7-3", "apply what the review found, then pass the sync check and mark the pull request ready"}
)

// reason is what the model is told. It names the step rather than repeating the
// rule, because the run stopped with the rule already in context.
const reason = `Blocked: this /issue-handle run has not reached a Ready pull request.

The next step is step %s: %s.

The skill finishes by marking the pull request ready in step 7-3, after the
sync check, so the turn does not end before that. Carry on from there in this
same turn.

If this stop is intentional — an escalation, or a question only the user can
answer — say why you are stopping and stop again. This guard does not block
twice in one turn.
`

// systemMessage is what the user sees, so that a turn continuing on its own is
// not a mystery.
const systemMessage = "issue-handle-guard: refused the end of the turn — the run is still before step %s."

// Hook is the guard.
type Hook struct{}

// New returns the hook.
func New() Hook { return Hook{} }

// Run implements the hook contract.
func (Hook) Run(_ context.Context, in hooks.Payload) hooks.Result {
	switch {
	// Already continuing because of a stop hook, so this is the round an
	// intentional stop is entitled to.
	case in.StopHookActive:
		return hooks.Result{}
	// No answer about background work, or work still running: ending the turn
	// to wait for it is the correct behaviour.
	case in.BackgroundTasks == nil || *in.BackgroundTasks > 0:
		return hooks.Result{}
	case in.PermissionMode == "plan" || in.TranscriptPath == "":
		return hooks.Result{}
	}

	r, err := scan(in.TranscriptPath)
	if err != nil {
		return hooks.Result{}
	}
	next, blocked := r.next()
	if !blocked {
		return hooks.Result{}
	}

	return hooks.Result{Directive: hooks.Directive{
		SystemMessage: fmt.Sprintf(systemMessage, next.number),
		StopDecision:  "block",
		Reason:        fmt.Sprintf(reason, next.number, next.what),
	}}
}

// run is how far the last /issue-handle launch has got.
type run struct {
	// active is false until a launch of this skill is seen, and false again
	// once another skill the user typed supersedes it.
	active       bool
	planApproved bool
	prCreated    bool
	reviewed     bool
	ready        bool
}

// next names the step the run owes, and reports false when there is nothing to
// hold the turn open for.
func (r run) next() (step, bool) {
	switch {
	case !r.active || !r.planApproved || r.ready:
		return step{}, false
	case !r.prCreated:
		return createPR, true
	case !r.reviewed:
		return askReview, true
	default:
		return answerIt, true
	}
}

// kind is what a tool call whose result still matters was doing.
type kind int

const (
	planning kind = iota
	creating
	readying
)

// record is a transcript line, read only as far as this guard needs it.
type record struct {
	Type    string `json:"type"`
	IsMeta  bool   `json:"isMeta"`
	Message struct {
		// Content is a string on a prompt or a typed command and an array of
		// blocks on a tool call or its result, so it arrives raw and is
		// decoded once its shape is known.
		Content jsontext.Value `json:"content"`
	} `json:"message"`
}

// block is one entry of an array content.
type block struct {
	Type      string `json:"type"`
	Name      string `json:"name"`
	ID        string `json:"id"`
	ToolUseID string `json:"tool_use_id"`
	IsError   bool   `json:"is_error"`
	Input     struct {
		Command      string `json:"command"`
		SubagentType string `json:"subagent_type"`
	} `json:"input"`
}

// scan reads the transcript once from the top, keeping only the state of the
// most recent launch. A session transcript reaches tens of megabytes, so
// nothing is held but the current line.
func scan(path string) (run, error) {
	f, err := os.Open(path)
	if err != nil {
		return run{}, err
	}
	defer f.Close()

	var r run
	// pending holds the calls whose result decides a stage, and command holds
	// a typed command waiting for the next user record to say whether a skill
	// body followed it.
	pending, command := map[string]kind{}, ""

	// bufio.Reader rather than bufio.Scanner: a single record of more than a
	// megabyte is ordinary here, and a scan that stopped at a buffer limit
	// would miss a later gh pr ready and block a run that had finished.
	lines := bufio.NewReader(f)
	for {
		line, err := lines.ReadBytes('\n')
		if err != nil {
			// A tail with no newline is a write still in progress — the
			// transcript is written asynchronously — not a broken record.
			// Reading it as malformed would switch the guard off in the one
			// moment it is needed.
			if errors.Is(err, io.EOF) {
				return r, nil
			}
			return run{}, err
		}

		var rec record
		if err := json.Unmarshal(line, &rec); err != nil {
			return run{}, err
		}

		switch rec.Type {
		case "user":
			if command != "" {
				// The expanded skill body arrives in the record after the
				// tags, which is what separates a skill from a built-in: a
				// built-in leaves a <local-command-stdout> record instead.
				typedName := command
				command = ""
				if rec.IsMeta {
					r, pending = run{active: typedName == launch}, map[string]kind{}
					break
				}
			}
			if name, ok := typed(rec); ok {
				command = name
				break
			}
			for _, b := range blocks(rec) {
				if b.Type != "tool_result" {
					continue
				}
				k, ok := pending[b.ToolUseID]
				if !ok {
					continue
				}
				delete(pending, b.ToolUseID)
				// A result carries is_error only sometimes on success, so the
				// test is that it is not true rather than that it is absent.
				if b.IsError {
					continue
				}
				switch k {
				case planning:
					r.planApproved = true
				case creating:
					r.prCreated = true
				case readying:
					r.ready = true
				}
			}
		case "assistant":
			for _, b := range blocks(rec) {
				if b.Type != "tool_use" {
					continue
				}
				switch {
				case b.Name == "ExitPlanMode":
					pending[b.ID] = planning
				case b.Name == "Agent" && b.Input.SubagentType == reviewer:
					r.reviewed = true
				case b.Name == "Bash" && strings.Contains(b.Input.Command, "gh pr create"):
					pending[b.ID] = creating
				case b.Name == "Bash" && strings.Contains(b.Input.Command, "gh pr ready") &&
					!strings.Contains(b.Input.Command, "--undo"):
					pending[b.ID] = readying
				}
			}
		}
	}
}

// typed reads the command a user typed at the prompt, and reports false for
// everything else. Whether the command launched anything is not its question;
// that is the following record's to answer.
//
// The record opens with one of the tags and its content is a string. Both
// matter: a skill body is an array whose text can quote the same tags, as the
// body of the issue this guard came from does.
func typed(rec record) (string, bool) {
	if rec.IsMeta || rec.Message.Content.Kind() != '"' {
		return "", false
	}
	var s string
	if err := json.Unmarshal(rec.Message.Content, &s); err != nil {
		return "", false
	}
	s = strings.TrimSpace(s)
	if !strings.HasPrefix(s, "<command-message>") && !strings.HasPrefix(s, "<command-name>") {
		return "", false
	}
	_, rest, ok := strings.Cut(s, "<command-name>")
	if !ok {
		return "", false
	}
	name, _, ok := strings.Cut(rest, "</command-name>")
	if !ok {
		return "", false
	}
	return name, true
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
