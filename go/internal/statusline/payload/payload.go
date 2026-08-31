// Package payload is the status line's view of the JSON Claude Code writes to
// its standard input.
//
// It is one of the four contracts the status line reads — beside git's
// porcelain output, gh's pull request JSON and the exchange rate API — and like
// those it is kept in a package of its own, so that a change to what Claude
// Code sends is a change in one place.
package payload

import (
	"bytes"
	"encoding/json/v2"
	"time"
)

// Fields is the part of the payload the status line renders.
//
// The types follow what the payload declares rather than simplifying it.
//
// The percentages and the reset times are pointers because Claude Code sends
// them only sometimes, and the difference is visible: no rate-limit segment at
// all versus "5h:0%", and no context bar versus an empty one. The cost and the
// duration are always sent, and an absent one would render exactly as a zero
// does, so a pointer would be describing a state that cannot arise.
//
// A duration and a reset time are integers because that is how Claude Code
// declares them — it rounds the reset time before sending it — while a cost and
// a percentage are the results of a division.
type Fields struct {
	SessionID     string        `json:"session_id"`
	Workspace     Workspace     `json:"workspace"`
	Model         Model         `json:"model"`
	Cost          Cost          `json:"cost"`
	ContextWindow ContextWindow `json:"context_window"`
	RateLimits    RateLimits    `json:"rate_limits"`
}

// Workspace is where the session is working.
type Workspace struct {
	CurrentDir string `json:"current_dir"`
	ProjectDir string `json:"project_dir"`
}

// Model is the model the session is talking to.
type Model struct {
	DisplayName string `json:"display_name"`
}

// Cost is what the session has spent and how long it has been running.
type Cost struct {
	TotalUSD   float64 `json:"total_cost_usd"`
	DurationMS int64   `json:"total_duration_ms"`
}

// ContextWindow is how much of the context window the session has used.
type ContextWindow struct {
	UsedPercentage *float64 `json:"used_percentage"`
}

// RateLimits are the usage windows. Each is absent until the first response of
// a session, and absent again once it has reset.
type RateLimits struct {
	FiveHour Window `json:"five_hour"`
	SevenDay Window `json:"seven_day"`
}

// Window is one usage window and when it resets.
type Window struct {
	UsedPercentage *float64 `json:"used_percentage"`
	ResetsAt       *int64   `json:"resets_at"`
}

// Parse reads the payload. No input, malformed input and an unexpected shape
// all yield the zero Fields rather than an error, because the status line
// renders in every one of those cases and never reports them.
func Parse(stdin []byte) Fields {
	var f Fields
	if err := json.UnmarshalRead(bytes.NewReader(stdin), &f); err != nil {
		// The partial result a failed decode leaves behind depends on the order
		// the members arrived in: a field ahead of the bad one keeps its value
		// and one behind it does not. Discarding the lot makes the render a
		// function of the payload rather than of the order it was written in.
		return Fields{}
	}
	return f
}

// UnixTime reads a payload timestamp, which is seconds since the epoch.
func UnixTime(p *int64) (time.Time, bool) {
	if p == nil {
		return time.Time{}, false
	}
	return time.Unix(*p, 0), true
}
