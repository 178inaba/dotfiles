package statusline

import (
	"bytes"
	"encoding/json/v2"
	"time"
)

// Fields is the status line's view of the payload Claude Code writes to its
// standard input.
//
// The numbers are pointers so that an absent field stays distinguishable from a
// zero. The difference is visible: no rate-limit segment at all versus "5h:0%",
// and no context bar versus an empty one.
type Fields struct {
	SessionID string `json:"session_id"`
	Workspace struct {
		CurrentDir string `json:"current_dir"`
		ProjectDir string `json:"project_dir"`
	} `json:"workspace"`
	Model struct {
		DisplayName string `json:"display_name"`
	} `json:"model"`
	Cost struct {
		TotalUSD   *float64 `json:"total_cost_usd"`
		DurationMS *float64 `json:"total_duration_ms"`
	} `json:"cost"`
	ContextWindow struct {
		UsedPercentage *float64 `json:"used_percentage"`
	} `json:"context_window"`
	RateLimits struct {
		FiveHour rateWindow `json:"five_hour"`
		SevenDay rateWindow `json:"seven_day"`
	} `json:"rate_limits"`
}

// rateWindow is one usage window and when it resets.
type rateWindow struct {
	UsedPercentage *float64 `json:"used_percentage"`
	ResetsAt       *float64 `json:"resets_at"`
}

// ParseFields reads the payload. No input, malformed input and an unexpected
// shape all yield the zero Fields rather than an error, because the status line
// renders in every one of those cases and never reports them.
func ParseFields(stdin []byte) Fields {
	var f Fields
	if err := json.UnmarshalRead(bytes.NewReader(stdin), &f); err != nil {
		// One unexpected shape costs every field rather than its own, as it did
		// when all eleven values came from a single jq program that printed
		// nothing once any of its paths failed.
		return Fields{}
	}
	return f
}

// unixTime reads a payload timestamp, which is seconds since the epoch.
func unixTime(p *float64) (time.Time, bool) {
	if p == nil {
		return time.Time{}, false
	}
	return time.Unix(int64(*p), 0), true
}
