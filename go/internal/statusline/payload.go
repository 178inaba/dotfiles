package statusline

import (
	"bytes"
	"encoding/json"
)

// Fields is the status line's view of the payload Claude Code writes to its
// standard input.
//
// Every value is a string because the shell only ever had strings to work with,
// and the formatting quirks the segments reproduce follow from that.
type Fields struct {
	CurrentDir       string
	ProjectDir       string
	ModelDisplayName string
	TotalCostUSD     string
	ContextUsedPct   string
	TotalDurationMS  string
	FiveHourUsedPct  string
	SevenDayUsedPct  string
	FiveHourResetsAt string
	SevenDayResetsAt string
	SessionID        string
}

// payload is the shape of the input. Only what the status line renders is
// listed; anything else Claude Code sends is ignored.
//
// The numbers are json.Number so each keeps the literal it arrived as: 1.230
// has to render as 1.230, which a float64 round trip would not.
type payload struct {
	SessionID string `json:"session_id"`
	Workspace struct {
		CurrentDir string `json:"current_dir"`
		ProjectDir string `json:"project_dir"`
	} `json:"workspace"`
	Model struct {
		DisplayName string `json:"display_name"`
	} `json:"model"`
	Cost struct {
		TotalUSD   json.Number `json:"total_cost_usd"`
		DurationMS json.Number `json:"total_duration_ms"`
	} `json:"cost"`
	ContextWindow struct {
		UsedPercentage json.Number `json:"used_percentage"`
	} `json:"context_window"`
	RateLimits struct {
		FiveHour rateWindow `json:"five_hour"`
		SevenDay rateWindow `json:"seven_day"`
	} `json:"rate_limits"`
}

type rateWindow struct {
	UsedPercentage json.Number `json:"used_percentage"`
	ResetsAt       json.Number `json:"resets_at"`
}

// ParseFields reads the payload. No input, malformed input and an unexpected
// shape all yield the zero Fields rather than an error, because the status line
// renders in every one of those cases and never reports them.
func ParseFields(stdin []byte) Fields {
	dec := json.NewDecoder(bytes.NewReader(stdin))
	dec.UseNumber()

	var p payload
	if err := dec.Decode(&p); err != nil {
		// One unexpected shape costs every field rather than its own. That is
		// what the shell did: all eleven values came from a single jq program,
		// which prints nothing at all once any one of its paths fails.
		return Fields{}
	}

	return Fields{
		CurrentDir:       p.Workspace.CurrentDir,
		ProjectDir:       p.Workspace.ProjectDir,
		ModelDisplayName: p.Model.DisplayName,
		TotalCostUSD:     p.Cost.TotalUSD.String(),
		ContextUsedPct:   p.ContextWindow.UsedPercentage.String(),
		TotalDurationMS:  p.Cost.DurationMS.String(),
		FiveHourUsedPct:  p.RateLimits.FiveHour.UsedPercentage.String(),
		SevenDayUsedPct:  p.RateLimits.SevenDay.UsedPercentage.String(),
		FiveHourResetsAt: p.RateLimits.FiveHour.ResetsAt.String(),
		SevenDayResetsAt: p.RateLimits.SevenDay.ResetsAt.String(),
		SessionID:        p.SessionID,
	}
}
