package statusline

import (
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ccjson"
	"github.com/178inaba/dotfiles/go/internal/shellfmt"
)

// Fields is the status line's view of the payload Claude Code writes to its
// standard input.
//
// Every value is the string jq's tostring produced, because the shell only ever
// had strings to work with and its formatting quirks follow from that.
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

// fieldPaths are the eleven lookups, in the order the shell read them back.
var fieldPaths = [][]string{
	{"workspace", "current_dir"},
	{"workspace", "project_dir"},
	{"model", "display_name"},
	{"cost", "total_cost_usd"},
	{"context_window", "used_percentage"},
	{"cost", "total_duration_ms"},
	{"rate_limits", "five_hour", "used_percentage"},
	{"rate_limits", "seven_day", "used_percentage"},
	{"rate_limits", "five_hour", "resets_at"},
	{"rate_limits", "seven_day", "resets_at"},
	{"session_id"},
}

// ParseFields reads the payload. Every failure — no input, malformed JSON, a
// path that cannot be walked — yields the zero Fields rather than an error,
// because the status line renders in all of those cases and never reports them.
func ParseFields(payload []byte) Fields {
	doc, ok := ccjson.Decode(payload)
	if !ok {
		return Fields{}
	}

	values := make([]string, len(fieldPaths))
	for i, p := range fieldPaths {
		v, err := ccjson.Lookup(doc, p)
		if err != nil {
			// jq exits non-zero on the first bad path and prints nothing, so
			// one unexpected shape costs every field, not just its own.
			return Fields{}
		}
		values[i] = ccjson.ToString(v)
	}

	// The eleven values went through the shell joined by newlines and were read
	// back one line at a time, so a value carrying a newline shifts every field
	// after it and trailing empty ones are lost. Reproduced rather than fixed:
	// a directory name with a newline in it is not worth a divergence.
	values = shellfmt.Lines(strings.Join(values, "\n"), len(fieldPaths))
	return Fields{
		CurrentDir:       values[0],
		ProjectDir:       values[1],
		ModelDisplayName: values[2],
		TotalCostUSD:     values[3],
		ContextUsedPct:   values[4],
		TotalDurationMS:  values[5],
		FiveHourUsedPct:  values[6],
		SevenDayUsedPct:  values[7],
		FiveHourResetsAt: values[8],
		SevenDayResetsAt: values[9],
		SessionID:        values[10],
	}
}
