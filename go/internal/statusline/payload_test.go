package statusline

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestParseFields(t *testing.T) {
	tests := []struct {
		name  string
		stdin string
		want  Fields
	}{
		{
			name: "every field",
			stdin: `{"session_id":"b257201c","workspace":{"current_dir":"/w","project_dir":"/p"},` +
				`"model":{"display_name":"Opus"},"cost":{"total_cost_usd":1.23,"total_duration_ms":5400000},` +
				`"context_window":{"used_percentage":42.5},` +
				`"rate_limits":{"five_hour":{"used_percentage":35,"resets_at":9999999999},` +
				`"seven_day":{"used_percentage":73,"resets_at":9999999998}}}`,
			want: Fields{
				CurrentDir: "/w", ProjectDir: "/p", ModelDisplayName: "Opus",
				TotalCostUSD: "1.23", ContextUsedPct: "42.5", TotalDurationMS: "5400000",
				FiveHourUsedPct: "35", SevenDayUsedPct: "73",
				FiveHourResetsAt: "9999999999", SevenDayResetsAt: "9999999998",
				SessionID: "b257201c",
			},
		},
		{
			name:  "workspace only",
			stdin: `{"workspace":{"current_dir":"/w","project_dir":"/p"}}`,
			want:  Fields{CurrentDir: "/w", ProjectDir: "/p"},
		},
		{
			name:  "empty stdin",
			stdin: "",
		},
		{
			// The shell reads stdin through $(cat), which drops every trailing
			// newline, so input that is nothing but newlines is no input.
			name:  "only newlines",
			stdin: "\n\n\n",
		},
		{
			// jq fails and the shell reads an empty result rather than an
			// error, so a malformed payload renders the fallback status line
			// instead of breaking it.
			name:  "malformed json",
			stdin: `{"workspace":`,
		},
		{
			// One bad shape loses every field, not just its own: the shell ran
			// all eleven lookups through a single jq program, which prints
			// nothing once any of them fails.
			name:  "a scalar where an object belongs",
			stdin: `{"workspace":"x","session_id":"otherwise-fine"}`,
		},
		{
			name:  "an array where an object belongs",
			stdin: `{"workspace":[],"session_id":"also-lost"}`,
		},
		{
			// A missing branch is null, and null indexes to null rather than
			// failing, so unrelated fields survive.
			name:  "absent objects",
			stdin: `{"session_id":"kept"}`,
			want:  Fields{SessionID: "kept"},
		},
		{
			// A zero is a value, not an absent field: a session that has just
			// started really does render 0%.
			name:  "zero is a value",
			stdin: `{"context_window":{"used_percentage":0},"rate_limits":{"five_hour":{"used_percentage":0}}}`,
			want:  Fields{ContextUsedPct: "0", FiveHourUsedPct: "0"},
		},
		{
			name:  "null is absent",
			stdin: `{"model":{"display_name":null},"context_window":{"used_percentage":null}}`,
		},
		{
			// tostring hands back the literal the input carried, trailing zero
			// included.
			name:  "number literals are preserved",
			stdin: `{"cost":{"total_cost_usd":1.230},"context_window":{"used_percentage":100.0}}`,
			want:  Fields{TotalCostUSD: "1.230", ContextUsedPct: "100.0"},
		},
		{
			// Known divergence from the shell, and an improvement on it: it
			// joined the eleven values with newlines and read them back a line
			// at a time, so a value carrying one shifted every field after it.
			name:  "a newline in a value stays in that value",
			stdin: `{"workspace":{"current_dir":"a\nb","project_dir":"/p"},"model":{"display_name":"Opus"}}`,
			want:  Fields{CurrentDir: "a\nb", ProjectDir: "/p", ModelDisplayName: "Opus"},
		},
		{
			// The other known divergence: jq's tostring rendered whatever it
			// found, so these two produced "{\"a\":1}" and "123". Neither
			// shape occurs — display_name is a string and session_id a UUID —
			// and the alternative is decoding into any and re-implementing
			// tostring.
			name:  "a value of the wrong type loses every field",
			stdin: `{"model":{"display_name":{"a":1}}}`,
		},
		{
			name:  "a number where a string belongs loses every field",
			stdin: `{"session_id":123}`,
		},
		{
			// json.Number accepts a quoted number, which is what jq's tostring
			// did with one too.
			name:  "a quoted number is still a number",
			stdin: `{"context_window":{"used_percentage":"42"}}`,
			want:  Fields{ContextUsedPct: "42"},
		},
		{
			name:  "a top level null yields nothing",
			stdin: `null`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if diff := cmp.Diff(tt.want, ParseFields([]byte(tt.stdin))); diff != "" {
				t.Errorf("ParseFields mismatch (-want +got):\n%s", diff)
			}
		})
	}
}
