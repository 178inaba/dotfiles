package ccjson

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestParse(t *testing.T) {
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
			// jq refuses to index a string and exits non-zero, which loses
			// every field and not just the offending one.
			name:  "a scalar where an object belongs",
			stdin: `{"workspace":"x","session_id":"kept-by-a-typed-decoder"}`,
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
			// // in jq only replaces null and false, so a zero percentage is a
			// value and really does render as 0%.
			name:  "zero is a value, null and false are not",
			stdin: `{"context_window":{"used_percentage":0},"model":{"display_name":null},"session_id":false}`,
			want:  Fields{ContextUsedPct: "0"},
		},
		{
			// tostring hands back the literal the input carried, trailing zero
			// included.
			name:  "number literals are preserved",
			stdin: `{"cost":{"total_cost_usd":1.230},"context_window":{"used_percentage":100.0}}`,
			want:  Fields{TotalCostUSD: "1.230", ContextUsedPct: "100.0"},
		},
		{
			// The shell splits the eleven values on newlines, so a value
			// carrying one shifts every field after it. Reproduced rather than
			// fixed: a directory name with a newline in it is not worth a
			// divergence.
			name:  "a newline in a value shifts the rest",
			stdin: `{"workspace":{"current_dir":"a\nb","project_dir":"/p"},"model":{"display_name":"Opus"}}`,
			want:  Fields{CurrentDir: "a", ProjectDir: "b", ModelDisplayName: "/p", TotalCostUSD: "Opus"},
		},
		{
			name:  "a composite value becomes its json text",
			stdin: `{"model":{"display_name":{"a":1}}}`,
			want:  Fields{ModelDisplayName: `{"a":1}`},
		},
		{
			name:  "a top level null yields nothing",
			stdin: `null`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if diff := cmp.Diff(tt.want, Parse([]byte(tt.stdin))); diff != "" {
				t.Errorf("Parse mismatch (-want +got):\n%s", diff)
			}
		})
	}
}
