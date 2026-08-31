package payload

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestParse(t *testing.T) {
	t.Parallel()

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
				SessionID:     "b257201c",
				Workspace:     Workspace{CurrentDir: "/w", ProjectDir: "/p"},
				Model:         Model{DisplayName: "Opus"},
				Cost:          Cost{TotalUSD: 1.23, DurationMS: 5400000},
				ContextWindow: ContextWindow{UsedPercentage: new(42.5)},
				RateLimits: RateLimits{
					FiveHour: Window{UsedPercentage: new(35.0), ResetsAt: new(int64(9999999999))},
					SevenDay: Window{UsedPercentage: new(73.0), ResetsAt: new(int64(9999999998))},
				},
			},
		},
		{
			name:  "workspace only",
			stdin: `{"workspace":{"current_dir":"/w","project_dir":"/p"}}`,
			want:  Fields{Workspace: Workspace{CurrentDir: "/w", ProjectDir: "/p"}},
		},
		{
			// A zero is a value, not an absent field: a session that has just
			// started really does render 0%, and an absent one renders no bar
			// at all. That is what the pointers are for.
			name:  "zero is a value",
			stdin: `{"context_window":{"used_percentage":0},"rate_limits":{"five_hour":{"used_percentage":0}}}`,
			want: Fields{
				ContextWindow: ContextWindow{UsedPercentage: new(0.0)},
				RateLimits:    RateLimits{FiveHour: Window{UsedPercentage: new(0.0)}},
			},
		},
		{
			name:  "null is absent",
			stdin: `{"model":{"display_name":null},"context_window":{"used_percentage":null}}`,
		},
		{
			// A percentage carries a fraction and a duration does not, which is
			// what the payload declares. Sending one where the other belongs is
			// a change of contract, and a display that empties says so where a
			// silently truncated timestamp would not.
			name:  "a fractional duration is rejected",
			stdin: `{"session_id":"b257201c","cost":{"total_duration_ms":5400000.5}}`,
		},
		{name: "no input", stdin: ""},
		{name: "only newlines", stdin: "\n\n\n"},
		{name: "malformed json", stdin: `{"workspace":`},
		{
			// One bad shape loses every field, not just its own. The decoder
			// would have kept session_id here, and would have dropped it had
			// the two members been the other way round; the display must not
			// turn on that.
			name:  "a scalar where an object belongs",
			stdin: `{"workspace":"x","session_id":"otherwise-fine"}`,
		},
		{
			name:  "a value of the wrong type loses every field",
			stdin: `{"session_id":123}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if diff := cmp.Diff(tt.want, Parse([]byte(tt.stdin))); diff != "" {
				t.Errorf("Parse mismatch (-want +got):\n%s", diff)
			}
		})
	}
}
