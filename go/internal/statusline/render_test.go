package statusline

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

// The expectations below are the bytes the bash implementation produced for the
// same input, captured from it under /bin/bash before it was deleted. They
// cover the whole of stdout — the first and last lines and the trailing newline
// included — which the shell suite never did: it only ever compared the second
// line and matched substrings elsewhere.
//
// escapes renders control bytes visibly, so a failure reads as a diff of what
// changed rather than a wall of identical-looking terminal codes.
var escapes = cmp.Transformer("escapes", func(b []byte) []string {
	r := strings.NewReplacer("\x1b", "<ESC>", "\a", "<BEL>")
	return strings.Split(r.Replace(string(b)), "\n")
})

func TestRender(t *testing.T) {
	tests := []struct {
		name string
		data Data
		want string
	}{
		{
			// The shortest possible status line, and the one that catches a
			// render built by joining non-empty segments: the last line's
			// colour codes are unconditional, so it is four escapes and nothing
			// else, and there is no second line at all.
			name: "nothing but a directory",
			data: Data{Current: "/tmp", Home: "/Users/x"},
			want: "\x1b[0;34m/tmp\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			name: "the home directory is abbreviated",
			data: Data{
				Fields:  Fields{ProjectDir: "/Users/x/proj"},
				Current: "/Users/x/proj",
				Home:    "/Users/x",
			},
			want: "\x1b[0;34m~/proj\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			name: "a working directory below the project is shown after it",
			data: Data{
				Fields:  Fields{ProjectDir: "/Users/x/proj"},
				Current: "/Users/x/proj/sub",
				Home:    "/Users/x",
			},
			want: "\x1b[0;34m~/proj > ~/proj/sub\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			name: "every segment at once",
			data: Data{
				Current: "/w",
				Fields: Fields{
					ProjectDir: "/w", SessionID: "b257201c",
					ModelDisplayName: "Opus", TotalCostUSD: "1.23", TotalDurationMS: "5400000",
					ContextUsedPct:  "42.5",
					FiveHourUsedPct: "35", SevenDayUsedPct: "95",
				},
				Home: "/Users/x",
				Git:  " (main +1 ~1 ↑1)",
				Rate: "160.00",
			},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(main +1 ~1 ↑1)\x1b[0m \x1b[0;90mb257201c\x1b[0m\n" +
				"\x1b[0;35m[Opus]\x1b[0m \x1b[0;32m▓▓▓▓░░░░░░ 42%\x1b[0m " +
				"\x1b[0;32m5h:35%\x1b[0m \x1b[0;31m7d:95%\x1b[0m\x1b[0;36m ¥197\x1b[0m \x1b[0;36m1h30m\x1b[0m\n",
		},
		{
			// A session outside a repository still shows its id, so a
			// transcript can be found while the session is running.
			name: "the session id stands alone without a repository",
			data: Data{Current: "/tmp", Home: "/Users/x", Fields: Fields{SessionID: "b257201c"}},
			want: "\x1b[0;34m/tmp\x1b[0m\n" +
				"\x1b[0;90mb257201c\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// Only the underlined number is the link, and "PR " keeps the
			// terminal's own colour, so what is clickable looks clickable.
			name: "a pull request badge is a link on the number alone",
			data: Data{
				Current: "/w", Home: "/Users/x", Git: " (feat ↑∅)",
				PR: "123 NONE https://example.test/pull/123",
			},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(feat ↑∅)\x1b[0m PR \x1b[38;5;220m" +
				"\x1b]8;;https://example.test/pull/123\a\x1b[4m#123\x1b[24m\x1b]8;;\a\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			name: "a pull request without a link is plain text",
			data: Data{Current: "/w", Home: "/Users/x", Git: " (feat ↑∅)", PR: "127 APPROVED "},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(feat ↑∅)\x1b[0m PR \x1b[0;32m#127\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// A state nobody has seen before must not claim a review is
			// pending, so it falls back to no colour at all.
			name: "an unrecognised review state is left uncoloured",
			data: Data{Current: "/w", Home: "/Users/x", Git: " (feat ↑∅)", PR: "135 SOME_FUTURE_VALUE "},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(feat ↑∅)\x1b[0m PR #135\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// Detached: no branch, so no badge even with a record in the cache.
			name: "no branch means no badge",
			data: Data{Current: "/w", Home: "/Users/x", Git: " ()", PR: "123 NONE https://e/1"},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m()\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// The one thing this renders that the shell version did not: a
			// stale binary is invisible otherwise, and the display is the only
			// channel that can say so on every redraw.
			name: "a failed self-rebuild is reported on every redraw",
			data: Data{Current: "/w", Home: "/Users/x", BuildError: "internal/statusline/render.go:12:2: undefined: nope"},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m " +
				"\x1b[0;31m⚠ ccx build failed: internal/statusline/render.go:12:2: undefined: nope\x1b[0m\n",
		},
		{
			name: "a long build error is cut to one line",
			data: Data{Current: "/w", Home: "/Users/x", BuildError: strings.Repeat("x", 80)},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m " +
				"\x1b[0;31m⚠ ccx build failed: " + strings.Repeat("x", 59) + "…\x1b[0m\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if diff := cmp.Diff([]byte(tt.want), Render(tt.data), escapes); diff != "" {
				t.Errorf("Render mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestContextBar(t *testing.T) {
	tests := []struct {
		name, used, want string
	}{
		{name: "empty renders nothing", used: "", want: ""},
		{name: "zero", used: "0", want: " \x1b[0;32m░░░░░░░░░░ 0%\x1b[0m"},
		// Truncated, not rounded: 42.9 is still 42.
		{name: "truncated to the whole percent", used: "42.9", want: " \x1b[0;32m▓▓▓▓░░░░░░ 42%\x1b[0m"},
		{name: "green below seventy", used: "69", want: " \x1b[0;32m▓▓▓▓▓▓░░░░ 69%\x1b[0m"},
		{name: "yellow from seventy", used: "70", want: " \x1b[1;33m▓▓▓▓▓▓▓░░░ 70%\x1b[0m"},
		{name: "red from ninety", used: "90", want: " \x1b[0;31m▓▓▓▓▓▓▓▓▓░ 90%\x1b[0m"},
		{name: "full", used: "100", want: " \x1b[0;31m▓▓▓▓▓▓▓▓▓▓ 100%\x1b[0m"},
		{
			// Over a hundred draws a longer bar rather than capping, so an
			// impossible number is visible instead of hidden.
			name: "over full", used: "150", want: " \x1b[0;31m▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 150%\x1b[0m",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := contextBar(tt.used); got != tt.want {
				t.Errorf("contextBar(%q) = %q, want %q", tt.used, got, tt.want)
			}
		})
	}
}

func TestHumanDuration(t *testing.T) {
	tests := []struct {
		name    string
		seconds int
		want    string
	}{
		// Below a minute there is nothing worth showing, and a counter ticking
		// every second would be worse than nothing.
		{name: "under a minute", seconds: 30},
		{name: "zero", seconds: 0},
		{name: "negative", seconds: -5},
		{name: "minutes", seconds: 90, want: "1m"},
		{name: "hours and minutes", seconds: 5400, want: "1h30m"},
		{name: "days and hours", seconds: 90000, want: "1d1h"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := humanDuration(tt.seconds); got != tt.want {
				t.Errorf("humanDuration(%d) = %q, want %q", tt.seconds, got, tt.want)
			}
		})
	}
}

func TestRateLimits(t *testing.T) {
	const now = int64(1000)

	tests := []struct {
		name string
		f    Fields
		want string
	}{
		{name: "neither window", f: Fields{}, want: ""},
		{
			name: "five hour only",
			f:    Fields{FiveHourUsedPct: "35"},
			want: " \x1b[0;32m5h:35%\x1b[0m",
		},
		{
			name: "seven day only",
			f:    Fields{SevenDayUsedPct: "95"},
			want: " \x1b[0;31m7d:95%\x1b[0m",
		},
		{
			// The countdown sits outside the reset code, so it takes the
			// terminal's colour rather than the threshold's.
			name: "a countdown follows the percentage",
			f:    Fields{FiveHourUsedPct: "35", FiveHourResetsAt: "6400"},
			want: " \x1b[0;32m5h:35%\x1b[0m(1h30m)",
		},
		{
			name: "a reset already past shows no countdown",
			f:    Fields{FiveHourUsedPct: "35", FiveHourResetsAt: "1"},
			want: " \x1b[0;32m5h:35%\x1b[0m",
		},
		{
			// printf rounds half to even, so 72.5 is 72 and 89.5 is 90.
			name: "percentages round half to even",
			f:    Fields{FiveHourUsedPct: "72.5", SevenDayUsedPct: "89.5"},
			want: " \x1b[1;33m5h:72%\x1b[0m \x1b[0;31m7d:90%\x1b[0m",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := rateLimits(Data{Fields: tt.f, Now: now}); got != tt.want {
				t.Errorf("rateLimits = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestCost(t *testing.T) {
	tests := []struct {
		name      string
		model     string
		usd, rate string
		want      string
	}{
		{name: "no model means no cost", usd: "1.23", rate: "160.00", want: ""},
		{name: "no cost field", model: "Opus", want: ""},
		// Below a cent the figure would round to zero and say nothing.
		{name: "below a cent", model: "Opus", usd: "0.004", rate: "160.00", want: ""},
		{name: "exactly a cent", model: "Opus", usd: "0.01", rate: "160.00", want: " ¥2"},
		{name: "zero", model: "Opus", usd: "0", rate: "160.00", want: ""},
		{name: "converted to yen", model: "Opus", usd: "1.23", rate: "160.00", want: " ¥197"},
		// Without a rate the dollars are shown rather than nothing.
		{name: "dollars without a rate", model: "Opus", usd: "1.23", want: " $1.23"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d := Data{
				Fields: Fields{ModelDisplayName: tt.model, TotalCostUSD: tt.usd},
				Rate:   tt.rate,
			}
			if got := cost(d); got != tt.want {
				t.Errorf("cost = %q, want %q", got, tt.want)
			}
		})
	}
}
