package statusline

import (
	"strings"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/statusline/gitstate"
	"github.com/178inaba/dotfiles/go/internal/statusline/prinfo"
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
	t.Parallel()

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
			data: Data{Current: "/Users/x/proj", Home: "/Users/x"},
			want: "\x1b[0;34m~/proj\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// On a path boundary, so a sibling that merely shares the prefix is
			// left alone rather than rendered as "~-backup".
			name: "a sibling sharing the prefix is left alone",
			data: Data{Current: "/Users/x-backup/proj", Home: "/Users/x"},
			want: "\x1b[0;34m/Users/x-backup/proj\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			name: "a working directory below the project is shown after it",
			data: withProject(Data{Current: "/Users/x/proj/sub", Home: "/Users/x"}, "/Users/x/proj"),
			want: "\x1b[0;34m~/proj > ~/proj/sub\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			name: "every segment at once",
			data: full(),
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(main +1 ~1 ↑1)\x1b[0m \x1b[0;90mb257201c\x1b[0m\n" +
				"\x1b[0;35m[Opus]\x1b[0m \x1b[0;32m▓▓▓▓░░░░░░ 42%\x1b[0m " +
				"\x1b[0;32m5h:35%\x1b[0m \x1b[0;31m7d:95%\x1b[0m\x1b[0;36m ¥197\x1b[0m \x1b[0;36m1h30m\x1b[0m\n",
		},
		{
			// A session outside a repository still shows its id, so a
			// transcript can be found while the session is running.
			name: "the session id stands alone without a repository",
			data: withSession(Data{Current: "/tmp", Home: "/Users/x"}, "b257201c"),
			want: "\x1b[0;34m/tmp\x1b[0m\n" +
				"\x1b[0;90mb257201c\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// Only the underlined number is the link, and "PR " keeps the
			// terminal's own colour, so what is clickable looks clickable.
			name: "a pull request badge is a link on the number alone",
			data: Data{
				Current: "/w", Home: "/Users/x",
				Git: &gitstate.Status{Branch: "feat"},
				PR:  &prinfo.Info{Number: 123, State: prinfo.StateNoReviewRequested, URL: "https://example.test/pull/123"},
			},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(feat ↑∅)\x1b[0m PR \x1b[38;5;220m" +
				"\x1b]8;;https://example.test/pull/123\a\x1b[4m#123\x1b[24m\x1b]8;;\a\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// Red is the one review state that asks for work, so it is checked
			// on its own rather than left to the shared colour switch.
			name: "changes requested is red",
			data: Data{
				Current: "/w", Home: "/Users/x",
				Git: &gitstate.Status{Branch: "feat"},
				PR:  &prinfo.Info{Number: 125, State: prinfo.StateChangesRequested, URL: "https://example.test/pull/125"},
			},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(feat ↑∅)\x1b[0m PR \x1b[0;31m" +
				"\x1b]8;;https://example.test/pull/125\a\x1b[4m#125\x1b[24m\x1b]8;;\a\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// The three parts of the second line in one render: the badge sits
			// between the repository and the session id.
			name: "the badge sits between the branch and the session id",
			data: withSession(Data{
				Current: "/w", Home: "/Users/x",
				Git: &gitstate.Status{Branch: "feat"},
				PR:  &prinfo.Info{Number: 123, State: prinfo.StateNoReviewRequested, URL: "https://example.test/pull/123"},
			}, "b257201c"),
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(feat ↑∅)\x1b[0m PR \x1b[38;5;220m" +
				"\x1b]8;;https://example.test/pull/123\a\x1b[4m#123\x1b[24m\x1b]8;;\a\x1b[0m" +
				" \x1b[0;90mb257201c\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			name: "a pull request without a link is plain text",
			data: Data{
				Current: "/w", Home: "/Users/x",
				Git: &gitstate.Status{Branch: "feat"},
				PR:  &prinfo.Info{Number: 127, State: prinfo.StateApproved},
			},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(feat ↑∅)\x1b[0m PR \x1b[0;32m#127\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// A state nobody has seen before must not claim a review is
			// pending, so it falls back to no colour at all.
			name: "an unrecognised review state is left uncoloured",
			data: Data{
				Current: "/w", Home: "/Users/x",
				Git: &gitstate.Status{Branch: "feat"},
				PR:  &prinfo.Info{Number: 135, State: "SOME_FUTURE_VALUE"},
			},
			want: "\x1b[0;34m/w\x1b[0m\n" +
				"\x1b[0;32m(feat ↑∅)\x1b[0m PR #135\x1b[0m\n" +
				"\x1b[0;35m\x1b[0m\x1b[0;36m\x1b[0m\n",
		},
		{
			// Detached: no branch, so no badge even with a record in the cache.
			name: "no branch means no badge",
			data: Data{
				Current: "/w", Home: "/Users/x",
				Git: &gitstate.Status{},
				PR:  &prinfo.Info{Number: 123, State: prinfo.StateNoReviewRequested, URL: "https://e/1"},
			},
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
			t.Parallel()
			if diff := cmp.Diff([]byte(tt.want), Render(tt.data), escapes); diff != "" {
				t.Errorf("Render mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func withProject(d Data, project string) Data {
	d.Fields.Workspace.ProjectDir = project
	return d
}

func withSession(d Data, id string) Data {
	d.Fields.SessionID = id
	return d
}

func full() Data {
	d := Data{
		Current: "/w", Home: "/Users/x",
		Git:  &gitstate.Status{Branch: "main", HasUpstream: true, Ahead: 1, Staged: 1, Modified: 1},
		Rate: 160,
	}
	d.Fields.SessionID = "b257201c"
	d.Fields.Workspace.ProjectDir = "/w"
	d.Fields.Model.DisplayName = "Opus"
	d.Fields.Cost.TotalUSD, d.Fields.Cost.DurationMS = f64(1.23), f64(5400000)
	d.Fields.ContextWindow.UsedPercentage = f64(42.5)
	d.Fields.RateLimits.FiveHour.UsedPercentage = f64(35)
	d.Fields.RateLimits.SevenDay.UsedPercentage = f64(95)
	return d
}

func TestContextBar(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		used *float64
		want string
	}{
		{name: "absent renders nothing"},
		{name: "zero", used: f64(0), want: " \x1b[0;32m░░░░░░░░░░ 0%\x1b[0m"},
		// Truncated, not rounded: 42.9 is still 42.
		{name: "truncated to the whole percent", used: f64(42.9), want: " \x1b[0;32m▓▓▓▓░░░░░░ 42%\x1b[0m"},
		{name: "green below seventy", used: f64(69), want: " \x1b[0;32m▓▓▓▓▓▓░░░░ 69%\x1b[0m"},
		{name: "yellow from seventy", used: f64(70), want: " \x1b[1;33m▓▓▓▓▓▓▓░░░ 70%\x1b[0m"},
		{name: "red from ninety", used: f64(90), want: " \x1b[0;31m▓▓▓▓▓▓▓▓▓░ 90%\x1b[0m"},
		{name: "full", used: f64(100), want: " \x1b[0;31m▓▓▓▓▓▓▓▓▓▓ 100%\x1b[0m"},
		{
			// Over a hundred draws a longer bar rather than capping, so an
			// impossible number is visible instead of hidden.
			name: "over full", used: f64(150), want: " \x1b[0;31m▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 150%\x1b[0m",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := contextBar(tt.used); got != tt.want {
				t.Errorf("contextBar = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestHumanDuration(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		d    time.Duration
		want string
	}{
		// Below a minute there is nothing worth showing, and a counter ticking
		// every second would be worse than nothing.
		{name: "under a minute", d: 30 * time.Second},
		{name: "zero"},
		{name: "negative", d: -5 * time.Second},
		{name: "minutes", d: 90 * time.Second, want: "1m"},
		{name: "hours and minutes", d: 90 * time.Minute, want: "1h30m"},
		{name: "days and hours", d: 25 * time.Hour, want: "1d1h"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := humanDuration(tt.d); got != tt.want {
				t.Errorf("humanDuration(%v) = %q, want %q", tt.d, got, tt.want)
			}
		})
	}
}

func TestRateLimits(t *testing.T) {
	t.Parallel()

	now := time.Unix(1000, 0)

	tests := []struct {
		name  string
		build func(*Fields)
		want  string
	}{
		{name: "neither window", build: func(*Fields) {}},
		{
			name:  "five hour only",
			build: func(f *Fields) { f.RateLimits.FiveHour.UsedPercentage = f64(35) },
			want:  " \x1b[0;32m5h:35%\x1b[0m",
		},
		{
			name:  "seven day only",
			build: func(f *Fields) { f.RateLimits.SevenDay.UsedPercentage = f64(95) },
			want:  " \x1b[0;31m7d:95%\x1b[0m",
		},
		{
			// The countdown sits outside the reset code, so it takes the
			// terminal's colour rather than the threshold's.
			name: "a countdown follows the percentage",
			build: func(f *Fields) {
				f.RateLimits.FiveHour = rateWindow{UsedPercentage: f64(35), ResetsAt: f64(6400)}
			},
			want: " \x1b[0;32m5h:35%\x1b[0m(1h30m)",
		},
		{
			name: "a reset already past shows no countdown",
			build: func(f *Fields) {
				f.RateLimits.FiveHour = rateWindow{UsedPercentage: f64(35), ResetsAt: f64(1)}
			},
			want: " \x1b[0;32m5h:35%\x1b[0m",
		},
		{
			// Half to even, as C's printf and Go's fmt both round.
			name: "percentages round half to even",
			build: func(f *Fields) {
				f.RateLimits.FiveHour.UsedPercentage = f64(72.5)
				f.RateLimits.SevenDay.UsedPercentage = f64(89.5)
			},
			want: " \x1b[1;33m5h:72%\x1b[0m \x1b[0;31m7d:90%\x1b[0m",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			var f Fields
			tt.build(&f)
			if got := rateLimits(Data{Fields: f, Now: now}); got != tt.want {
				t.Errorf("rateLimits = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestCost(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		model string
		usd   *float64
		rate  float64
		want  string
	}{
		{name: "no model means no cost", usd: f64(1.23), rate: 160},
		{name: "no cost field", model: "Opus", rate: 160},
		// Below a cent the figure would round to zero and say nothing.
		{name: "below half a cent", model: "Opus", usd: f64(0.004), rate: 160},
		// Rounded rather than truncated, so this one still shows.
		{name: "just over half a cent", model: "Opus", usd: f64(0.006), rate: 160, want: " ¥1"},
		{name: "exactly a cent", model: "Opus", usd: f64(0.01), rate: 160, want: " ¥2"},
		{name: "zero", model: "Opus", usd: f64(0), rate: 160},
		{name: "converted to yen", model: "Opus", usd: f64(1.23), rate: 160, want: " ¥197"},
		// Without a rate the dollars are shown rather than nothing.
		{name: "dollars without a rate", model: "Opus", usd: f64(1.23), want: " $1.23"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			var d Data
			d.Fields.Model.DisplayName = tt.model
			d.Fields.Cost.TotalUSD = tt.usd
			d.Rate = tt.rate
			if got := cost(d); got != tt.want {
				t.Errorf("cost = %q, want %q", got, tt.want)
			}
		})
	}
}
