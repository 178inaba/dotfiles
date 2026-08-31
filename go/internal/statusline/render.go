package statusline

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/statusline/gitstate"
	"github.com/178inaba/dotfiles/go/internal/statusline/prinfo"
)

// Terminal escapes, spelled out rather than generated: they are part of the
// output byte for byte.
const (
	red       = "\x1b[0;31m"
	green     = "\x1b[0;32m"
	yellow    = "\x1b[1;33m"
	blue      = "\x1b[0;34m"
	cyan      = "\x1b[0;36m"
	purple    = "\x1b[0;35m"
	gray      = "\x1b[0;90m"
	underline = "\x1b[4m"
	// underlineOff rather than a full reset, so the colour set for the pull
	// request number survives the link.
	underlineOff = "\x1b[24m"
	// prYellow is a 256-colour yellow because the bold 1;33 one is reassigned
	// to something dim by the Solarized palettes this setup uses.
	prYellow = "\x1b[38;5;220m"
	reset    = "\x1b[0m"

	// A hyperlink is opened and closed with OSC 8. Terminals that do not
	// understand it fall back to plain text on Claude Code's side.
	oscLink = "\x1b]8;;"
	bel     = "\a"
)

// Data is everything one render needs: the payload as it was parsed, the state
// the caches held, and the outcome of the self-rebuild check.
type Data struct {
	Fields Fields
	// Home is what the leading path is abbreviated against.
	Home string
	// Current is the working directory the payload named, or the process's own
	// when it named none.
	Current string
	// Git is the repository state, nil outside a repository.
	Git *gitstate.Status
	// PR is the branch's pull request, nil when there is none.
	PR *prinfo.Info
	// Rate is the dollars-to-yen rate, zero when there is none and the cost
	// falls back to dollars.
	Rate float64
	// Now is the clock the countdowns are measured against.
	Now time.Time
	// BuildError is the first line of a failed self-rebuild, empty when the
	// binary is current.
	BuildError string
}

// Render returns the exact bytes to write, trailing newline included.
//
// Three lines at most: the directory, then the repository and session, then the
// model and its counters. The middle line disappears outside a repository with
// no session id; the last never does — its colour codes are emitted
// unconditionally, so with nothing to say it is four escape sequences.
func Render(d Data) []byte {
	var b strings.Builder

	b.WriteString(blue + directory(d) + reset)

	if line := second(d); line != "" {
		b.WriteString("\n" + line)
	}

	b.WriteString("\n" + purple + model(d) + reset)
	b.WriteString(contextBar(d.Fields.ContextWindow.UsedPercentage))
	b.WriteString(rateLimits(d))
	b.WriteString(cyan + cost(d) + reset)
	b.WriteString(duration(d.Fields.Cost.DurationMS))
	b.WriteString(warning(d.BuildError))
	b.WriteString("\n")

	return []byte(b.String())
}

// directory is the first line: the project, and the working directory after it
// when the two differ.
func directory(d Data) string {
	project := d.Fields.Workspace.ProjectDir
	if project == "" {
		project = d.Current
	}

	out := abbreviateHome(project, d.Home)
	if d.Current != project {
		out += " > " + abbreviateHome(d.Current, d.Home)
	}
	return out
}

// abbreviateHome replaces the home directory with a tilde, on a path boundary
// so that a sibling directory sharing the prefix is left alone.
func abbreviateHome(path, home string) string {
	if home == "" {
		return path
	}
	if path == home {
		return "~"
	}
	if rest, ok := strings.CutPrefix(path, home+"/"); ok {
		return "~/" + rest
	}
	return path
}

// second is the repository fragment, the pull request badge and the session id.
//
// The session id is always shown, so that a transcript can be found while the
// session is still running; it is on this line because the first can already be
// two paths long, and it appears alone when there is no repository.
func second(d Data) string {
	line := ""
	if d.Git != nil {
		line = green + d.Git.Segment() + reset + pullRequest(d)
	}
	if id := d.Fields.SessionID; id != "" {
		if line != "" {
			line += " "
		}
		line += gray + id + reset
	}
	return line
}

// pullRequest renders the badge for the current branch.
func pullRequest(d Data) string {
	// A detached head has no branch to have a pull request for.
	if d.PR == nil || d.PR.Number == 0 || d.Git == nil || d.Git.Branch == "" {
		return ""
	}

	// "PR " stays in the terminal's own colour and only the number is coloured,
	// matching the dot on Claude Code's own badge. A state that is not in this
	// list — a draft, or something a future gh invents — is left uncoloured
	// rather than asserting that a review is pending.
	color := ""
	switch d.PR.State {
	case prinfo.StateApproved:
		color = green
	case prinfo.StateChangesRequested:
		color = red
	case prinfo.StateReviewRequired, prinfo.StateNoReviewRequested:
		color = prYellow
	}

	// Only the underlined number is the link, so that what is clickable looks
	// clickable. There is always one to make: gh reports a url for every pull
	// request that has a number, and one without a number was returned above.
	number := underline + "#" + strconv.Itoa(d.PR.Number) + underlineOff
	return " PR " + color + oscLink + d.PR.URL + bel + number + oscLink + bel + reset
}

func model(d Data) string {
	if d.Fields.Model.DisplayName == "" {
		return ""
	}
	return "[" + d.Fields.Model.DisplayName + "]"
}

// contextBar is a ten-block gauge of the context window.
func contextBar(used *float64) string {
	if used == nil {
		return ""
	}
	// Truncated rather than rounded, so the gauge never claims a percentage the
	// session has not reached.
	pct := int(*used)

	const width = 10
	filled := pct * width / 100
	// Left uncorrected on purpose: a percentage over 100 draws a longer bar
	// instead of quietly capping, which is a visible symptom rather than a
	// hidden one.
	empty := width - filled
	bar := strings.Repeat("▓", max(filled, 0)) + strings.Repeat("░", max(empty, 0))

	return " " + thresholdColor(pct) + bar + " " + strconv.Itoa(pct) + "%" + reset
}

// rateLimits is the five-hour and seven-day usage with the time until each
// resets.
func rateLimits(d Data) string {
	five := window("5h", d.Fields.RateLimits.FiveHour, d.Now)
	seven := window("7d", d.Fields.RateLimits.SevenDay, d.Now)
	switch {
	case five == "" && seven == "":
		return ""
	case five == "":
		return " " + seven
	case seven == "":
		return " " + five
	default:
		return " " + five + " " + seven
	}
}

// window renders one usage figure, empty when the payload carried none.
//
// Rounded rather than truncated, unlike the context bar: this one is a figure
// and not a gauge. The countdown sits outside the reset code so it takes the
// terminal's own colour rather than the threshold's.
func window(label string, w rateWindow, now time.Time) string {
	if w.UsedPercentage == nil {
		return ""
	}
	pct := int(math.RoundToEven(*w.UsedPercentage))

	out := thresholdColor(pct) + label + ":" + strconv.Itoa(pct) + "%" + reset
	if left := countdown(w.ResetsAt, now); left != "" {
		out += "(" + left + ")"
	}
	return out
}

// showsCost reports whether the cost segment will be rendered.
//
// The state layer asks before looking the exchange rate up, because a miss
// there records an attempt and starts a background fetch.
func showsCost(f Fields) bool {
	// The cost hangs off the model segment: with no model named there is no
	// session to attribute it to.
	if f.Model.DisplayName == "" {
		return false
	}
	// Anything under a cent would render as zero and say nothing. Rounded to
	// the nearest cent rather than truncated, so a cost of 0.006 still shows.
	return math.RoundToEven(f.Cost.TotalUSD*100) >= 1
}

// cost is the session cost, in yen when a rate is cached and dollars otherwise.
func cost(d Data) string {
	if !showsCost(d.Fields) {
		return ""
	}
	usd := d.Fields.Cost.TotalUSD
	if d.Rate == 0 {
		return fmt.Sprintf(" $%.2f", usd)
	}
	return fmt.Sprintf(" ¥%.0f", usd*d.Rate)
}

// duration is how long the session has been running.
func duration(ms int64) string {
	d := humanDuration(time.Duration(ms) * time.Millisecond)
	if d == "" {
		return ""
	}
	return " " + cyan + d + reset
}

// warning reports a self-rebuild that failed, and keeps reporting it for as
// long as the source stays broken. It is the one thing this status line shows
// that the shell version did not: a stale binary is invisible otherwise, and
// the display is the only channel that can say so on every redraw.
func warning(buildError string) string {
	if buildError == "" {
		return ""
	}
	const limit = 60
	if r := []rune(buildError); len(r) > limit {
		buildError = string(r[:limit-1]) + "…"
	}
	return " " + red + "⚠ ccx build failed: " + buildError + reset
}

// countdown is the time left until a reset, empty once it has passed.
func countdown(resetsAt *int64, now time.Time) string {
	at, ok := unixTime(resetsAt)
	if !ok {
		return ""
	}
	return humanDuration(at.Sub(now))
}

// humanDuration renders a span at two units of precision, and renders nothing
// at all below a minute — a session that has just started shows no age rather
// than a number ticking every second.
func humanDuration(d time.Duration) string {
	if d <= 0 {
		return ""
	}
	days := int(d / (24 * time.Hour))
	hours := int(d % (24 * time.Hour) / time.Hour)
	minutes := int(d % time.Hour / time.Minute)
	switch {
	case days > 0:
		return strconv.Itoa(days) + "d" + strconv.Itoa(hours) + "h"
	case hours > 0:
		return strconv.Itoa(hours) + "h" + strconv.Itoa(minutes) + "m"
	case minutes > 0:
		return strconv.Itoa(minutes) + "m"
	default:
		return ""
	}
}

// thresholdColor is red past ninety percent, yellow past seventy, green below.
func thresholdColor(pct int) string {
	switch {
	case pct >= 90:
		return red
	case pct >= 70:
		return yellow
	default:
		return green
	}
}
