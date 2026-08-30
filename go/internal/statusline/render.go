package statusline

import (
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ccjson"
	"github.com/178inaba/dotfiles/go/internal/shellfmt"
	"github.com/178inaba/dotfiles/go/internal/statusline/gitstate"
	"github.com/178inaba/dotfiles/go/internal/statusline/prinfo"
)

// Terminal escapes, spelled out rather than generated. They are part of the
// output byte for byte, and the display was matched against the shell version
// with them in place.
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
	Fields ccjson.Fields
	// Home is what the leading path is abbreviated against.
	Home string
	// Cwd stands in for a payload that named no directory.
	Cwd string
	// Git is the cached repository fragment, empty outside a repository.
	Git string
	// PR is the cached pull request record, empty when there is none.
	PR string
	// Rate is the cached dollars-to-yen rate, empty when there is none and the
	// cost falls back to dollars.
	Rate string
	// Now is the clock the countdowns are measured against.
	Now int64
	// BuildError is the first line of a failed self-rebuild, empty when the
	// binary is current.
	BuildError string
}

// Render returns the exact bytes to write, trailing newline included.
//
// Three lines at most: the directory, then the repository and session, then the
// model and its counters. The middle line disappears outside a repository with
// no session id, but the last line never does — its colour codes are emitted
// unconditionally, so it is four escape sequences and nothing else when there
// is nothing to say. That is what the shell version did, and a status line that
// changed height as fields came and went would be worse anyway.
func Render(d Data) []byte {
	var b strings.Builder

	b.WriteString(blue + directory(d) + reset)

	if line := second(d); line != "" {
		b.WriteString("\n" + line)
	}

	b.WriteString("\n" + purple + strings.TrimPrefix(model(d), " ") + reset)
	b.WriteString(contextBar(d.Fields.ContextUsedPct))
	b.WriteString(rateLimits(d))
	b.WriteString(cyan + cost(d) + reset)
	b.WriteString(duration(d.Fields.TotalDurationMS))
	b.WriteString(warning(d.BuildError))
	b.WriteString("\n")

	return []byte(b.String())
}

// directory is the first line: the project, and the working directory after it
// when the two differ.
func directory(d Data) string {
	current := d.Fields.CurrentDir
	if current == "" {
		current = d.Cwd
	}
	project := d.Fields.ProjectDir
	if project == "" {
		project = current
	}

	out := shellfmt.AbbreviateHome(project, d.Home)
	if current != project {
		out += " > " + shellfmt.AbbreviateHome(current, d.Home)
	}
	return out
}

// second is the repository fragment, the pull request badge and the session id.
//
// The session id is always shown, so that a transcript can be found while the
// session is still running; it is on this line because the first can already be
// two paths long, and it appears alone when there is no repository.
func second(d Data) string {
	line := ""
	if d.Git != "" {
		line = green + strings.TrimPrefix(d.Git, " ") + reset + pullRequest(d)
	}
	if id := d.Fields.SessionID; id != "" {
		if line != "" {
			line += " "
		}
		line += gray + id + reset
	}
	return line
}

// pullRequest renders the badge, which is empty when the branch has no open
// pull request.
func pullRequest(d Data) string {
	if gitstate.BranchOf(d.Git) == "" {
		return ""
	}
	info, ok := prinfo.Parse(d.PR)
	if !ok {
		return ""
	}

	// "PR " stays in the terminal's own colour and only the number is coloured,
	// matching the dot on Claude Code's own badge. A state that is not in this
	// list — a draft, or something a future gh invents — is left uncoloured
	// rather than asserting that a review is pending.
	color := ""
	switch info.State {
	case prinfo.StateApproved:
		color = green
	case prinfo.StateChangesRequested:
		color = red
	case prinfo.StateReviewRequired, prinfo.StateNoReviewRequested:
		color = prYellow
	}

	text := "#" + info.Number
	if info.URL != "" {
		// Only the underlined number is the link, so that what is clickable
		// looks clickable.
		text = oscLink + info.URL + bel + underline + text + underlineOff + oscLink + bel
	}
	return " PR " + color + text + reset
}

// model is the display name in brackets, empty when the payload named none.
func model(d Data) string {
	if d.Fields.ModelDisplayName == "" {
		return ""
	}
	return " [" + d.Fields.ModelDisplayName + "]"
}

// contextBar is a ten-block gauge of the context window.
func contextBar(used string) string {
	if used == "" {
		return ""
	}
	// Truncated rather than rounded, so the gauge never claims a percentage the
	// session has not reached.
	pct := shellfmt.TruncateDecimal(used)
	n := number(pct)

	const width = 10
	filled := n * width / 100
	// Left uncorrected on purpose: a percentage over 100 draws a longer bar
	// instead of quietly capping, which is a visible symptom rather than a
	// hidden one.
	empty := width - filled
	bar := strings.Repeat("▓", max(filled, 0)) + strings.Repeat("░", max(empty, 0))

	return " " + thresholdColor(n) + bar + " " + pct + "%" + reset
}

// rateLimits is the five-hour and seven-day usage with the time until each
// resets.
func rateLimits(d Data) string {
	f := d.Fields
	if f.FiveHourUsedPct == "" && f.SevenDayUsedPct == "" {
		return ""
	}

	out := ""
	if f.FiveHourUsedPct != "" {
		out = window("5h", f.FiveHourUsedPct, f.FiveHourResetsAt, d.Now)
	}
	if f.SevenDayUsedPct != "" {
		if out != "" {
			out += " "
		}
		out += window("7d", f.SevenDayUsedPct, f.SevenDayResetsAt, d.Now)
	}
	return " " + out
}

// window renders one usage figure. The countdown sits outside the reset code so
// it takes the terminal's own colour rather than the threshold's.
func window(label, used, resetsAt string, now int64) string {
	pct := shellfmt.Round(used)
	out := thresholdColor(number(pct)) + label + ":" + pct + "%" + reset
	if left := countdown(resetsAt, now); left != "" {
		out += "(" + left + ")"
	}
	return out
}

// ShowsCost reports whether the cost segment will be rendered.
//
// The state layer asks before looking the exchange rate up, because looking it
// up is not free: a miss records an attempt and starts a background fetch, and
// a session with nothing to convert should do neither.
func ShowsCost(f ccjson.Fields) bool {
	// The cost hangs off the model segment: with no model named there is no
	// session to attribute it to.
	if f.ModelDisplayName == "" || f.TotalCostUSD == "" {
		return false
	}
	// Anything under a cent would render as zero and say nothing.
	cents := shellfmt.RoundFloat(shellfmt.AwkNumber(f.TotalCostUSD) * 100)
	return number(cents) >= 1
}

// cost is the session cost, in yen when a rate is cached and dollars otherwise.
func cost(d Data) string {
	if !ShowsCost(d.Fields) {
		return ""
	}
	if d.Rate == "" {
		return " $" + shellfmt.Money(d.Fields.TotalCostUSD)
	}
	return " ¥" + shellfmt.RoundFloat(shellfmt.AwkNumber(d.Fields.TotalCostUSD)*shellfmt.AwkNumber(d.Rate))
}

// duration is how long the session has been running, absent below a minute.
func duration(ms string) string {
	if ms == "" {
		return ""
	}
	d := humanDuration(number(shellfmt.TruncateDecimal(ms)) / 1000)
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
func countdown(resetsAt string, now int64) string {
	if resetsAt == "" {
		return ""
	}
	return humanDuration(number(shellfmt.TruncateDecimal(resetsAt)) - int(now))
}

// humanDuration renders a span at two units of precision, and renders nothing
// at all below a minute — a session that has just started shows no age rather
// than a number ticking every second.
func humanDuration(seconds int) string {
	if seconds <= 0 {
		return ""
	}
	days := seconds / 86400
	hours := seconds % 86400 / 3600
	minutes := seconds % 3600 / 60
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

// number reads an integer the way the shell's arithmetic did, where anything
// unparseable — an empty string included — is zero.
func number(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		return 0
	}
	return n
}
