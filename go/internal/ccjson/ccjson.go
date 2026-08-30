// Package ccjson reads the JSON Claude Code writes to a status line command's
// standard input.
//
// It is deliberately not a struct with json tags. The shell version piped the
// payload through one jq program and read the eleven results back with as many
// `read` calls, and three behaviours of that pipeline are visible in the
// rendered output: jq aborts on the whole document if any path indexes a
// non-object, `//` treats only null and false as absent so a zero survives, and
// splitting the results on newlines shifts every later field when a value
// contains one. A typed decoder reproduces none of them, so the pipeline is
// modelled instead.
package ccjson

import (
	"bytes"
	"encoding/json"
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/shellfmt"
)

// Fields is the status line's view of the payload. Every value is the string
// jq's tostring produced, because the shell only ever had strings to work with
// and its formatting quirks follow from that.
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

// paths are the eleven lookups, in the order the shell read them back.
var paths = [][]string{
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

// Parse reads the payload. Every failure — no input, malformed JSON, a path
// that cannot be walked — yields the zero Fields rather than an error, because
// the status line renders in all of those cases and never reports them.
func Parse(stdin []byte) Fields {
	// $(cat) drops trailing newlines, and the shell then skipped the whole
	// pipeline when nothing was left.
	if shellfmt.Capture(stdin) == "" {
		return Fields{}
	}

	dec := json.NewDecoder(bytes.NewReader(stdin))
	// UseNumber keeps the literal the input carried: 1.230 has to render as
	// 1.230, which a float64 round trip would not.
	dec.UseNumber()
	var doc any
	if err := dec.Decode(&doc); err != nil {
		return Fields{}
	}

	values := make([]string, len(paths))
	for i, p := range paths {
		v, err := lookup(doc, p)
		if err != nil {
			// jq exits non-zero on the first bad path and prints nothing, so
			// one unexpected shape costs every field, not just its own.
			return Fields{}
		}
		values[i] = toString(v)
	}

	values = split(values)
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

// split puts the values back through the shell's transport: joined by newlines,
// stripped of the trailing ones by the command substitution, then read line by
// line. Doing it rather than assigning the values directly is what reproduces
// the shift a value containing a newline causes.
func split(values []string) []string {
	lines := strings.Split(shellfmt.Capture([]byte(strings.Join(values, "\n"))), "\n")
	out := make([]string, len(paths))
	copy(out, lines)
	return out
}

// lookup walks a path the way jq does: a missing or null branch yields null,
// and anything else that cannot be indexed is an error.
func lookup(doc any, path []string) (any, error) {
	cur := doc
	for _, key := range path {
		if cur == nil {
			return nil, nil
		}
		obj, ok := cur.(map[string]any)
		if !ok {
			return nil, fmt.Errorf("cannot index %T with %q", cur, key)
		}
		cur = obj[key]
	}
	return cur, nil
}

// toString is jq's `// "" | tostring`: null and false are absent, a number
// keeps its literal, a string is itself, and anything composite becomes its
// compact JSON text.
//
// Known divergence: jq preserves an object's key order and Go's map does not.
// No field read here has ever held an object.
func toString(v any) string {
	switch t := v.(type) {
	case nil:
		return ""
	case bool:
		if !t {
			return ""
		}
		return "true"
	case string:
		return t
	case json.Number:
		return t.String()
	default:
		b, err := json.Marshal(v)
		if err != nil {
			return ""
		}
		return string(b)
	}
}
