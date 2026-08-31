// Package ccjson reads the JSON Claude Code writes to a command's standard
// input, reproducing the jq pipelines the shell implementations used.
//
// It is deliberately not a set of structs with json tags. Three behaviours of
// those pipelines are visible in what the commands print, and a typed decoder
// reproduces none of them:
//
//   - jq aborts on the whole document when any path indexes a non-object, so
//     one unexpected shape costs every field rather than its own;
//   - `// ""` treats only null and false as absent, so a zero is a value;
//   - `tostring` hands back the literal a number arrived as, trailing zero and
//     all.
//
// Each command supplies its own list of paths.
package ccjson

import (
	"bytes"
	"encoding/json"
	"fmt"

	"github.com/178inaba/dotfiles/go/internal/shellfmt"
)

// Decode reads a payload. The second value is false for anything the shell
// would have skipped or that jq would have rejected: no input, input that is
// nothing but newlines, and malformed JSON.
func Decode(stdin []byte) (any, bool) {
	// $(cat) drops trailing newlines, and the shell then skipped the whole
	// pipeline when nothing was left.
	if shellfmt.Capture(stdin) == "" {
		return nil, false
	}

	dec := json.NewDecoder(bytes.NewReader(stdin))
	// UseNumber keeps the literal the input carried: 1.230 has to render as
	// 1.230, which a float64 round trip would not.
	dec.UseNumber()
	var doc any
	if err := dec.Decode(&doc); err != nil {
		return nil, false
	}
	return doc, true
}

// Lookup walks a path the way jq does: a missing or null branch yields null,
// and anything else that cannot be indexed is an error.
//
// The error matters as much as the value. jq exits non-zero on the first bad
// path and prints nothing at all, so a caller that hits one has lost every
// field, not just this one.
func Lookup(doc any, path []string) (any, error) {
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

// ToString is jq's `// "" | tostring`: null and false are absent, a number
// keeps its literal, a string is itself, and anything composite becomes its
// compact JSON text.
//
// Known divergence: jq preserves an object's key order and Go's map does not.
// No field any command reads has ever held an object.
func ToString(v any) string {
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
