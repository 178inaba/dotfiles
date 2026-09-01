package cmd

import (
	"encoding/json/jsontext"
	"encoding/json/v2"
	"io"
)

// The script subcommands answer in JSON on standard output, and this is the
// only place in the module that turns a value into those bytes.
//
// Keeping it here rather than in the packages that produce the values is what
// the shell could not do: every script wrote its own output, which is why
// scripts/warnings-lib.sh had to exist to stop eleven spellings of the same
// contract from drifting apart. The implementation packages return typed
// values and never see an io.Writer, so there is one place that decides what
// the output looks like and one place to test it.
//
// What it looks like is what `jq -n` and `jq -nc` produced, byte for byte. That
// is not a contract anyone depends on — the readers are a model and the `jq`
// invocations in SKILL.md, both of which ignore whitespace — but it lets the
// output recorded from each shell script serve as the specification of its
// replacement. Three of jq's defaults are load-bearing for that: no HTML
// escaping, keys in the order the value declares them rather than sorted, and
// a trailing newline. encoding/json/v2 agrees on all three, so the parity costs
// nothing; if it ever stopped being free, the goldens would go rather than the
// design.

// renderJSON writes v as the indented JSON `jq -n` produced.
func renderJSON(w io.Writer, v any) error {
	return render(w, v, jsontext.WithIndent("  "))
}

// renderCompactJSON writes v as the single line `jq -nc` produced.
//
// The choice between the two is per exit rather than per command:
// respond-threads.sh answered its "no threads to act on" case with jq -n and
// its ordinary case with jq -nc, so a command that picked one shape for all of
// its exits would change one of them.
func renderCompactJSON(w io.Writer, v any) error {
	return render(w, v)
}

func render(w io.Writer, v any, opts ...json.Options) error {
	b, err := json.Marshal(v, opts...)
	if err != nil {
		return err
	}
	_, err = w.Write(append(b, '\n'))
	return err
}
