package cmd

import (
	"encoding/json/jsontext"
	"encoding/json/v2"
	"io"
	"reflect"

	"github.com/178inaba/dotfiles/go/internal/contract"
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
// What it looks like started as what `jq -n` and `jq -nc` produced, byte for
// byte, which is what let the output recorded from each shell script serve as
// the specification of its replacement. That job is done — the last shell
// script is gone — and it was never a contract anyone depends on, since the
// readers are a model and the `jq` invocations in SKILL.md, both of which
// ignore whitespace. What the goldens hold is now simply the format, and
// TestRenderJSON is what keeps it from drifting.

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

// render is also where a document the module produces meets its own
// declaration, this being the way out of the two boundaries one crosses.
//
// Against the bytes rather than the value, for the reason the way in checks
// them: a field left out by omitzero is indistinguishable from an empty one
// until it has been encoded. A violation here is a bug in this module's own
// producing code rather than bad input, so it fails the command and nothing is
// written.
func render(w io.Writer, v any, opts ...json.Options) error {
	b, err := json.Marshal(v, opts...)
	if err != nil {
		return err
	}
	// The producing type is what a refusal names, since there is no file here
	// to name and every exit is wrapped in silentError — a message saying only
	// "the output" would reach a developer with nothing to look under, and
	// several commands print more than one document. A nil interface has no
	// type to name and nothing declared on it, which is where that stops.
	if t := reflect.TypeOf(v); t != nil {
		if err := contract.Validate(b, t, t.String()); err != nil {
			return err
		}
	}
	_, err = w.Write(append(b, '\n'))
	return err
}
