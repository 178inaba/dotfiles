package cmd

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/contract"
)

// help is one command's whole --help text.
//
// Only intro is written by hand, and only because a type cannot say what a
// command answers, what has to be true first, or what it does besides print.
// Everything below it is rendered from the types the command reads and writes,
// so a renamed field changes the help without anyone editing it.
type help struct {
	intro  string
	blocks []block
	// statuses is what the command exits with. The same table RunE returns
	// from, so the numbers cannot disagree.
	statuses statuses
}

// block is one document a command deals in.
//
// A slice rather than an output and an input, because `ccx pr context` prints
// one document and writes another, and the one it writes is the one its
// callers actually read.
type block struct {
	heading string
	typ     reflect.Type
	mode    contract.Mode
}

// prints is the document a command writes to standard output.
func prints(t reflect.Type) block {
	return block{heading: "Output (JSON on standard output)", typ: t, mode: contract.Output}
}

// writes is a document a command puts somewhere other than standard output.
func writes(heading string, t reflect.Type) block {
	return block{heading: heading, typ: t, mode: contract.Output}
}

// reads is a document a command takes in.
func reads(heading string, t reflect.Type) block {
	return block{heading: heading, typ: t, mode: contract.Input}
}

// renderFailed marks a help whose contract could not be rendered. Degrading
// here rather than panicking keeps one bad type from breaking every command,
// and a test asserts no registered command says it.
const renderFailed = "the contract could not be rendered"

// String is the Long text cobra prints.
func (h help) String() string {
	var b strings.Builder
	b.WriteString(strings.TrimSpace(h.intro) + "\n")

	for _, blk := range h.blocks {
		b.WriteString("\n" + blk.heading + ":\n")
		out, err := contract.Render(blk.typ, blk.mode)
		if err != nil {
			out = fmt.Sprintf("  (%s: %v)\n", renderFailed, err)
		}
		b.WriteString(out)
	}
	if len(h.statuses) > 0 {
		b.WriteString("\nExit status:\n" + h.statuses.render())
	}
	return b.String()
}
