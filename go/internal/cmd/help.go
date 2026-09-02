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
// command answers or what it does besides print. The rest is rendered, so a
// renamed field changes the help without anyone editing it.
type help struct {
	intro  string
	blocks []block
	// The same table RunE returns from, so the numbers cannot disagree.
	statuses statuses
}

// block is one document a command deals in.
//
// A list rather than an output and an input: `ccx pr context` prints one
// document and writes another, and the one it writes is the one read.
type block struct {
	heading string
	typ     reflect.Type
	mode    contract.Mode
}

func prints(t reflect.Type) block {
	return block{heading: "Output (JSON on standard output)", typ: t, mode: contract.Output}
}

func writes(heading string, t reflect.Type) block {
	return block{heading: heading, typ: t, mode: contract.Output}
}

func reads(heading string, t reflect.Type) block {
	return block{heading: heading, typ: t, mode: contract.Input}
}

// renderFailed degrades rather than panicking, so one bad type does not break
// every command. A test asserts no registered command says it.
const renderFailed = "the contract could not be rendered"

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
