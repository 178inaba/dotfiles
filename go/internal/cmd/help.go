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
// command answers, what it needs to be true first, or what it does besides
// print. Everything below it is rendered from the types the command reads and
// writes, so a renamed field changes the help without anyone editing it.
type help struct {
	intro string
	// outputHeading names what the output block describes, for the commands
	// whose standard output is not the whole story: `ccx pr context` prints a
	// path and writes the document the path names.
	outputHeading string
	output        reflect.Type
	// inputHeading names the document the command reads. A command owns what
	// it accepts as much as what it prints.
	inputHeading string
	input        reflect.Type
	statuses     statuses
}

// renderFailed marks a help whose contract could not be rendered. Degrading
// here rather than panicking keeps one bad type from breaking every command,
// and a test asserts no registered command says it.
const renderFailed = "the contract could not be rendered"

// String is the Long text cobra prints.
func (h help) String() string {
	var b strings.Builder
	b.WriteString(strings.TrimSpace(h.intro) + "\n")

	if h.output != nil {
		b.WriteString("\n" + heading(h.outputHeading, "Output (JSON on standard output)") + ":\n")
		b.WriteString(renderContract(h.output, contract.Output))
	}
	if h.input != nil {
		b.WriteString("\n" + heading(h.inputHeading, "Input (JSON)") + ":\n")
		b.WriteString(renderContract(h.input, contract.Input))
	}
	if len(h.statuses) > 0 {
		b.WriteString("\nExit status:\n" + h.statuses.render())
	}
	return b.String()
}

func heading(given, fallback string) string {
	if given == "" {
		return fallback
	}
	return given
}

func renderContract(t reflect.Type, mode contract.Mode) string {
	out, err := contract.Render(t, mode)
	if err != nil {
		return fmt.Sprintf("  (%s: %v)\n", renderFailed, err)
	}
	return out
}
