//go:build ignore

// Command generate writes the contract's doc table.
//
// Excluded from the build so that nothing links go/parser but this, and so
// that `go install ./...` has one binary to install rather than two. The work
// is in gen, which the lint step and its own tests read.
package main

import (
	"fmt"
	"os"

	"github.com/178inaba/dotfiles/go/internal/contract/gen"
)

func main() {
	if err := gen.Write("."); err != nil {
		fmt.Fprintf(os.Stderr, "generate: %v\n", err)
		os.Exit(1)
	}
}
