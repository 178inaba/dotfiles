// Command ccx dispatches the Claude Code extensions in this repository.
package main

import (
	"os"

	"github.com/178inaba/dotfiles/go/internal/cmd"
)

func main() {
	// Nothing may read os.Stdin before Execute: it runs the self-rebuild check
	// first, and a rebuild replaces this process with one that inherits the
	// argv but not whatever has already been consumed from the input pipe.
	os.Exit(cmd.Execute(os.Args[1:], os.Stdin, os.Stdout, os.Stderr))
}
