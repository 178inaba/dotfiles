// Command ccx dispatches the Claude Code extensions in this repository.
package main

import (
	"context"
	"os"

	"github.com/178inaba/dotfiles/go/internal/cmd"
)

func main() {
	// Nothing may read os.Stdin before Execute; see selfbuild.Run.
	os.Exit(cmd.Execute(context.Background(), os.Args[1:], os.Stdin, os.Stdout, os.Stderr))
}
