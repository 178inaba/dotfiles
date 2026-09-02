// Command gh stands in front of the real gh and guards its writing
// subcommands. It is installed into ~/.local/shims, which zsh/.zprofile puts
// ahead of Homebrew on PATH; see the ghshim package for what it refuses and
// why.
package main

import (
	"context"
	"os"

	"github.com/178inaba/dotfiles/go/internal/ghshim"
)

func main() {
	// Nothing may read os.Stdin: the hand-off leaves it for the real gh, and
	// see selfbuild.Run.
	os.Exit(ghshim.Execute(context.Background(), os.Args[1:], os.Stderr))
}
