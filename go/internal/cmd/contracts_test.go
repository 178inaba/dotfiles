package cmd

import (
	"strings"
	"testing"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// helpWidth is what a rendered contract is allowed to reach. The renderer
// wraps to it; a status line is written by hand and is checked here.
const helpWidth = 88

// TestContractsRender is the guard behind the degradation in help.String: a
// type that cannot be rendered produces a help saying so rather than a panic,
// and this is what stops one shipping.
func TestContractsRender(t *testing.T) {
	for path, h := range contracts {
		text := h.String()
		if strings.Contains(text, renderFailed) {
			t.Errorf("%s: %s", path, text)
		}
		for _, line := range strings.Split(text, "\n") {
			if len([]rune(line)) > helpWidth {
				t.Errorf("%s: line is %d wide, over %d:\n%s", path, len([]rune(line)), helpWidth, line)
			}
		}
	}
}

// TestContractsNameRealCommands keeps the table honest in the direction the
// help cannot show: a key that matches no command is a contract nobody prints,
// and a command wired to a missing key prints its Short and nothing else.
func TestContractsNameRealCommands(t *testing.T) {
	root := newRootCmd(selfbuild.State{})

	paths := map[string]*cobra.Command{}
	var walk func(*cobra.Command)
	walk = func(c *cobra.Command) {
		for _, sub := range c.Commands() {
			paths[strings.TrimPrefix(sub.CommandPath(), "ccx ")] = sub
			walk(sub)
		}
	}
	walk(root)

	for path := range contracts {
		c, ok := paths[path]
		if !ok {
			t.Errorf("contracts has %q, which is not a command", path)
			continue
		}
		if c.Long == "" {
			t.Errorf("%q has a contract but does not use it; add Long: longFor(%q)", path, path)
		}
	}
	for path, c := range paths {
		if c.Long != "" && contracts[path].intro == "" {
			t.Errorf("%q has a Long that came from somewhere other than the contract table", path)
		}
	}
}
