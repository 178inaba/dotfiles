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

// TestEverySkillFacingCommandHasAContract is the list itself, written out.
//
// These are the commands a skill reads the output of, so these are the ones
// whose contract has to be obtainable from the command. The statusline, the
// hooks and the refresh commands are not here: a hook answers Claude Code with
// an exit status and no skill reads any of them.
func TestEverySkillFacingCommandHasAContract(t *testing.T) {
	want := []string{
		"issue tree",
		"issue sections schema", "issue sections list", "issue sections check", "issue sections find",
		"pr context", "pr freshness", "pr prepare-review", "pr post-review", "pr reply-threads",
		"worktree detect", "worktree create", "worktree resolve", "worktree checkout",
		"worktree collect", "worktree delete",
		"review pending", "review verify", "review clone",
		"skill frontmatter", "skill refs",
	}
	for _, path := range want {
		if _, ok := contracts[path]; !ok {
			t.Errorf("%q has no contract", path)
		}
	}
	if len(contracts) != len(want) {
		t.Errorf("contracts has %d entries, want the %d listed here", len(contracts), len(want))
	}
}
