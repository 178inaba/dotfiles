package cmd

import (
	"bytes"
	"io"
	"strings"
	"testing"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/contract"

	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

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
			if len([]rune(line)) > contract.LineWidth {
				t.Errorf("%s: line is %d wide, over %d:\n%s", path, len([]rune(line)), contract.LineWidth, line)
			}
		}
	}
}

// TestContractsNameRealCommands keeps the table honest in the direction the
// help cannot show: a key that matches no command is a contract nobody prints.
//
// The binding is the key itself, since the help hook looks a command up by its
// path when help is asked for. A key that matches nothing is therefore the
// only way a contract can go unprinted.
func TestContractsNameRealCommands(t *testing.T) {
	root := newRootCmd(selfbuild.State{})

	paths := map[string]bool{}
	var walk func(*cobra.Command)
	walk = func(c *cobra.Command) {
		for _, sub := range c.Commands() {
			paths[commandPath(sub)] = true
			walk(sub)
		}
	}
	walk(root)

	for path := range contracts {
		if !paths[path] {
			t.Errorf("contracts has %q, which is not a command", path)
		}
	}
}

// TestHelpRendersTheContract is the other half: asking a command for help has
// to reach the table, which is what the hook on the root is for.
func TestHelpRendersTheContract(t *testing.T) {
	var out bytes.Buffer
	if code := run(t.Context(), []string{"worktree", "collect", "--help"}, nil, &out, io.Discard, selfbuild.State{}); code != 0 {
		t.Fatalf("--help exited %d", code)
	}
	for _, want := range []string{"Output (JSON on standard output)", "in_use_by_process", "Exit status:"} {
		if !strings.Contains(out.String(), want) {
			t.Errorf("help does not carry %q", want)
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
