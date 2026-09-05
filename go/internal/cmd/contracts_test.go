package cmd

import (
	"bytes"
	"io"
	"regexp"
	"strings"
	"testing"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/contract"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// TestContractsRender is what stops the degradation in help.String shipping:
// a type that cannot be rendered says so instead of panicking.
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

// goIdentifier is a word only a Go reader could resolve: capital-led parts run
// together, which is how this module spells a declaration and not English.
var goIdentifier = regexp.MustCompile(`\b[A-Z][a-z0-9]+([A-Z][A-Za-z0-9]*)+\b`)

var notAnIdentifier = map[string]bool{
	"GitHub": true, "GraphQL": true,
}

// TestNoGoNamesInTheRenderedContract is the guard behind the rule that a doc
// comment now has two readers. Three Go names had reached a published help
// before it existed.
//
// Only the rendered blocks: an intro is hand-written English and may
// legitimately name a Go type it is telling the reader about.
func TestNoGoNamesInTheRenderedContract(t *testing.T) {
	for path, h := range contracts {
		for _, blk := range h.blocks {
			text, err := contract.Render(blk.typ, blk.mode)
			if err != nil {
				t.Errorf("%s: %v", path, err)
				continue
			}
			for _, word := range goIdentifier.FindAllString(text, -1) {
				if !notAnIdentifier[word] {
					t.Errorf("%s: the contract names %q, which only a Go reader can resolve", path, word)
				}
			}
		}
	}
}

// TestContractsNameRealCommands catches the one way a contract goes unprinted:
// the help hook looks a command up by its path, so a key matching no command
// is a contract nobody reaches.
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

// TestHelpRendersTheContract is the other half: asking for help has to reach
// the table, which is what the hook on the root is for.
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

// TestEverySkillFacingCommandHasAContract is the list itself, written out: the
// commands a skill reads the output of. The statusline, the hooks and the
// refresh commands are not among them.
func TestEverySkillFacingCommandHasAContract(t *testing.T) {
	want := []string{
		"plan docs",
		"issue tree",
		"issue sections schema", "issue sections list", "issue sections check", "issue sections find",
		"pr context", "pr seen", "pr freshness", "pr prepare-review", "pr post-review", "pr reply-threads",
		"pr comment",
		"worktree detect", "worktree create", "worktree resolve", "worktree checkout",
		"worktree collect", "worktree delete",
		"review pending", "review verify", "review clone",
		"skill frontmatter", "skill contract",
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
