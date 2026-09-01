package worktreeguard

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// trees is the set of layouts a session can be standing in.
type trees struct {
	base, main, worktree, other, manual, external string
	outside, bareOne, bareTwo, link               string
}

// build makes the fixtures with real git. A fake would only prove that the
// guard agrees with an idea of what git reports, and the classification it
// depends on — which tree owns a path — is entirely git's answer.
func build(t *testing.T) trees {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	w := trees{
		base:     base,
		main:     filepath.Join(base, "repo"),
		outside:  filepath.Join(base, "outside"),
		external: filepath.Join(base, "ext-wt"),
		bareOne:  filepath.Join(base, "bare-wt1"),
		bareTwo:  filepath.Join(base, "bare-wt2"),
		link:     filepath.Join(base, "link-wt"),
	}
	w.worktree = filepath.Join(w.main, ".claude", "worktrees", "feature-test")
	w.other = filepath.Join(w.main, ".claude", "worktrees", "other-wt")
	w.manual = filepath.Join(w.main, "wt-manual")

	gittest.InitWithCommit(t, w.main)
	gittest.Write(t, filepath.Join(w.main, "api", "handler.go"), "package api\n")
	gittest.Run(t, w.main, "add", ".")
	gittest.Run(t, w.main, "commit", "-qm", "api")

	gittest.Run(t, w.main, "worktree", "add", "-b", "wt-feature", w.worktree)
	gittest.Run(t, w.main, "worktree", "add", "-b", "wt-other", w.other)
	gittest.Run(t, w.main, "worktree", "add", "-b", "wt-manual", w.manual)
	gittest.Run(t, w.main, "worktree", "add", "-b", "wt-ext", w.external)

	gittest.Write(t, filepath.Join(w.outside, "notes.md"), "notes\n")
	// A sibling whose name merely starts with the repository's: a guard that
	// compares strings rather than paths would claim this one.
	gittest.Write(t, filepath.Join(base, "repo-extra", "x.txt"), "x\n")
	gittest.Write(t, filepath.Join(base, "unrelated.txt"), "x\n")
	if err := os.Symlink(w.worktree, w.link); err != nil {
		t.Fatalf("Symlink: %v", err)
	}

	bare := filepath.Join(base, "bare.git")
	gittest.Run(t, base, "clone", "-q", "--bare", w.main, bare)
	gittest.Run(t, bare, "worktree", "add", "-b", "wt-bare1", w.bareOne)
	gittest.Run(t, bare, "worktree", "add", "-b", "wt-bare2", w.bareTwo)
	return w
}

func TestRun(t *testing.T) {
	t.Parallel()
	w := build(t)

	tests := []struct {
		name string
		in   hooks.Payload
		want hooks.Decision
	}{
		// Allowed.
		{
			// What the dispatcher hands over when the input would not parse.
			// A guard that cannot read its input lets the call through.
			name: "an unreadable payload",
			in:   hooks.Payload{},
		},
		{name: "a tool that edits nothing", in: hooks.Payload{ToolName: "Bash", Dir: w.worktree}},
		{name: "an edit with no path", in: hooks.Payload{ToolName: "Edit", Dir: w.worktree}},
		{name: "an edit with no working directory", in: edit(filepath.Join(w.main, "file.txt"), "")},
		{
			// The tools promise an absolute path; a relative one means the
			// payload is not what this reads, so there is nothing to judge.
			name: "a relative path", in: edit("api/handler.go", w.worktree),
		},
		{name: "a session outside any repository", in: edit(filepath.Join(w.main, "file.txt"), w.outside)},
		{
			// The guard is about a worktree session reaching out, not about the
			// main tree reaching in.
			name: "the main tree editing a worktree", in: edit(filepath.Join(w.worktree, "file.txt"), w.main),
		},
		{name: "a worktree editing its own file", in: edit(filepath.Join(w.worktree, "api", "handler.go"), w.worktree)},
		{name: "a worktree creating a file it does not have yet", in: writeTool(filepath.Join(w.worktree, "new", "dir", "new.txt"), w.worktree)},
		{name: "a path in no tree at all", in: edit(filepath.Join(w.outside, "notes.md"), w.worktree)},
		{name: "a sibling whose name shares a prefix", in: edit(filepath.Join(w.base, "repo-extra", "x.txt"), w.worktree)},
		{name: "the same worktree reached through a symlink", in: edit(filepath.Join(w.link, "file.txt"), w.worktree)},
		{name: "a bare layout editing a path in no worktree", in: edit(filepath.Join(w.base, "unrelated.txt"), w.bareOne)},

		// Blocked.
		{
			name: "a worktree editing the main tree",
			in:   edit(filepath.Join(w.main, "api", "handler.go"), w.worktree), want: hooks.Block,
		},
		{
			name: "a subdirectory of a worktree editing the main tree",
			in:   edit(filepath.Join(w.main, "api", "handler.go"), filepath.Join(w.worktree, "api")), want: hooks.Block,
		},
		{
			name: "a worktree creating a file in the main tree",
			in:   writeTool(filepath.Join(w.main, "api", "new.go"), w.worktree), want: hooks.Block,
		},
		{
			name: "a worktree editing a sibling worktree",
			in:   edit(filepath.Join(w.other, "file.txt"), w.worktree), want: hooks.Block,
		},
		{
			name: "a worktree editing a sibling that sits inside the main tree",
			in:   edit(filepath.Join(w.manual, "file.txt"), w.worktree), want: hooks.Block,
		},
		{
			name: "a worktree editing a sibling outside the repository",
			in:   edit(filepath.Join(w.external, "file.txt"), w.worktree), want: hooks.Block,
		},
		{
			name: "a bare layout editing a sibling worktree",
			in:   edit(filepath.Join(w.bareTwo, "file.txt"), w.bareOne), want: hooks.Block,
		},
		{
			// NotebookEdit names its target differently; the payload has
			// already folded the two into one field.
			name: "a notebook in the main tree",
			in: hooks.Payload{
				ToolName: "NotebookEdit", FilePath: filepath.Join(w.main, "nb.ipynb"), Dir: w.worktree,
			},
			want: hooks.Block,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got := New(runner.Exec{}).Run(t.Context(), tt.in)
			if got.Decision != tt.want {
				t.Errorf("Decision = %d, want %d (message=%q)", got.Decision, tt.want, got.Message)
			}
			if tt.want == hooks.Allow && got.Message != "" {
				t.Errorf("message = %q, want none", got.Message)
			}
		})
	}
}

// TestBlockMessageSuggestsTheWorktreePath is the point of blocking: the model
// has to be handed the path it meant, or it retries the same edit.
func TestBlockMessageSuggestsTheWorktreePath(t *testing.T) {
	t.Parallel()
	w := build(t)

	tests := []struct {
		name string
		in   hooks.Payload
		// suffix is what the message has to contain: the path it suggests, or
		// the name it gives the owning tree.
		suffix string
	}{
		{
			name:   "a main tree path is remapped under the worktree",
			in:     edit(filepath.Join(w.main, "api", "handler.go"), w.worktree),
			suffix: "/.claude/worktrees/feature-test/api/handler.go",
		},
		{
			// wt-manual is a worktree of its own that happens to live inside
			// the main tree, so the path is relative to it and not to the main
			// tree: the longest prefix is the owner.
			name:   "a sibling inside the main tree is remapped from the sibling",
			in:     edit(filepath.Join(w.manual, "file.txt"), w.worktree),
			suffix: "/.claude/worktrees/feature-test/file.txt",
		},
		{
			// A bare repository has no main worktree, so neither of its
			// worktrees may be described as one. Dropping the bare entry from
			// the listing used to promote the first worktree into its place.
			name:   "neither worktree of a bare repository is the main tree",
			in:     edit(filepath.Join(w.bareOne, "file.txt"), w.bareTwo),
			suffix: "another worktree",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got := New(runner.Exec{}).Run(t.Context(), tt.in)
			if !strings.Contains(got.Message, tt.suffix) {
				t.Errorf("message does not suggest %q:\n%s", tt.suffix, got.Message)
			}
		})
	}
}

func edit(target, dir string) hooks.Payload {
	return hooks.Payload{ToolName: "Edit", FilePath: target, Dir: dir}
}

func writeTool(target, dir string) hooks.Payload {
	return hooks.Payload{ToolName: "Write", FilePath: target, Dir: dir}
}
