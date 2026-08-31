package worktreeguard

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

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
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}

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

	initRepo(t, w.main)
	write(t, filepath.Join(w.main, "api", "handler.go"), "package api\n")
	git(t, w.main, "add", ".")
	git(t, w.main, "commit", "-qm", "api")

	git(t, w.main, "worktree", "add", "-b", "wt-feature", w.worktree)
	git(t, w.main, "worktree", "add", "-b", "wt-other", w.other)
	git(t, w.main, "worktree", "add", "-b", "wt-manual", w.manual)
	git(t, w.main, "worktree", "add", "-b", "wt-ext", w.external)

	write(t, filepath.Join(w.outside, "notes.md"), "notes\n")
	// A sibling whose name merely starts with the repository's: a guard that
	// compares strings rather than paths would claim this one.
	write(t, filepath.Join(base, "repo-extra", "x.txt"), "x\n")
	write(t, filepath.Join(base, "unrelated.txt"), "x\n")
	if err := os.Symlink(w.worktree, w.link); err != nil {
		t.Fatalf("Symlink: %v", err)
	}

	bare := filepath.Join(base, "bare.git")
	git(t, base, "clone", "-q", "--bare", w.main, bare)
	git(t, bare, "worktree", "add", "-b", "wt-bare1", w.bareOne)
	git(t, bare, "worktree", "add", "-b", "wt-bare2", w.bareTwo)
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

			var stderr strings.Builder
			got := New(runner.Exec{}).Run(t.Context(), tt.in, &stderr)
			if got.Decision != tt.want {
				t.Errorf("Decision = %d, want %d (stderr=%q)", got.Decision, tt.want, stderr.String())
			}
			if tt.want == hooks.Allow && stderr.Len() != 0 {
				t.Errorf("stderr = %q, want empty", stderr.String())
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
		name   string
		in     hooks.Payload
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
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stderr strings.Builder
			New(runner.Exec{}).Run(t.Context(), tt.in, &stderr)
			if !strings.Contains(stderr.String(), tt.suffix) {
				t.Errorf("stderr does not suggest %q:\n%s", tt.suffix, stderr.String())
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

func initRepo(t *testing.T, dir string) {
	t.Helper()
	write(t, filepath.Join(dir, "file.txt"), "x\n")
	git(t, dir, "init", "-q")
	git(t, dir, "config", "user.email", "test@example.com")
	git(t, dir, "config", "user.name", "test")
	git(t, dir, "add", ".")
	git(t, dir, "commit", "-qm", "first")
}

func write(t *testing.T, name, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(name), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.WriteFile(name, []byte(content), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
}

func git(t *testing.T, dir string, args ...string) {
	t.Helper()
	cmd := exec.Command("git", args...)
	cmd.Dir = dir
	// Set on the command rather than with t.Setenv so the test stays parallel.
	cmd.Env = append(os.Environ(), "GIT_CONFIG_GLOBAL="+os.DevNull, "GIT_CONFIG_SYSTEM="+os.DevNull)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git %v: %v\n%s", args, err, out)
	}
}
