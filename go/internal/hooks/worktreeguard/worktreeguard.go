// Package worktreeguard stops a session working in a linked worktree from
// editing another tree of the same repository.
//
// EnterWorktree moves the session's working directory, but the absolute paths
// the edit tools take are not rewritten with it. An absolute path picked up
// before the switch — from a Read during investigation — still names the main
// tree, and reusing it silently edits the wrong one. Read returns the same
// content in either tree, so nothing gives it away until much later.
package worktreeguard

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// message is what the model is shown, and the path it should have used.
const message = `Blocked: this %[1]s targets a path outside the current worktree.

  session cwd (worktree): %[2]s
  target path:            %[3]s
  owning tree:            %[4]s (%[5]s)

This session works in a linked git worktree, but the target path belongs to
%[5]s of the same repository. This usually happens when an
absolute path obtained before the worktree switch (e.g. Read during
investigation) is reused for %[1]s, silently modifying the wrong tree.

Fix: re-run %[1]s with the corresponding path inside the worktree:
  %[6]s

If you really intend to modify that tree, do it explicitly via Bash
(e.g. git -C "%[4]s" ...) or confirm with the user first.
`

// Hook is the guard.
type Hook struct{ runner runner.Runner }

// New returns the hook.
func New(r runner.Runner) Hook { return Hook{runner: r} }

// Run implements the hook contract.
//
// Every question it cannot answer lets the call through. A guard that starts
// blocking edits because git was unavailable would be worse than the accident
// it prevents.
func (h Hook) Run(ctx context.Context, in hooks.Payload, stderr io.Writer) hooks.Result {
	switch in.ToolName {
	case "Edit", "Write", "NotebookEdit":
	default:
		return hooks.Result{}
	}
	// The edit tools promise an absolute path. A relative one means this is
	// not the payload this reads, so there is nothing to judge.
	if in.Dir == "" || !filepath.IsAbs(in.FilePath) {
		return hooks.Result{}
	}

	root, ok := h.linkedWorktree(ctx, in.Dir)
	if !ok {
		return hooks.Result{}
	}

	// Physical paths throughout, so that /tmp against /private/tmp does not
	// make a prefix comparison fail.
	rootPhys := physical(root)
	targetPhys := physical(in.FilePath)
	if within(targetPhys, rootPhys) {
		return hooks.Result{}
	}

	owner, label, ok := h.owner(ctx, in.Dir, rootPhys, targetPhys)
	if !ok {
		// Somewhere else entirely — a scratchpad, another project. Not this
		// guard's business.
		return hooks.Result{}
	}

	suggested := filepath.Join(rootPhys, strings.TrimPrefix(targetPhys, owner+"/"))
	fmt.Fprintf(stderr, message, in.ToolName, rootPhys, in.FilePath, owner, label, suggested)
	return hooks.Result{Decision: hooks.Block}
}

// linkedWorktree returns the root of the tree the session is standing in, and
// false when that is the main worktree or not a repository at all.
func (h Hook) linkedWorktree(ctx context.Context, dir string) (string, bool) {
	out, err := h.runner.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", dir, "rev-parse", "--path-format=absolute",
			"--show-toplevel", "--git-dir", "--git-common-dir"},
	})
	if err != nil {
		return "", false
	}
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	if len(lines) < 3 {
		return "", false
	}
	// A main worktree is the one whose git directory is the common one, and it
	// is free to edit whatever it likes.
	if lines[1] == lines[2] {
		return "", false
	}
	return lines[0], true
}

// owner returns the tree a path belongs to and how to describe it.
//
// The trees are read from git rather than guessed from where they sit on disk,
// which is what makes a bare repository with worktrees, or a worktree created
// outside the repository directory, classify correctly. A worktree can live
// inside the main tree, so the longest matching prefix wins.
func (h Hook) owner(ctx context.Context, dir, self, target string) (string, string, bool) {
	out, err := h.runner.Run(ctx, runner.Command{
		Name: "git", Args: []string{"-C", dir, "worktree", "list", "--porcelain"},
	})
	if err != nil {
		return "", "", false
	}

	var root, label string
	for i, tree := range parse(string(out)) {
		phys := physical(tree)
		if phys == self || !within(target, phys) || len(phys) <= len(root) {
			continue
		}
		root = phys
		// The listing leads with the main worktree, when there is one.
		if i == 0 {
			label = "the main tree"
		} else {
			label = "another worktree"
		}
	}
	return root, label, root != ""
}

// parse reads the worktree list, dropping the bare entry: it holds no files,
// so nothing can be edited in it.
func parse(out string) []string {
	var trees []string
	for line := range strings.Lines(out) {
		switch line = strings.TrimSpace(line); {
		case strings.HasPrefix(line, "worktree "):
			trees = append(trees, strings.TrimPrefix(line, "worktree "))
		case line == "bare" && len(trees) > 0:
			trees = trees[:len(trees)-1]
		}
	}
	return trees
}

// within reports whether a path is the tree or inside it.
func within(path, tree string) bool {
	return path == tree || strings.HasPrefix(path, tree+"/")
}

// physical resolves the symlinks in a path's directories.
//
// The last component is left alone, which is what lets a path that does not
// exist yet — a Write creating a file — be compared at all, and is also why a
// symlink to a file passes through unresolved. Resolving it would block an
// edit whose target merely happens to live in the repository.
func physical(path string) string {
	var suffix string
	for {
		if fi, err := os.Stat(path); err == nil && fi.IsDir() {
			break
		}
		parent := filepath.Dir(path)
		if parent == path {
			return path + suffix
		}
		suffix = "/" + filepath.Base(path) + suffix
		path = parent
	}
	real, err := filepath.EvalSymlinks(path)
	if err != nil {
		return path + suffix
	}
	return real + suffix
}
