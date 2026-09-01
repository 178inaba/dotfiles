package worktree

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Entry is one worktree of a repository.
//
// The shell parsed `git worktree list --porcelain` with awk in three different
// places, each cutting the fields its own way. One parser instead, because the
// format is one thing and three readings of it drift.
type Entry struct {
	Path string
	// Head is the commit the worktree has checked out.
	Head string
	// Branch is the short branch name, empty on a detached head.
	Branch string
	// Main says this is the repository's own worktree rather than a linked
	// one. It is the first entry git prints, and no command here treats it as
	// a worktree it may create, enter or remove.
	Main bool
}

// List returns the worktrees of the repository at root, the main one first.
func List(ctx context.Context, r runner.Runner, root string) ([]Entry, error) {
	out, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"-C", root, "worktree", "list", "--porcelain"}})
	if err != nil {
		return nil, fmt.Errorf("list the worktrees of %s: %w", root, err)
	}
	return parseList(string(out)), nil
}

// parseList reads `git worktree list --porcelain`, whose records are separated
// by a blank line and whose lines are a keyword and an optional value.
func parseList(out string) []Entry {
	var entries []Entry
	for record := range strings.SplitSeq(strings.TrimSpace(out), "\n\n") {
		var e Entry
		for line := range strings.SplitSeq(record, "\n") {
			key, value, _ := strings.Cut(line, " ")
			switch key {
			case "worktree":
				e.Path = value
			case "HEAD":
				e.Head = value
			case "branch":
				// Always a full ref here; the short name is what every caller
				// compares against.
				e.Branch = strings.TrimPrefix(value, "refs/heads/")
			}
		}
		if e.Path == "" {
			continue
		}
		e.Main = len(entries) == 0
		entries = append(entries, e)
	}
	return entries
}

// MainRoot returns the root of the repository dir belongs to, from a linked
// worktree as well as from the repository itself.
//
// The common git directory is what makes that work: every worktree shares one,
// and it sits inside the main worktree.
func MainRoot(ctx context.Context, r runner.Runner, dir string) (string, error) {
	out, err := r.Run(ctx, runner.Command{
		Dir:  dir,
		Name: "git",
		Args: []string{"rev-parse", "--path-format=absolute", "--git-common-dir"},
	})
	if err != nil {
		return "", fmt.Errorf("not inside a git repository")
	}
	return filepath.Dir(strings.TrimSpace(string(out))), nil
}
