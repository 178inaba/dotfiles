// Package worktree creates, resolves and tidies up the git worktrees the
// skills work in.
package worktree

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// includeFile is the list of gitignored files to carry into a new worktree.
//
// Claude Code copies them itself when EnterWorktree makes the worktree; the
// commands here make theirs with `git worktree add`, which goes straight past
// that, so they reproduce it. One implementation rather than one per command,
// because the thing being reproduced is a single native behaviour and two
// copies of it would drift apart the next time it changes.
const includeFile = ".worktreeinclude"

// worktreesDir holds the worktrees of a repository, and never supplies a file
// to copy: what lives there belongs to another worktree.
const worktreesDir = ".claude/worktrees/"

// copyWorktreeInclude copies the files includeFile names from srcRoot into the
// worktree, and returns how many arrived along with whatever it refused to do.
//
// The list is read from the worktree rather than from srcRoot, because it is
// the checked-out commit that says what its own working copy needs. Its being
// absent, or a symlink, means there is nothing to do.
//
// Three things are refused, and every one of them is a way for a file to end up
// somewhere it was not asked to go. The worktree may be somebody else's pull
// request branch, so its committed contents — the list itself and any symlink
// among the destinations — are not to be trusted with the gitignored files of
// the repository they are being copied out of.
func copyWorktreeInclude(ctx context.Context, r runner.Runner, srcRoot, worktreePath string) (int, []string, error) {
	// Everything written goes through this rather than through plain paths.
	// os.Root follows a symlink inside the worktree and refuses one that leaves
	// it, which is the second of the three guards and the only one it can keep
	// on its own.
	root, err := os.OpenRoot(worktreePath)
	if err != nil {
		return 0, nil, fmt.Errorf("failed to resolve: %s", worktreePath)
	}
	defer root.Close()

	if info, err := root.Lstat(includeFile); err != nil || !info.Mode().IsRegular() {
		return 0, nil, nil
	}

	files, err := included(ctx, r, srcRoot, filepath.Join(worktreePath, includeFile))
	if err != nil {
		return 0, nil, err
	}

	var warnings []string
	copied := 0
	for _, file := range files {
		if strings.HasPrefix(file, worktreesDir) {
			continue
		}
		// A symlink among the sources would be copied as whatever it points
		// at, which is not what the list asked for.
		if info, err := os.Lstat(filepath.Join(srcRoot, file)); err == nil && info.Mode()&os.ModeSymlink != 0 {
			warnings = append(warnings, "skipped symlink in .worktreeinclude: "+file)
			continue
		}

		// Any refusal from the root is the guard: a directory on the way out
		// of the worktree is the only thing that reaches this, since the
		// worktree was created moments ago and is writable.
		if err := root.MkdirAll(filepath.Dir(file), 0o755); err != nil {
			warnings = append(warnings, "skipped .worktreeinclude entry (destination escapes worktree): "+file)
			continue
		}
		// The last component needs its own check, because a symlink that stays
		// inside the worktree is one os.Root will follow — and following it
		// would overwrite whatever the commit tracks at the other end.
		if info, err := root.Lstat(file); err == nil && info.Mode()&os.ModeSymlink != 0 {
			warnings = append(warnings, "skipped .worktreeinclude entry (destination is a committed symlink): "+file)
			continue
		}

		if err := copyInto(root, filepath.Join(srcRoot, file), file); err != nil {
			return copied, warnings, fmt.Errorf("failed to copy: %s", file)
		}
		copied++
	}
	return copied, warnings, nil
}

// included lists the files in srcRoot that the list names and git ignores.
//
// Both halves are needed and neither implies the other: a file may match a
// pattern in the list without being gitignored, and copying that one would
// duplicate something the worktree already gets from the commit. The shell
// piped one git into another; here the second listing is the gitignored
// untracked files and the answer is the intersection, in the order the first
// produced.
func included(ctx context.Context, r runner.Runner, srcRoot, list string) ([]string, error) {
	matching, err := lsFiles(ctx, r, srcRoot, "--exclude-from="+list)
	if err != nil {
		return nil, err
	}
	ignored, err := lsFiles(ctx, r, srcRoot, "--exclude-standard")
	if err != nil {
		return nil, err
	}

	var out []string
	for _, file := range matching {
		if slices.Contains(ignored, file) {
			out = append(out, file)
		}
	}
	return out, nil
}

// lsFiles lists the untracked files of srcRoot that the given exclude source
// covers.
func lsFiles(ctx context.Context, r runner.Runner, srcRoot, exclude string) ([]string, error) {
	out, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", srcRoot, "ls-files", "-z", "--others", "--ignored", exclude},
	})
	if err != nil {
		return nil, fmt.Errorf("list the files %s covers in %s: %w", exclude, srcRoot, err)
	}
	// -z, because a file name may contain anything but a NUL — and git quotes
	// the ones it cannot print otherwise.
	var files []string
	for name := range strings.SplitSeq(string(out), "\x00") {
		if name != "" {
			files = append(files, name)
		}
	}
	return files, nil
}

// copyInto copies one file into the worktree, keeping the mode and the
// modification time the way `cp -p` did: a secret arrives no more readable than
// it was.
func copyInto(root *os.Root, srcPath, name string) error {
	src, err := os.Open(srcPath)
	if err != nil {
		return err
	}
	defer src.Close()

	info, err := src.Stat()
	if err != nil {
		return err
	}
	dst, err := root.OpenFile(name, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, info.Mode().Perm())
	if err != nil {
		return err
	}
	if _, err := io.Copy(dst, src); err != nil {
		dst.Close()
		return err
	}
	if err := dst.Close(); err != nil {
		return err
	}
	// Explicitly, because the mode passed to OpenFile applies only when the
	// file did not already exist.
	if err := root.Chmod(name, info.Mode().Perm()); err != nil {
		return err
	}
	return root.Chtimes(name, time.Time{}, info.ModTime())
}
