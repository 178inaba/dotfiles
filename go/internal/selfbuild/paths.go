package selfbuild

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
	"syscall"
)

// sourceRoot derives <repo>/go from the stow symlink at ~/.claude/settings.json.
//
// Deriving it from the settings file rather than the working directory is what
// keeps a linked worktree from ever becoming the source root: the home symlink
// always points into the main tree, so editing <repo>/.claude/worktrees/x/go
// leaves the installed binary alone, which is the intended behaviour.
//
// The second return value is false whenever the layout does not hold, and the
// caller then does nothing at all. That silence is deliberate: the binary has
// to stay usable on a machine where this repository is not stowed.
func sourceRoot(d Deps) (string, bool) {
	if d.Home == "" {
		return "", false
	}
	settings := filepath.Join(d.Home, ".claude", "settings.json")

	fi, err := d.Lstat(settings)
	if err != nil || fi.Mode()&os.ModeSymlink == 0 {
		// Not a symlink means the configuration is not stowed from a checkout,
		// so there is no source to be stale against. os.Lstat rather than
		// filepath.EvalSymlinks because the latter happily returns a plain file
		// unchanged and would hide exactly this case.
		return "", false
	}
	link, err := d.Readlink(settings)
	if err != nil {
		return "", false
	}
	// stow writes relative links (../.dotfiles/claude/.claude/settings.json),
	// so resolve against the directory holding the link, not the process.
	if !filepath.IsAbs(link) {
		link = filepath.Join(filepath.Dir(settings), link)
	}
	// <repo>/claude/.claude/settings.json up three levels is <repo>.
	repo := filepath.Dir(filepath.Dir(filepath.Dir(filepath.Clean(link))))
	root := filepath.Join(repo, "go")
	if _, err := d.Stat(filepath.Join(root, "go.mod")); err != nil {
		return "", false
	}
	return root, true
}

// binDir is where go install writes, for an explicit GOBIN or the default one.
//
// It is resolved in process rather than by running `go env`: this runs on every
// invocation, and spawning the go command would cost more than the whole check.
func binDir(d Deps, gobin string) string {
	if gobin != "" {
		return gobin
	}
	if v := goEnv(d, "GOBIN"); v != "" {
		return v
	}
	gopath := goEnv(d, "GOPATH")
	if gopath == "" {
		gopath = filepath.Join(d.Home, "go")
	}
	return filepath.Join(gopath, "bin")
}

// goEnv reads a go command setting: the environment first, then the go env
// file, matching the go command's own precedence.
func goEnv(d Deps, key string) string {
	if v := d.Getenv(key); v != "" {
		return v
	}
	name := d.Getenv("GOENV")
	if name == "" {
		config, err := os.UserConfigDir()
		if err != nil {
			return ""
		}
		name = filepath.Join(config, "go", "env")
	}
	f, err := os.Open(name)
	if err != nil {
		return ""
	}
	defer f.Close()

	prefix := key + "="
	s := bufio.NewScanner(f)
	for s.Scan() {
		if line := s.Text(); strings.HasPrefix(line, prefix) {
			return strings.TrimPrefix(line, prefix)
		}
	}
	return ""
}

// samePath reports whether two paths name the same file. Symlinks are resolved
// so that a symlinked GOBIN still counts as installed, but an unresolvable path
// falls back to a textual comparison rather than claiming a mismatch.
func samePath(a, b string) bool {
	if filepath.Clean(a) == filepath.Clean(b) {
		return true
	}
	ra, err := filepath.EvalSymlinks(a)
	if err != nil {
		return false
	}
	rb, err := filepath.EvalSymlinks(b)
	if err != nil {
		return false
	}
	return ra == rb
}

// reExec replaces this process, so the freshly installed binary handles the
// invocation that noticed it was stale. Exec rather than a child process keeps
// the exit code and the standard streams exactly where the caller expects them.
func reExec(argv0 string, argv, env []string) error {
	return syscall.Exec(argv0, argv, env)
}
