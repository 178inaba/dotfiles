package selfbuild

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"syscall"
)

// Repo derives the repository from the stow symlink at ~/.claude/settings.json.
//
// It is exported for the hooks, which reach for files elsewhere in the
// repository — the frontmatter checker, until Sub 3 brings it in process — and
// have no Deps to resolve a home directory from.
func Repo() (string, bool) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", false
	}
	return repo(home)
}

// sourceRoot is the module within it.
func sourceRoot(d Deps) (string, bool) {
	r, ok := repo(d.Home)
	if !ok {
		return "", false
	}
	return filepath.Join(r, "go"), true
}

// repo resolves the symlink and checks that what it found looks like this
// repository.
//
// Deriving it from the settings file rather than the working directory is what
// keeps a linked worktree from ever becoming the source root: the home symlink
// always points into the main tree, so editing <repo>/.claude/worktrees/x/go
// leaves the installed binary alone, which is the intended behaviour.
//
// A false second value means the layout does not hold and the caller does
// nothing at all: the binary has to stay usable on a machine where this
// repository is not stowed.
func repo(home string) (string, bool) {
	if home == "" {
		return "", false
	}
	settings := filepath.Join(home, ".claude", "settings.json")

	fi, err := os.Lstat(settings)
	if err != nil || fi.Mode()&os.ModeSymlink == 0 {
		// Not a symlink means the configuration is not stowed from a checkout,
		// so there is no source to be stale against. os.Lstat rather than
		// filepath.EvalSymlinks because the latter happily returns a plain file
		// unchanged and would hide exactly this case.
		return "", false
	}
	link, err := os.Readlink(settings)
	if err != nil {
		return "", false
	}
	// stow writes relative links (../.dotfiles/claude/.claude/settings.json),
	// so resolve against the directory holding the link, not the process.
	if !filepath.IsAbs(link) {
		link = filepath.Join(filepath.Dir(settings), link)
	}
	// <repo>/claude/.claude/settings.json up three levels is <repo>.
	found := filepath.Dir(filepath.Dir(filepath.Dir(filepath.Clean(link))))
	if _, err := os.Stat(filepath.Join(found, "go", "go.mod")); err != nil {
		return "", false
	}
	return found, true
}

// binDir is where go install writes: the target's own directory when it has
// one, and the go command's default otherwise.
//
// The default is resolved in process rather than by running `go env`: this runs
// on every invocation, and spawning the go command would cost more than the
// whole check.
func binDir(d Deps, gobin string) string {
	if gobin != "" {
		return filepath.Join(d.Home, gobin)
	}

	env := goEnv(d, "GOBIN", "GOPATH")
	if env["GOBIN"] != "" {
		return env["GOBIN"]
	}
	gopath := env["GOPATH"]
	if gopath == "" {
		gopath = filepath.Join(d.Home, "go")
	}
	return filepath.Join(gopath, "bin")
}

// goEnv reads go command settings: the environment first, then the go env file,
// matching the go command's own precedence. It takes every key it will be asked
// for at once so the file is opened no more than once per invocation.
func goEnv(d Deps, keys ...string) map[string]string {
	out := make(map[string]string, len(keys))
	var missing []string
	for _, k := range keys {
		if v := d.Getenv(k); v != "" {
			out[k] = v
			continue
		}
		missing = append(missing, k)
	}
	if len(missing) == 0 {
		return out
	}

	name := d.Getenv("GOENV")
	if name == "" {
		config, err := os.UserConfigDir()
		if err != nil {
			return out
		}
		name = filepath.Join(config, "go", "env")
	}
	f, err := os.Open(name)
	if err != nil {
		return out
	}
	defer f.Close()

	s := bufio.NewScanner(f)
	for s.Scan() {
		key, value, ok := strings.Cut(s.Text(), "=")
		if ok && slices.Contains(missing, key) {
			out[key] = value
		}
	}
	if err := s.Err(); err != nil {
		// A partial read leaves GOBIN or GOPATH looking unset, and the caller
		// would then install to the wrong directory rather than fail.
		fmt.Fprintf(d.Debug, "ccx selfbuild: cannot read %s: %v\n", name, err)
		return nil
	}
	return out
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
