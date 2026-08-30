// Package selfbuild keeps the installed binary in step with the repository it
// was built from.
//
// On every start the binary compares the newest modification time under
// <repo>/go with its own and, when it is behind, reinstalls itself and re-execs
// with the original argv. That is what makes "edit, and the next invocation
// reflects it" true for edits made through an editor, through the shell, or by
// git switching branches — a PostToolUse hook would only see the first kind.
//
// Nothing here writes to the user's streams. Run returns a State and each
// subcommand decides how to report it, because the right channel differs: the
// statusline renders a warning segment, a hook must not disturb its exit code,
// a script prints to standard error.
package selfbuild

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	// disableEnv turns the whole mechanism off. It exists so the cost of a
	// non-stale check can be measured against a run that does not make it, and
	// it is what the detached refresh children are started with: a stale parent
	// spawning a child that took the build lock and re-execed would race it.
	disableEnv = "CCX_SELFBUILD"
	// reexecEnv marks the process that a rebuild already re-execed into, so a
	// toolchain that somehow left the timestamps unchanged cannot loop.
	reexecEnv = "CCX_SELFBUILD_REEXECED"
	// debugEnv makes the decision observable.
	debugEnv = "CCX_DEBUG"
)

// target is one thing to install. Adding a binary is adding an entry: the
// package to build and the GOBIN to put it in ("" is the go command's default).
type target struct {
	pkg   string
	gobin string
}

// targets is the fixed list. cmd/gh joins it, with GOBIN ~/.local/shims, when
// that binary exists; never list a package that has not been written yet.
var targets = []target{{pkg: "./cmd/ccx"}}

// State is what the check left behind.
type State struct {
	// Failed reports that the current source state does not build — either the
	// build ran here and failed, or an earlier invocation recorded a failure
	// for a source state that has not changed since.
	Failed bool
	// FirstError is the first line of the compiler output for that failure.
	FirstError string
}

// Deps are the seams. Use NewDeps for the real ones.
type Deps struct {
	Home     string
	Args     []string
	Getenv   func(string) string
	Environ  func() []string
	Exe      func() (string, error)
	Stat     func(string) (os.FileInfo, error)
	Lstat    func(string) (os.FileInfo, error)
	Readlink func(string) (string, error)
	LookPath func(string) (string, error)
	Chtimes  func(string, time.Time, time.Time) error
	Now      func() time.Time
	Run      runner.Runner
	// ReExec replaces this process. It only returns on failure.
	ReExec func(argv0 string, argv, env []string) error
	Debug  io.Writer
}

// NewDeps wires the real implementations. Home is empty when it cannot be
// resolved, which makes Run skip.
func NewDeps(args []string) Deps {
	home, _ := os.UserHomeDir()
	d := Deps{
		Home:     home,
		Args:     args,
		Getenv:   os.Getenv,
		Environ:  os.Environ,
		Exe:      os.Executable,
		Stat:     os.Stat,
		Lstat:    os.Lstat,
		Readlink: os.Readlink,
		LookPath: exec.LookPath,
		Chtimes:  os.Chtimes,
		Now:      time.Now,
		Run:      runner.Exec{},
		ReExec:   reExec,
		Debug:    io.Discard,
	}
	if os.Getenv(debugEnv) == "1" {
		d.Debug = os.Stderr
	}
	return d
}

// Run performs the check. It does not return when a rebuild succeeds, because
// the process is replaced.
//
// Callers must not read standard input before this returns: the re-exec hands
// the replacement process the original argv but not anything already consumed
// from the pipe, so a statusline that read its JSON first would see it vanish.
func Run(d Deps) State {
	log := func(format string, a ...any) {
		if d.Debug != nil {
			fmt.Fprintf(d.Debug, "ccx selfbuild: "+format+"\n", a...)
		}
	}

	if d.Getenv(disableEnv) == "0" {
		log("suppressed by env")
		return State{}
	}
	if d.Getenv(reexecEnv) != "" {
		log("skipped: already re-execed")
		return State{}
	}

	root, ok := sourceRoot(d)
	if !ok {
		log("skipped: no source root")
		return State{}
	}

	exe, err := d.Exe()
	if err != nil {
		log("skipped: cannot locate the running binary: %v", err)
		return State{}
	}
	if !isInstalled(d, exe) {
		// A hand-built binary (go run, go build -o) must not install the main
		// tree over the user's ~/go/bin and re-exec into it: that would swap
		// out the very code the user is verifying.
		log("skipped: %s is not an installed target", exe)
		return State{}
	}

	files, err := scan(root)
	if err != nil {
		log("skipped: cannot scan %s: %v", root, err)
		return State{}
	}
	exeInfo, err := d.Stat(exe)
	if err != nil {
		log("skipped: cannot stat %s: %v", exe, err)
		return State{}
	}
	if !files.newest.After(exeInfo.ModTime()) {
		log("fresh")
		return State{}
	}

	sum := files.sum()
	if rec, ok := readFailure(d); ok && rec.sum == sum {
		// The parent contract: report the failure once, on the invocation that
		// tried, then stop retrying until the source changes. The statusline is
		// the exception and keeps showing its segment, which is why the state
		// is returned rather than swallowed.
		log("suppressed by hash")
		return State{Failed: true, FirstError: rec.firstError}
	}

	if _, err := d.LookPath("go"); err != nil {
		// Claude Code starts the statusline through sh -c, whose PATH need not
		// include the Go toolchain. Recording that as a build failure would
		// wedge the warning on until the source happened to change, so it is a
		// skip like the others.
		log("skipped: no go toolchain on PATH")
		return State{}
	}

	unlock, ok := lock(d)
	if !ok {
		log("skipped: another process holds the build lock")
		return State{}
	}
	defer unlock()

	if out, err := install(d, root); err != nil {
		first := firstLine(out)
		writeFailure(d, sum, first)
		log("failed: %s", first)
		return State{Failed: true, FirstError: first}
	}
	removeFailure(d)
	// go install skips the copy when the binary is already current, leaving its
	// timestamp behind the source that just changed back to a state it had
	// built before. Without this the check would stay stale and reinstall on
	// every invocation forever.
	touch(d)
	log("rebuilt")

	if err := d.ReExec(exe, append([]string{exe}, d.Args...), reexecEnviron(d)); err != nil {
		// Nothing is broken: this process is the previous build and still runs.
		log("re-exec failed: %v", err)
	}
	return State{}
}

// install builds every target, returning the compiler output of the first
// failure.
func install(d Deps, root string) ([]byte, error) {
	for _, t := range targets {
		c := runner.Command{
			Name: "go",
			Args: []string{"-C", root, "install", t.pkg},
		}
		if t.gobin != "" {
			c.Env = []string{"GOBIN=" + t.gobin}
		}
		out, err := d.Run.Run(context.Background(), c)
		if err != nil {
			if stderr := runner.Stderr(err); len(stderr) > 0 {
				return stderr, err
			}
			return out, err
		}
	}
	return nil, nil
}

// touch stamps every installed target with the current time.
func touch(d Deps) {
	now := d.Now()
	for _, t := range targets {
		p := installPath(d, t)
		if err := d.Chtimes(p, now, now); err != nil {
			fmt.Fprintf(d.Debug, "ccx selfbuild: cannot touch %s: %v\n", p, err)
		}
	}
}

// reexecEnviron is the current environment plus the loop marker.
func reexecEnviron(d Deps) []string {
	env := d.Environ()
	out := make([]string, 0, len(env)+1)
	for _, kv := range env {
		if !strings.HasPrefix(kv, reexecEnv+"=") {
			out = append(out, kv)
		}
	}
	return append(out, reexecEnv+"=1")
}

// firstLine is the first useful line of the compiler output, which is all the
// reporting channels have room for.
//
// The go command banners each failing package with "# import/path" before the
// diagnostics; that line says nothing the user cannot see, so the first real
// diagnostic is taken instead and the banner is only used as a fallback.
func firstLine(out []byte) string {
	var banner string
	for line := range strings.SplitSeq(strings.ReplaceAll(string(out), "\r\n", "\n"), "\n") {
		trimmed := strings.TrimSpace(line)
		switch {
		case trimmed == "":
		case strings.HasPrefix(trimmed, "#"):
			if banner == "" {
				banner = trimmed
			}
		default:
			return trimmed
		}
	}
	if banner != "" {
		return banner
	}
	return "build failed"
}

// isInstalled reports whether exe is one of the targets' install paths.
func isInstalled(d Deps, exe string) bool {
	for _, t := range targets {
		if samePath(exe, installPath(d, t)) {
			return true
		}
	}
	return false
}

// installPath is where go install would put t.
func installPath(d Deps, t target) string {
	return filepath.Join(binDir(d, t.gobin), path.Base(t.pkg))
}
