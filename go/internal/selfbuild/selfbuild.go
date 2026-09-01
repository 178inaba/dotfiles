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
	"slices"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	// disableEnv turns the whole mechanism off. It exists so the cost of a
	// non-stale check can be measured against a run that does not make it, and
	// it is what ChildEnv hands to a detached child.
	disableEnv = "CCX_SELFBUILD"
	// reexecEnv marks the process that a rebuild already re-execed into, so a
	// toolchain that somehow left the timestamps unchanged cannot loop.
	reexecEnv = "CCX_SELFBUILD_REEXECED"
	// debugEnv makes the decision observable.
	debugEnv = "CCX_DEBUG"
)

// target is one thing to install.
//
// gobin is relative to the home directory rather than absolute, because the
// only reason a target needs one is to land somewhere other than the shared
// GOBIN; an empty one means the go command's default. Home-relative rather than
// a literal ~ because no shell expands it here — go install would create a
// directory actually called "~".
type target struct {
	pkg   string
	gobin string
}

// targets is the fixed list. cmd/gh has a gobin of its own because the shim's
// whole function is shadowing the real gh, and ~/go/bin is a shared namespace
// where go install github.com/cli/cli/v2/cmd/gh@latest would overwrite it.
// Never list a package that has not been written yet.
//
// A function rather than a variable so nothing — a later subcommand, a test —
// can change what the whole process installs.
func targets() []target {
	return []target{
		{pkg: "./cmd/ccx"},
		{pkg: "./cmd/gh", gobin: ".local/shims"},
	}
}

// ChildEnv is what a process this binary spawns must carry.
//
// The self-rebuild is the parent's job. A child that repeated it could take the
// build lock and re-exec itself while the parent that started it is still
// running, so it is told not to.
func ChildEnv() []string {
	return []string{disableEnv + "=0"}
}

// State is what the check left behind.
type State struct {
	// Failed reports that the current source state does not build — either the
	// build ran here and failed, or an earlier invocation recorded a failure
	// for a source state that has not changed since.
	//
	// A status line reports this on every redraw: it is a display, and a stale
	// binary has to stay visible for as long as it is stale.
	Failed bool
	// JustFailed narrows that to the invocation that actually ran the build.
	//
	// Anything that is not a display reports on this one instead, so a broken
	// tree produces one message rather than one per hook and per script until
	// somebody fixes it.
	JustFailed bool
	// FirstError is the first line of the compiler output for that failure.
	FirstError string
}

// Report is what a command says about a build that has just failed, prefixed
// with its own name. Empty unless this invocation is the one that ran the
// build, so that a broken tree produces one message rather than one per hook
// and per script until somebody fixes it.
//
// The wording lives here because the three callers — the hooks, the script
// subcommands and the gh shim — say the same thing on channels of their own,
// and a sentence written out three times drifts.
func (s State) Report(name string) string {
	if !s.JustFailed {
		return ""
	}
	return name + ": the Go module does not build, so this ran the previously installed binary: " + s.FirstError
}

// Deps are the seams. Use NewDeps for the real ones.
//
// Filesystem calls are not among them: the tests build a real home directory in
// a temporary tree, which exercises the symlink resolution rather than a
// description of it.
type Deps struct {
	// Home is empty when it cannot be resolved, which makes Run skip.
	Home string
	// CacheDir holds the build lock and the record of a failed build, in the
	// same tree the status line caches under: one directory to inspect, and one
	// to remove when something needs resetting.
	CacheDir string
	// Exe is this process's path, empty when it cannot be resolved.
	Exe     string
	Args    []string
	Getenv  func(string) string
	Environ func() []string
	// LookPath is how the go command is found, so that a machine without one
	// can be arranged in a test.
	LookPath func(string) (string, error)
	Chtimes  func(string, time.Time, time.Time) error
	Run      runner.Runner
	// ReExec replaces this process. It only returns on failure.
	ReExec func(argv0 string, argv, env []string) error
	Debug  io.Writer
}

// NewDeps wires the real implementations.
func NewDeps(args []string) Deps {
	home, _ := os.UserHomeDir()
	exe, _ := os.Executable()
	d := Deps{
		Home:     home,
		CacheDir: cache.Dir(),
		Exe:      exe,
		Args:     args,
		Getenv:   os.Getenv,
		Environ:  os.Environ,
		LookPath: exec.LookPath,
		Chtimes:  os.Chtimes,
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
func Run(ctx context.Context, d Deps) State {
	if d.Debug == nil {
		d.Debug = io.Discard
	}
	log := func(format string, a ...any) {
		fmt.Fprintf(d.Debug, "ccx selfbuild: "+format+"\n", a...)
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
	if d.Exe == "" {
		log("skipped: cannot locate the running binary")
		return State{}
	}
	if !isInstalled(d) {
		// A hand-built binary (go run, go build -o) must not install the main
		// tree over the user's ~/go/bin and re-exec into it: that would swap
		// out the very code the user is verifying.
		log("skipped: %s is not an installed target", d.Exe)
		return State{}
	}
	exeInfo, err := os.Stat(d.Exe)
	if err != nil {
		log("skipped: cannot stat %s: %v", d.Exe, err)
		return State{}
	}

	// The tree's hash is only built on the stale path, below.
	stale, err := isStale(root, exeInfo.ModTime())
	if err != nil {
		log("skipped: cannot scan %s: %v", root, err)
		return State{}
	}
	if !stale {
		log("fresh")
		return State{}
	}

	source, err := scanSource(root)
	if err != nil {
		log("skipped: cannot scan %s: %v", root, err)
		return State{}
	}
	if rec, ok := readFailure(d); ok && rec.sum == source.sum {
		// Reported once, on the invocation that tried, and then not retried
		// until the source changes.
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

	if out, err := install(ctx, d, root, targets()); err != nil {
		first := firstLine(out)
		writeFailure(d, source.sum, first)
		log("failed: %s", first)
		return State{Failed: true, JustFailed: true, FirstError: first}
	}
	removeFailure(d)
	// go install skips the copy when the binary is already current, leaving its
	// timestamp behind the source that just changed back to a state it had
	// built before. Without this the check would stay stale and reinstall on
	// every invocation forever.
	touch(d, source.newest, targets())
	log("rebuilt")

	if err := d.ReExec(d.Exe, append([]string{d.Exe}, d.Args...), reexecEnviron(d)); err != nil {
		// Nothing is broken: this process is the previous build and still runs.
		log("re-exec failed: %v", err)
	}
	return State{}
}

// install builds every target, returning the compiler output of the first
// failure. The list is a parameter so a test can pin what a second binary
// would be given without reaching into package state.
func install(ctx context.Context, d Deps, root string, ts []target) ([]byte, error) {
	for _, t := range ts {
		c := runner.Command{
			Name: "go",
			Args: []string{"-C", root, "install", t.pkg},
		}
		if t.gobin != "" {
			// Resolved, not passed through: a target's gobin is home-relative,
			// and go install refuses a GOBIN that is not absolute.
			c.Env = []string{"GOBIN=" + binDir(d, t.gobin)}
		}
		out, err := d.Run.Run(ctx, c)
		if err != nil {
			if stderr := runner.Stderr(err); len(stderr) > 0 {
				return stderr, err
			}
			return out, err
		}
	}
	return nil, nil
}

// touch stamps every installed target with the source state it was built from.
//
// Not with the current time: a build takes a few hundred milliseconds, and an
// edit that lands inside that window would be stamped over and never noticed.
// Stamping with what the build actually saw leaves anything newer newer, so the
// next invocation picks it up.
func touch(d Deps, source time.Time, ts []target) {
	for _, t := range ts {
		p := installPath(d, t)
		if err := d.Chtimes(p, source, source); err != nil {
			fmt.Fprintf(d.Debug, "ccx selfbuild: cannot touch %s: %v\n", p, err)
		}
	}
}

func reexecEnviron(d Deps) []string {
	env := slices.DeleteFunc(d.Environ(), func(kv string) bool {
		return strings.HasPrefix(kv, reexecEnv+"=")
	})
	return append(env, reexecEnv+"=1")
}

// firstLine is the first useful line of the compiler output, which is all the
// reporting channels have room for.
//
// The go command banners each failing package with "# import/path" before the
// diagnostics; that line says nothing the user cannot see, so the first real
// diagnostic is taken instead and the banner is only used as a fallback.
func firstLine(out []byte) string {
	var banner string
	for line := range strings.Lines(string(out)) {
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

func isInstalled(d Deps) bool {
	return slices.ContainsFunc(targets(), func(t target) bool {
		return samePath(d.Exe, installPath(d, t))
	})
}

// installPath is where go install would put t.
func installPath(d Deps, t target) string {
	return filepath.Join(binDir(d, t.gobin), path.Base(t.pkg))
}
