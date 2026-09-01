// Package runner starts external processes and detached copies of this binary.
//
// Every process the module starts goes through here. The shell scripts this
// module replaces made themselves testable with environment seams (GH_BIN,
// CURL_BIN, CAFFEINATE_BIN); the Go code injects a Runner instead, so the
// binary has no test-only environment variables to honour at runtime.
package runner

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"syscall"
)

// Command is one external invocation.
type Command struct {
	// Dir is the working directory. Empty inherits the caller's, which the
	// statusline relies on: the shell implementation ran git in the process
	// working directory and used the current_dir field only as a cache key.
	Dir string
	// Env holds extra KEY=VALUE entries appended to the process environment.
	Env  []string
	Name string
	Args []string
}

// Runner executes external commands.
type Runner interface {
	// Run returns the command's standard output. Standard error is not part of
	// the result — the shell implementation sent it to /dev/null everywhere —
	// but a failure carries it in *Error for diagnostics.
	Run(ctx context.Context, c Command) ([]byte, error)
}

// Error is a command that failed, with whatever it wrote to standard error.
// Carrying the output on the error rather than returning it keeps the success
// path a plain byte slice, and lets fakes reproduce a failure without having to
// fabricate an os/exec ExitError.
type Error struct {
	Name   string
	Err    error
	Stderr []byte
}

func (e *Error) Error() string { return fmt.Sprintf("%s: %v", e.Name, e.Err) }

func (e *Error) Unwrap() error { return e.Err }

// Spawner starts a detached copy of this binary and returns immediately.
type Spawner interface {
	// Spawn appends env to the child's environment; see selfbuild.ChildEnv.
	Spawn(env []string, args ...string) error
}

// Detacher starts another program that outlives this process.
type Detacher interface {
	// Detach returns the child's process id, which is the only handle on it
	// once this process is gone.
	Detach(name string, args ...string) (int, error)
}

// Signaller reaches a process this one need not have started.
type Signaller interface {
	// Terminate asks the process to exit.
	Terminate(pid int) error
	// Alive reports whether the process exists.
	Alive(pid int) bool
}

// Exec is the real Runner, Spawner, Detacher and Signaller.
type Exec struct {
	// Executable names the binary Spawn re-runs. Empty means this process,
	// which is what production wants; a test points it at the test binary so
	// the spawn path can be exercised without building cmd/ccx.
	Executable string
}

// Run implements Runner.
func (Exec) Run(ctx context.Context, c Command) ([]byte, error) {
	cmd := exec.CommandContext(ctx, c.Name, c.Args...)
	cmd.Dir = c.Dir
	if len(c.Env) > 0 {
		cmd.Env = append(os.Environ(), c.Env...)
	}
	out, err := cmd.Output()
	if err != nil {
		e := &Error{Name: c.Name, Err: err}
		if ee, ok := errors.AsType[*exec.ExitError](err); ok {
			e.Stderr = ee.Stderr
		}
		return out, e
	}
	return out, nil
}

// Spawn replaces the shell idiom
// `( cmd </dev/null >/dev/null 2>&1 & )`: a child that outlives this process,
// which a goroutine cannot be.
func (e Exec) Spawn(env []string, args ...string) error {
	self := e.Executable
	if self == "" {
		var err error
		if self, err = os.Executable(); err != nil {
			return err
		}
	}
	_, err := detach(self, env, args)
	return err
}

// Detach implements Detacher. It is Spawn for a program that is not this
// binary, and it hands back the process id because a caffeinate outlives the
// hook that started it and only its pid identifies it to the hook that stops
// it.
func (Exec) Detach(name string, args ...string) (int, error) {
	return detach(name, nil, args)
}

// detach starts name and forgets about it, returning the child's process id.
//
// Leaving all three standard streams nil wires them to /dev/null. Inheriting
// stdout would be fatal: the statusline writes to a pipe Claude Code reads to
// EOF, so a child holding the write end would block the render until its
// network call finished.
func detach(name string, env, args []string) (int, error) {
	cmd := exec.Command(name, args...)
	if len(env) > 0 {
		cmd.Env = append(os.Environ(), env...)
	}
	// Setsid detaches further than the shell original did, which left the child
	// in the caller's process group: a harness that kills the statusline's
	// process group on timeout cannot take the refresh down with it.
	//
	// syscall rather than golang.org/x/sys, which the syscall package asks new
	// code to prefer "where possible": exec.Cmd.SysProcAttr is typed
	// *syscall.SysProcAttr, and x/sys only aliases it, so the choice is forced
	// here. The module's four other system calls (Terminate and Alive below,
	// Flock and Exec in selfbuild) stay on syscall rather than have it import
	// both packages for five calls.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		return 0, err
	}
	pid := cmd.Process.Pid
	// Release rather than Wait: nothing here cares about the outcome, and the
	// child must survive this process.
	return pid, cmd.Process.Release()
}

// Terminate implements Signaller.
//
// SIGTERM rather than os.Process.Kill's SIGKILL, so that a caffeinate releases
// its power assertion on the way out instead of leaving the machine awake until
// the kernel notices.
func (Exec) Terminate(pid int) error {
	p, err := os.FindProcess(pid)
	if err != nil {
		return err
	}
	return p.Signal(syscall.SIGTERM)
}

// Alive implements Signaller. Signal 0 performs the permission and existence
// checks and delivers nothing, which is the shell's kill -0: a process owned by
// somebody else reads as gone, and one that has exited but not been waited for
// reads as alive. Both are what the hooks have always seen.
func (Exec) Alive(pid int) bool {
	p, err := os.FindProcess(pid)
	if err != nil {
		return false
	}
	return p.Signal(syscall.Signal(0)) == nil
}

// Git runs one git command in dir and returns its single line of output.
//
// Every command in this module that asks git a question asks it this way: -C
// rather than a working directory, because the answer is about one repository
// and not about wherever the process happens to be standing.
func Git(ctx context.Context, r Runner, dir string, args ...string) (string, error) {
	out, err := r.Run(ctx, Command{Name: "git", Args: append([]string{"-C", dir}, args...)})
	if err != nil {
		return "", fmt.Errorf("git %s in %s: %w", strings.Join(args, " "), dir, err)
	}
	return strings.TrimSpace(string(out)), nil
}

// Stderr returns what a failed command wrote to standard error.
func Stderr(err error) []byte {
	if e, ok := errors.AsType[*Error](err); ok {
		return e.Stderr
	}
	return nil
}

var (
	_ Runner    = Exec{}
	_ Spawner   = Exec{}
	_ Detacher  = Exec{}
	_ Signaller = Exec{}
)
