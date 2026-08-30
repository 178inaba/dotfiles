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
	// Spawn appends env to the child's environment. The caller has to be able
	// to tell the child what it is, because a detached copy that repeated the
	// parent's startup work would race it.
	Spawn(env []string, args ...string) error
}

// executablePath is a seam for tests, which point it at the test binary so the
// spawn path can be exercised without building cmd/ccx.
var executablePath = os.Executable

// Exec is the real Runner and Spawner.
type Exec struct{}

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
//
// Leaving all three standard streams nil wires them to /dev/null. Inheriting
// stdout would be the one fatal mistake here: the statusline writes to a pipe
// Claude Code reads to EOF, so a child holding the write end would block the
// render until its network call finished. No unit test catches that, which is
// why it is spelled out.
func (Exec) Spawn(env []string, args ...string) error {
	self, err := executablePath()
	if err != nil {
		return err
	}

	cmd := exec.Command(self, args...)
	if len(env) > 0 {
		cmd.Env = append(os.Environ(), env...)
	}
	// Setsid detaches further than the shell original did, which left the child
	// in the caller's process group: a harness that kills the statusline's
	// process group on timeout cannot take the refresh down with it.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		return err
	}
	// Release rather than Wait: nothing here cares about the outcome, and the
	// child must survive this process.
	return cmd.Process.Release()
}

// Stderr returns what a failed command wrote to standard error.
func Stderr(err error) []byte {
	if e, ok := errors.AsType[*Error](err); ok {
		return e.Stderr
	}
	return nil
}

var (
	_ Runner  = Exec{}
	_ Spawner = Exec{}
)
