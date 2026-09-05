package runner

import (
	"context"
	"os"
	"os/exec"
	"syscall"
	"testing"
	"time"
)

// childEnv turns an invocation of the test binary into the detached child that
// TestSpawnDoesNotWait starts. Dispatching in TestMain keeps the spawn path
// under test without building a separate helper binary.
const childEnv = "RUNNER_TEST_CHILD"

func TestMain(m *testing.M) {
	if marker := os.Getenv(childEnv); marker != "" {
		// Sleep first: the parent must return long before this finishes, which
		// is the whole point of a detached spawn.
		time.Sleep(300 * time.Millisecond)
		if err := os.WriteFile(marker, []byte("ok"), 0o644); err != nil {
			os.Exit(1)
		}
		os.Exit(0)
	}
	os.Exit(m.Run())
}

func TestRun(t *testing.T) {
	got, err := Exec{}.Run(t.Context(), Command{Name: "sh", Args: []string{"-c", "printf out; printf err >&2"}})
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	// Standard error stays out of the result, matching the 2>/dev/null the
	// shell implementation applied to every external call.
	if string(got) != "out" {
		t.Errorf("stdout = %q, want %q", got, "out")
	}
}

func TestRunFailureCarriesStderr(t *testing.T) {
	_, err := Exec{}.Run(t.Context(), Command{Name: "sh", Args: []string{"-c", "echo boom >&2; exit 3"}})
	if err == nil {
		t.Fatal("Run succeeded, want a failure")
	}
	if got, want := string(Stderr(err)), "boom\n"; got != want {
		t.Errorf("Stderr = %q, want %q", got, want)
	}
}

func TestRunAppendsEnv(t *testing.T) {
	got, err := Exec{}.Run(t.Context(), Command{
		Env:  []string{"CCX_TEST_VALUE=set"},
		Name: "sh", Args: []string{"-c", "printf %s \"$CCX_TEST_VALUE\""},
	})
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if string(got) != "set" {
		t.Errorf("environment value = %q, want %q", got, "set")
	}
}

// TestRunFeedsStdin covers both halves of the field, because the second is
// what a command reading standard input sees when nobody set it: a child with
// no stdin at all would block rather than reach the end of its input.
func TestRunFeedsStdin(t *testing.T) {
	// NUL bytes and a newline, which is what the check-attr caller writes and
	// what a line-oriented seam would corrupt.
	in := []byte("a\x00b\nc\x00")
	got, err := Exec{}.Run(t.Context(), Command{
		Stdin: in,
		Name:  "cat",
	})
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if string(got) != string(in) {
		t.Errorf("stdin = %q, want %q", got, in)
	}

	if got, err = (Exec{}).Run(t.Context(), Command{Name: "cat"}); err != nil {
		t.Fatalf("Run without a stdin: %v", err)
	}
	if len(got) != 0 {
		t.Errorf("stdin = %q, want it empty when the field is unset", got)
	}
}

func TestRunHonoursContext(t *testing.T) {
	ctx, cancel := context.WithCancel(t.Context())
	cancel()
	if _, err := (Exec{}).Run(ctx, Command{Name: "sh", Args: []string{"-c", "sleep 5"}}); err == nil {
		t.Error("Run succeeded, want the cancelled context to stop it")
	}
}

// TestSpawnDoesNotWait is the property the statusline depends on: the render
// must finish while the refresh it started is still running.
func TestSpawnDoesNotWait(t *testing.T) {
	marker := t.TempDir() + "/done"
	t.Setenv(childEnv, marker)

	self, err := os.Executable()
	if err != nil {
		t.Fatalf("Executable: %v", err)
	}

	start := time.Now()
	if err := (Exec{Executable: self}).Spawn(nil); err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	if elapsed := time.Since(start); elapsed > 200*time.Millisecond {
		t.Errorf("Spawn blocked for %v, want it to return immediately", elapsed)
	}

	deadline := time.Now().Add(5 * time.Second)
	for {
		if _, err := os.Stat(marker); err == nil {
			return
		}
		if time.Now().After(deadline) {
			t.Fatal("the spawned child never ran")
		}
		time.Sleep(10 * time.Millisecond)
	}
}

func TestDetachReturnsAPidAndDoesNotWait(t *testing.T) {
	start := time.Now()
	pid, err := Exec{}.Detach("sh", "-c", "sleep 30")
	if err != nil {
		t.Fatalf("Detach: %v", err)
	}
	if elapsed := time.Since(start); elapsed > 200*time.Millisecond {
		t.Errorf("Detach blocked for %v, want it to return immediately", elapsed)
	}
	if pid <= 0 {
		t.Fatalf("pid = %d, want a real process id", pid)
	}
	t.Cleanup(func() {
		_ = Exec{}.Terminate(pid)
		reap(t, pid)
	})

	if !(Exec{}).Alive(pid) {
		t.Errorf("Alive(%d) = false, want the process just started to be alive", pid)
	}
}

func TestTerminateSendsSIGTERM(t *testing.T) {
	// SIGTERM rather than SIGKILL is what lets caffeinate release its power
	// assertion, so the test pins the signal and not merely the death.
	pid, err := Exec{}.Detach("sh", "-c", "sleep 30")
	if err != nil {
		t.Fatalf("Detach: %v", err)
	}
	if err := (Exec{}).Terminate(pid); err != nil {
		t.Fatalf("Terminate: %v", err)
	}

	status := reap(t, pid)
	if !status.Signaled() {
		t.Fatalf("wait status = %v, want the process to have been signalled", status)
	}
	if got := status.Signal(); got != syscall.SIGTERM {
		t.Errorf("signal = %v, want %v", got, syscall.SIGTERM)
	}
}

func TestAliveIsFalseForAProcessThatHasGone(t *testing.T) {
	// Run and reap, so the pid names nothing at all: a terminated child that
	// nobody has waited for is a zombie, which kill -0 still reports as alive.
	cmd := exec.Command("sh", "-c", "exit 0")
	if err := cmd.Run(); err != nil {
		t.Fatalf("Run: %v", err)
	}
	if (Exec{}).Alive(cmd.Process.Pid) {
		t.Errorf("Alive(%d) = true, want false for a process that has been reaped", cmd.Process.Pid)
	}
}

// reap waits for a detached child of this process and returns how it ended.
// Nothing in production does this — a hook never starts the caffeinate it
// later kills — but the test process would otherwise collect zombies.
func reap(t *testing.T, pid int) syscall.WaitStatus {
	t.Helper()
	var status syscall.WaitStatus
	deadline := time.Now().Add(5 * time.Second)
	for {
		got, err := syscall.Wait4(pid, &status, syscall.WNOHANG, nil)
		if got == pid {
			return status
		}
		if err != nil && err != syscall.EINTR {
			t.Fatalf("Wait4(%d): %v", pid, err)
		}
		if time.Now().After(deadline) {
			t.Fatalf("process %d never exited", pid)
		}
		time.Sleep(10 * time.Millisecond)
	}
}
