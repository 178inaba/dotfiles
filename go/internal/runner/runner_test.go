package runner

import (
	"context"
	"os"
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

func TestRunUsesDir(t *testing.T) {
	dir := t.TempDir()
	got, err := Exec{}.Run(t.Context(), Command{Dir: dir, Name: "sh", Args: []string{"-c", "printf %s \"$PWD\""}})
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if string(got) != dir {
		t.Errorf("working directory = %q, want %q", got, dir)
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
	executablePath = func() (string, error) { return self, nil }
	t.Cleanup(func() { executablePath = os.Executable })

	start := time.Now()
	if err := (Exec{}).Spawn(); err != nil {
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
