package ghshim

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/google/go-cmp/cmp"
)

// The cases of the shell suite that were about the hand-off rather than the
// decision. The shell observed them through a stub that recorded its argv and
// returned a known status; here the hand-off is a seam, so what it was called
// with is read directly.
//
// The stub proved three things at once — that the shim execs, that it passes
// argv through, and that standard output, standard error, the exit status and
// standard input all survive. Only the first two are this program's doing: the
// rest is what replacing the process means, and syscall.Exec is what does it.
// Standard input is stronger here than it was there: Execute has no reader to
// pass on and this package opens none, so it cannot consume what gh will read.

// handOff records what run tried to exec.
type handOff struct {
	called bool
	argv0  string
	argv   []string
	env    []string
	err    error
}

func (h *handOff) exec(argv0 string, argv, env []string) error {
	h.called = true
	h.argv0, h.argv, h.env = argv0, argv, env
	return h.err
}

// realGH lays out an executable gh outside this program's own directory and
// returns its path along with a PATH that finds it.
func realGH(t *testing.T) (path, pathList, selfDir string) {
	t.Helper()

	realDir := ghTree(t, 0o755)
	self := ghTree(t, 0o755)
	return filepath.Join(realDir, "gh"), realDir + string(filepath.ListSeparator) + self, self
}

func testDeps(t *testing.T, h *handOff) deps {
	t.Helper()

	realPath, pathList, selfDir := realGH(t)
	t.Cleanup(func() {
		if h.called && h.argv0 != realPath {
			t.Errorf("handed off to %q, want %q", h.argv0, realPath)
		}
	})
	return deps{
		env:      testEnv(),
		pathList: pathList,
		selfDir:  selfDir,
		exec:     h.exec,
		environ:  func() []string { return []string{"PATH=" + pathList} },
	}
}

func TestRunHandsOffAnAllowedCommand(t *testing.T) {
	t.Parallel()

	var h handOff
	d := testDeps(t, &h)
	var stderr bytes.Buffer

	argv := []string{"issue", "create", "-R", "foo/bar", "--title", "x", "--body", "y"}
	run(argv, &stderr, d)

	if !h.called {
		t.Fatalf("the real gh was not run; stderr = %q", stderr.String())
	}
	// argv0 is the resolved gh, and the arguments reach it unchanged.
	if diff := cmp.Diff(append([]string{h.argv0}, argv...), h.argv); diff != "" {
		t.Errorf("argv mismatch (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff(d.environ(), h.env); diff != "" {
		t.Errorf("env mismatch (-want +got):\n%s", diff)
	}
	if stderr.Len() != 0 {
		t.Errorf("stderr = %q, want nothing", stderr.String())
	}
}

// TestRunWritesNothingWithoutClaudeCode is the shell case "no CLAUDECODE: the
// shim writes nothing of its own to stderr": outside a session the guard is
// invisible, including on a command it would otherwise refuse.
func TestRunWritesNothingWithoutClaudeCode(t *testing.T) {
	t.Parallel()

	var h handOff
	d := testDeps(t, &h)
	d.env.ClaudeCode = ""
	var stderr bytes.Buffer

	run([]string{"issue", "create", "--title", "x", "--body", "y"}, &stderr, d)

	if !h.called {
		t.Fatalf("the real gh was not run; stderr = %q", stderr.String())
	}
	if stderr.Len() != 0 {
		t.Errorf("stderr = %q, want nothing", stderr.String())
	}
}

func TestRunRefuses(t *testing.T) {
	t.Parallel()

	var h handOff
	d := testDeps(t, &h)
	var stderr bytes.Buffer

	argv := []string{"issue", "create", "--title", "x", "--body", "y"}
	code := run(argv, &stderr, d)

	if h.called {
		t.Error("the real gh was run for a command that should have been refused")
	}
	if code != blockExit {
		t.Errorf("exit = %d, want %d", code, blockExit)
	}
	// Against Decide rather than the golden: what this test is about is that
	// run puts the block's message on stderr and nothing else. The wording is
	// decide_test.go's to pin, and it owns rule1-issue-create.golden alone.
	//
	// Fatal on a nil block, so that a decision which stopped refusing this
	// command is reported rather than dereferenced.
	block := Decide(argv, testEnv())
	if block == nil {
		t.Fatal("Decide did not refuse the command run refused")
	}
	if got := stderr.String(); got != block.Message {
		t.Errorf("stderr differs from the block message:\n%s", cmp.Diff(block.Message, got))
	}
}

// TestRunWithoutARealGH is the shell case "a missing real gh does not silently
// succeed". It is checked before the reading fast path, so it applies to every
// invocation.
func TestRunWithoutARealGH(t *testing.T) {
	t.Parallel()

	var h handOff
	d := testDeps(t, &h)
	d.pathList = t.TempDir()
	var stderr bytes.Buffer

	code := run([]string{"pr", "view", "1"}, &stderr, d)

	if h.called {
		t.Error("something was run as gh")
	}
	if code != blockExit {
		t.Errorf("exit = %d, want %d", code, blockExit)
	}
	got := stderr.String()
	if want := wantGolden(t, "no-real-gh", "", got); got != want {
		t.Errorf("stderr differs from no-real-gh.golden (re-run with -update):\n%s", cmp.Diff(want, got))
	}
}

// TestRunFailsClosedOnAPanic is the shell case "internal error during judgement
// does not exec", which the shell arranged with BASH_ENV and a readonly
// variable. Go has no equivalent, so the bug is injected directly: what the
// case fixes is that a fault leaves the guard refusing rather than passing.
func TestRunFailsClosedOnAPanic(t *testing.T) {
	t.Parallel()

	var h handOff
	d := testDeps(t, &h)
	d.env.Dir = func() string { panic("injected fault") }
	var stderr bytes.Buffer

	// A command the first rule refuses, so that the message asks for the
	// working directory and the fault is raised inside the judgement.
	code := run([]string{"issue", "create", "--title", "x"}, &stderr, d)

	if h.called {
		t.Error("the real gh was run after a fault")
	}
	if code != blockExit {
		t.Errorf("exit = %d, want %d", code, blockExit)
	}
	if !strings.Contains(stderr.String(), "injected fault") {
		t.Errorf("stderr = %q, want it to name the fault", stderr.String())
	}
}

// TestRunFaultDoesNotStopReads is the other half of the shell pair: failing
// closed must not turn into failing at everything. A read never reaches the
// part of the judgement a fault could come from, so it is handed off as usual.
func TestRunFaultDoesNotStopReads(t *testing.T) {
	t.Parallel()

	var h handOff
	d := testDeps(t, &h)
	d.env.Dir = func() string { panic("injected fault") }
	var stderr bytes.Buffer

	run([]string{"pr", "view", "1"}, &stderr, d)

	if !h.called {
		t.Fatalf("the real gh was not run; stderr = %q", stderr.String())
	}
	if stderr.Len() != 0 {
		t.Errorf("stderr = %q, want nothing", stderr.String())
	}
}

func TestRunReportsAFailedBuildOnce(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		build selfbuild.State
		want  string
	}{
		{
			name:  "the invocation that ran the build",
			build: selfbuild.State{Failed: true, JustFailed: true, FirstError: "ghshim/decide.go:1:1: nope"},
			want:  "ghshim/decide.go:1:1: nope",
		},
		{
			// A failure recorded earlier for the same source is not repeated.
			name:  "a later invocation in the same state",
			build: selfbuild.State{Failed: true, FirstError: "ghshim/decide.go:1:1: nope"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var h handOff
			d := testDeps(t, &h)
			d.build = tt.build
			var stderr bytes.Buffer

			run([]string{"pr", "view", "1"}, &stderr, d)

			if !h.called {
				t.Fatalf("the real gh was not run; stderr = %q", stderr.String())
			}
			if tt.want == "" {
				if stderr.Len() != 0 {
					t.Errorf("stderr = %q, want nothing", stderr.String())
				}
				return
			}
			if !strings.Contains(stderr.String(), tt.want) {
				t.Errorf("stderr = %q, want it to name %q", stderr.String(), tt.want)
			}
		})
	}
}

func TestRunReportsAHandOffThatCouldNotStart(t *testing.T) {
	t.Parallel()

	h := handOff{err: errors.New("permission denied")}
	d := testDeps(t, &h)
	var stderr bytes.Buffer

	code := run([]string{"pr", "view", "1"}, &stderr, d)

	if code != blockExit {
		t.Errorf("exit = %d, want %d", code, blockExit)
	}
	if !strings.Contains(stderr.String(), "permission denied") {
		t.Errorf("stderr = %q, want it to name the failure", stderr.String())
	}
}

// TestPackageReadsNoStandardInput holds the property Execute's signature is
// shaped for: consuming standard input here would take the body gh is about to
// read for itself.
func TestPackageReadsNoStandardInput(t *testing.T) {
	t.Parallel()

	// Globbed rather than listed, so that a file added later cannot leave the
	// check quietly covering less than the package.
	sources, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}
	if len(sources) == 0 {
		t.Fatal("no sources found")
	}
	for _, name := range sources {
		if strings.HasSuffix(name, "_test.go") {
			continue
		}
		b, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("ReadFile(%q): %v", name, err)
		}
		if bytes.Contains(b, []byte("os.Stdin")) {
			t.Errorf("%s names os.Stdin", name)
		}
	}
}
