package selfbuild

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// fakeRunner records the commands it was asked to run and fails on demand.
type fakeRunner struct {
	calls  []runner.Command
	err    error
	stderr string
}

func (f *fakeRunner) Run(_ context.Context, c runner.Command) ([]byte, error) {
	f.calls = append(f.calls, c)
	if f.err != nil {
		return nil, &runner.Error{Name: c.Name, Err: f.err, Stderr: []byte(f.stderr)}
	}
	return nil, nil
}

type reexec struct {
	called bool
	argv0  string
	argv   []string
	env    []string
}

// harness is a throwaway machine: a home directory holding a stowed
// ~/.claude/settings.json, the repository it points into, and an installed
// binary in GOPATH/bin.
type harness struct {
	t       *testing.T
	home    string
	repo    string
	root    string
	exe     string
	env     map[string]string
	runner  *fakeRunner
	reexec  reexec
	debug   bytes.Buffer
	now     time.Time
	touched map[string]time.Time
}

func newHarness(t *testing.T) *harness {
	t.Helper()

	home := t.TempDir()
	h := &harness{
		t:       t,
		home:    home,
		repo:    filepath.Join(home, ".dotfiles"),
		exe:     filepath.Join(home, "go", "bin", "ccx"),
		env:     map[string]string{"GOPATH": filepath.Join(home, "go")},
		runner:  &fakeRunner{},
		now:     time.Date(2026, 8, 31, 12, 0, 0, 0, time.UTC),
		touched: map[string]time.Time{},
	}
	h.root = filepath.Join(h.repo, "go")

	mkdirAll(t, filepath.Join(h.repo, "claude", ".claude"))
	mkdirAll(t, h.root)
	mkdirAll(t, filepath.Join(home, ".claude"))
	mkdirAll(t, filepath.Dir(h.exe))

	writeFile(t, filepath.Join(h.repo, "claude", ".claude", "settings.json"), "{}")
	writeFile(t, filepath.Join(h.root, "go.mod"), "module example\n\ngo 1.27\n")
	writeFile(t, h.exe, "binary")

	// stow links relatively, which is the case the resolution has to handle.
	if err := os.Symlink(filepath.Join("..", ".dotfiles", "claude", ".claude", "settings.json"),
		filepath.Join(home, ".claude", "settings.json")); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	h.setFresh()
	return h
}

// setStale makes the source newer than the binary.
func (h *harness) setStale() {
	h.t.Helper()
	h.chtimes(h.exe, h.now.Add(-time.Hour))
	h.chtimes(filepath.Join(h.root, "go.mod"), h.now)
}

// setFresh makes the binary newer than the source.
func (h *harness) setFresh() {
	h.t.Helper()
	h.chtimes(filepath.Join(h.root, "go.mod"), h.now.Add(-time.Hour))
	h.chtimes(h.exe, h.now)
}

func (h *harness) chtimes(p string, mod time.Time) {
	h.t.Helper()
	if err := os.Chtimes(p, mod, mod); err != nil {
		h.t.Fatalf("chtimes %s: %v", p, err)
	}
}

func (h *harness) deps() Deps {
	return Deps{
		Home:     h.home,
		Args:     []string{"statusline"},
		Exe:      h.exe,
		Getenv:   func(k string) string { return h.env[k] },
		Environ:  func() []string { return []string{"PATH=/usr/bin"} },
		LookPath: func(string) (string, error) { return "/usr/bin/go", nil },
		Chtimes: func(p string, _, mod time.Time) error {
			h.touched[p] = mod
			return os.Chtimes(p, mod, mod)
		},
		Now: func() time.Time { return h.now },
		Run: h.runner,
		ReExec: func(argv0 string, argv, env []string) error {
			h.reexec = reexec{called: true, argv0: argv0, argv: argv, env: env}
			return nil
		},
		Debug: &h.debug,
	}
}

func mkdirAll(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
}

func writeFile(t *testing.T, name, body string) {
	t.Helper()
	if err := os.WriteFile(name, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
}

// TestRunDoesNothing covers every reason the check declines to build. All of
// them must be silent and leave the caller with a clean State, because the
// binary has to stay usable on a machine where the repository is not stowed.
func TestRunDoesNothing(t *testing.T) {
	tests := []struct {
		name    string
		setup   func(*harness)
		wantLog string
	}{
		{
			name:    "fresh binary",
			setup:   func(*harness) {},
			wantLog: "fresh",
		},
		{
			name: "disabled by environment",
			setup: func(h *harness) {
				h.setStale()
				h.env[disableEnv] = "0"
			},
			wantLog: "suppressed by env",
		},
		{
			name: "already re-execed",
			setup: func(h *harness) {
				h.setStale()
				h.env[reexecEnv] = "1"
			},
			wantLog: "already re-execed",
		},
		{
			name: "settings.json is not a symlink",
			setup: func(h *harness) {
				h.setStale()
				link := filepath.Join(h.home, ".claude", "settings.json")
				if err := os.Remove(link); err != nil {
					t.Fatalf("remove: %v", err)
				}
				writeFile(t, link, "{}")
			},
			wantLog: "no source root",
		},
		{
			name: "the link does not lead to a module",
			setup: func(h *harness) {
				h.setStale()
				if err := os.Remove(filepath.Join(h.root, "go.mod")); err != nil {
					t.Fatalf("remove: %v", err)
				}
			},
			wantLog: "no source root",
		},
		{
			name: "the running binary is not an installed target",
			setup: func(h *harness) {
				h.setStale()
				hand := filepath.Join(h.home, "hand-built-ccx")
				writeFile(t, hand, "binary")
				h.chtimes(hand, h.now.Add(-time.Hour))
				h.exe = hand
			},
			wantLog: "not an installed target",
		},
		{
			name: "no go toolchain on PATH",
			setup: func(h *harness) {
				h.setStale()
			},
			wantLog: "no go toolchain",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := newHarness(t)
			tt.setup(h)

			d := h.deps()
			if tt.wantLog == "no go toolchain" {
				d.LookPath = func(string) (string, error) { return "", os.ErrNotExist }
			}

			got := Run(d)

			if diff := cmp.Diff(State{}, got); diff != "" {
				t.Errorf("State mismatch (-want +got):\n%s", diff)
			}
			if len(h.runner.calls) != 0 {
				t.Errorf("ran %v, want no commands", h.runner.calls)
			}
			if h.reexec.called {
				t.Error("re-execed, want no re-exec")
			}
			if !strings.Contains(h.debug.String(), tt.wantLog) {
				t.Errorf("debug log = %q, want it to mention %q", h.debug.String(), tt.wantLog)
			}
		})
	}
}

func TestRunRebuildsAndReExecs(t *testing.T) {
	h := newHarness(t)
	h.setStale()

	got := Run(h.deps())

	if diff := cmp.Diff(State{}, got); diff != "" {
		t.Errorf("State mismatch (-want +got):\n%s", diff)
	}

	wantCalls := []runner.Command{{Name: "go", Args: []string{"-C", h.root, "install", "./cmd/ccx"}}}
	if diff := cmp.Diff(wantCalls, h.runner.calls); diff != "" {
		t.Errorf("commands mismatch (-want +got):\n%s", diff)
	}

	// go install skips the copy when the binary is already current, so without
	// this stamp a reverted edit would leave the binary permanently "stale" and
	// rebuild on every single invocation.
	//
	// The stamp is the newest source the build saw, not the time it finished:
	// an edit landing during those few hundred milliseconds has to stay newer
	// than the binary, or it is never noticed.
	want := mustScan(t, h.root).newest
	if got, ok := h.touched[h.exe]; !ok || !got.Equal(want) {
		t.Errorf("touched[%s] = %v (present=%t), want %v", h.exe, got, ok, want)
	}

	if !h.reexec.called {
		t.Fatal("did not re-exec")
	}
	if h.reexec.argv0 != h.exe {
		t.Errorf("re-exec argv0 = %q, want %q", h.reexec.argv0, h.exe)
	}
	if diff := cmp.Diff([]string{h.exe, "statusline"}, h.reexec.argv); diff != "" {
		t.Errorf("re-exec argv mismatch (-want +got):\n%s", diff)
	}
	if !slices.Contains(h.reexec.env, reexecEnv+"=1") {
		t.Errorf("re-exec env = %v, want it to carry %s=1", h.reexec.env, reexecEnv)
	}
}

func TestRunRecordsBuildFailure(t *testing.T) {
	h := newHarness(t)
	h.setStale()
	h.runner.err = os.ErrInvalid
	h.runner.stderr = "# github.com/178inaba/dotfiles/go/internal/statusline\n" +
		"internal/statusline/render.go:12:2: undefined: nope\n"

	got := Run(h.deps())

	want := State{Failed: true, JustFailed: true, FirstError: "internal/statusline/render.go:12:2: undefined: nope"}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("State mismatch (-want +got):\n%s", diff)
	}
	if h.reexec.called {
		t.Error("re-execed after a failed build")
	}

	// A second start in the same source state reports the same breakage without
	// paying for another build; that is what keeps a broken tree from running
	// go install on every five-second tick.
	// The second start reports the same breakage but not as a fresh one, which
	// is how a hook knows to stay quiet while a status line keeps warning.
	suppressed := State{Failed: true, FirstError: want.FirstError}
	h.runner.calls = nil
	if diff := cmp.Diff(suppressed, Run(h.deps())); diff != "" {
		t.Errorf("second State mismatch (-want +got):\n%s", diff)
	}
	if len(h.runner.calls) != 0 {
		t.Errorf("second run executed %v, want nothing", h.runner.calls)
	}
	if !strings.Contains(h.debug.String(), "suppressed by hash") {
		t.Errorf("debug log = %q, want it to mention the hash suppression", h.debug.String())
	}
}

func TestRunRetriesAfterTheSourceChanges(t *testing.T) {
	h := newHarness(t)
	h.setStale()
	h.runner.err = os.ErrInvalid
	h.runner.stderr = "boom\n"
	Run(h.deps())

	// Any edit — including one that only reverts an earlier one — is a
	// different source state and earns another attempt.
	writeFile(t, filepath.Join(h.root, "main.go"), "package main\n")
	h.chtimes(filepath.Join(h.root, "main.go"), h.now)
	h.runner.calls = nil
	h.runner.err = nil

	if diff := cmp.Diff(State{}, Run(h.deps())); diff != "" {
		t.Errorf("State mismatch (-want +got):\n%s", diff)
	}
	if len(h.runner.calls) != 1 {
		t.Errorf("ran %v, want one install", h.runner.calls)
	}
	if _, err := os.Stat(failurePath(h.deps())); !os.IsNotExist(err) {
		t.Errorf("failure record still present after a successful build (err=%v)", err)
	}
}

func TestRunYieldsTheBuildLock(t *testing.T) {
	h := newHarness(t)
	h.setStale()

	release, ok := lock(h.deps())
	if !ok {
		t.Fatal("could not take the lock")
	}
	defer release()

	if diff := cmp.Diff(State{}, Run(h.deps())); diff != "" {
		t.Errorf("State mismatch (-want +got):\n%s", diff)
	}
	if len(h.runner.calls) != 0 {
		t.Errorf("ran %v, want nothing while the lock is held", h.runner.calls)
	}
	if !strings.Contains(h.debug.String(), "build lock") {
		t.Errorf("debug log = %q, want it to mention the lock", h.debug.String())
	}
}

func TestSourceRoot(t *testing.T) {
	h := newHarness(t)

	got, ok := sourceRoot(h.deps())
	if !ok {
		t.Fatal("sourceRoot did not resolve")
	}
	// The link is relative, so resolving it against the process working
	// directory rather than the link's own would land somewhere else entirely.
	if got != h.root {
		t.Errorf("sourceRoot = %q, want %q", got, h.root)
	}
}

func mustScan(t *testing.T, root string) source {
	t.Helper()
	s, err := scanSource(root)
	if err != nil {
		t.Fatalf("scanSource: %v", err)
	}
	return s
}

// TestInstallResolvesGOBIN pins the pair contract: a target names its directory
// once, and both the install and the check that finds the binary afterwards
// resolve it the same way.
func TestInstallResolvesGOBIN(t *testing.T) {
	h := newHarness(t)
	shims := target{pkg: "./cmd/gh", gobin: ".local/shims"}
	targets = append(targets, shims)
	t.Cleanup(func() { targets = targets[:len(targets)-1] })

	if _, err := install(h.deps(), h.root); err != nil {
		t.Fatalf("install: %v", err)
	}

	want := []runner.Command{
		{Name: "go", Args: []string{"-C", h.root, "install", "./cmd/ccx"}},
		{
			Name: "go",
			Args: []string{"-C", h.root, "install", "./cmd/gh"},
			// Absolute, because go install refuses anything else, and equal to
			// what installPath expects the binary to be at.
			Env: []string{"GOBIN=" + filepath.Join(h.home, ".local", "shims")},
		},
	}
	if diff := cmp.Diff(want, h.runner.calls); diff != "" {
		t.Errorf("commands mismatch (-want +got):\n%s", diff)
	}
	if got := installPath(h.deps(), shims); got != filepath.Join(h.home, ".local", "shims", "gh") {
		t.Errorf("installPath = %q, does not match the GOBIN the install was given", got)
	}
}

func TestInstallPathFollowsGOBIN(t *testing.T) {
	h := newHarness(t)

	if got, want := installPath(h.deps(), target{pkg: "./cmd/ccx"}), h.exe; got != want {
		t.Errorf("installPath = %q, want %q", got, want)
	}

	// A target's own directory wins, which is how cmd/gh will land in
	// ~/.local/shims rather than the shared ~/go/bin. It is resolved against
	// home rather than written with a tilde, because nothing here runs a shell
	// and go install would create a directory actually called "~".
	got := installPath(h.deps(), target{pkg: "./cmd/gh", gobin: ".local/shims"})
	if want := filepath.Join(h.home, ".local", "shims", "gh"); got != want {
		t.Errorf("installPath = %q, want %q", got, want)
	}
}

func TestFirstLine(t *testing.T) {
	tests := []struct {
		name string
		out  string
		want string
	}{
		{name: "empty falls back to a marker", out: "", want: "build failed"},
		{name: "leading blank lines are skipped", out: "\n\n  boom  \nnext\n", want: "boom"},
		{name: "carriage returns are stripped", out: "first\r\nsecond\r\n", want: "first"},
		{
			// The go command's package banner carries no diagnostic, so the
			// first real one is worth more in a one-line warning.
			name: "the package banner is skipped",
			out:  "# example.com/pkg\nfile.go:1:2: undefined: nope\n",
			want: "file.go:1:2: undefined: nope",
		},
		{
			name: "a banner on its own is still better than nothing",
			out:  "# example.com/pkg\n",
			want: "# example.com/pkg",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := firstLine([]byte(tt.out)); got != tt.want {
				t.Errorf("firstLine(%q) = %q, want %q", tt.out, got, tt.want)
			}
		})
	}
}

func TestScanSourceSumChangesWithTheTree(t *testing.T) {
	root := t.TempDir()
	writeFile(t, filepath.Join(root, "a.go"), "package a\n")

	before := mustSum(t, root)
	if same := mustSum(t, root); same != before {
		t.Errorf("sum is not stable: %q then %q", before, same)
	}

	writeFile(t, filepath.Join(root, "a.go"), "package a // edited\n")
	if after := mustSum(t, root); after == before {
		t.Error("sum did not change after an edit")
	}
}

func mustSum(t *testing.T, root string) string {
	t.Helper()
	s, err := scanSource(root)
	if err != nil {
		t.Fatalf("scanSource: %v", err)
	}
	return s.sum
}

func TestIsStale(t *testing.T) {
	root := t.TempDir()
	writeFile(t, filepath.Join(root, "a.go"), "package a\n")
	mod := time.Date(2026, 8, 31, 12, 0, 0, 0, time.UTC)
	if err := os.Chtimes(filepath.Join(root, "a.go"), mod, mod); err != nil {
		t.Fatalf("chtimes: %v", err)
	}

	tests := []struct {
		name   string
		binary time.Time
		want   bool
	}{
		{name: "the binary is newer", binary: mod.Add(time.Second)},
		// Equal timestamps are not stale: the comparison is strictly after, so
		// a build that lands in the same second as the edit is not rebuilt on
		// every invocation from then on.
		{name: "the timestamps match", binary: mod},
		{name: "the binary is older", binary: mod.Add(-time.Second), want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := isStale(root, tt.binary)
			if err != nil {
				t.Fatalf("isStale: %v", err)
			}
			if got != tt.want {
				t.Errorf("isStale = %t, want %t", got, tt.want)
			}
		})
	}
}
