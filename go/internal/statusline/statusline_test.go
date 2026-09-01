package statusline

import (
	"context"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/statusline/prinfo"
)

var now = time.Date(2026, 8, 31, 12, 0, 0, 0, time.UTC)

type fakeRunner struct {
	out   string
	err   error
	calls []runner.Command
}

func (f *fakeRunner) Run(_ context.Context, c runner.Command) ([]byte, error) {
	f.calls = append(f.calls, c)
	if f.err != nil {
		return nil, f.err
	}
	return []byte(f.out), nil
}

type fakeSpawner struct {
	calls [][]string
	envs  [][]string
}

func (f *fakeSpawner) Spawn(env []string, args ...string) error {
	f.calls = append(f.calls, args)
	f.envs = append(f.envs, env)
	return nil
}

type harness struct {
	cfg        Config
	runner     *fakeRunner
	spawner    *fakeSpawner
	buildError string
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	dir := t.TempDir()
	h := &harness{runner: &fakeRunner{}, spawner: &fakeSpawner{}}
	h.cfg = Config{
		Runner:  h.runner,
		Spawner: h.spawner,
		Now:     func() time.Time { return now },
		// Deliberately not the payload's directory: every test that sends one
		// then asserts that the payload is what the status line describes,
		// rather than passing because the two agree.
		Getwd:       func() (string, error) { return "/cwd", nil },
		Home:        "/home/nobody",
		GitCacheDir: filepath.Join(dir, "git"),
		PRCacheDir:  filepath.Join(dir, "pr"),
		FXCacheDir:  filepath.Join(dir, "usd-jpy"),
		ChildEnv:    selfbuild.ChildEnv(),
	}
	return h
}

func (h *harness) run(t *testing.T, payload string) string {
	t.Helper()
	var out strings.Builder
	if err := Run(t.Context(), h.cfg, strings.NewReader(payload), &out, h.buildError); err != nil {
		t.Fatalf("Run: %v", err)
	}
	return out.String()
}

func (h *harness) prDir() string {
	return cache.Path(h.cfg.PRCacheDir, "/w", "main")
}

// gitCalls is the whole command list a render makes when it reads the
// repository, which is one git invocation naming dir.
func gitCalls(dir string) []runner.Command {
	return []runner.Command{{
		Name: "git",
		Args: []string{"-C", dir, "--no-optional-locks", "status", "--porcelain=v2", "--branch"},
	}}
}

const (
	porcelain = "# branch.head main\n# branch.upstream origin/main\n# branch.ab +1 -0\n"
	workspace = `{"workspace":{"current_dir":"/w","project_dir":"/w"}}`
	withCost  = `{"workspace":{"current_dir":"/w","project_dir":"/w"},` +
		`"model":{"display_name":"Opus"},"cost":{"total_cost_usd":1.23}}`
)

func TestRunCachesTheRepositoryState(t *testing.T) {
	t.Parallel()

	h := newHarness(t)
	h.runner.out = porcelain

	if got := h.run(t, workspace); !strings.Contains(got, "(main ↑1)") {
		t.Errorf("output = %q, want it to show the branch", got)
	}
	if diff := cmp.Diff(gitCalls("/w"), h.runner.calls); diff != "" {
		t.Errorf("commands mismatch (-want +got):\n%s", diff)
	}

	// The five-second cache is what keeps a redraw to one git invocation per
	// refresh cycle rather than one per render.
	h.runner.calls = nil
	h.run(t, workspace)
	if len(h.runner.calls) != 0 {
		t.Errorf("second render ran %v, want nothing", h.runner.calls)
	}
}

func TestRunFallsBackToTheWorkingDirectory(t *testing.T) {
	t.Parallel()

	h := newHarness(t)
	h.runner.out = porcelain

	// A payload without a workspace leaves the process directory as the only
	// answer there is, and that is the one git is asked about.
	h.run(t, `{}`)
	if diff := cmp.Diff(gitCalls("/cwd"), h.runner.calls); diff != "" {
		t.Errorf("commands mismatch (-want +got):\n%s", diff)
	}
}

func TestRunOutsideARepository(t *testing.T) {
	t.Parallel()

	h := newHarness(t)
	h.runner.err = os.ErrNotExist

	// Two lines: no repository and no session id means no middle line at all.
	if got := h.run(t, workspace); strings.Count(got, "\n") != 2 {
		t.Errorf("output = %q, want two lines", got)
	}

	// The failure is cached like any other answer, so a directory outside a
	// repository is not re-checked on every redraw.
	h.runner.calls = nil
	h.run(t, workspace)
	if len(h.runner.calls) != 0 {
		t.Errorf("second render ran %v, want the cached answer to be used", h.runner.calls)
	}
}

func TestRunRefreshes(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		payload string
		// seed writes cache records before the render.
		seed func(t *testing.T, h *harness)

		wantSpawns []string
	}{
		{
			// The order follows the render: the badge is resolved before the
			// cost segment asks for a rate.
			name:       "a first render starts both refreshes",
			payload:    withCost,
			wantSpawns: []string{RefreshPRCommandName, RefreshFXCommandName},
		},
		{
			// Nothing to convert means nothing to fetch: a session below a cent
			// should not be making network requests.
			name:       "no cost means no exchange rate is fetched",
			payload:    workspace,
			wantSpawns: []string{RefreshPRCommandName},
		},
		{
			name:    "fresh caches start nothing",
			payload: withCost,
			seed: func(t *testing.T, h *harness) {
				write(t, h.cfg.FXCacheDir, "usd-jpy", 162.22)
				write(t, h.prDir(), "/w:main", prinfo.Info{Number: 123})
			},
		},
		{
			// A fetch already in flight must not be started again by the render
			// five seconds later.
			name:    "a recent attempt starts nothing",
			payload: withCost,
			seed: func(t *testing.T, h *harness) {
				attempted(t, h.cfg.FXCacheDir)
				attempted(t, h.prDir())
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			h := newHarness(t)
			h.runner.out = porcelain
			if tt.seed != nil {
				tt.seed(t, h)
			}

			h.run(t, tt.payload)

			var got []string
			for _, c := range h.spawner.calls {
				got = append(got, c[0])
			}
			if diff := cmp.Diff(tt.wantSpawns, got); diff != "" {
				t.Errorf("spawned commands mismatch (-want +got):\n%s", diff)
			}

			// git is the only thing the redraw runs: everything that reaches
			// the network is a detached child, because a slow one would hold up
			// the pipe Claude Code reads the status line from.
			for _, c := range h.runner.calls {
				if c.Name != "git" {
					t.Errorf("%s ran in the foreground (calls: %v)", c.Name, h.runner.calls)
				}
			}
		})
	}
}

func TestRunPassesTheParentsValuesToTheChild(t *testing.T) {
	t.Parallel()

	h := newHarness(t)
	h.runner.out = porcelain

	h.run(t, workspace)

	// The child is told not to repeat the parent's self-rebuild check, which it
	// would otherwise race.
	if diff := cmp.Diff([][]string{selfbuild.ChildEnv()}, h.spawner.envs); diff != "" {
		t.Errorf("child environment mismatch (-want +got):\n%s", diff)
	}

	// The parent hands over what it computed rather than letting the child work
	// it out again: the cache path is cut to a fixed length, and two
	// derivations of that could disagree.
	want := [][]string{{
		RefreshPRCommandName,
		"--now=" + strconv.FormatInt(now.Unix(), 10),
		"--cache=" + h.prDir(),
		"--key=/w:main",
		"--branch=main",
		"--dir=/w",
	}}
	if diff := cmp.Diff(want, h.spawner.calls); diff != "" {
		t.Errorf("spawn mismatch (-want +got):\n%s", diff)
	}
}

func TestRunSkipsThePullRequestWithoutABranch(t *testing.T) {
	t.Parallel()

	h := newHarness(t)
	h.runner.out = "# branch.head (detached)\n"

	h.run(t, workspace)

	if len(h.spawner.calls) != 0 {
		t.Errorf("spawned %v, want nothing on a detached head", h.spawner.calls)
	}
}

func TestRunReportsAFailedSelfRebuild(t *testing.T) {
	t.Parallel()

	h := newHarness(t)
	h.runner.err = os.ErrNotExist
	h.buildError = "internal/x.go:1:2: undefined: nope"

	if got := h.run(t, ""); !strings.Contains(got, "⚠ ccx build failed: internal/x.go:1:2: undefined: nope") {
		t.Errorf("output = %q, want it to carry the build warning", got)
	}
}

func write[T any](t *testing.T, dir, key string, value T) {
	t.Helper()
	if err := cache.Write(dir, key, now, value); err != nil {
		t.Fatalf("seed %s: %v", dir, err)
	}
}

// attempted records a refresh attempt, the same way the foreground does before
// it spawns one.
func attempted(t *testing.T, dir string) {
	t.Helper()
	if !cache.ShouldAttempt(dir, now, time.Minute) {
		t.Fatalf("seed %s: an attempt was already recorded", dir)
	}
}
