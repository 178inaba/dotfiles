package statusline

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

const now = int64(1756600000)

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
		Runner:       h.runner,
		Spawner:      h.spawner,
		Now:          func() time.Time { return time.Unix(now, 0) },
		Getwd:        func() (string, error) { return "/w", nil },
		Home:         "/home/nobody",
		GitCacheBase: filepath.Join(dir, "git-cache"),
		PRCacheBase:  filepath.Join(dir, "pr-cache"),
		FXCachePath:  filepath.Join(dir, "usd-jpy"),
		ChildEnv:     selfbuild.ChildEnv(),
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

// names is what the runner was asked to execute, for asserting that something
// was or was not run in the foreground.
func (h *harness) names() []string {
	var names []string
	for _, c := range h.calls() {
		names = append(names, c.Name)
	}
	return names
}

func (h *harness) calls() []runner.Command { return h.runner.calls }

const porcelain = "# branch.head main\n# branch.upstream origin/main\n# branch.ab +1 -0\n"

func TestRunCachesTheRepositoryState(t *testing.T) {
	h := newHarness(t)
	h.runner.out = porcelain

	if got := h.run(t, `{"workspace":{"current_dir":"/w","project_dir":"/w"}}`); !strings.Contains(got, "(main ↑1)") {
		t.Errorf("output = %q, want it to show the branch", got)
	}
	if diff := cmp.Diff([]runner.Command{{
		Name: "git",
		Args: []string{"--no-optional-locks", "status", "--porcelain=v2", "--branch"},
	}}, h.calls()); diff != "" {
		t.Errorf("commands mismatch (-want +got):\n%s", diff)
	}

	// The five-second cache is what keeps a redraw to one git invocation per
	// refresh cycle rather than one per render.
	h.runner.calls = nil
	h.run(t, `{"workspace":{"current_dir":"/w","project_dir":"/w"}}`)
	if len(h.calls()) != 0 {
		t.Errorf("second render ran %v, want nothing", h.calls())
	}
}

func TestRunOutsideARepository(t *testing.T) {
	h := newHarness(t)
	h.runner.err = os.ErrNotExist

	got := h.run(t, `{"workspace":{"current_dir":"/w","project_dir":"/w"}}`)

	// Two lines: no repository and no session id means no middle line at all.
	if lines := strings.Count(got, "\n"); lines != 2 {
		t.Errorf("output = %q, want two lines", got)
	}

	// The failure is cached like any other answer, so a directory outside a
	// repository is not re-checked on every redraw.
	h.runner.calls = nil
	h.run(t, `{"workspace":{"current_dir":"/w","project_dir":"/w"}}`)
	if len(h.calls()) != 0 {
		t.Errorf("second render ran %v, want the cached answer to be used", h.calls())
	}
}

func TestRunRefreshes(t *testing.T) {
	const payload = `{"workspace":{"current_dir":"/w","project_dir":"/w"},` +
		`"model":{"display_name":"Opus"},"cost":{"total_cost_usd":1.23}}`

	tests := []struct {
		name    string
		payload string
		// seed writes cache files before the render.
		seed func(*testing.T, *harness)

		wantSpawns []string
	}{
		{
			name:    "a first render starts both refreshes",
			payload: payload,
			// The order follows the render: the badge is resolved before the
			// cost segment asks for a rate.
			wantSpawns: []string{RefreshPRCommandName, RefreshFXCommandName},
		},
		{
			// Nothing to convert means nothing to fetch: a session below a cent
			// should not be making network requests.
			name:       "no cost means no exchange rate is fetched",
			payload:    `{"workspace":{"current_dir":"/w","project_dir":"/w"}}`,
			wantSpawns: []string{RefreshPRCommandName},
		},
		{
			name:    "fresh caches start nothing",
			payload: payload,
			seed: func(t *testing.T, h *harness) {
				writeFile(t, h.cfg.FXCachePath, "1756600000\n162.22\n")
				writeFile(t, prPath(h), "1756600000\n/w:main\n123 NONE https://e/1")
			},
		},
		{
			// A fetch already in flight must not be started again by the render
			// five seconds later.
			name:    "a recent attempt starts nothing",
			payload: payload,
			seed: func(t *testing.T, h *harness) {
				writeFile(t, h.cfg.FXCachePath+".attempt", "1756599990\n")
				writeFile(t, prPath(h)+".attempt", "1756599990\n")
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
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

			// gh never runs in the foreground: a slow one would hold up the
			// pipe Claude Code reads the status line from.
			for _, name := range h.names() {
				if name == "gh" {
					t.Errorf("gh ran in the foreground (calls: %v)", h.names())
				}
			}
		})
	}
}

func TestRunPassesTheParentsValuesToTheChild(t *testing.T) {
	h := newHarness(t)
	h.runner.out = porcelain

	h.run(t, `{"workspace":{"current_dir":"/w","project_dir":"/w"}}`)

	// The parent hands over what it computed rather than letting the child work
	// it out again: the cache path is cut to a fixed length, and two
	// derivations of that could disagree about which file to write.
	// The child is told not to repeat the parent's self-rebuild check, which it
	// would otherwise race.
	if diff := cmp.Diff([][]string{selfbuild.ChildEnv()}, h.spawner.envs); diff != "" {
		t.Errorf("child environment mismatch (-want +got):\n%s", diff)
	}

	want := []string{
		RefreshPRCommandName,
		"--now=1756600000",
		"--cache=" + prPath(h),
		"--key=/w:main",
		"--branch=main",
	}
	if diff := cmp.Diff([][]string{want}, h.spawner.calls); diff != "" {
		t.Errorf("spawn mismatch (-want +got):\n%s", diff)
	}
}

func TestRunSkipsThePullRequestWithoutABranch(t *testing.T) {
	h := newHarness(t)
	h.runner.out = "# branch.head (detached)\n"

	h.run(t, `{"workspace":{"current_dir":"/w","project_dir":"/w"}}`)

	if len(h.spawner.calls) != 0 {
		t.Errorf("spawned %v, want nothing on a detached head", h.spawner.calls)
	}
}

func TestRunReportsAFailedSelfRebuild(t *testing.T) {
	h := newHarness(t)
	h.runner.err = os.ErrNotExist
	h.buildError = "internal/x.go:1:2: undefined: nope"

	got := h.run(t, "")

	if !strings.Contains(got, "⚠ ccx build failed: internal/x.go:1:2: undefined: nope") {
		t.Errorf("output = %q, want it to carry the build warning", got)
	}
}

func prPath(h *harness) string {
	return filepath.Join(filepath.Dir(h.cfg.PRCacheBase), "pr-cache-_w:main")
}

func writeFile(t *testing.T, name, body string) {
	t.Helper()
	if err := os.WriteFile(name, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
}
