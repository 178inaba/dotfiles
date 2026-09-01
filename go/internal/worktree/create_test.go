package worktree

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// base is the branch the fixtures start worktrees from. It carries a
// .worktreeinclude, which is what makes the copy step reachable.
const base = "include"

// clone makes a working copy of the fixture origin, with the remote-tracking
// refs a clone brings.
func clone(t *testing.T, bare string) string {
	t.Helper()

	return gittest.Clone(t, bare, filepath.Join(t.TempDir(), "repo"))
}

func TestCreate(t *testing.T) {
	t.Parallel()

	bare, _ := origin(t)
	repo := clone(t, bare)
	gittest.Write(t, filepath.Join(repo, ".env"), "SECRET=1\n")

	beforeHead := gittest.Rev(t, repo, "HEAD")
	beforeBranch := strings.TrimSpace(gittest.Run(t, repo, "branch", "--show-current"))

	got, err := Create(t.Context(), runner.Exec{}, repo, "wt", "feature/42-x", base)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	path := filepath.Join(repo, ".claude", "worktrees", "wt")
	want := Created{
		Status:      CreateOK,
		Path:        &path,
		Branch:      "feature/42-x",
		StartRef:    new("origin/" + base),
		CopiedFiles: 1,
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Create (-want +got):\n%s", diff)
	}
	if got, want := gittest.Rev(t, path, "HEAD"), gittest.Rev(t, repo, "origin/"+base); got != want {
		t.Errorf("the worktree is at %s, want %s", got, want)
	}
	if got := strings.TrimSpace(gittest.Run(t, path, "branch", "--show-current")); got != "feature/42-x" {
		t.Errorf("the worktree is on %q, want %q", got, "feature/42-x")
	}
	// The whole reason this exists rather than EnterWorktree(name:): the main
	// tree keeps its head and its branch.
	if got := gittest.Rev(t, repo, "HEAD"); got != beforeHead {
		t.Errorf("the main worktree moved to %s, want it left at %s", got, beforeHead)
	}
	if got := strings.TrimSpace(gittest.Run(t, repo, "branch", "--show-current")); got != beforeBranch {
		t.Errorf("the main worktree is on %q, want %q", got, beforeBranch)
	}
}

// TestCreateCopiesTheIncludedFiles checks the wiring rather than the copying:
// that the copy runs with the right source and destination, and that its count
// and its warnings reach the result. The edge cases belong to
// TestCopyWorktreeInclude.
func TestCreateCopiesTheIncludedFiles(t *testing.T) {
	t.Parallel()

	bare, _ := origin(t)
	repo := clone(t, bare)
	gittest.Write(t, filepath.Join(repo, ".env"), "SECRET=1\n")
	gittest.Write(t, filepath.Join(repo, "real-secrets"), "real\n")
	if err := os.MkdirAll(filepath.Join(repo, "config"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	symlink(t, "../real-secrets", filepath.Join(repo, "config", "secrets.json"))

	got, err := Create(t.Context(), runner.Exec{}, repo, "wt", "feature/42-x", base)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	if got.CopiedFiles != 1 {
		t.Errorf("CopiedFiles = %d, want 1", got.CopiedFiles)
	}
	if _, err := os.Stat(filepath.Join(repo, ".claude", "worktrees", "wt", ".env")); err != nil {
		t.Errorf(".env did not reach the worktree: %v", err)
	}
	want := []string{"skipped symlink in .worktreeinclude: config/secrets.json"}
	if diff := cmp.Diff(want, got.Warnings); diff != "" {
		t.Errorf("warnings (-want +got):\n%s", diff)
	}
}

func TestCreateStartRef(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// setUp prepares the clone and returns the base branch to start from.
		setUp        func(t *testing.T, repo string) string
		wantStartRef string
		wantWarnings []string
	}{
		{
			name:         "the remote-tracking branch",
			setUp:        func(*testing.T, string) string { return base },
			wantStartRef: "origin/" + base,
		},
		{
			// No remote-tracking ref at all, which is what a branch that only
			// exists locally looks like — including after a fetch that failed.
			name: "a local branch when there is no remote one",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "switch", "-qc", "local-only", "origin/"+base)
				gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "local base work")
				gittest.Run(t, repo, "switch", "-q", "main")
				return "local-only"
			},
			wantStartRef: "local-only",
			wantWarnings: []string{"origin/local-only not found; started from local branch local-only"},
		},
		{
			// The remote still wins, but silently starting from it would leave
			// the author's unpushed commits out of the new worktree.
			name: "a local branch ahead of the remote one",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "switch", "-qc", base, "origin/"+base)
				gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "unpushed local work")
				gittest.Run(t, repo, "switch", "-q", "main")
				return base
			},
			wantStartRef: "origin/" + base,
			wantWarnings: []string{"local branch include has commits not on origin/include; worktree starts from origin/include"},
		},
	}

	bare, _ := origin(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo := clone(t, bare)
			from := tc.setUp(t, repo)

			got, err := Create(t.Context(), runner.Exec{}, repo, "wt", "feature/42-x", from)
			if err != nil {
				t.Fatalf("Create: %v", err)
			}
			if got.StartRef == nil || *got.StartRef != tc.wantStartRef {
				t.Errorf("StartRef = %v, want %q", got.StartRef, tc.wantStartRef)
			}
			if diff := cmp.Diff(tc.wantWarnings, got.Warnings); diff != "" {
				t.Errorf("warnings (-want +got):\n%s", diff)
			}
			if got, want := gittest.Rev(t, filepath.Join(repo, ".claude", "worktrees", "wt"), "HEAD"), gittest.Rev(t, repo, tc.wantStartRef); got != want {
				t.Errorf("the worktree is at %s, want %s", got, want)
			}
		})
	}
}

// TestCreateStops covers the two conditions that leave the decision to a
// person: either could be the remains of earlier work on the same issue, and a
// command that threw it away would be answering a question it was not asked.
func TestCreateStops(t *testing.T) {
	t.Parallel()

	bare, _ := origin(t)

	t.Run("the branch already exists", func(t *testing.T) {
		t.Parallel()

		repo := clone(t, bare)
		gittest.Run(t, repo, "branch", "feature/42-x", "origin/"+base)

		got, err := Create(t.Context(), runner.Exec{}, repo, "wt", "feature/42-x", base)
		if err != nil {
			t.Fatalf("Create: %v", err)
		}
		if diff := cmp.Diff(Created{Status: CreateBranchExists, Branch: "feature/42-x"}, got); diff != "" {
			t.Errorf("Create (-want +got):\n%s", diff)
		}
		if _, err := os.Stat(filepath.Join(repo, ".claude", "worktrees", "wt")); !os.IsNotExist(err) {
			t.Error("a worktree was created for a branch that already existed")
		}
	})

	t.Run("the path already exists", func(t *testing.T) {
		t.Parallel()

		repo := clone(t, bare)
		if err := os.MkdirAll(filepath.Join(repo, ".claude", "worktrees", "wt"), 0o755); err != nil {
			t.Fatalf("MkdirAll: %v", err)
		}

		got, err := Create(t.Context(), runner.Exec{}, repo, "wt", "feature/42-x", base)
		if err != nil {
			t.Fatalf("Create: %v", err)
		}
		if diff := cmp.Diff(Created{Status: CreatePathExists, Branch: "feature/42-x"}, got); diff != "" {
			t.Errorf("Create (-want +got):\n%s", diff)
		}
		out := gittest.Run(t, repo, "branch", "--list", "feature/42-x")
		if strings.TrimSpace(out) != "" {
			t.Errorf("a branch was created for a path that already existed: %s", out)
		}
	})
}

func TestCreateWithoutABase(t *testing.T) {
	t.Parallel()

	bare, _ := origin(t)
	repo := clone(t, bare)

	got, err := Create(t.Context(), runner.Exec{}, repo, "wt", "feature/42-x", "no-such-base")
	if err == nil {
		t.Fatalf("Create = %+v, want a failure", got)
	}
	if want := "base branch not found"; !strings.Contains(err.Error(), want) {
		t.Errorf("Create error = %q, want it to mention %q", err, want)
	}
}

func TestDetect(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// setUp prepares the clone, and returns the branch it expects Detect to
		// find, or empty for a clone where nothing should match.
		setUp     func(t *testing.T, repo string) string
		issue     int
		wantFound bool
	}{
		{
			name: "the current naming",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "worktree", "add", "-q", filepath.Join(repo, ".claude", "worktrees", "wt"), "-b", "feature/42-x", "origin/"+base)
				return "feature/42-x"
			},
			issue: 42, wantFound: true,
		},
		{
			// The worktrees EnterWorktree(name:) made are still on disk, and
			// resuming into one is the whole point of asking.
			name: "the naming EnterWorktree used",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "worktree", "add", "-q", filepath.Join(repo, ".claude", "worktrees", "legacy"), "-b", "worktree-feature-42-old-style", "origin/"+base)
				return "worktree-feature-42-old-style"
			},
			issue: 42, wantFound: true,
		},
		{
			name: "a number that is only a prefix",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "worktree", "add", "-q", filepath.Join(repo, ".claude", "worktrees", "wt"), "-b", "feature/42-x", "origin/"+base)
				return ""
			},
			issue: 4,
		},
		{
			name: "a number that is only a suffix",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "worktree", "add", "-q", filepath.Join(repo, ".claude", "worktrees", "wt"), "-b", "feature/42-x", "origin/"+base)
				return ""
			},
			issue: 142,
		},
		{
			// Resuming into the main worktree would mean working in the
			// repository itself, which is what the worktree was avoiding.
			name: "the main worktree has the branch checked out",
			setUp: func(t *testing.T, repo string) string {
				gittest.Run(t, repo, "switch", "-qc", "feature/42-x", "origin/"+base)
				return ""
			},
			issue: 42,
		},
		{
			name:  "nothing matches",
			setUp: func(*testing.T, string) string { return "" },
			issue: 42,
		},
	}

	bare, _ := origin(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			repo := clone(t, bare)
			branch := tc.setUp(t, repo)

			got, err := Detect(t.Context(), runner.Exec{}, repo, tc.issue)
			if err != nil {
				t.Fatalf("Detect(%d): %v", tc.issue, err)
			}
			if got.Found != tc.wantFound {
				t.Fatalf("Detect(%d) = %+v, want found=%v", tc.issue, got, tc.wantFound)
			}
			if !tc.wantFound {
				if got.Path != nil || got.Branch != nil {
					t.Errorf("Detect(%d) = %+v, want the path and branch null", tc.issue, got)
				}
				return
			}
			if got.Branch == nil || *got.Branch != branch {
				t.Errorf("Detect(%d) branch = %v, want %q", tc.issue, got.Branch, branch)
			}
			// git answers with the resolved path, and on macOS the temporary
			// directory is reached through a symlink.
			resolved, err := filepath.EvalSymlinks(filepath.Join(repo, ".claude", "worktrees"))
			if err != nil {
				t.Fatalf("EvalSymlinks: %v", err)
			}
			if got.Path == nil || !strings.HasPrefix(*got.Path, resolved) {
				t.Errorf("Detect(%d) path = %q, want one under %s", tc.issue, *got.Path, resolved)
			}
		})
	}
}

func TestParseList(t *testing.T) {
	t.Parallel()

	out := "worktree /repo\nHEAD abc123\nbranch refs/heads/main\n\n" +
		"worktree /repo/.claude/worktrees/wt\nHEAD def456\nbranch refs/heads/feature/42-x\n\n" +
		"worktree /repo/.claude/worktrees/detached\nHEAD 789abc\ndetached\n\n"

	want := []Entry{
		{Path: "/repo", Branch: "main", Main: true},
		{Path: "/repo/.claude/worktrees/wt", Branch: "feature/42-x"},
		{Path: "/repo/.claude/worktrees/detached"},
	}
	if diff := cmp.Diff(want, parseList(out)); diff != "" {
		t.Errorf("parseList (-want +got):\n%s", diff)
	}
}

func TestMainRoot(t *testing.T) {
	t.Parallel()

	bare, _ := origin(t)
	repo := clone(t, bare)
	worktreePath := filepath.Join(repo, ".claude", "worktrees", "wt")
	gittest.Run(t, repo, "worktree", "add", "-q", "--detach", worktreePath, "origin/"+base)

	for _, dir := range []string{repo, worktreePath} {
		got, err := MainRoot(t.Context(), runner.Exec{}, dir)
		if err != nil {
			t.Fatalf("MainRoot(%q): %v", dir, err)
		}
		// The fixture lives under the temporary directory, which is reached
		// through a symlink on macOS; git answers with the resolved path.
		want, err := filepath.EvalSymlinks(repo)
		if err != nil {
			t.Fatalf("EvalSymlinks: %v", err)
		}
		if got != want {
			t.Errorf("MainRoot(%q) = %q, want %q", dir, got, want)
		}
	}
}

func TestMainRootOutsideARepository(t *testing.T) {
	t.Parallel()

	gittest.SkipWithoutGit(t)
	if got, err := MainRoot(t.Context(), runner.Exec{}, t.TempDir()); err == nil {
		t.Fatalf("MainRoot = %q, want a failure", got)
	}
}
