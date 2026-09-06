package worktree

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// The lists come out in the order git lists the worktrees, which is git's own
// and carries nothing for a reader: what the report says is which candidates
// ended up where, not in what sequence they were reached.
var (
	byRemoved = cmpopts.SortSlices(func(a, b Swept) bool { return a.Path+a.Branch < b.Path+b.Branch })
	byKept    = cmpopts.SortSlices(func(a, b Kept) bool { return a.Target < b.Target })
)

// agentWorktree adds a worktree where the harness puts an isolated agent's,
// and on the branch it gives it, and returns its path.
//
// Inside the main working tree, which is where the harness really places them
// — measured for issue #251 — and what the segment match is written against.
func agentWorktree(t *testing.T, repo, id, start string) string {
	t.Helper()

	wt := filepath.Join(repo, ".claude", "worktrees", "agent-"+id)
	gittest.Run(t, repo, "worktree", "add", "-q", wt, "-b", "worktree-agent-"+id, start)
	return wt
}

// lockAsHarness locks a worktree the way the harness does, in its wording.
func lockAsHarness(t *testing.T, repo, wt, id string) {
	t.Helper()

	reason := fmt.Sprintf("claude agent agent-%s (pid 2 start Sun Sep  6 18:53:19 2026)", id)
	gittest.Run(t, repo, "worktree", "lock", "--reason", reason, wt)
}

// isLocked says whether git still holds a lock on wt.
func isLocked(t *testing.T, repo, wt string) bool {
	t.Helper()

	for _, e := range parseList(gittest.Run(t, repo, "worktree", "list", "--porcelain")) {
		if e.Path == wt {
			return e.Locked
		}
	}
	t.Fatalf("%s is no longer a worktree of %s", wt, repo)
	return false
}

// branchExists says whether the repository still holds a branch.
func branchExists(t *testing.T, repo, branch string) bool {
	t.Helper()

	_, err := runner.Git(t.Context(), runner.Exec{}, repo, "rev-parse", "--verify", "--quiet", "refs/heads/"+branch)
	return err == nil
}

// sweepFixture builds a repository holding one of every outcome a sweep can
// have, and returns it.
//
// The temporary directory is resolved first: git prints resolved paths, so an
// expectation built from an unresolved one would differ on macOS alone, where
// /var is a symlink into /private/var. Delete needed none of this, echoing back
// the paths it was handed.
func sweepFixture(t *testing.T) string {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	repo := filepath.Join(base, "repo")
	gittest.Init(t, repo, "-b", "main")
	gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "init")

	// A leftover, dirty the way an agent that failed to restore its tree
	// leaves one: only --force removes it.
	left := agentWorktree(t, repo, "left", "main")
	gittest.Write(t, filepath.Join(left, "file.txt"), "changed\n")

	// Two running agents, which git's lock is what tells apart. The second
	// stands for a lock taken without a reason.
	lockAsHarness(t, repo, agentWorktree(t, repo, "locked", "main"), "locked")
	gittest.Run(t, repo, "worktree", "lock", agentWorktree(t, repo, "bare-lock", "main"))

	// A leftover whose agent committed: the worktree goes, the branch stays.
	ahead := agentWorktree(t, repo, "ahead", "main")
	gittest.Run(t, ahead, "commit", "-q", "--allow-empty", "-m", "the agent's own commit")

	// A leftover somebody is standing in, which the working-directory table is
	// the second guard against.
	agentWorktree(t, repo, "busy", "main")

	// A record whose directory somebody deleted by hand, which git calls
	// prunable. Measured: git worktree remove --force takes the record away
	// and succeeds, so nothing special is owed to it here.
	if err := os.RemoveAll(agentWorktree(t, repo, "prunable", "main")); err != nil {
		t.Fatalf("RemoveAll: %v", err)
	}

	// Branches with no worktree: what a previous sweep kept, or a worktree
	// removed by hand.
	gittest.Run(t, repo, "branch", "worktree-agent-gone", "main")
	gittest.Run(t, repo, "branch", "worktree-agent-gone-ahead", "worktree-agent-ahead")

	// Neither half of the naming is enough on its own. An issue-handle
	// worktree sits in the same directory under a branch of its own, and a
	// worktree on an agent branch may sit somewhere the harness never puts one.
	gittest.Run(t, repo, "worktree", "add", "-q",
		filepath.Join(repo, ".claude", "worktrees", "feature-9-x"), "-b", "feature/9-x", "main")
	gittest.Run(t, repo, "worktree", "add", "-q",
		filepath.Join(repo, ".claude", "worktrees", "elsewhere"), "-b", "worktree-agent-elsewhere", "main")

	return repo
}

func TestSweep(t *testing.T) {
	t.Parallel()

	repo := sweepFixture(t)
	wt := func(name string) string { return filepath.Join(repo, ".claude", "worktrees", name) }
	pid := hold(t, wt("agent-busy"))
	aheadHead := gittest.Rev(t, repo, "refs/heads/worktree-agent-ahead")

	got, err := Sweep(t.Context(), runner.Exec{}, repo)
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	want := SweepReport{
		Removed: []Swept{
			{Type: KindWorktree, Path: wt("agent-left"), Branch: "worktree-agent-left", BranchDeleted: true},
			{Type: KindWorktree, Path: wt("agent-ahead"), Branch: "worktree-agent-ahead"},
			{Type: KindWorktree, Path: wt("agent-prunable"), Branch: "worktree-agent-prunable", BranchDeleted: true},
			{Type: KindBranch, Branch: "worktree-agent-gone", BranchDeleted: true},
		},
		Kept: []Kept{
			{
				Type: KindWorktree, Target: wt("agent-locked"), Branch: "worktree-agent-locked",
				Reason: KeptLocked,
				Detail: "実行中のエージェントがロック中: claude agent agent-locked (pid 2 start Sun Sep  6 18:53:19 2026)",
			},
			{
				Type: KindWorktree, Target: wt("agent-bare-lock"), Branch: "worktree-agent-bare-lock",
				Reason: KeptLocked, Detail: "実行中のエージェントがロック中（理由なし）",
			},
			{
				Type: KindBranch, Target: "worktree-agent-ahead", Reason: KeptBranchBeyondHead,
				Detail: "HEAD に含まれない commit を持つ", Head: aheadHead,
			},
			{
				Type: KindWorktree, Target: wt("agent-busy"), Branch: "worktree-agent-busy",
				Reason: KeptInUseByProcess,
				Detail: fmt.Sprintf("使用中のプロセスあり: sleep (PID %d)", pid),
			},
			{
				Type: KindBranch, Target: "worktree-agent-gone-ahead", Reason: KeptBranchBeyondHead,
				Detail: "HEAD に含まれない commit を持つ", Head: aheadHead,
			},
		},
		Failures: []Failure{},
	}
	if diff := cmp.Diff(want, got, byRemoved, byKept); diff != "" {
		t.Errorf("Sweep (-want +got):\n%s", diff)
	}

	for _, name := range []string{"agent-left", "agent-ahead"} {
		if _, err := os.Stat(wt(name)); !os.IsNotExist(err) {
			t.Errorf("%s survived its removal", name)
		}
	}
	// A locked worktree is another session's running agent, so the lock is
	// left exactly as it was: nothing here ever unlocks anything.
	for _, name := range []string{"agent-locked", "agent-bare-lock"} {
		if _, err := os.Stat(wt(name)); err != nil {
			t.Errorf("the locked worktree %s was removed anyway: %v", name, err)
		}
		if !isLocked(t, repo, wt(name)) {
			t.Errorf("%s came back unlocked", name)
		}
	}
	if _, err := os.Stat(wt("agent-busy")); err != nil {
		t.Errorf("the worktree in use was removed anyway: %v", err)
	}
	// Each half of the naming on its own leaves a worktree alone, and so does
	// being the main one.
	for _, name := range []string{"feature-9-x", "elsewhere"} {
		if _, err := os.Stat(wt(name)); err != nil {
			t.Errorf("%s is not named as the harness names them and went anyway: %v", name, err)
		}
	}
	if _, err := os.Stat(filepath.Join(repo, ".git")); err != nil {
		t.Errorf("the main worktree went: %v", err)
	}

	for _, branch := range []string{"worktree-agent-left", "worktree-agent-prunable", "worktree-agent-gone"} {
		if branchExists(t, repo, branch) {
			t.Errorf("%s survived its deletion", branch)
		}
	}
	for _, branch := range []string{
		"worktree-agent-ahead", "worktree-agent-gone-ahead", "worktree-agent-locked",
		"worktree-agent-bare-lock", "worktree-agent-busy", "worktree-agent-elsewhere", "feature/9-x",
	} {
		if !branchExists(t, repo, branch) {
			t.Errorf("%s was deleted", branch)
		}
	}

	// A second pass over what the first one left: what was removed is not
	// reported again, and a branch kept for carrying a commit is — which is
	// the reminder it is kept as.
	again, err := Sweep(t.Context(), runner.Exec{}, repo)
	if err != nil {
		t.Fatalf("Sweep again: %v", err)
	}
	if diff := cmp.Diff([]Swept{}, again.Removed); diff != "" {
		t.Errorf("the second pass removed something (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff(want.Kept, again.Kept, byKept); diff != "" {
		t.Errorf("the second pass kept something else (-want +got):\n%s", diff)
	}
	if len(again.Failures) != 0 {
		t.Errorf("the second pass failed at %+v", again.Failures)
	}
}

// TestSweepAgainstTheCallersHead is the pair that pins which head a branch is
// held against: the working directory's, not the default branch's. One case
// alone would pass for the wrong reason.
func TestSweepAgainstTheCallersHead(t *testing.T) {
	t.Parallel()

	// fixture builds a repository whose linked worktree is a commit ahead of
	// main, with a leftover agent worktree at that commit.
	fixture := func(t *testing.T) (repo, linked, agent string) {
		t.Helper()
		gittest.SkipWithoutGit(t)

		base, err := filepath.EvalSymlinks(t.TempDir())
		if err != nil {
			t.Fatalf("EvalSymlinks: %v", err)
		}
		repo = filepath.Join(base, "repo")
		gittest.Init(t, repo, "-b", "main")
		gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "init")

		linked = filepath.Join(repo, ".claude", "worktrees", "feature-1-x")
		gittest.Run(t, repo, "worktree", "add", "-q", linked, "-b", "feature/1-x", "main")
		gittest.Run(t, linked, "commit", "-q", "--allow-empty", "-m", "the session's own commit")

		return repo, linked, agentWorktree(t, repo, "x", "feature/1-x")
	}

	t.Run("from the worktree the leftover branched off", func(t *testing.T) {
		t.Parallel()

		repo, linked, agent := fixture(t)
		got, err := Sweep(t.Context(), runner.Exec{}, linked)
		if err != nil {
			t.Fatalf("Sweep: %v", err)
		}

		want := []Swept{{Type: KindWorktree, Path: agent, Branch: "worktree-agent-x", BranchDeleted: true}}
		if diff := cmp.Diff(want, got.Removed); diff != "" {
			t.Errorf("removed (-want +got):\n%s", diff)
		}
		if len(got.Kept) != 0 {
			t.Errorf("kept = %+v, want none", got.Kept)
		}
		if branchExists(t, repo, "worktree-agent-x") {
			t.Error("the branch survived, though its head is the caller's own")
		}
	})

	t.Run("from the main worktree, which is behind it", func(t *testing.T) {
		t.Parallel()

		repo, _, agent := fixture(t)
		got, err := Sweep(t.Context(), runner.Exec{}, repo)
		if err != nil {
			t.Fatalf("Sweep: %v", err)
		}

		want := []Swept{{Type: KindWorktree, Path: agent, Branch: "worktree-agent-x"}}
		if diff := cmp.Diff(want, got.Removed); diff != "" {
			t.Errorf("removed (-want +got):\n%s", diff)
		}
		wantKept := []Kept{{
			Type: KindBranch, Target: "worktree-agent-x", Reason: KeptBranchBeyondHead,
			Detail: "HEAD に含まれない commit を持つ",
			Head:   gittest.Rev(t, repo, "refs/heads/worktree-agent-x"),
		}}
		if diff := cmp.Diff(wantKept, got.Kept, byKept); diff != "" {
			t.Errorf("kept (-want +got):\n%s", diff)
		}
		if !branchExists(t, repo, "worktree-agent-x") {
			t.Error("the branch was deleted, though the caller's head does not hold its commit")
		}
	})
}

// TestSweepOutsideARepository pins the one premise that fails the command
// rather than being reported: with no repository there is nothing to sweep.
func TestSweepOutsideARepository(t *testing.T) {
	t.Parallel()
	gittest.SkipWithoutGit(t)

	if got, err := Sweep(t.Context(), runner.Exec{}, t.TempDir()); err == nil {
		t.Fatalf("Sweep = %+v, want a failure", got)
	} else if !strings.Contains(err.Error(), "not a git repository") {
		t.Errorf("error = %q, want it to name the missing repository", err)
	}
}
