package worktree

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// deleteFixture builds a repository holding one of every outcome a deletion
// can have, and returns it.
func deleteFixture(t *testing.T) string {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	repo := filepath.Join(base, "repo")
	gittest.Init(t, repo, "-b", "main")
	gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "init")

	branch := func(name string) {
		gittest.Run(t, repo, "switch", "-qc", name)
		gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", name)
		gittest.Run(t, repo, "switch", "-q", "main")
	}

	// A worktree and its branch, merged: both should go.
	wt := filepath.Join(base, "wt-del")
	gittest.Run(t, repo, "worktree", "add", "-q", wt, "-b", "wt-del", "main")
	gittest.Run(t, wt, "commit", "-q", "--allow-empty", "-m", "a")
	gittest.Run(t, repo, "merge", "-q", "wt-del")

	// A clean worktree whose branch is unmerged under a verdict that says
	// merged: the worktree goes and -d keeps the branch.
	kept := filepath.Join(base, "wt-kept-branch")
	gittest.Run(t, repo, "worktree", "add", "-q", kept, "-b", "wt-kept-branch", "main")
	gittest.Run(t, kept, "commit", "-q", "--allow-empty", "-m", "unmerged")

	// Unmerged, so only -D can remove it.
	branch("closed-br")
	// Unmerged with a verdict that says merged: -d refuses, which is the
	// second opinion that verdict is deliberately left open to.
	branch("fake-merged")
	// Checked out elsewhere, which git refuses on its own.
	gittest.Run(t, repo, "worktree", "add", "-q", filepath.Join(base, "wt-live"), "-b", "live-br", "main")
	// Merged, so -d agrees.
	branch("merged-br")
	gittest.Run(t, repo, "merge", "-q", "merged-br")
	// Its head has moved since the verdict was formed.
	branch("closed-stale")
	// Squash-merged pull requests: none is an ancestor of main, so only -D
	// removes them, and the recorded head decides whether it may.
	branch("squashed-br")
	branch("squashed-moved")
	branch("squashed-unrelated")
	// Behind the head that merged: the commit the reset throws away stands in
	// for the pull request's head, reachable afterwards as @{1}.
	gittest.Run(t, repo, "switch", "-qc", "squashed-behind")
	gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "squashed-behind")
	gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "squashed-behind tip")
	gittest.Run(t, repo, "reset", "-q", "--hard", "HEAD~1")
	gittest.Run(t, repo, "switch", "-q", "main")
	// Clean and merged, but somebody is standing in it.
	busy := filepath.Join(base, "wt-busy")
	gittest.Run(t, repo, "worktree", "add", "-q", busy, "-b", "wt-busy", "main")
	gittest.Run(t, repo, "merge", "-q", "wt-busy")

	return repo
}

func TestDelete(t *testing.T) {
	t.Parallel()

	repo := deleteFixture(t)
	base := filepath.Dir(repo)
	busy, del, kept := filepath.Join(base, "wt-busy"), filepath.Join(base, "wt-del"), filepath.Join(base, "wt-kept-branch")
	pid := hold(t, busy)

	candidates := Candidates{
		Worktrees: []Candidate{
			{Path: busy, Branch: "wt-busy", Verdict: VerdictMergedNoPR},
			{Path: del, Branch: "wt-del", Verdict: VerdictMergedNoPR, Detail: "wt-del detail"},
			{Path: kept, Branch: "wt-kept-branch", Verdict: VerdictMergedNoPR, Detail: "wt-kept-branch detail"},
		},
		Branches: []BranchCandidate{
			{Branch: "fake-merged", Verdict: VerdictMergedNoPR},
			{Branch: "closed-br", Verdict: VerdictPRClosed, Detail: "closed-br detail", HeadOID: gittest.Rev(t, repo, "refs/heads/closed-br")},
			// A head that is not the branch's, standing in for a commit made
			// while the branches were being judged.
			{Branch: "closed-stale", Verdict: VerdictPRClosed, HeadOID: gittest.Rev(t, repo, "main")},
			{Branch: "live-br", Verdict: VerdictMergedNoPR},
			{Branch: "merged-br", Verdict: VerdictMergedNoPR, Detail: "merged-br detail"},
			{Branch: "squashed-br", Verdict: VerdictPRMerged, Detail: "squashed-br detail", HeadOID: gittest.Rev(t, repo, "refs/heads/squashed-br")},
			// An ancestor of the recorded head is enough: equality is the
			// closed pull request's rule, not the merged one's.
			{Branch: "squashed-behind", Verdict: VerdictPRMerged, Detail: "squashed-behind detail", HeadOID: gittest.Rev(t, repo, "refs/heads/squashed-behind@{1}")},
			// The branch has moved past the head that merged.
			{Branch: "squashed-moved", Verdict: VerdictPRMerged, HeadOID: gittest.Rev(t, repo, "refs/heads/squashed-moved~1")},
			{Branch: "squashed-unrelated", Verdict: VerdictPRMerged, HeadOID: gittest.Rev(t, repo, "refs/heads/closed-br")},
		},
	}

	got, err := Delete(t.Context(), runner.Exec{}, repo, candidates)
	if err != nil {
		t.Fatalf("Delete: %v", err)
	}

	// The worktree in use is refused, and everything after it still happens:
	// the list is one batch, and stopping at the first refusal would leave the
	// caller working out which half ran.
	// Each removed item carries the verdict and detail it was deleted on, and a
	// worktree's branch is reported on the worktree rather than among the
	// branch candidates.
	wantRemoved := Removed{
		Worktrees: []RemovedWorktree{
			{Path: del, Branch: "wt-del", BranchRemoved: true, Verdict: VerdictMergedNoPR, Detail: "wt-del detail"},
			{Path: kept, Branch: "wt-kept-branch", BranchRemoved: false, Verdict: VerdictMergedNoPR, Detail: "wt-kept-branch detail"},
		},
		Branches: []RemovedBranch{
			{Branch: "closed-br", Verdict: VerdictPRClosed, Detail: "closed-br detail"},
			{Branch: "merged-br", Verdict: VerdictMergedNoPR, Detail: "merged-br detail"},
			{Branch: "squashed-br", Verdict: VerdictPRMerged, Detail: "squashed-br detail"},
			{Branch: "squashed-behind", Verdict: VerdictPRMerged, Detail: "squashed-behind detail"},
		},
	}
	if diff := cmp.Diff(wantRemoved, got.Removed); diff != "" {
		t.Errorf("removed (-want +got):\n%s", diff)
	}
	if _, err := os.Stat(del); !os.IsNotExist(err) {
		t.Errorf("%s survived its removal", del)
	}
	if _, err := os.Stat(busy); err != nil {
		t.Errorf("the worktree in use was removed anyway: %v", err)
	}

	tests := []struct {
		name         string
		target       string
		wantKind     TargetKind
		wantContains string
	}{
		{
			name: "a worktree somebody is standing in", target: busy, wantKind: KindWorktree,
			wantContains: fmt.Sprintf("in use by sleep (PID %d)", pid),
		},
		{
			// -D skips git's safety net, so the verdict's head is checked
			// again here: commits made since it was formed would go, and a
			// closed pull request's refs/pull/N/head no longer brings them
			// back.
			name: "a head that has moved since the verdict", target: "closed-stale", wantKind: KindBranch,
			wantContains: "no longer matches",
		},
		{
			name: "a merged branch moved past the head that merged", target: "squashed-moved", wantKind: KindBranch,
			wantContains: "no longer contained",
		},
		{
			name: "a merged branch recorded against an unrelated head", target: "squashed-unrelated", wantKind: KindBranch,
			wantContains: "no longer contained",
		},
		{name: "an unmerged branch called merged", target: "fake-merged", wantKind: KindBranch},
		{name: "the unmerged branch of a removed worktree", target: "wt-kept-branch", wantKind: KindBranch},
		{name: "a branch checked out elsewhere", target: "live-br", wantKind: KindBranch},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			i := slices.IndexFunc(got.Failures, func(f Failure) bool { return f.Target == tc.target })
			if i < 0 {
				t.Fatalf("%s did not fail; failures were %+v", tc.target, got.Failures)
			}
			f := got.Failures[i]
			if f.Type != tc.wantKind {
				t.Errorf("type = %q, want %q", f.Type, tc.wantKind)
			}
			if f.Error == "" {
				t.Error("the failure carries no reason")
			}
			if tc.wantContains != "" && !strings.Contains(f.Error, tc.wantContains) {
				t.Errorf("error = %q, want it to mention %q", f.Error, tc.wantContains)
			}
		})
	}

	for _, branch := range []string{"closed-stale", "fake-merged", "live-br", "squashed-moved", "squashed-unrelated", "wt-kept-branch"} {
		if !branchExists(t, repo, branch) {
			t.Errorf("%s was deleted despite its failure", branch)
		}
	}
	for _, branch := range []string{"wt-del", "closed-br", "merged-br", "squashed-br", "squashed-behind"} {
		if branchExists(t, repo, branch) {
			t.Errorf("%s survived its deletion", branch)
		}
	}
}

// TestDeleteWithoutAHeadToVerify covers the other half of the -D guard: a
// candidate that reached here with no head at all is refused rather than
// deleted with the flag that skips git's own check.
func TestDeleteWithoutAHeadToVerify(t *testing.T) {
	t.Parallel()

	repo := deleteFixture(t)
	tests := []struct {
		name      string
		candidate BranchCandidate
	}{
		{name: "a closed pull request", candidate: BranchCandidate{Branch: "closed-br", Verdict: VerdictPRClosed}},
		{name: "a merged pull request", candidate: BranchCandidate{Branch: "squashed-br", Verdict: VerdictPRMerged}},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := Delete(t.Context(), runner.Exec{}, repo, Candidates{Branches: []BranchCandidate{tc.candidate}})
			if err != nil {
				t.Fatalf("Delete: %v", err)
			}
			if len(got.Failures) != 1 || !strings.Contains(got.Failures[0].Error, "<missing>") {
				t.Errorf("failures = %+v, want one naming the missing head", got.Failures)
			}
			if !branchExists(t, repo, tc.candidate.Branch) {
				t.Errorf("%s was deleted despite its failure", tc.candidate.Branch)
			}
		})
	}
}

func TestCWDHolders(t *testing.T) {
	t.Parallel()

	base := t.TempDir()
	held := filepath.Join(base, "held")
	free := filepath.Join(base, "free")
	// A name the held one is a prefix of: without the separator in the
	// comparison, this one's process would answer for the other.
	sibling := filepath.Join(base, "held-sibling")
	nested := filepath.Join(base, "nested", "sub")
	for _, dir := range []string{held, free, sibling, nested} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatalf("MkdirAll: %v", err)
		}
	}

	heldPID := hold(t, held)
	siblingPID := hold(t, sibling)
	nestedPID := hold(t, nested)

	table, err := loadCWDTable(t.Context(), runner.Exec{})
	if err != nil {
		t.Fatalf("loadCWDTable: %v", err)
	}

	// The directory is named the way the caller has it — under /var on macOS —
	// while lsof answers with /private/var, so the query has to be resolved
	// before it is compared.
	if got, want := table.holders(held), fmt.Sprintf("sleep (PID %d)", heldPID); got != want {
		t.Errorf("holders(held) = %q, want %q", got, want)
	}
	if got := table.holders(free); got != "" {
		t.Errorf("holders(free) = %q, want none", got)
	}
	// A process below the directory holds it too, and several are one line.
	got := table.holders(base)
	for _, pid := range []int{heldPID, siblingPID, nestedPID} {
		if want := fmt.Sprintf("sleep (PID %d)", pid); !strings.Contains(got, want) {
			t.Errorf("holders(base) = %q, want it to name %s", got, want)
		}
	}
	if !strings.Contains(got, ", ") {
		t.Errorf("holders(base) = %q, want the holders joined with a comma", got)
	}
}

// TestLoadCWDTableFailsClosed is the property the guard rests on: lsof failing
// must not read as "nothing is in use", which would switch the check off at the
// one moment it could not be evaluated.
func TestLoadCWDTableFailsClosed(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		run  func(context.Context, runner.Command) ([]byte, error)
	}{
		{
			name: "lsof exits non-zero",
			run:  func(context.Context, runner.Command) ([]byte, error) { return nil, errors.New("permission denied") },
		},
		{
			// Output that does not hold this process's own working directory
			// is output lsof did not really produce.
			name: "lsof answers without this process",
			run: func(context.Context, runner.Command) ([]byte, error) {
				return []byte("p1\ncsleep\nfcwd\nn/somewhere\n"), nil
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			if got, err := loadCWDTable(t.Context(), runnerFunc(tc.run)); err == nil {
				t.Fatalf("loadCWDTable = %v, want a failure", got)
			}
		})
	}
}

// runnerFunc adapts a function to runner.Runner.
type runnerFunc func(context.Context, runner.Command) ([]byte, error)

func (f runnerFunc) Run(ctx context.Context, c runner.Command) ([]byte, error) { return f(ctx, c) }
