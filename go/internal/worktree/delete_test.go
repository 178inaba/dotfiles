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
	gittest.Run(t, base, "init", "-q", "-b", "main", repo)
	gittest.Run(t, repo, "config", "user.email", "test@example.com")
	gittest.Run(t, repo, "config", "user.name", "test")
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
	busy, del := filepath.Join(base, "wt-busy"), filepath.Join(base, "wt-del")
	pid := hold(t, busy)

	candidates := Candidates{
		Worktrees: []WorktreeCandidate{
			{Path: busy, Branch: "wt-busy", Verdict: VerdictMergedNoPR},
			{Path: del, Branch: "wt-del", Verdict: VerdictMergedNoPR},
		},
		Branches: []BranchCandidate{
			{Branch: "fake-merged", Verdict: VerdictMergedNoPR},
			{Branch: "closed-br", Verdict: VerdictPRClosed, HeadOID: ref(t, repo, "refs/heads/closed-br")},
			// A head that is not the branch's, standing in for a commit made
			// between the verdict and the approval.
			{Branch: "closed-stale", Verdict: VerdictPRClosed, HeadOID: ref(t, repo, "main")},
			{Branch: "live-br", Verdict: VerdictMergedNoPR},
			{Branch: "merged-br", Verdict: VerdictMergedNoPR},
		},
	}

	got, err := Delete(t.Context(), runner.Exec{}, repo, candidates)
	if err != nil {
		t.Fatalf("Delete: %v", err)
	}

	// The worktree in use is refused, and everything after it still happens:
	// the list is a batch somebody approved, and stopping at the first refusal
	// would leave them working out which half ran.
	wantRemoved := Removed{Worktrees: []string{del}, Branches: []string{"wt-del", "closed-br", "merged-br"}}
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
			// git would remove it happily, and every command that process ran
			// afterwards would fail; only this check stops it.
			name: "a head that has moved since the verdict", target: "closed-stale", wantKind: KindBranch,
			wantContains: "no longer matches",
		},
		{name: "an unmerged branch called merged", target: "fake-merged", wantKind: KindBranch},
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

	for _, branch := range []string{"closed-stale", "fake-merged", "live-br"} {
		if _, err := run(t.Context(), runner.Exec{}, repo, "rev-parse", "--verify", "--quiet", "refs/heads/"+branch); err != nil {
			t.Errorf("%s was deleted despite its failure", branch)
		}
	}
	for _, branch := range []string{"wt-del", "closed-br", "merged-br"} {
		if _, err := run(t.Context(), runner.Exec{}, repo, "rev-parse", "--verify", "--quiet", "refs/heads/"+branch); err == nil {
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
	got, err := Delete(t.Context(), runner.Exec{}, repo, Candidates{
		Branches: []BranchCandidate{{Branch: "closed-br", Verdict: VerdictPRClosed}},
	})
	if err != nil {
		t.Fatalf("Delete: %v", err)
	}
	if len(got.Failures) != 1 || !strings.Contains(got.Failures[0].Error, "<missing>") {
		t.Errorf("failures = %+v, want one naming the missing head", got.Failures)
	}
}

func TestParseCandidates(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		in      string
		want    Candidates
		wantErr string
	}{
		{
			name: "what collect wrote",
			in:   `{"candidates":{"worktrees":[{"path":"/w","branch":"b","verdict":"merged_no_pr","head_oid":""}],"branches":[]}}`,
			want: Candidates{
				Worktrees: []WorktreeCandidate{{Path: "/w", Branch: "b", Verdict: VerdictMergedNoPR}},
				Branches:  []BranchCandidate{},
			},
		},
		{name: "not json at all", in: "not json", wantErr: "invalid JSON on stdin"},
		// An empty object would otherwise read as an approved list of nothing,
		// and the command would report a successful deletion of none.
		{name: "no candidates", in: `{}`, wantErr: "stdin JSON missing .candidates"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := ParseCandidates([]byte(tc.in))
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("ParseCandidates = %+v, %v; want the error %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseCandidates: %v", err)
			}
			if diff := cmp.Diff(tc.want, got); diff != "" {
				t.Errorf("ParseCandidates (-want +got):\n%s", diff)
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
