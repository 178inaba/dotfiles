package worktree

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// cleanupGitHub answers the two things Collect asks: which repository this is,
// and what pull requests a branch has. A branch with no entry has none, which
// is what GitHub answers for a branch nobody ever opened one for.
func cleanupGitHub(t *testing.T, prs map[string]string, offline bool) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		if offline {
			w.WriteHeader(http.StatusServiceUnavailable)
			fmt.Fprint(w, `{"message":"unavailable"}`)
			return
		}
		if r.URL.Path != "/graphql" {
			fmt.Fprint(w, `{"name":"repo","owner":{"login":"owner"}}`)
			return
		}

		var req struct {
			Variables struct {
				HeadRefName string `json:"headRefName"`
			} `json:"variables"`
		}
		if err := json.UnmarshalRead(r.Body, &req); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		nodes := prs[req.Variables.HeadRefName]
		if nodes == "" {
			nodes = "[]"
		}
		fmt.Fprintf(w, `{"data":{"repository":{"pullRequests":{"nodes":%s}}}}`, nodes)
	}))
}

// pr renders one pull request of a branch. merged is the merge time, empty for
// one closed without merging.
func pr(number int, state, merged, headOID string) string {
	return fmt.Sprintf(`{"number":%d,"state":%q,"mergedAt":%s,"headRefOid":%q}`,
		number, state, quotedOrNull(merged), headOID)
}

func quotedOrNull(s string) string {
	if s == "" {
		return "null"
	}
	return fmt.Sprintf("%q", s)
}

const mergedAt = "2026-01-01T00:00:00Z"

// cleanupFixture builds the repository /cleanup-merged is asked about, with one
// branch or worktree per situation the judgement has to tell apart. It returns
// the repository and the pull requests GitHub should answer with.
func cleanupFixture(t *testing.T) (repo string, prs map[string]string) {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	bare := filepath.Join(base, "origin.git")
	repo = filepath.Join(base, "repo")
	gittest.Init(t, bare, "--bare", "-b", "main")
	gittest.Clone(t, bare, repo)
	gittest.Run(t, repo, "commit", "-q", "--allow-empty", "-m", "init")
	gittest.Run(t, repo, "push", "-q", "-u", "origin", "main")
	gittest.Run(t, repo, "remote", "set-head", "origin", "main")
	// origin is the local bare repository the fixture pushes to; the second
	// remote is only there for the repository to have a GitHub name, which is
	// what the pull request lookups are keyed by. gh ranks it above origin.
	gittest.Run(t, repo, "remote", "add", "github", "git@github.com:owner/repo.git")

	commit := func(dir, name string) {
		gittest.Write(t, filepath.Join(dir, name), name+"\n")
		gittest.Run(t, dir, "add", name)
		gittest.Run(t, dir, "commit", "-qm", "add "+name)
	}
	branchAt := func(name, file string, push bool) {
		gittest.Run(t, repo, "switch", "-qc", name)
		commit(repo, file)
		if push {
			gittest.Run(t, repo, "push", "-q", "-u", "origin", name)
		}
		gittest.Run(t, repo, "switch", "-q", "main")
	}
	oid := func(branch string) string { return gittest.Rev(t, repo, "refs/heads/"+branch) }

	prs = map[string]string{}

	// Merged into the default branch with no pull request at all.
	branchAt("merged-nopr", "a.txt", true)
	gittest.Run(t, repo, "merge", "-q", "merged-nopr")
	gittest.Run(t, repo, "push", "-q", "origin", "main")

	// A merged pull request whose head is exactly the local branch.
	branchAt("pr-merged-br", "b.txt", true)
	prs["pr-merged-br"] = "[" + pr(123, "MERGED", mergedAt, oid("pr-merged-br")) + "]"

	// Unmerged and without a pull request: work in flight, which belongs in
	// neither list.
	branchAt("inflight", "c.txt", true)

	// A merged pull request, but with a commit that was never pushed.
	gittest.Run(t, repo, "switch", "-qc", "unpushed-br")
	commit(repo, "d.txt")
	gittest.Run(t, repo, "push", "-q", "-u", "origin", "unpushed-br")
	commit(repo, "d2.txt")
	gittest.Run(t, repo, "switch", "-q", "main")
	prs["unpushed-br"] = "[" + pr(124, "MERGED", mergedAt, oid("unpushed-br")) + "]"

	// Merged, and protected all the same.
	gittest.Run(t, repo, "branch", "-q", "develop", "main")

	// A pull request closed without merging, at exactly the local head. The
	// tag of the same name is the regression: git resolves a tag before a
	// branch, so a bare rev-parse would compare the wrong commit.
	branchAt("closedpr", "e.txt", true)
	prs["closedpr"] = "[" + pr(7, "CLOSED", "", oid("closedpr")) + "]"
	gittest.Run(t, repo, "tag", "closedpr", "main")

	// The same, with a commit made after the pull request closed.
	gittest.Run(t, repo, "switch", "-qc", "closed-local-ahead")
	commit(repo, "i.txt")
	gittest.Run(t, repo, "push", "-q", "-u", "origin", "closed-local-ahead")
	prs["closed-local-ahead"] = "[" + pr(9, "CLOSED", "", oid("closed-local-ahead")) + "]"
	commit(repo, "i2.txt")
	gittest.Run(t, repo, "switch", "-q", "main")

	// Closed unmerged, at the local head, and never pushed anywhere: the
	// unpushed checks must not fire on it or the case is unreachable.
	branchAt("closed-noup", "j.txt", false)
	prs["closed-noup"] = "[" + pr(10, "CLOSED", "", oid("closed-noup")) + "]"

	// Pushed to after its pull request merged, which no other check catches.
	gittest.Run(t, repo, "switch", "-qc", "merged-local-ahead")
	commit(repo, "n.txt")
	gittest.Run(t, repo, "push", "-q", "-u", "origin", "merged-local-ahead")
	prs["merged-local-ahead"] = "[" + pr(127, "MERGED", mergedAt, oid("merged-local-ahead")) + "]"
	commit(repo, "n2.txt")
	gittest.Run(t, repo, "push", "-q", "origin", "merged-local-ahead")
	gittest.Run(t, repo, "switch", "-q", "main")

	// Behind the head that merged, which is safe: nothing local is lost.
	gittest.Run(t, repo, "switch", "-qc", "merged-local-behind")
	commit(repo, "o.txt")
	gittest.Run(t, repo, "push", "-q", "-u", "origin", "merged-local-behind")
	commit(repo, "o2.txt")
	behind := oid("merged-local-behind")
	gittest.Run(t, repo, "reset", "-q", "--hard", "HEAD~1")
	gittest.Run(t, repo, "switch", "-q", "main")
	prs["merged-local-behind"] = "[" + pr(128, "MERGED", mergedAt, behind) + "]"

	// Closed with a merge time, which is what GitHub calls a merged pull
	// request as well: not an unmerged one.
	branchAt("closedmerged", "f.txt", true)
	prs["closedmerged"] = "[" + pr(8, "CLOSED", mergedAt, "") + "]"

	// An open pull request beside a closed one keeps the branch in flight.
	branchAt("reopened", "r.txt", true)
	prs["reopened"] = "[" + pr(30, "OPEN", "", "") + "," + pr(29, "CLOSED", "", "") + "]"

	worktree := func(name, branch string) string {
		path := filepath.Join(base, name)
		gittest.Run(t, repo, "worktree", "add", "-q", path, "-b", branch, "main")
		return path
	}

	wtMerged := worktree("wt-merged", "wt-merged")
	commit(wtMerged, "g.txt")
	gittest.Run(t, repo, "merge", "-q", "wt-merged")
	gittest.Run(t, repo, "push", "-q", "origin", "main")

	wtDirty := worktree("wt-dirty", "wt-dirty")
	gittest.Write(t, filepath.Join(wtDirty, "dirty.txt"), "dirty\n")
	prs["wt-dirty"] = "[" + pr(125, "MERGED", mergedAt, oid("wt-dirty")) + "]"

	wtNoUpstream := worktree("wt-noupstream", "wt-noupstream")
	commit(wtNoUpstream, "h.txt")
	prs["wt-noupstream"] = "[" + pr(126, "MERGED", mergedAt, oid("wt-noupstream")) + "]"

	gittest.Run(t, repo, "worktree", "add", "-q", "--detach", filepath.Join(base, "wt-detached"), "main")

	// The dirty check comes before the closed pull request's exemption, and is
	// the last guard left on the path that deletes with -D.
	wtClosedDirty := worktree("wt-closed-dirty", "wt-closed-dirty")
	commit(wtClosedDirty, "k.txt")
	prs["wt-closed-dirty"] = "[" + pr(11, "CLOSED", "", oid("wt-closed-dirty")) + "]"
	gittest.Write(t, filepath.Join(wtClosedDirty, "x.txt"), "dirty\n")

	wtClosedNoUp := worktree("wt-closed-noup", "wt-closed-noup")
	commit(wtClosedNoUp, "l.txt")
	prs["wt-closed-noup"] = "[" + pr(12, "CLOSED", "", oid("wt-closed-noup")) + "]"

	wtMergedAhead := worktree("wt-merged-ahead", "wt-merged-ahead")
	commit(wtMergedAhead, "p.txt")
	gittest.Run(t, wtMergedAhead, "push", "-q", "-u", "origin", "wt-merged-ahead")
	prs["wt-merged-ahead"] = "[" + pr(129, "MERGED", mergedAt, oid("wt-merged-ahead")) + "]"
	commit(wtMergedAhead, "p2.txt")
	gittest.Run(t, wtMergedAhead, "push", "-q", "origin", "wt-merged-ahead")

	return repo, prs
}

// candidateBranches, skippedFor and the rest read one collection the way the
// assertions want to talk about it.
func candidateBranches(c Collection) []string {
	var out []string
	for _, b := range c.Candidates.Branches {
		out = append(out, b.Branch)
	}
	return out
}

func candidateWorktrees(c Collection) []string {
	var out []string
	for _, w := range c.Candidates.Worktrees {
		out = append(out, w.Branch)
	}
	return out
}

func branchCandidate(c Collection, name string) (BranchCandidate, bool) {
	for _, b := range c.Candidates.Branches {
		if b.Branch == name {
			return b, true
		}
	}
	return BranchCandidate{}, false
}

func worktreeCandidate(c Collection, branch string) (WorktreeCandidate, bool) {
	for _, w := range c.Candidates.Worktrees {
		if w.Branch == branch {
			return w, true
		}
	}
	return WorktreeCandidate{}, false
}

func skippedFor(c Collection, target string) (Skipped, bool) {
	for _, s := range c.Skipped {
		if s.Target == target || s.Branch == target {
			return s, true
		}
	}
	return Skipped{}, false
}

func TestCollect(t *testing.T) {
	t.Parallel()

	repo, prs := cleanupFixture(t)
	got, err := Collect(t.Context(), runner.Exec{}, cleanupGitHub(t, prs, false), repo)
	if err != nil {
		t.Fatalf("Collect: %v", err)
	}
	if got.Degraded {
		t.Errorf("Degraded = true, want false; warnings were %v", got.Warnings)
	}

	t.Run("candidates", func(t *testing.T) {
		tests := []struct {
			name       string
			branch     string
			isWorktree bool
			want       Verdict
			wantDetail string
		}{
			{name: "merged with no pull request", branch: "merged-nopr", want: VerdictMergedNoPR, wantDetail: "main にマージ済み（PRなし）"},
			// The detail is compared whole: a number cut out of the wrong
			// field would put a commit hash in it.
			{name: "a merged pull request", branch: "pr-merged-br", want: VerdictPRMerged, wantDetail: "PR #123 MERGED"},
			{name: "behind the head that merged", branch: "merged-local-behind", want: VerdictPRMerged, wantDetail: "PR #128 MERGED"},
			{name: "closed without merging", branch: "closedpr", want: VerdictPRClosed, wantDetail: "PR #7 CLOSED（未マージ・PR head 一致）"},
			// Closed pull requests skip the unpushed checks, or a branch whose
			// remote is gone would never qualify.
			{name: "closed without merging and never pushed", branch: "closed-noup", want: VerdictPRClosed, wantDetail: "PR #10 CLOSED（未マージ・PR head 一致）"},
			{name: "a merged worktree", branch: "wt-merged", isWorktree: true, want: VerdictMergedNoPR},
			{name: "a closed worktree never pushed", branch: "wt-closed-noup", isWorktree: true, want: VerdictPRClosed},
		}

		for _, tc := range tests {
			t.Run(tc.name, func(t *testing.T) {
				var verdict Verdict
				var detail string
				if tc.isWorktree {
					w, ok := worktreeCandidate(got, tc.branch)
					if !ok {
						t.Fatalf("%s is not a worktree candidate; they were %v", tc.branch, candidateWorktrees(got))
					}
					verdict, detail = w.Verdict, w.Detail
				} else {
					b, ok := branchCandidate(got, tc.branch)
					if !ok {
						t.Fatalf("%s is not a branch candidate; they were %v", tc.branch, candidateBranches(got))
					}
					verdict, detail = b.Verdict, b.Detail
				}
				if verdict != tc.want {
					t.Errorf("verdict = %q, want %q", verdict, tc.want)
				}
				if tc.wantDetail != "" && detail != tc.wantDetail {
					t.Errorf("detail = %q, want %q", detail, tc.wantDetail)
				}
			})
		}
	})

	t.Run("skipped", func(t *testing.T) {
		tests := []struct {
			name         string
			target       string
			want         SkipReason
			wantDetail   string
			wantContains []string
		}{
			{name: "unpushed commits", target: "unpushed-br", want: SkipUnpushedCommits, wantDetail: "未 push commit あり"},
			{name: "an uncommitted change", target: "wt-dirty", want: SkipUncommittedChanges, wantDetail: "未コミット変更あり"},
			{name: "commits with nowhere to have pushed them", target: "wt-noupstream", want: SkipNoUpstreamWithCommits, wantDetail: "upstream 未設定 & 自前 commit あり"},
			// The dirty check runs before the closed pull request's exemption.
			{name: "an uncommitted change on a closed pull request", target: "wt-closed-dirty", want: SkipUncommittedChanges},
			{name: "commits the pull request never saw", target: "closed-local-ahead", want: SkipLocalCommitsBeyondPR, wantContains: []string{"9"}},
			{
				name: "commits pushed after the merge", target: "merged-local-ahead",
				want: SkipCommitsBeyondMergedPR, wantContains: []string{"127", "add n2.txt"},
			},
			{name: "a worktree pushed to after the merge", target: "wt-merged-ahead", want: SkipCommitsBeyondMergedPR},
		}

		for _, tc := range tests {
			t.Run(tc.name, func(t *testing.T) {
				s, ok := skippedFor(got, tc.target)
				if !ok {
					t.Fatalf("%s was not skipped; skipped were %+v", tc.target, got.Skipped)
				}
				if s.Reason != tc.want {
					t.Errorf("reason = %q, want %q", s.Reason, tc.want)
				}
				if tc.wantDetail != "" && s.Detail != tc.wantDetail {
					t.Errorf("detail = %q, want %q", s.Detail, tc.wantDetail)
				}
				for _, want := range tc.wantContains {
					if !strings.Contains(s.Detail, want) {
						t.Errorf("detail = %q, want it to mention %q", s.Detail, want)
					}
				}
				if slices.Contains(candidateBranches(got), tc.target) || slices.Contains(candidateWorktrees(got), tc.target) {
					t.Errorf("%s is both skipped and a candidate", tc.target)
				}
			})
		}
	})

	t.Run("left out entirely", func(t *testing.T) {
		tests := []struct {
			name   string
			branch string
		}{
			// Neither a candidate nor a skip: nothing says the work is over.
			{name: "unmerged with no pull request", branch: "inflight"},
			{name: "an open pull request beside a closed one", branch: "reopened"},
			// Closed with a merge time is a merged pull request, and the
			// branch was never merged into the default branch.
			{name: "closed after merging", branch: "closedmerged"},
			{name: "a protected branch", branch: "develop"},
		}

		for _, tc := range tests {
			t.Run(tc.name, func(t *testing.T) {
				if slices.Contains(candidateBranches(got), tc.branch) {
					t.Errorf("%s is a candidate", tc.branch)
				}
				if _, ok := skippedFor(got, tc.branch); ok && tc.branch != "develop" {
					t.Errorf("%s was skipped, want it left out entirely", tc.branch)
				}
			})
		}
	})

	t.Run("a closed candidate carries the head it was verified against", func(t *testing.T) {
		b, ok := branchCandidate(got, "closedpr")
		if !ok {
			t.Fatal("closedpr is not a candidate")
		}
		if want := gittest.Rev(t, repo, "refs/heads/closedpr"); b.HeadOID != want {
			t.Errorf("head_oid = %q, want %q", b.HeadOID, want)
		}
	})

	t.Run("a detached worktree is reported rather than judged", func(t *testing.T) {
		if !slices.ContainsFunc(got.Detached, func(p string) bool { return strings.HasSuffix(p, "wt-detached") }) {
			t.Errorf("detached = %v, want it to hold the detached worktree", got.Detached)
		}
	})

	t.Run("a worktree the caller is not in", func(t *testing.T) {
		w, ok := worktreeCandidate(got, "wt-merged")
		if !ok {
			t.Fatal("wt-merged is not a candidate")
		}
		if w.IsCurrent {
			t.Error("is_current = true, want false")
		}
	})
}

// TestCollectSkipsAWorktreeInUse is the guard that keeps a session's ground
// from being deleted underneath it: git would remove the worktree happily, and
// every command that process ran afterwards would fail.
func TestCollectSkipsAWorktreeInUse(t *testing.T) {
	t.Parallel()

	repo, prs := cleanupFixture(t)
	held := filepath.Join(filepath.Dir(repo), "wt-merged")
	pid := hold(t, held)

	got, err := Collect(t.Context(), runner.Exec{}, cleanupGitHub(t, prs, false), repo)
	if err != nil {
		t.Fatalf("Collect: %v", err)
	}

	s, ok := skippedFor(got, "wt-merged")
	if !ok {
		t.Fatalf("wt-merged was not skipped; skipped were %+v", got.Skipped)
	}
	if s.Reason != SkipInUseByProcess {
		t.Errorf("reason = %q, want %q", s.Reason, SkipInUseByProcess)
	}
	if want := fmt.Sprintf("(PID %d)", pid); !strings.Contains(s.Detail, want) {
		t.Errorf("detail = %q, want it to name %s", s.Detail, want)
	}
	if slices.Contains(candidateWorktrees(got), "wt-merged") {
		t.Error("a worktree in use is also a candidate")
	}
}

// hold starts a process sitting in dir and returns its process id.
func hold(t *testing.T, dir string) int {
	t.Helper()

	if _, err := exec.LookPath("lsof"); err != nil {
		t.Skip("lsof is not installed")
	}
	cmd := exec.Command("sleep", "120")
	cmd.Dir = dir
	if err := cmd.Start(); err != nil {
		t.Fatalf("start a process in %s: %v", dir, err)
	}
	t.Cleanup(func() {
		_ = cmd.Process.Kill()
		_, _ = cmd.Process.Wait()
	})
	// lsof reads /proc-equivalent state, and the child has to have got as far
	// as its own exec for its working directory to be there.
	waitForCWD(t, cmd.Process.Pid)
	return cmd.Process.Pid
}

func waitForCWD(t *testing.T, pid int) {
	t.Helper()

	for range 50 {
		out, err := exec.Command("lsof", "-a", "-d", "cwd", "-p", fmt.Sprint(pid), "-F", "n").Output()
		if err == nil && len(out) > 0 {
			return
		}
		time.Sleep(20 * time.Millisecond)
	}
	t.Fatalf("the process %d never reported a working directory", pid)
}

// TestCollectFromInsideAWorktree covers the exemption: the session asking the
// question is itself the process holding its own worktree, so the in-use check
// would otherwise stop it from ever offering to clean up where it stands.
func TestCollectFromInsideAWorktree(t *testing.T) {
	t.Parallel()

	repo, prs := cleanupFixture(t)
	inside := filepath.Join(filepath.Dir(repo), "wt-merged")

	got, err := Collect(t.Context(), runner.Exec{}, cleanupGitHub(t, prs, false), inside)
	if err != nil {
		t.Fatalf("Collect: %v", err)
	}

	w, ok := worktreeCandidate(got, "wt-merged")
	if !ok {
		t.Fatalf("wt-merged is not a candidate; they were %v", candidateWorktrees(got))
	}
	if !w.IsCurrent {
		t.Error("is_current = false, want true")
	}
	// Its branch belongs to the worktree entry, not to the branch list.
	if slices.Contains(candidateBranches(got), "wt-merged") {
		t.Error("the worktree's branch is also a branch candidate")
	}
}

// TestCollectFromTheCurrentBranch is the one branch that a worktree checkout
// does not disqualify: the main repository can switch away from its own branch
// and then delete it.
func TestCollectFromTheCurrentBranch(t *testing.T) {
	t.Parallel()

	repo, prs := cleanupFixture(t)
	gittest.Run(t, repo, "switch", "-q", "merged-nopr")

	got, err := Collect(t.Context(), runner.Exec{}, cleanupGitHub(t, prs, false), repo)
	if err != nil {
		t.Fatalf("Collect: %v", err)
	}
	b, ok := branchCandidate(got, "merged-nopr")
	if !ok {
		t.Fatalf("merged-nopr is not a candidate; they were %v", candidateBranches(got))
	}
	if !b.IsCurrent {
		t.Error("is_current = false, want true")
	}
}

// TestCollectUnderABareRepository is a regression: a bare main worktree has no
// branch line of its own, so taking the main worktree from the branch lines
// would make the first linked worktree look like it and drop it silently.
func TestCollectUnderABareRepository(t *testing.T) {
	t.Parallel()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	bare := filepath.Join(base, "bare.git")
	gittest.Init(t, bare, "--bare", "-b", "main")

	seed := gittest.Clone(t, bare, filepath.Join(base, "seed"))
	gittest.Run(t, seed, "commit", "-q", "--allow-empty", "-m", "init")
	gittest.Run(t, seed, "push", "-q", "-u", "origin", "main")

	wt := filepath.Join(base, "bare-wt")
	gittest.Run(t, bare, "worktree", "add", "-q", wt, "-b", "bare-feat", "main")

	got, err := Collect(t.Context(), runner.Exec{}, cleanupGitHub(t, nil, true), wt)
	if err != nil {
		t.Fatalf("Collect: %v", err)
	}
	w, ok := worktreeCandidate(got, "bare-feat")
	if !ok {
		t.Fatalf("bare-feat is not a candidate; they were %v", candidateWorktrees(got))
	}
	if !w.IsCurrent {
		t.Error("is_current = false, want true")
	}
	// The current-branch exception applies to the main worktree only, and a
	// bare one is never where a branch is checked out.
	if slices.Contains(candidateBranches(got), "bare-feat") {
		t.Error("the worktree's branch is also a branch candidate")
	}
}

func TestCollectOffline(t *testing.T) {
	t.Parallel()

	repo, prs := cleanupFixture(t)
	got, err := Collect(t.Context(), runner.Exec{}, cleanupGitHub(t, prs, true), repo)
	if err != nil {
		t.Fatalf("Collect: %v", err)
	}

	if !got.Degraded || len(got.Warnings) == 0 {
		t.Errorf("Collect = degraded %v with warnings %v, want the degradation reported", got.Degraded, got.Warnings)
	}
	// git still knows what merged into the default branch, which is the whole
	// of what the offline judgement is.
	b, ok := branchCandidate(got, "merged-nopr")
	if !ok {
		t.Fatalf("merged-nopr is not a candidate; they were %v", candidateBranches(got))
	}
	if want := "main にマージ済み（PRなし・オフライン判定）"; b.Detail != want {
		t.Errorf("detail = %q, want %q", b.Detail, want)
	}
	// Everything that needed a pull request to be judged is gone from the list.
	if slices.Contains(candidateBranches(got), "pr-merged-br") {
		t.Error("a branch judged only by its pull request is a candidate offline")
	}
}
