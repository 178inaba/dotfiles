package worktree

import (
	"context"
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Verdict is why a worktree or a branch is finished with.
type Verdict string

const (
	// VerdictPRMerged is a pull request that merged, with the local branch at
	// or behind the head that merged.
	VerdictPRMerged Verdict = "pr_merged"
	// VerdictMergedNoPR is a branch git says is merged into the default one,
	// which is what a branch merged locally or squashed without a pull request
	// looks like.
	VerdictMergedNoPR Verdict = "merged_no_pr"
	// VerdictPRClosed is a pull request closed without merging, whose head
	// matches the local branch exactly — GitHub keeps refs/pull/N/head, so
	// `gh pr checkout N` brings it all back.
	VerdictPRClosed Verdict = "pr_closed"
)

// TargetKind says whether an entry is about a worktree or a bare branch.
type TargetKind string

const (
	// KindWorktree is a checkout with a directory of its own, which has to be
	// left before it can be removed.
	KindWorktree TargetKind = "worktree"
	// KindBranch is a branch with no worktree, which needs no evacuation.
	KindBranch TargetKind = "branch"
)

// SkipReason is why something that looked finished is being left alone.
type SkipReason string

const (
	// SkipUncommittedChanges is work that exists nowhere but this checkout.
	SkipUncommittedChanges SkipReason = "uncommitted_changes"
	// SkipUnpushedCommits is work that exists nowhere but this clone.
	SkipUnpushedCommits SkipReason = "unpushed_commits"
	// SkipNoUpstreamWithCommits is a branch with commits and nowhere to have
	// pushed them, which the unpushed check cannot see because it needs an
	// upstream to compare against.
	SkipNoUpstreamWithCommits SkipReason = "no_upstream_with_commits"
	// SkipCommitsBeyondMergedPR is a branch pushed to after its pull request
	// merged. Nothing else catches it: the commits are on the remote, so every
	// unpushed check passes and `git branch -d` agrees to delete.
	SkipCommitsBeyondMergedPR SkipReason = "commits_beyond_merged_pr"
	// SkipLocalCommitsBeyondPR is the same as SkipCommitsBeyondMergedPR for
	// commits that were never pushed at all.
	SkipLocalCommitsBeyondPR SkipReason = "local_commits_beyond_pr"
	// SkipInUseByProcess is a worktree some process is still standing in.
	SkipInUseByProcess SkipReason = "in_use_by_process"
)

// Candidate is a worktree that can be removed. A branch with no worktree of
// its own is a BranchCandidate instead.
type Candidate struct {
	Path    string  `json:"path"`
	Branch  string  `json:"branch"`
	Verdict Verdict `json:"verdict"`
	// Detail is for a person reading the list, and is written in the language
	// the rest of this skill speaks.
	Detail string `json:"detail"`
	// IsCurrent marks the worktree the caller is standing in, which has to be
	// left before it can be removed.
	IsCurrent bool `json:"is_current"`
	// HeadOID is set only for VerdictPRClosed, and is the head the deletion
	// checks again before it uses the flag that skips git's own safety net.
	HeadOID string `json:"head_oid"`
}

// BranchCandidate is a branch with no worktree of its own.
type BranchCandidate struct {
	Branch    string  `json:"branch"`
	Verdict   Verdict `json:"verdict"`
	Detail    string  `json:"detail"`
	IsCurrent bool    `json:"is_current"`
	HeadOID   string  `json:"head_oid"`
}

// Skipped is something that was judged finished and is being left alone anyway.
type Skipped struct {
	Type   TargetKind `json:"type"`
	Target string     `json:"target"`
	// Branch is absent for a bare branch, whose name is already the target.
	Branch string     `json:"branch,omitzero"`
	Reason SkipReason `json:"reason"`
	Detail string     `json:"detail"`
}

// Candidates is the two lists of what may be deleted, and is also what Delete
// takes back.
type Candidates struct {
	Worktrees []Candidate       `json:"worktrees"`
	Branches  []BranchCandidate `json:"branches"`
}

// Collection is the whole answer to "what is finished with here".
type Collection struct {
	// Degraded says GitHub could not be reached, so the judgement fell back to
	// what git alone knows and no pull request was consulted.
	Degraded        bool       `json:"degraded"`
	DefaultBranch   string     `json:"default_branch"`
	CurrentWorktree string     `json:"current_worktree"`
	Candidates      Candidates `json:"candidates"`
	Skipped         []Skipped  `json:"skipped"`
	// Detached worktrees are reported rather than judged: with no branch there
	// is nothing to compare against a pull request.
	Detached []string `json:"detached"`
	Warnings []string `json:"warnings"`
}

// protectedBranches are never candidates however merged they look.
var protectedBranches = []string{"main", "master", "develop"}

// Collect finds the worktrees and branches whose work is finished.
//
// It deletes nothing. The list goes to a person for approval first, and Delete
// takes back whatever survives that — which is why the two are separate
// commands rather than one.
func Collect(ctx context.Context, r runner.Runner, c *ghapi.Client, dir string) (Collection, error) {
	col := &collector{r: r, c: c, dir: dir}
	return col.collect(ctx)
}

type collector struct {
	r    runner.Runner
	c    *ghapi.Client
	dir  string
	repo ghapi.Repo

	defaultBranch string
	// mergedInto is the branches git considers merged into the comparison ref.
	mergedInto map[string]bool
	table      cwdTable

	degraded bool
	warnings []string
}

func (c *collector) warn(format string, a ...any) {
	c.warnings = append(c.warnings, fmt.Sprintf(format, a...))
}

func (c *collector) git(ctx context.Context, args ...string) (string, error) {
	return runner.Git(ctx, c.r, c.dir, args...)
}

func (c *collector) collect(ctx context.Context) (Collection, error) {
	if _, err := c.git(ctx, "rev-parse", "--git-dir"); err != nil {
		return Collection{}, fmt.Errorf("not a git repository")
	}

	// main where a repository has no origin/HEAD at all — the same fallback
	// the skill's prose has always assumed.
	c.defaultBranch = DefaultBranch(ctx, c.r, c.dir)
	if c.defaultBranch == "" {
		c.defaultBranch = "main"
	}
	if _, err := c.git(ctx, "fetch", "origin", c.defaultBranch); err != nil {
		c.warn("git fetch origin %s に失敗（ローカル %s が stale の可能性あり）", c.defaultBranch, c.defaultBranch)
	}

	currentWorktree, err := c.git(ctx, "rev-parse", "--show-toplevel")
	if err != nil {
		return Collection{}, fmt.Errorf("not a git repository")
	}
	currentBranch, _ := c.git(ctx, "branch", "--show-current")

	// The remote's copy decides what is merged, because a local default branch
	// that has not been pulled would call nothing merged.
	mergedBase := "origin/" + c.defaultBranch
	if _, err := c.git(ctx, "rev-parse", "--verify", "--quiet", mergedBase); err != nil {
		mergedBase = c.defaultBranch
		c.warn("origin/%s が存在しないためローカル %s で判定", c.defaultBranch, c.defaultBranch)
	}
	// lstrip=2 rather than the short name: a tag of the same name makes git
	// disambiguate the short one into heads/<name>, which is not a branch name.
	merged, _ := c.git(ctx, "branch", "--merged", mergedBase, "--format=%(refname:lstrip=2)")
	c.mergedInto = lineSet(merged)

	if c.repo, err = c.c.CurrentRepo(ctx, c.r, c.dir); err != nil {
		c.degraded = true
		c.warn("gh が利用できないためオフライン判定（PR 情報なし）")
	}

	entries, err := List(ctx, c.r, c.dir)
	if err != nil {
		return Collection{}, err
	}
	if c.table, err = loadCWDTable(ctx, c.r); err != nil {
		return Collection{}, err
	}

	out := Collection{
		DefaultBranch:   c.defaultBranch,
		CurrentWorktree: currentWorktree,
		Candidates:      Candidates{Worktrees: []Candidate{}, Branches: []BranchCandidate{}},
		Skipped:         []Skipped{},
		Detached:        []string{},
	}

	// The first entry is the main worktree even where it is bare and has no
	// branch line of its own; taking it from the branch lines instead would
	// make the first linked worktree look like the main one and drop it.
	main := entries[0].Path
	checkedOut := make(map[string]bool)
	for _, e := range entries {
		if e.Branch != "" {
			checkedOut[e.Branch] = true
		}
		if e.Path == main {
			continue
		}
		if e.Branch == "" {
			out.Detached = append(out.Detached, e.Path)
			continue
		}

		j := c.judge(ctx, e.Branch)
		if j.skip != "" {
			out.Skipped = append(out.Skipped, Skipped{
				Type: KindWorktree, Target: e.Path, Branch: e.Branch, Reason: j.skip, Detail: j.detail,
			})
			continue
		}
		if j.verdict == "" {
			continue
		}

		isCurrent := e.Path == currentWorktree
		// Untracked files count here and nowhere else in this package: this is
		// about to delete the directory they are in, not fast-forward past
		// them. It is also the last dirty guard on the path that deletes with
		// -D, so it comes before the closed pull request's exemption.
		var reason SkipReason
		var detail string
		if status, _ := runner.Git(ctx, c.r, e.Path, "status", "--porcelain"); status != "" {
			reason, detail = SkipUncommittedChanges, skipDetails[SkipUncommittedChanges]
		} else {
			reason, detail = c.safety(ctx, e.Path, "HEAD", j.verdict)
		}
		// The caller's own worktree is exempt from the in-use check, since the
		// session asking the question is the process holding it. The procedure
		// is to leave first, and Delete checks again at the moment of removal.
		if reason == "" && !isCurrent {
			if holders := c.table.holders(e.Path); holders != "" {
				reason, detail = SkipInUseByProcess, "使用中のプロセスあり: "+holders
			}
		}
		if reason != "" {
			out.Skipped = append(out.Skipped, Skipped{
				Type: KindWorktree, Target: e.Path, Branch: e.Branch, Reason: reason, Detail: detail,
			})
			continue
		}
		out.Candidates.Worktrees = append(out.Candidates.Worktrees, Candidate{
			Path: e.Path, Branch: e.Branch, Verdict: j.verdict, Detail: j.detail,
			IsCurrent: isCurrent, HeadOID: j.headOID,
		})
	}

	branches, _ := c.git(ctx, "branch", "--format=%(refname:lstrip=2)")
	for branch := range strings.SplitSeq(branches, "\n") {
		if branch == "" || c.protected(branch) {
			continue
		}
		// A branch checked out somewhere is handled as that worktree's, or
		// refused by git; the one exception is the main worktree's own current
		// branch, which can be deleted after switching away from it.
		if checkedOut[branch] && (branch != currentBranch || currentWorktree != main) {
			continue
		}

		j := c.judge(ctx, branch)
		if j.skip != "" {
			out.Skipped = append(out.Skipped, Skipped{
				Type: KindBranch, Target: branch, Reason: j.skip, Detail: j.detail,
			})
			continue
		}
		if j.verdict == "" {
			continue
		}
		if reason, detail := c.safety(ctx, c.dir, branch, j.verdict); reason != "" {
			out.Skipped = append(out.Skipped, Skipped{
				Type: KindBranch, Target: branch, Reason: reason, Detail: detail,
			})
			continue
		}
		out.Candidates.Branches = append(out.Candidates.Branches, BranchCandidate{
			Branch: branch, Verdict: j.verdict, Detail: j.detail,
			IsCurrent: branch == currentBranch, HeadOID: j.headOID,
		})
	}

	out.Degraded = c.degraded
	out.Warnings = c.warnings
	return out, nil
}

func (c *collector) protected(branch string) bool {
	return slices.Contains(protectedBranches, branch) || branch == c.defaultBranch
}

// judgement is what the pull requests of one branch say about it. An empty
// verdict with no skip means work still in flight, which belongs in neither
// list.
type judgement struct {
	verdict Verdict
	skip    SkipReason
	// detail belongs to whichever of the two above is set; skip is what says
	// which, and they are never both filled.
	detail  string
	headOID string
}

func (c *collector) judge(ctx context.Context, branch string) judgement {
	if !c.degraded {
		prs, err := c.pullRequests(ctx, branch)
		if err == nil {
			return c.judgeWithPRs(ctx, branch, prs)
		}
		c.degraded = true
		c.warn("gh pr list が失敗したためオフライン判定に切替（branch: %s 以降）", branch)
	}
	if c.mergedInto[branch] {
		return judgement{
			verdict: VerdictMergedNoPR,
			detail:  fmt.Sprintf("%s にマージ済み（PRなし・オフライン判定）", c.defaultBranch),
		}
	}
	return judgement{}
}

func (c *collector) judgeWithPRs(ctx context.Context, branch string, prs []branchPR) judgement {
	// An open pull request wins over anything else the branch has ever had:
	// work in progress is not a candidate however many old pull requests
	// merged or closed beside it.
	for _, pr := range prs {
		if pr.State == ghapi.StateOpen {
			return judgement{}
		}
	}
	for _, pr := range prs {
		if pr.State == ghapi.StateMerged {
			return c.judgeMerged(ctx, branch, pr)
		}
	}
	if len(prs) == 0 {
		if c.mergedInto[branch] {
			return judgement{
				verdict: VerdictMergedNoPR,
				detail:  fmt.Sprintf("%s にマージ済み（PRなし）", c.defaultBranch),
			}
		}
		return judgement{}
	}
	for _, pr := range prs {
		// GitHub calls a merged pull request closed as well, so the unmerged
		// ones are the ones with no merge time.
		if pr.State == ghapi.StateClosed && pr.MergedAt == "" {
			return c.judgeClosed(ctx, branch, pr)
		}
	}
	return judgement{}
}

// maxBeyond is how many commits the display detail names before rounding the
// rest into a count.
const maxBeyond = 5

// judgeMerged decides about a branch whose pull request merged.
//
// The local branch has to be at the merged head or behind it. A commit pushed
// to the branch after the merge is on the remote, so every unpushed check
// passes and `git branch -d` agrees — this comparison is the only thing
// standing between that commit and being the last copy nobody has.
func (c *collector) judgeMerged(ctx context.Context, branch string, pr branchPR) judgement {
	// refs/heads/ spelled out: git resolves a tag before a branch of the same
	// name, and the tag's commit would be compared instead.
	if pr.HeadRefOID != "" && isAncestor(ctx, c.r, c.dir, "refs/heads/"+branch, pr.HeadRefOID) {
		return judgement{verdict: VerdictPRMerged, detail: fmt.Sprintf("PR #%d MERGED", pr.Number)}
	}

	j := judgement{skip: SkipCommitsBeyondMergedPR}
	if pr.HeadRefOID == "" {
		j.detail = fmt.Sprintf("PR #%d MERGED だがマージされた head (不明) がローカルに存在しない", pr.Number)
		return j
	}
	if _, err := c.git(ctx, "rev-parse", "--verify", "--quiet", pr.HeadRefOID+"^{commit}"); err != nil {
		j.detail = fmt.Sprintf("PR #%d MERGED だがマージされた head (%s) がローカルに存在しない", pr.Number, pr.HeadRefOID)
		return j
	}

	span := pr.HeadRefOID + "..refs/heads/" + branch
	count, _ := c.git(ctx, "rev-list", "--count", span)
	log, _ := c.git(ctx, "log", "--oneline", "-n", strconv.Itoa(maxBeyond), span)
	beyond := strings.ReplaceAll(log, "\n", ", ")
	if n, err := strconv.Atoi(count); err == nil && n > maxBeyond {
		beyond += fmt.Sprintf(", 他 %d 件", n-maxBeyond)
	}
	j.detail = fmt.Sprintf("PR #%d MERGED だがマージされた head より先の commit あり: %s", pr.Number, beyond)
	return j
}

// judgeClosed decides about a branch whose pull request was closed unmerged.
//
// Deleting one needs `git branch -D`, where git's own merge check does not
// apply, so the check is that the local head is exactly the pull request's: at
// that point GitHub still holds refs/pull/N/head and nothing is lost. Anything
// else means there are commits the pull request never saw.
func (c *collector) judgeClosed(ctx context.Context, branch string, pr branchPR) judgement {
	local, _ := c.git(ctx, "rev-parse", "refs/heads/"+branch)
	if pr.HeadRefOID != "" && local == pr.HeadRefOID {
		return judgement{
			verdict: VerdictPRClosed,
			detail:  fmt.Sprintf("PR #%d CLOSED（未マージ・PR head 一致）", pr.Number),
			headOID: local,
		}
	}
	return judgement{
		skip:   SkipLocalCommitsBeyondPR,
		detail: fmt.Sprintf("PR #%d CLOSED（未マージ）だが PR head と不一致（ローカル限定 commit あり）", pr.Number),
	}
}

// safety is the last look before something becomes a candidate: are there
// commits here that deleting it would be the end of.
//
// dir is where git runs and rev is what it is asked about — a worktree asks
// about its own HEAD, a bare branch about itself from the repository. Keeping
// them one procedure is what stops a fix landing on the branch path and not
// the worktree path.
func (c *collector) safety(ctx context.Context, dir, rev string, verdict Verdict) (SkipReason, string) {
	// A closed pull request has already been checked against its head, which
	// is the same worry the unpushed checks have — and its remote branch is
	// usually gone, so no_upstream_with_commits would fire on every one of
	// them and the case would never be reachable.
	if verdict == VerdictPRClosed {
		return "", ""
	}
	if log, _ := runner.Git(ctx, c.r, dir, "log", rev+"@{u}.."+rev, "--oneline"); log != "" {
		return SkipUnpushedCommits, skipDetails[SkipUnpushedCommits]
	}
	// With no upstream the check above has nothing to compare against and
	// passes in silence, so a branch that was never pushed needs the default
	// branch as its yardstick instead.
	if _, err := runner.Git(ctx, c.r, dir, "rev-parse", "--abbrev-ref", rev+"@{u}"); err != nil {
		if log, _ := runner.Git(ctx, c.r, dir, "log", c.defaultBranch+".."+rev, "--oneline"); log != "" {
			return SkipNoUpstreamWithCommits, skipDetails[SkipNoUpstreamWithCommits]
		}
	}
	return "", ""
}

// skipDetails are what the reasons look like in the list a person reads.
var skipDetails = map[SkipReason]string{
	SkipUncommittedChanges:    "未コミット変更あり",
	SkipUnpushedCommits:       "未 push commit あり",
	SkipNoUpstreamWithCommits: "upstream 未設定 & 自前 commit あり",
}

// branchPR is a pull request of one branch, as much of it as the judgement
// needs.
type branchPR struct {
	Number int           `json:"number"`
	State  ghapi.PRState `json:"state"`
	// MergedAt is empty for a pull request that closed without merging, which
	// is the only way to tell those apart: GitHub reports both as closed.
	MergedAt   string `json:"mergedAt"`
	HeadRefOID string `json:"headRefOid"`
}

// branchPRsQuery is `gh pr list --head <branch> --state all --limit 20`.
//
// GraphQL rather than REST, whose head filter wants owner:ref and misses a
// fork's, and which applies its limit at a different point.
const branchPRsQuery = `
query($owner: String!, $name: String!, $headRefName: String!) {
  repository(owner: $owner, name: $name) {
    pullRequests(headRefName: $headRefName, first: 20, orderBy: {field: CREATED_AT, direction: DESC}) {
      nodes {
        number
        state
        mergedAt
        headRefOid
      }
    }
  }
}`

func (c *collector) pullRequests(ctx context.Context, branch string) ([]branchPR, error) {
	var out struct {
		Repository struct {
			PullRequests struct {
				Nodes []branchPR `json:"nodes"`
			} `json:"pullRequests"`
		} `json:"repository"`
	}
	vars := map[string]any{"owner": c.repo.Owner, "name": c.repo.Name, "headRefName": branch}
	if err := c.c.GraphQL(ctx, branchPRsQuery, vars, &out); err != nil {
		return nil, err
	}
	return out.Repository.PullRequests.Nodes, nil
}

// lineSet reads a list of branch names into a set. A name may hold anything but
// a newline, so matching whole lines is exact.
func lineSet(out string) map[string]bool {
	set := make(map[string]bool)
	for line := range strings.SplitSeq(out, "\n") {
		if line != "" {
			set[line] = true
		}
	}
	return set
}
