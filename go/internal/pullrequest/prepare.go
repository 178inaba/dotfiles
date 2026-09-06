package pullrequest

import (
	"context"
	"fmt"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// Modes are the three decisions that follow from whose pull request this is.
type Modes struct {
	// The findings are posted as a review rather than acted on,
	// which is what reviewing somebody else's work means.
	Comment bool `json:"comment"`
	// This repository's own conventions apply, which they
	// do to our own work and not to a stranger's.
	PersonalRules bool `json:"personal_rules"`
	// The findings are acted on here rather than posted. Never true at the
	// same time as comment: a review either tells somebody else what it found
	// or fixes it, and doing both would post remarks about code that has
	// already changed.
	Autofix bool `json:"autofix"`
}

// Flags echo what the command was asked for, including a pull request number
// that was inferred rather than given.
type Flags struct {
	PRNumber  *int `json:"pr_number"`
	Issue     *int `json:"issue"`
	Worktree  bool `json:"worktree"`
	LocalOnly bool `json:"local_only"`
	NoAutofix bool `json:"no_autofix"`
}

// Preparation is everything a review needs settled before it starts.
//
// Most of the fields are null on a stopping status, because each is only
// established as the step that produces it succeeds.
type Preparation struct {
	// ok, branch_mismatch, or whichever freshness status stopped the
	// preparation. Everything below is null on a stopping status.
	Status string `json:"status"`
	Flags  Flags  `json:"flags"`
	// False is the ordinary degradation to a local review rather than
	// a failure. Everything else that goes wrong stops instead: confusing the
	// two would let a review of somebody else's work run with this
	// repository's own conventions and automatic fixing switched on.
	PRExists    bool    `json:"pr_exists"`
	HeadRef     *string `json:"head_ref"`
	ContextPath *string `json:"context_path"`
	// work_dir, review_path and threads_path are handed out rather than left
	// to the caller to name, which is what binds a review's working files to
	// one pull request — or, where there is no pull request, to the branch,
	// which is the only thing left to tell two runs apart. review_path and
	// threads_path are null in that state, since there is nothing to post to.
	WorkDir     *string `json:"work_dir"`
	ReviewPath  *string `json:"review_path"`
	ThreadsPath *string `json:"threads_path"`
	// The change this checkout holds that the document does not
	// carry: the commits of <base>..HEAD and the diff of <base>...HEAD, in the
	// document's own shape and with the same generated flag on its files.
	// <base> is the base branch's remote-tracking ref, or the local branch of
	// that name where there is no remote-tracking ref — warnings says which.
	// Present only where there is no pull request and where the checkout is
	// the author's own with commits not pushed yet (freshness ahead_own); null
	// otherwise. Not the document's diff, which is taken at pr.head_oid and
	// stays there.
	LocalChange *Change `json:"local_change"`
	Modes       *Modes  `json:"modes"`
	// The whole freshness report, so that a caller stopping on one can say
	// what it compared.
	Freshness *worktree.FreshnessReport `json:"freshness"`
	// The issues the review checks the work against: the one --issue named, or
	// else the ones the pull request body's closing keywords point at.
	Issues []LinkedIssue `json:"issues"`
	// The degradations that did not stop the preparation: an issue
	// that could not be read, named as owner/repo#N, anything that was still
	// cut short after the limits were raised, and a base branch the local
	// change had to be taken against locally because there was no
	// remote-tracking ref for it. Empty rather than null when there was
	// nothing to report.
	Warnings []string `json:"warnings"`
}

// Options are what the command line asked for.
type Options struct {
	// OutDir is where the document and the directory paired with it go.
	OutDir string
	// Number is the pull request, zero to infer it from the branch.
	Number int
	// Issue overrides the issues the pull request body names.
	Issue     int
	Worktree  bool
	LocalOnly bool
	NoAutofix bool
	// StateHome is where the record of the last judgment is kept, empty where
	// there is nowhere to look. Read from the environment by the command line
	// and carried here, as the fetch limits are.
	StateHome string
}

// Store writes a fetched context to the path it is given.
//
// Supplied by the caller, because turning a value into the bytes of the
// contract belongs to the command layer while the decision to fetch a second
// time with the limits raised belongs here. Where the file goes is settled by
// OpenDocument before either of them, since the document carries the path of
// the diff file that sits beside it.
type Store func(path string, c Context) error

// Prepare settles everything a review needs before it starts: which pull
// request, whether the checkout matches it, its context, its freshness, and
// which of the three modes the review runs in.
func Prepare(ctx context.Context, r runner.Runner, c *ghapi.Client, repo ghapi.Repo, dir string, o Options, store Store) (Preparation, error) {
	p := Preparation{
		Flags:    Flags{Worktree: o.Worktree, LocalOnly: o.LocalOnly, NoAutofix: o.NoAutofix},
		PRExists: true,
		Issues:   []LinkedIssue{},
		Warnings: []string{},
	}
	if o.Issue != 0 {
		p.Flags.Issue = &o.Issue
	}

	// Probed before anything is fetched, so that "there is no pull request" is
	// settled apart from "the fetch failed".
	pr, err := probe(ctx, r, c, repo, dir, o.Number)
	switch {
	case err != nil && o.Number != 0:
		return Preparation{}, err
	case err != nil:
		p.PRExists = false
	default:
		p.Flags.PRNumber = &pr.Number
		p.HeadRef = &pr.HeadRefName
	}

	if !p.PRExists {
		return p.localOnly(ctx, r, repo, dir, o)
	}

	// Only where the number was given and no worktree was resolved: the
	// inferred path matches by construction, and so does a resolved worktree.
	if o.Number != 0 && !o.Worktree {
		branch, err := runner.Git(ctx, r, dir, "rev-parse", "--abbrev-ref", "HEAD")
		if err != nil {
			return Preparation{}, err
		}
		if branch != pr.HeadRefName {
			p.Status = "branch_mismatch"
			return p, nil
		}
	}

	doc, err := OpenDocument(ctx, r, dir, o.OutDir, repo, pr)
	if err != nil {
		return Preparation{}, err
	}

	fetched, limits, err := p.fetch(ctx, c, repo, pr, store, doc, o.StateHome)
	if err != nil {
		return Preparation{}, err
	}
	p.ContextPath = &doc.Path
	p.WorkDir, p.ReviewPath, p.ThreadsPath = &doc.Work.Dir, &doc.Work.ReviewPath, &doc.Work.ThreadsPath

	// This fetches the base branch a second time, since the check fetches for
	// itself and `ccx pr freshness` calls it alone. Left as it is: the two
	// answer differently to a fetch that fails — reading the change stops the
	// run, the check reports fetch_failed — and giving the check a way to skip
	// its own fetch would put that decision in the caller of both.
	freshness, err := worktree.CheckFreshness(ctx, r, dir, fetched.Checkout())
	if err != nil {
		return Preparation{}, fmt.Errorf("the freshness check failed: %v", err)
	}
	p.Freshness = &freshness

	// Read only where the checkout runs past the document, and after the check
	// that says whether it does. The document itself is left alone: its diff
	// is taken at head_oid, and one whose head_oid and diff disagree is
	// something no reader could detect. No fallback for the base ref here —
	// reading the change has already fetched it.
	if freshness.Status == worktree.FreshnessAheadOwn {
		change, err := ReadLocalChange(ctx, r, dir, "origin/"+fetched.PR.BaseRef, doc.Work.LocalDiffPath)
		if err != nil {
			return Preparation{}, err
		}
		p.LocalChange = &change
	}
	// The reasons an issue could not be read belong here as well: this is the
	// only output the caller of prepare-review reads, and a title that came
	// back null with no word of why is unexplainable from it alone.
	p.Warnings = append(p.Warnings, fetched.Warnings...)
	p.Issues = fetched.LinkedIssues
	if o.Issue != 0 {
		// Read the way the body's own issues are, and only into what the
		// review checks against: the document keeps what the pull request
		// says it closes, which the flag does not change. Where the body
		// closes that very issue the document already has it, and reading it
		// again would be two round trips for a value in hand.
		named, warnings, err := namedIssue(ctx, c, repo, o.Issue, fetched.LinkedIssues, limits.IssueComments)
		if err != nil {
			return Preparation{}, err
		}
		p.Issues, p.Warnings = named, append(p.Warnings, warnings...)
	}
	p.Modes = modesFor(true, fetched.IsOwnPR, o)

	// The three statuses that let a review go on all mean the checkout is
	// where it should be; the rest are for the caller to resolve.
	switch freshness.Status {
	case worktree.FreshnessOK, worktree.FreshnessSynced, worktree.FreshnessAheadOwn:
		p.Status = "ok"
	default:
		p.Status = string(freshness.Status)
	}
	return p, nil
}

// namedIssue is the one --issue asked for, taken from what the pull request
// body already named where that is the same issue and read from GitHub where
// it is not.
//
// limit is the one the document was read under, raised rerun included. It is
// not raised a second time for this issue: the rerun belongs to the document,
// and an issue the body never named was no part of what it fetched twice.
func namedIssue(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, number int, linked []LinkedIssue, limit int) ([]LinkedIssue, []string, error) {
	for _, i := range linked {
		// Only an issue in this repository: the flag is a bare number, so an
		// entry the body wrote as owner/repo#N is a different issue.
		if i.Repo == nil && i.Number == number {
			return []LinkedIssue{i}, nil, nil
		}
	}
	return readIssues(ctx, c, repo, []LinkedIssue{{Number: number}}, limit)
}

// probe settles which pull request is meant without fetching anything.
func probe(ctx context.Context, r runner.Runner, c *ghapi.Client, repo ghapi.Repo, dir string, number int) (ghapi.PullRequest, error) {
	if number == 0 {
		return c.PullRequestForCurrentBranch(ctx, r, dir, repo)
	}
	pr, err := c.PullRequest(ctx, repo, number)
	if err != nil || pr.HeadRefName == "" {
		return ghapi.PullRequest{}, fmt.Errorf(
			"failed to look up PR #%d (not found, unauthenticated, or network error)", number)
	}
	return pr, nil
}

// localOnly is the degradation to reviewing against the default branch, which
// is what a branch with no pull request gets.
//
// The change itself is read here rather than left to the caller: this is the
// one state with no document at all, and a review that had to compose its own
// range would read a diff with no file list and no generated flag, which is
// the exclusion the whole reading rests on.
func (p Preparation) localOnly(ctx context.Context, r runner.Runner, repo ghapi.Repo, dir string, o Options) (Preparation, error) {
	branch := worktree.DefaultBranch(ctx, r, dir)
	if branch == "" {
		branch = "main"
	}
	// Fetched even here: a diff against a stale remote-tracking ref reports
	// changes that are already on the base branch. Being offline is no reason
	// to refuse a local review, so it is only a warning.
	if _, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"-C", dir, "fetch", "-q", "origin", branch}}); err != nil {
		p.Warnings = append(p.Warnings,
			fmt.Sprintf("git fetch origin %s failed; diff may be computed against a stale remote tracking ref", branch))
	}
	// The work dir is bound to the branch, since there is no number to bind it
	// to and a fixed name in the shared scratch directory is what a parallel
	// run on another branch writes over. A detached head has no name to bind
	// it to at all, which is the one checkout this state cannot serve.
	head, err := runner.Git(ctx, r, dir, "rev-parse", "--abbrev-ref", "HEAD")
	if err != nil {
		return Preparation{}, fmt.Errorf("failed to read the current branch in %s: %v", dir, err)
	}
	if head == "HEAD" {
		return Preparation{}, fmt.Errorf(
			"%s is on a detached head, and a review with no pull request is bound to a branch; check out a branch and run this again", dir)
	}
	work, err := EnsureBranchWorkFiles(o.OutDir, repo, head)
	if err != nil {
		return Preparation{}, err
	}
	p.WorkDir = &work.Dir

	ref, warning, err := localBase(ctx, r, dir, branch)
	if err != nil {
		return Preparation{}, err
	}
	if warning != "" {
		p.Warnings = append(p.Warnings, warning)
	}
	change, err := ReadLocalChange(ctx, r, dir, ref, work.LocalDiffPath)
	if err != nil {
		return Preparation{}, err
	}
	p.LocalChange = &change

	p.Modes = modesFor(false, false, o)
	p.Status = "ok"
	return p, nil
}

// localBase is the ref the local change is taken against, and what to say
// about it where that is not the remote-tracking one.
//
// A repository nobody has pushed has no origin/<branch> at all, and refusing
// there would be refusing the local review this whole path exists to give. The
// fallback is only for that: where the remote-tracking ref is present but
// behind, it is still what the range is taken against, so that a fetch which
// failed for being offline keeps meaning what the warning above says it means.
func localBase(ctx context.Context, r runner.Runner, dir, branch string) (ref, warning string, err error) {
	remote := "origin/" + branch
	if _, err := runner.Git(ctx, r, dir, "rev-parse", "--verify", "--quiet", remote+"^{commit}"); err == nil {
		return remote, "", nil
	}
	// refs/heads/ rather than the bare name: git resolves a tag of the same
	// name first, and a tag is not the branch the review means.
	if _, err := runner.Git(ctx, r, dir, "rev-parse", "--verify", "--quiet", "refs/heads/"+branch+"^{commit}"); err != nil {
		return "", "", fmt.Errorf(
			"neither %s nor the local branch %s is in %s, so there is nothing to diff against; fetch the base branch or name it with a pull request", remote, branch, dir)
	}
	return branch, fmt.Sprintf(
		"%s is not in this repository; the local change was taken against the local branch %s, which may be behind what has been pushed", remote, branch), nil
}

// fetch reads the context, and reads it once more where something was cut
// short.
//
// The rerun raises only the limits that were actually reached, each to the
// total the first attempt reported. Once, not in a loop: the totals came from
// that same answer, so a second truncation means something else is wrong and
// the caller is told rather than kept waiting.
//
// The limits it settled on come back with the document, because an issue read
// afterwards — the one --issue names — is read the way the document's own were.
func (p *Preparation) fetch(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, pr ghapi.PullRequest, store Store, doc Document, stateHome string) (Context, Limits, error) {
	fetched, err := Fetch(ctx, c, repo, pr, DefaultLimits, doc.Change, stateHome)
	if err != nil {
		return Context{}, Limits{}, fmt.Errorf(
			"failed to fetch the pull request context while the PR exists; fix the environment issue instead of falling back to a no-PR review")
	}

	// Whether anything was cut short is answered from the value in hand, so
	// the document is stored once: writing the truncated one first would put
	// hundreds of kilobytes on disk only to replace them.
	limits, raised := raisedLimits(fetched)
	if raised {
		// The same change: what git already answered cannot have changed, and
		// rerunning it would be several fetches and a diff for nothing.
		if fetched, err = Fetch(ctx, c, repo, pr, limits, doc.Change, stateHome); err != nil {
			return Context{}, Limits{}, fmt.Errorf("failed to fetch the pull request context on the raised-limit rerun: %v", err)
		}
	}
	if err := store(doc.Path, fetched); err != nil {
		return Context{}, Limits{}, err
	}
	if !raised {
		return fetched, limits, nil
	}

	if fetched.CommentsTruncated {
		p.Warnings = append(p.Warnings, fmt.Sprintf(
			"comments still truncated after raising MAX_COMMENTS to %d; rerun `ccx pr context` with a larger MAX_COMMENTS before reading comments", limits.Comments))
	}
	if fetched.ReviewsTruncated {
		p.Warnings = append(p.Warnings, fmt.Sprintf(
			"reviews still truncated after raising MAX_REVIEWS to %d; rerun `ccx pr context` with a larger MAX_REVIEWS before reading reviews", limits.Reviews))
	}
	if fetched.ThreadsTruncated {
		p.Warnings = append(p.Warnings, fmt.Sprintf(
			"review threads still truncated after raising MAX_THREADS to %d; rerun `ccx pr context` with a larger MAX_THREADS before reading review_threads", limits.Threads))
	}
	for _, thread := range fetched.ReviewThreads {
		if thread.CommentsTruncated {
			p.Warnings = append(p.Warnings, fmt.Sprintf(
				"thread comments still truncated after raising MAX_THREAD_COMMENTS to %d; rerun `ccx pr context` with a larger MAX_THREAD_COMMENTS before reading review_threads", limits.ThreadComments))
			break
		}
	}
	for _, issue := range fetched.LinkedIssues {
		if issue.CommentsTruncated || (issue.Parent != nil && issue.Parent.CommentsTruncated) {
			p.Warnings = append(p.Warnings, fmt.Sprintf(
				"issue comments still truncated after raising MAX_ISSUE_COMMENTS to %d; rerun `ccx pr context` with a larger MAX_ISSUE_COMMENTS before reading linked_issues", limits.IssueComments))
			break
		}
	}
	return fetched, limits, nil
}

// raisedLimits are the limits to try again with, and whether anything was cut
// short at all.
//
// The per-thread limit goes to the largest of the truncated threads' totals,
// since one limit has to cover them all.
func raisedLimits(c Context) (Limits, bool) {
	limits, raised := DefaultLimits, false
	if c.CommentsTruncated {
		limits.Comments, raised = c.CommentsTotalCount, true
	}
	if c.ReviewsTruncated {
		limits.Reviews, raised = c.ReviewsTotalCount, true
	}
	if c.ThreadsTruncated {
		limits.Threads, raised = c.ThreadsTotalCount, true
	}
	for _, thread := range c.ReviewThreads {
		if thread.CommentsTruncated && thread.CommentsTotalCount > limits.ThreadComments {
			limits.ThreadComments, raised = thread.CommentsTotalCount, true
		}
	}
	for _, issue := range c.LinkedIssues {
		if issue.CommentsTruncated && issue.CommentsTotalCount > limits.IssueComments {
			limits.IssueComments, raised = issue.CommentsTotalCount, true
		}
		// A parent is an issue for this too, and is read under the same limit.
		if p := issue.Parent; p != nil && p.CommentsTruncated && p.CommentsTotalCount > limits.IssueComments {
			limits.IssueComments, raised = p.CommentsTotalCount, true
		}
	}
	return limits, raised
}

// modesFor is the decision table.
//
// Our own work, and a local review with no pull request at all, are reviewed
// the way one reviews one's own: this repository's conventions apply and the
// findings are acted on. Somebody else's is commented on instead. The two
// flags only ever turn something off.
func modesFor(prExists, isOwn bool, o Options) *Modes {
	m := Modes{Comment: true}
	if !prExists || isOwn {
		m = Modes{PersonalRules: true, Autofix: true}
	}
	if o.LocalOnly {
		m.Comment = false
	}
	if o.NoAutofix {
		m.Autofix = false
	}
	return &m
}
