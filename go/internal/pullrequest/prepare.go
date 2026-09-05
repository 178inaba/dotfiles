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
	// one pull request.
	WorkDir     *string `json:"work_dir"`
	ReviewPath  *string `json:"review_path"`
	ThreadsPath *string `json:"threads_path"`
	// The branch to diff against, already prefixed with origin/.
	BaseBranch *string `json:"base_branch"`
	Modes      *Modes  `json:"modes"`
	// The whole freshness report, so that a caller stopping on one can say
	// what it compared.
	Freshness *worktree.FreshnessReport `json:"freshness"`
	// The issues the review checks the work against: the one --issue named, or
	// else the ones the pull request body's closing keywords point at.
	Issues []LinkedIssue `json:"issues"`
	// The degradations that did not stop the preparation: an issue
	// that could not be read, named as owner/repo#N, and anything that was
	// still cut short after the limits were raised. Empty rather than null
	// when there was nothing to report.
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
		return p.localOnly(ctx, r, dir, o), nil
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

	fetched, err := p.fetch(ctx, c, repo, pr, store, doc)
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

	base := "origin/" + fetched.PR.BaseRef
	p.BaseBranch = &base
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
		named, warnings, err := namedIssue(ctx, c, repo, o.Issue, fetched.LinkedIssues)
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
func namedIssue(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, number int, linked []LinkedIssue) ([]LinkedIssue, []string, error) {
	for _, i := range linked {
		// Only an issue in this repository: the flag is a bare number, so an
		// entry the body wrote as owner/repo#N is a different issue.
		if i.Repo == nil && i.Number == number {
			return []LinkedIssue{i}, nil, nil
		}
	}
	return readIssues(ctx, c, repo, []LinkedIssue{{Number: number}})
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
func (p Preparation) localOnly(ctx context.Context, r runner.Runner, dir string, o Options) Preparation {
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
	base := "origin/" + branch
	p.BaseBranch = &base
	p.Modes = modesFor(false, false, o)
	p.Status = "ok"
	return p
}

// fetch reads the context, and reads it once more where something was cut
// short.
//
// The rerun raises only the limits that were actually reached, each to the
// total the first attempt reported. Once, not in a loop: the totals came from
// that same answer, so a second truncation means something else is wrong and
// the caller is told rather than kept waiting.
func (p *Preparation) fetch(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, pr ghapi.PullRequest, store Store, doc Document) (Context, error) {
	fetched, err := Fetch(ctx, c, repo, pr, DefaultLimits, doc.Change)
	if err != nil {
		return Context{}, fmt.Errorf(
			"failed to fetch the pull request context while the PR exists; fix the environment issue instead of falling back to a no-PR review")
	}

	// Whether anything was cut short is answered from the value in hand, so
	// the document is stored once: writing the truncated one first would put
	// hundreds of kilobytes on disk only to replace them.
	limits, raised := raisedLimits(fetched)
	if raised {
		// The same change: what git already answered cannot have changed, and
		// rerunning it would be several fetches and a diff for nothing.
		if fetched, err = Fetch(ctx, c, repo, pr, limits, doc.Change); err != nil {
			return Context{}, fmt.Errorf("failed to fetch the pull request context on the raised-limit rerun: %v", err)
		}
	}
	if err := store(doc.Path, fetched); err != nil {
		return Context{}, err
	}
	if !raised {
		return fetched, nil
	}

	if fetched.CommentsTruncated {
		p.Warnings = append(p.Warnings, fmt.Sprintf(
			"comments still truncated after raising MAX_COMMENTS to %d; rerun `ccx pr context` with a larger MAX_COMMENTS before reading comments", limits.Comments))
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
	return fetched, nil
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
	if c.ThreadsTruncated {
		limits.Threads, raised = c.ThreadsTotalCount, true
	}
	for _, thread := range c.ReviewThreads {
		if thread.CommentsTruncated && thread.CommentsTotalCount > limits.ThreadComments {
			limits.ThreadComments, raised = thread.CommentsTotalCount, true
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
