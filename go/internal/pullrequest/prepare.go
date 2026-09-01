package pullrequest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// Modes are the three decisions that follow from whose pull request this is.
type Modes struct {
	// Comment says the findings are posted as a review rather than acted on,
	// which is what reviewing somebody else's work means.
	Comment bool `json:"comment"`
	// PersonalRules says this repository's own conventions apply, which they
	// do to our own work and not to a stranger's.
	PersonalRules bool `json:"personal_rules"`
	Autofix       bool `json:"autofix"`
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
	// Status is ok, branch_mismatch, or whichever freshness status stopped it.
	Status string `json:"status"`
	Flags  Flags  `json:"flags"`
	// PRExists false is the ordinary degradation to a local review rather than
	// a failure. Everything else that goes wrong stops instead: confusing the
	// two would let a review of somebody else's work run with this
	// repository's own conventions and automatic fixing switched on.
	PRExists    bool    `json:"pr_exists"`
	HeadRef     *string `json:"head_ref"`
	ContextPath *string `json:"context_path"`
	// WorkDir, ReviewPath and ThreadsPath are handed out rather than left to
	// the prompt; the binding is in reviewdir.go.
	WorkDir     *string `json:"work_dir"`
	ReviewPath  *string `json:"review_path"`
	ThreadsPath *string `json:"threads_path"`
	BaseBranch  *string `json:"base_branch"`
	Modes       *Modes  `json:"modes"`
	// Freshness is the whole report, so that a caller stopping on one can say
	// what it compared.
	Freshness *worktree.FreshnessReport `json:"freshness"`
	Issues    []LinkedIssue             `json:"issues"`
	Warnings  []string                  `json:"warnings"`
}

// Options are what the command line asked for.
type Options struct {
	// Number is the pull request, zero to infer it from the branch.
	Number int
	// Issue overrides the issues the pull request body names.
	Issue     int
	Worktree  bool
	LocalOnly bool
	NoAutofix bool
}

// Store writes a fetched context and returns the path it took.
//
// Supplied by the caller, because turning a value into the bytes of the
// contract belongs to the command layer while the decision to fetch a second
// time with the limits raised belongs here.
type Store func(Context) (string, error)

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

	fetched, path, err := p.fetch(ctx, c, repo, pr, store)
	if err != nil {
		return Preparation{}, err
	}
	p.ContextPath = &path

	work := WorkDir(path)
	if err := os.MkdirAll(work, 0o755); err != nil {
		return Preparation{}, fmt.Errorf("failed to create review work dir: %s", work)
	}
	review, threads := filepath.Join(work, "review.json"), filepath.Join(work, "threads.json")
	p.WorkDir, p.ReviewPath, p.ThreadsPath = &work, &review, &threads

	freshness, err := worktree.CheckFreshness(ctx, r, dir, worktree.PullRequest{
		HeadRef: fetched.PR.HeadRef, HeadOID: fetched.PR.HeadOID,
		BaseRef: fetched.PR.BaseRef, IsOwnPR: fetched.IsOwnPR,
	})
	if err != nil {
		return Preparation{}, fmt.Errorf("the freshness check failed: %v", err)
	}
	p.Freshness = &freshness

	base := "origin/" + fetched.PR.BaseRef
	p.BaseBranch = &base
	if o.Issue != 0 {
		p.Issues = []LinkedIssue{{Number: o.Issue}}
	} else if fetched.LinkedIssues != nil {
		p.Issues = fetched.LinkedIssues
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
func (p *Preparation) fetch(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, pr ghapi.PullRequest, store Store) (Context, string, error) {
	fetched, err := Fetch(ctx, c, repo, pr, DefaultLimits)
	if err != nil {
		return Context{}, "", fmt.Errorf(
			"failed to fetch the pull request context while the PR exists; fix the environment issue instead of falling back to a no-PR review")
	}

	// Whether anything was cut short is answered from the value in hand, so
	// the document is stored once: writing the truncated one first would put
	// hundreds of kilobytes on disk only to replace them.
	limits, raised := raisedLimits(fetched)
	if raised {
		if fetched, err = Fetch(ctx, c, repo, pr, limits); err != nil {
			return Context{}, "", fmt.Errorf("failed to fetch the pull request context on the raised-limit rerun: %v", err)
		}
	}
	path, err := store(fetched)
	if err != nil {
		return Context{}, "", err
	}
	if !raised {
		return fetched, path, nil
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
	return fetched, path, nil
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
