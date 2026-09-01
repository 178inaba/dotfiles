// Package prinfo keeps the pull request badge of the status line.
//
// Claude Code has a pull request badge of its own, and this one duplicates it
// deliberately: the built-in poller disables itself permanently and silently
// after a single slow fetch (anthropics/claude-code#80209), taking the pr.*
// fields of the status line payload with it. Revisit once that is fixed.
//
// Like the exchange rate, the badge is served stale while it revalidates: the
// redraw never waits on GitHub.
package prinfo

import (
	"context"
	"time"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

const (
	// maxAge matches the refresh interval of the badge this one stands in for.
	maxAge = time.Minute
	// retryInterval keeps a slow or failing lookup from being started again on
	// every redraw while the first one is still running.
	retryInterval = time.Minute
)

// State is a review state as it reaches the display, which is not GitHub's
// notion of a pull request's state — that one is open or merged or closed, and
// only decides whether a badge is shown at all.
type State string

// The review states. The first three are GitHub's own reviewDecision values;
// the last two are this package's.
const (
	StateApproved          State = "APPROVED"
	StateChangesRequested  State = "CHANGES_REQUESTED"
	StateReviewRequired    State = "REVIEW_REQUIRED"
	StateDraft             State = "DRAFT"
	StateNoReviewRequested State = "NONE"
)

// Info is a pull request worth showing. A zero Number means there is none, so
// "no pull request" is a cacheable answer rather than a missing record.
type Info struct {
	Number int    `json:"number"`
	State  State  `json:"state"`
	URL    string `json:"url"`
}

// Lookup returns the cached badge and whether the caller should start a
// refresh. A stale badge is still returned: it beats a gap while the refresh
// runs.
func Lookup(cacheDir, key string, now time.Time) (Info, bool) {
	rec, ok := cache.Read[Info](cacheDir, key)
	if ok && cache.Fresh(now, rec.At, maxAge) {
		return rec.Value, false
	}
	return rec.Value, cache.ShouldAttempt(cacheDir, now, retryInterval)
}

// Refresh asks GitHub about a branch and stores the answer.
//
// newClient is called at most once, and only where the answer needs GitHub.
// Building the client eagerly would undo what this is for: go-gh resolves its
// options in both of the clients ghapi.New builds, and for a token in the
// system keyring that resolution runs `gh auth token`, so a default branch —
// which is answered from git alone — would start two processes to learn
// nothing.
//
// A failure of any kind is cached as "no pull request" rather than reported: no
// pull request on the branch, no network, no credentials, and now a client that
// could not be built all end in the zero Info. That is a result and not an
// error, and caching it is what keeps an offline machine from trying again on
// every redraw. The error is the failure to store, which is the only way this
// can leave nothing behind — skipping the write would strand whatever badge is
// already on screen.
//
// dir is the repository the badge is about, and cacheDir is where the answer is
// filed; they are different directories and only the first is ever handed to
// git. dir is passed rather than inherited from the process because the record
// is keyed by it: an answer computed somewhere else would be filed under a
// directory it does not describe.
func Refresh(ctx context.Context, r runner.Runner, newClient func() (*ghapi.Client, error),
	cacheDir, key, branch, dir string, now time.Time,
) error {
	return cache.Write(cacheDir, key, now, badge(ctx, r, newClient, dir, branch))
}

// badge returns the pull request to show for branch, or the zero Info when
// there is none to show.
func badge(ctx context.Context, r runner.Runner, newClient func() (*ghapi.Client, error),
	dir, branch string,
) Info {
	// The default branch may be the head of a release pull request, but it is
	// not a branch-specific working context, so it is skipped before GitHub is
	// reached — and origin/HEAD answers that from git alone, which is what
	// keeps the common case free of both a request and a client. Empty is
	// "origin/HEAD says nothing", which is the case GitHub is asked about
	// below rather than an answer.
	def := worktree.DefaultBranch(ctx, r, dir)
	if def != "" && def == branch {
		return Info{}
	}

	// The remotes name the repository; ghapi.CurrentRepo would confirm the name
	// against the API, which this does not need. Nothing here renders a
	// repository name, and the one query that follows is answered for a
	// miscased or since-renamed one anyway.
	repo, err := ghapi.RemoteRepo(ctx, r, dir)
	if err != nil {
		// A repository with no usable remote fails both of the lookups below,
		// so it ends here rather than at each of them. `gh pr view` failed on
		// the same condition.
		return Info{}
	}
	c, err := newClient()
	if err != nil {
		return Info{}
	}

	if def == "" {
		// No origin/HEAD, as in a repository that gained its remote by hand.
		// When GitHub does not answer either, the badge is shown rather than
		// hidden: a badge too many beats a badge missing.
		if def, err = c.DefaultBranch(ctx, repo); err == nil && def == branch {
			return Info{}
		}
	}
	return fetch(ctx, c, r, dir, repo, branch)
}

// fetch returns the branch's pull request, or the zero Info when there is none
// to show.
func fetch(ctx context.Context, c *ghapi.Client, r runner.Runner, dir string, repo ghapi.Repo, branch string) Info {
	pr, err := c.PullRequestForBranch(ctx, r, dir, repo, branch)
	if err != nil {
		return Info{}
	}
	// A merged or closed pull request is not the current work.
	if pr.State != ghapi.StateOpen {
		return Info{}
	}
	return Info{Number: pr.Number, State: state(pr.IsDraft, pr.ReviewDecision), URL: pr.URL}
}

func state(isDraft bool, reviewDecision string) State {
	switch {
	case isDraft:
		return StateDraft
	case reviewDecision == "":
		return StateNoReviewRequested
	default:
		return State(reviewDecision)
	}
}
