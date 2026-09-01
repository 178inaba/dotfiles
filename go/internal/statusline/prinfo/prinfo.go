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
// newClient is called at most once, and only where the answer needs GitHub:
// ghapi.New resolves go-gh's options twice, and for a token in the system
// keyring that resolution execs `gh auth token`, which a default branch —
// answered from git alone — must not pay.
//
// Every failure is cached as "no pull request" rather than reported, which is
// what keeps an offline machine from asking again on every redraw. The one
// returned error is the failure to store: skipping the write would strand
// whatever badge is already on screen.
//
// dir is passed rather than inherited from the process because the record is
// keyed by it — an answer computed elsewhere would be filed under a directory
// it does not describe.
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
	// keeps the common case free of both a request and a client.
	def := worktree.DefaultBranch(ctx, r, dir)
	if def != "" && def == branch {
		return Info{}
	}

	// Not ghapi.CurrentRepo: nothing here renders a repository name, and the
	// one query that follows is answered for a miscased or since-renamed one
	// anyway, so its canonicalising round trip buys nothing.
	repo, err := ghapi.RemoteRepo(ctx, r, dir)
	if err != nil {
		// `gh pr view` failed on this condition too.
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
