package pullrequest

import (
	"context"
	"fmt"
	"time"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// readyRetryWait is how long Ready.Run waits before it re-reads a pull
// request whose only failing check is the author's own unpushed-looking
// commits.
const readyRetryWait = 3 * time.Second

// ReadyStatus is what came of the request to mark a pull request ready.
//
// One of the eight writes anything: ready calls the mutation, and every other
// value — already_ready included — leaves the pull request exactly as it was.
type ReadyStatus string

const (
	// ReadyMarked is a pull request this run took out of draft.
	ReadyMarked ReadyStatus = "ready"
	// ReadyAlready is one that was not a draft to begin with, which is a
	// success with nothing written.
	ReadyAlready ReadyStatus = "already_ready"
	// ReadyDirty is a working tree with something uncommitted in it,
	// untracked files included: a fix that exists only locally, which is
	// exactly what a pull request must not be declared ready over.
	ReadyDirty ReadyStatus = "dirty"
	// ReadyAheadOwn is the author's own checkout with commits not pushed yet,
	// still true after the one re-read this command allows it.
	ReadyAheadOwn ReadyStatus = "ahead_own"
	// ReadyBehind is a checkout the pull request's head is ahead of, left
	// exactly where it was: this command never moves anything.
	ReadyBehind ReadyStatus = "behind"
	// ReadyDiverged is a checkout with commits the pull request's head has
	// never seen.
	ReadyDiverged ReadyStatus = "diverged"
	// ReadyBranchMismatch is a checkout of something else entirely, a
	// detached head included.
	ReadyBranchMismatch ReadyStatus = "branch_mismatch"
	// ReadyFetchFailed most often means the head branch is not on origin at
	// all, which is what a pull request from a fork looks like from here.
	ReadyFetchFailed ReadyStatus = "fetch_failed"
)

// ReadyReport is the answer, with the pull request and the two commits that
// produced it, in the names `ccx pr freshness` uses for them.
type ReadyReport struct {
	Status  ReadyStatus `json:"status"`
	Number  int         `json:"number"`
	HeadRef string      `json:"head_ref"`
	HeadOID string      `json:"head_oid"`
	// The checkout's own head, read after every check. Always where the run
	// found it, since this command never moves the checkout.
	LocalHead string `json:"local_head"`
}

// Ready marks a pull request ready once the checkout here matches it.
type Ready struct {
	Client *ghapi.Client
	Runner runner.Runner
	Repo   ghapi.Repo
	Dir    string
	// Wait is the one pause before a lagging head is read again, nil for the
	// production default of a few seconds. Injected so the tests do not
	// sleep.
	Wait func()
}

// Run marks pr ready for review once the checkout in rd.Dir has nothing
// uncommitted and is at pr's head, stopping at the first check that fails.
//
// The checkout is never moved: a behind checkout is reported as behind rather
// than fast-forwarded, which is what sets this apart from CheckFreshness. When
// the only failing check is the author's own unpushed-looking commits
// (ahead_own), pr is re-read once after rd.Wait and, if its head moved, the
// comparison runs again before this decides; every other answer is final on
// the first read.
func (rd Ready) Run(ctx context.Context, pr ghapi.PullRequest) (ReadyReport, error) {
	wait := rd.Wait
	if wait == nil {
		wait = func() { time.Sleep(readyRetryWait) }
	}

	report := func(status ReadyStatus) (ReadyReport, error) {
		local, err := worktree.Head(ctx, rd.Runner, rd.Dir)
		if err != nil {
			return ReadyReport{}, err
		}
		return ReadyReport{
			Status: status, Number: pr.Number, HeadRef: pr.HeadRefName, HeadOID: pr.HeadRefOid, LocalHead: local,
		}, nil
	}

	clean, err := worktree.IsClean(ctx, rd.Runner, rd.Dir)
	if err != nil {
		return ReadyReport{}, err
	}
	if !clean {
		return report(ReadyDirty)
	}

	c, err := worktree.Compare(ctx, rd.Runner, rd.Dir, checkout(pr))
	if err != nil {
		return ReadyReport{}, err
	}
	if c == worktree.ComparisonAheadOwn {
		wait()
		lagging := pr.HeadRefOid
		if pr, err = rd.Client.PullRequest(ctx, rd.Repo, pr.Number); err != nil {
			return ReadyReport{}, err
		}
		// A head that has not moved gives the same answer against the same
		// checkout, so only a moved one is worth another fetch.
		if pr.HeadRefOid != lagging {
			if c, err = worktree.Compare(ctx, rd.Runner, rd.Dir, checkout(pr)); err != nil {
				return ReadyReport{}, err
			}
		}
	}

	switch c {
	case worktree.ComparisonOK:
		// Falls through to the draft check below.
	case worktree.ComparisonAheadOwn:
		return report(ReadyAheadOwn)
	case worktree.ComparisonBehind:
		return report(ReadyBehind)
	case worktree.ComparisonDiverged:
		return report(ReadyDiverged)
	case worktree.ComparisonBranchMismatch:
		return report(ReadyBranchMismatch)
	case worktree.ComparisonFetchFailed:
		return report(ReadyFetchFailed)
	default:
		return ReadyReport{}, fmt.Errorf("pullrequest: unhandled comparison %q", c)
	}

	if !pr.IsDraft {
		return report(ReadyAlready)
	}
	if err := rd.Client.MarkPullRequestReadyForReview(ctx, pr.ID); err != nil {
		return ReadyReport{}, err
	}
	return report(ReadyMarked)
}

// checkout is what worktree.Compare needs out of pr, the ghapi.PullRequest
// counterpart of Context.Checkout.
func checkout(pr ghapi.PullRequest) worktree.PullRequest {
	return worktree.PullRequest{
		HeadRef: pr.HeadRefName, HeadOID: pr.HeadRefOid, BaseRef: pr.BaseRefName, IsOwnPR: pr.IsOwn,
	}
}
