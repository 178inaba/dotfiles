package worktree

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/gitlock"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Freshness is how a local checkout stands against a pull request's head.
//
// Three of the seven allow the caller to go on; the rest are stops. Working
// from a stale checkout is what produces a review of the wrong diff, or a
// comment posted at a line number that has moved — which GitHub rejects with a
// 422 and which nobody notices until the review is missing.
type Freshness string

const (
	// FreshnessOK is a checkout already at the pull request's head.
	FreshnessOK Freshness = "ok"
	// FreshnessSynced is the same checkout after this command moved it there.
	FreshnessSynced Freshness = "synced"
	// FreshnessAheadOwn is the author's own checkout with commits not pushed
	// yet, which is ordinary rather than stale.
	FreshnessAheadOwn Freshness = "ahead_own"
	// FreshnessBehindDirty is a stale checkout with uncommitted changes in it,
	// which is for a person to resolve rather than for this command.
	FreshnessBehindDirty Freshness = "behind_dirty"
	// FreshnessDiverged is the same, with commits the remote has never seen.
	FreshnessDiverged Freshness = "diverged"
	// FreshnessBranchMismatch is a checkout of something else entirely, a
	// detached head included.
	FreshnessBranchMismatch Freshness = "branch_mismatch"
	// FreshnessFetchFailed is a fetch of the pull request's head or of its
	// base branch from origin that failed, which is to say the network or
	// origin itself: every pull request's head is on origin, a fork's
	// included.
	FreshnessFetchFailed Freshness = "fetch_failed"
)

// Comparison is where a checkout stands against a pull request's head, before
// anything has been done about it.
//
// Its own vocabulary rather than Freshness, because the two questions have
// different answers: this one can say behind, which Freshness never does — the
// command that owns it fast-forwards instead and answers synced or
// behind_dirty.
type Comparison string

const (
	// ComparisonOK is a checkout already at the pull request's head.
	ComparisonOK Comparison = "ok"
	// ComparisonAheadOwn is the author's own checkout with commits not pushed
	// yet, which is ordinary rather than stale.
	ComparisonAheadOwn Comparison = "ahead_own"
	// ComparisonBehind is a checkout the pull request's head is ahead of, left
	// exactly where it was: Compare never moves anything.
	ComparisonBehind Comparison = "behind"
	// ComparisonDiverged is a checkout with commits the pull request's head
	// has never seen.
	ComparisonDiverged Comparison = "diverged"
	// ComparisonBranchMismatch is a checkout of something else entirely, a
	// detached head included.
	ComparisonBranchMismatch Comparison = "branch_mismatch"
	// ComparisonFetchFailed is a fetch of the pull request's head or of its
	// base branch from origin that failed, which is to say the network or
	// origin itself: every pull request's head is on origin, a fork's
	// included.
	ComparisonFetchFailed Comparison = "fetch_failed"
)

// PullRequest is what the freshness check needs to know about the pull request
// the checkout is supposed to be following.
type PullRequest struct {
	Number  int
	HeadRef string
	// LocalBranch is the branch a checkout of the pull request is on, which
	// is the head branch's own name only where the head is in this
	// repository: see LocalBranch.
	LocalBranch string
	HeadOID     string
	BaseRef     string
	// IsOwnPR decides whether local commits on top of the head are the
	// author's own unpushed work or somebody else's history to leave alone.
	IsOwnPR bool
}

// FreshnessReport is the answer, with the two commits that produced it so that
// a caller can say what it compared.
type FreshnessReport struct {
	Status  Freshness `json:"status"`
	HeadRef string    `json:"head_ref"`
	// The branch the checkout has to be on to count as the pull
	// request's, which differs from head_ref for a pull request from a
	// fork. The one to switch to on branch_mismatch.
	LocalBranch string `json:"local_branch"`
	HeadOID     string `json:"head_oid"`
	// Read after any synchronisation, so it is where the checkout
	// ended up rather than where it started.
	LocalHead string `json:"local_head"`
}

// Compare fetches and answers where dir stands against the pull request's
// head, without moving anything.
//
// The head is fetched as refs/pull/<n>/head, which origin carries for every
// pull request — a fork's included, whose head branch it does not have — and
// the comparison is against head_oid rather than anything the fetch updated.
func Compare(ctx context.Context, r runner.Runner, dir string, pr PullRequest) (Comparison, error) {
	if err := FetchPullHead(ctx, r, dir, pr.Number, pr.HeadOID, pr.BaseRef); err != nil {
		if _, failed := errors.AsType[*FetchError](err); failed {
			return ComparisonFetchFailed, nil
		}
		return "", err
	}

	// A detached head answers with the literal HEAD, which matches no branch
	// name and so stops here too — as it should, since nothing says which
	// branch the commits belong to.
	branch, err := runner.Git(ctx, r, dir, "rev-parse", "--abbrev-ref", "HEAD")
	if err != nil {
		return "", err
	}
	if branch != pr.LocalBranch {
		return ComparisonBranchMismatch, nil
	}

	local, err := Head(ctx, r, dir)
	if err != nil {
		return "", err
	}
	if local == pr.HeadOID {
		return ComparisonOK, nil
	}

	if pr.IsOwnPR && IsAncestor(ctx, r, dir, pr.HeadOID, "HEAD") {
		return ComparisonAheadOwn, nil
	}
	if IsAncestor(ctx, r, dir, "HEAD", pr.HeadOID) {
		return ComparisonBehind, nil
	}
	return ComparisonDiverged, nil
}

// pullRef is where origin keeps the head of pull request number.
func pullRef(number int) string {
	return fmt.Sprintf("refs/pull/%d/head", number)
}

// FetchError is a fetch from origin that failed, told apart from the head
// being missing afterwards because the callers answer the two differently.
type FetchError struct {
	Refs []string
	Dir  string
	Err  error
}

func (e *FetchError) Error() string {
	return fmt.Sprintf("git fetch origin %s failed in %s: %v", strings.Join(e.Refs, " "), e.Dir, e.Err)
}

func (e *FetchError) Unwrap() error { return e.Err }

// FetchPullHead brings pull request number's head into dir from
// refs/pull/<n>/head, along with the refs named beside it, and checks that
// headOID arrived.
//
// refs/pull/<n>/head is on origin for every pull request, a fork's included,
// whose head branch origin does not have. The check is because the API's
// head_oid and refs/pull/<n>/head are not updated in step: a pull request that
// moved between the two being read leaves head_oid missing, and a missing
// commit reads as not being an ancestor of anything — a pull request that
// merely moved would be taken for one that diverged.
func FetchPullHead(ctx context.Context, r runner.Runner, dir string, number int, headOID string, alongside ...string) error {
	head := pullRef(number)
	refs := append(append([]string{}, alongside...), head)
	if _, err := gitlock.Git(ctx, r, dir, append([]string{"fetch", "-q", "origin"}, refs...)...); err != nil {
		return &FetchError{Refs: refs, Dir: dir, Err: err}
	}
	if _, err := runner.Git(ctx, r, dir, "cat-file", "-e", headOID+"^{commit}"); err != nil {
		return fmt.Errorf(
			"the pull request head %s is not in %s after fetching %s; it moved while the pull request was being read — run this again",
			headOID, dir, head)
	}
	return nil
}

// Head is the commit dir is standing on.
func Head(ctx context.Context, r runner.Runner, dir string) (string, error) {
	return runner.Git(ctx, r, dir, "rev-parse", "HEAD")
}

// CheckFreshness compares the checkout in dir with the pull request's head, and
// brings it up to date where that costs nothing.
//
// It fetches first, so a caller need not: the comparison is worthless against
// refs that were last updated before the pull request moved.
func CheckFreshness(ctx context.Context, r runner.Runner, dir string, pr PullRequest) (FreshnessReport, error) {
	report := func(status Freshness) (FreshnessReport, error) {
		local, err := Head(ctx, r, dir)
		if err != nil {
			return FreshnessReport{}, err
		}
		return FreshnessReport{
			Status: status, HeadRef: pr.HeadRef, LocalBranch: pr.LocalBranch, HeadOID: pr.HeadOID, LocalHead: local,
		}, nil
	}

	c, err := Compare(ctx, r, dir, pr)
	if err != nil {
		return FreshnessReport{}, err
	}

	switch c {
	case ComparisonOK:
		return report(FreshnessOK)
	case ComparisonAheadOwn:
		return report(FreshnessAheadOwn)
	case ComparisonBehind:
		status, err := fastForwardOrDirty(ctx, r, dir, pr.HeadOID)
		if err != nil {
			return FreshnessReport{}, err
		}
		return report(status)
	case ComparisonDiverged:
		return report(FreshnessDiverged)
	case ComparisonBranchMismatch:
		return report(FreshnessBranchMismatch)
	case ComparisonFetchFailed:
		return report(FreshnessFetchFailed)
	default:
		return FreshnessReport{}, fmt.Errorf("worktree: unhandled comparison %q", c)
	}
}
