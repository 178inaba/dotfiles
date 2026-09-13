package worktree

import (
	"context"
	"fmt"

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
	// FreshnessFetchFailed most often means the head branch is not on origin
	// at all, which is what a pull request from a fork looks like from here.
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
	// ComparisonFetchFailed most often means the head branch is not on origin
	// at all, which is what a pull request from a fork looks like from here.
	ComparisonFetchFailed Comparison = "fetch_failed"
)

// PullRequest is what the freshness check needs to know about the pull request
// the checkout is supposed to be following.
type PullRequest struct {
	HeadRef string
	HeadOID string
	BaseRef string
	// IsOwnPR decides whether local commits on top of the head are the
	// author's own unpushed work or somebody else's history to leave alone.
	IsOwnPR bool
}

// FreshnessReport is the answer, with the two commits that produced it so that
// a caller can say what it compared.
type FreshnessReport struct {
	Status  Freshness `json:"status"`
	HeadRef string    `json:"head_ref"`
	HeadOID string    `json:"head_oid"`
	// Read after any synchronisation, so it is where the checkout
	// ended up rather than where it started.
	LocalHead string `json:"local_head"`
}

// Compare fetches and answers where dir stands against the pull request's
// head, without moving anything.
func Compare(ctx context.Context, r runner.Runner, dir string, pr PullRequest) (Comparison, error) {
	if _, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", dir, "fetch", "-q", "origin", pr.BaseRef, pr.HeadRef},
	}); err != nil {
		return ComparisonFetchFailed, nil
	}

	// A detached head answers with the literal HEAD, which matches no branch
	// name and so stops here too — as it should, since nothing says which
	// branch the commits belong to.
	branch, err := runner.Git(ctx, r, dir, "rev-parse", "--abbrev-ref", "HEAD")
	if err != nil {
		return "", err
	}
	if branch != pr.HeadRef {
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
		return FreshnessReport{Status: status, HeadRef: pr.HeadRef, HeadOID: pr.HeadOID, LocalHead: local}, nil
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
