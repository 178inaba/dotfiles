package worktree

import (
	"context"
	"encoding/json/v2"
	"fmt"
	"strings"

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
	// FreshnessOK and FreshnessSynced are a checkout at the pull request's
	// head, before and after this command moved it there.
	FreshnessOK     Freshness = "ok"
	FreshnessSynced Freshness = "synced"
	// FreshnessAheadOwn is the author's own checkout with commits not pushed
	// yet, which is ordinary rather than stale.
	FreshnessAheadOwn Freshness = "ahead_own"
	// FreshnessBehindDirty and FreshnessDiverged are stops with work at stake:
	// uncommitted changes in the first, commits the remote has never seen in
	// the second. Both are for a person to resolve.
	FreshnessBehindDirty Freshness = "behind_dirty"
	FreshnessDiverged    Freshness = "diverged"
	// FreshnessBranchMismatch is a checkout of something else entirely, a
	// detached head included.
	FreshnessBranchMismatch Freshness = "branch_mismatch"
	// FreshnessFetchFailed most often means the head branch is not on origin
	// at all, which is what a pull request from a fork looks like from here.
	FreshnessFetchFailed Freshness = "fetch_failed"
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
	// LocalHead is read after any synchronisation, so it is where the checkout
	// ended up rather than where it started.
	LocalHead string `json:"local_head"`
}

// ParsePullRequest reads the four fields of a pull request context that the
// freshness check depends on.
//
// A subset of what `ccx pr context` writes, deliberately: this is a consumer of
// that contract, and naming only what it reads keeps a field it never looks at
// from becoming a reason this fails.
func ParsePullRequest(b []byte) (PullRequest, error) {
	var wire struct {
		PR struct {
			HeadOID string `json:"head_oid"`
			HeadRef string `json:"head_ref"`
			BaseRef string `json:"base_ref"`
		} `json:"pr"`
		// A pointer, because false is a meaningful answer and its absence is
		// not: reading a missing flag as false would treat the author's own
		// unpushed commits as somebody else's history.
		IsOwnPR *bool `json:"is_own_pr"`
	}
	if err := json.Unmarshal(b, &wire); err != nil {
		return PullRequest{}, fmt.Errorf("decode the pull request context: %w", err)
	}

	for _, field := range []struct{ name, value string }{
		{"pr.head_oid", wire.PR.HeadOID},
		{"pr.head_ref", wire.PR.HeadRef},
		{"pr.base_ref", wire.PR.BaseRef},
	} {
		if field.value == "" {
			return PullRequest{}, fmt.Errorf("%s missing", field.name)
		}
	}
	if wire.IsOwnPR == nil {
		return PullRequest{}, fmt.Errorf("is_own_pr missing")
	}
	return PullRequest{
		HeadRef: wire.PR.HeadRef,
		HeadOID: wire.PR.HeadOID,
		BaseRef: wire.PR.BaseRef,
		IsOwnPR: *wire.IsOwnPR,
	}, nil
}

// CheckFreshness compares the checkout in dir with the pull request's head, and
// brings it up to date where that costs nothing.
//
// It fetches first, so a caller need not: the comparison is worthless against
// refs that were last updated before the pull request moved.
func CheckFreshness(ctx context.Context, r runner.Runner, dir string, pr PullRequest) (FreshnessReport, error) {
	report := func(status Freshness) (FreshnessReport, error) {
		local, err := head(ctx, r, dir)
		if err != nil {
			return FreshnessReport{}, err
		}
		return FreshnessReport{Status: status, HeadRef: pr.HeadRef, HeadOID: pr.HeadOID, LocalHead: local}, nil
	}

	if _, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", dir, "fetch", "-q", "origin", pr.BaseRef, pr.HeadRef},
	}); err != nil {
		return report(FreshnessFetchFailed)
	}

	// A detached head answers with the literal HEAD, which matches no branch
	// name and so stops here too — as it should, since nothing says which
	// branch the commits belong to.
	branch, err := run(ctx, r, dir, "rev-parse", "--abbrev-ref", "HEAD")
	if err != nil {
		return FreshnessReport{}, err
	}
	if branch != pr.HeadRef {
		return report(FreshnessBranchMismatch)
	}

	local, err := head(ctx, r, dir)
	if err != nil {
		return FreshnessReport{}, err
	}
	if local == pr.HeadOID {
		return report(FreshnessOK)
	}

	if pr.IsOwnPR && isAncestor(ctx, r, dir, pr.HeadOID, "HEAD") {
		return report(FreshnessAheadOwn)
	}
	if isAncestor(ctx, r, dir, "HEAD", pr.HeadOID) {
		status, err := fastForwardOrDirty(ctx, r, dir, pr.HeadOID)
		if err != nil {
			return FreshnessReport{}, err
		}
		return report(status)
	}
	return report(FreshnessDiverged)
}

// head returns the commit dir has checked out.
func head(ctx context.Context, r runner.Runner, dir string) (string, error) {
	return run(ctx, r, dir, "rev-parse", "HEAD")
}

// run is one git command whose single line of output is the answer.
func run(ctx context.Context, r runner.Runner, dir string, args ...string) (string, error) {
	out, err := r.Run(ctx, runner.Command{Name: "git", Args: append([]string{"-C", dir}, args...)})
	if err != nil {
		return "", fmt.Errorf("git %s in %s: %w", strings.Join(args, " "), dir, err)
	}
	return strings.TrimSpace(string(out)), nil
}
