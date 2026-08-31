// Package prinfo keeps the pull request badge of the status line.
//
// Claude Code has a pull request badge of its own, and this one duplicates it
// deliberately: the built-in poller disables itself permanently and silently
// after a single slow fetch (anthropics/claude-code#80209), taking the pr.*
// fields of the status line payload with it. Revisit once that is fixed.
//
// Like the exchange rate, the badge is served stale while it revalidates: the
// redraw never waits on gh.
package prinfo

import (
	"context"
	"encoding/json/v2"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	// maxAge matches the refresh interval of the badge this one stands in for.
	maxAge = time.Minute
	// retryInterval keeps a slow or failing gh from being started again on
	// every redraw while the first call is still running.
	retryInterval = time.Minute
)

// State is a review state as it reaches the display, which is not gh's notion
// of a pull request's state — that one is open or merged or closed, and only
// decides whether a badge is shown at all.
type State string

// The review states. The first three are gh's own reviewDecision values; the
// last two are this package's.
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
// refresh. A stale badge is still returned: it beats a gap while gh runs.
func Lookup(dir, key string, now time.Time) (Info, bool) {
	rec, ok := cache.Read[Info](dir, key)
	if ok && cache.Fresh(now, rec.At, maxAge) {
		return rec.Value, false
	}
	return rec.Value, cache.ShouldAttempt(dir, now, retryInterval)
}

// Refresh asks gh about the current branch and stores the answer.
//
// A failure of any kind — no pull request, offline, not authenticated, all of
// which gh reports the same way — is cached as "no pull request" rather than
// reported. That is a result and not an error: caching it is what keeps an
// offline machine from calling gh on every redraw. The error is the failure to
// store, which is the only way this can leave nothing behind.
func Refresh(ctx context.Context, r runner.Runner, dir, key, branch string, now time.Time) error {
	var info Info
	// The default branch may be the head of a release pull request, but it is
	// not a branch-specific working context, so it is skipped before gh is even
	// asked.
	if !isDefaultBranch(ctx, r, branch) {
		info = fetch(ctx, r)
	}
	return cache.Write(dir, key, now, info)
}

// fetch returns the current branch's pull request, or the zero Info when there
// is none to show.
func fetch(ctx context.Context, r runner.Runner) Info {
	out, err := r.Run(ctx, runner.Command{
		Name: "gh",
		Args: []string{"pr", "view", "--json", "number,reviewDecision,state,isDraft,url"},
	})
	if err != nil {
		return Info{}
	}

	var pr struct {
		Number         int    `json:"number"`
		ReviewDecision string `json:"reviewDecision"`
		State          string `json:"state"`
		IsDraft        bool   `json:"isDraft"`
		URL            string `json:"url"`
	}
	if err := json.Unmarshal(out, &pr); err != nil {
		return Info{}
	}
	// A merged or closed pull request is not the current work.
	if pr.State != "OPEN" {
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

// isDefaultBranch reports whether branch is the repository's default.
//
// origin/HEAD, which cloning sets, is the answer when it is there; gh is only
// asked when it is not, as in a repository that gained its remote by hand. When
// neither knows, the answer is no — showing a badge that should have been
// hidden is the milder failure.
func isDefaultBranch(ctx context.Context, r runner.Runner, branch string) bool {
	out, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"symbolic-ref", "-q", "--short", "refs/remotes/origin/HEAD"},
	})
	def := ""
	if err == nil {
		def = strings.TrimPrefix(strings.TrimSpace(string(out)), "origin/")
	}
	if def == "" {
		def = defaultBranchFromGH(ctx, r)
	}
	return def != "" && branch == def
}

func defaultBranchFromGH(ctx context.Context, r runner.Runner) string {
	out, err := r.Run(ctx, runner.Command{
		Name: "gh",
		Args: []string{"repo", "view", "--json", "defaultBranchRef"},
	})
	if err != nil {
		return ""
	}
	var repo struct {
		DefaultBranchRef struct {
			Name string `json:"name"`
		} `json:"defaultBranchRef"`
	}
	if err := json.Unmarshal(out, &repo); err != nil {
		return ""
	}
	return repo.DefaultBranchRef.Name
}
