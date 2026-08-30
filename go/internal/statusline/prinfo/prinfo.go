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
	"encoding/json"
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/shellfmt"
)

const (
	// CacheBase names the per-directory-and-branch cache files. Keying on the
	// branch as well as the directory is what makes a branch switch take effect
	// at once instead of after the next expiry.
	CacheBase = "/tmp/claude-statusline-pr-cache"

	// maxAge matches the refresh interval of the badge this one stands in for.
	maxAge = 60
	// retryInterval keeps a slow or failing gh from being started again on
	// every redraw while the first call is still running.
	retryInterval = 60
)

// Review states as they reach the display. The first three are gh's own
// reviewDecision values; the last two are this package's.
const (
	StateApproved          = "APPROVED"
	StateChangesRequested  = "CHANGES_REQUESTED"
	StateReviewRequired    = "REVIEW_REQUIRED"
	StateDraft             = "DRAFT"
	StateNoReviewRequested = "NONE"
)

// Info is a pull request worth showing.
type Info struct {
	Number string
	State  string
	URL    string
}

// Lookup returns the cached record, which is empty when there is none, and
// whether the caller should start a refresh.
//
// A record written for a different key is discarded rather than shown: two very
// deep directories can share a cache file once the name is cut to length, and
// showing one's pull request under the other would be worse than showing none.
func Lookup(cachePath, cacheKey string, now int64) (string, bool) {
	result := ""
	rec, ok := cache.ReadKeyed(cachePath)
	fresh := false
	if ok && rec.Key == cacheKey {
		result = rec.Result
		fresh = cache.Fresh(now, rec.At, maxAge)
	}
	if fresh {
		return result, false
	}

	// The attempt file is derived from the cut path, so two keys that collide
	// share one throttle as well as one record.
	attemptPath := cachePath + ".attempt"
	if last, ok := cache.ReadAttempt(attemptPath); ok && cache.Fresh(now, last, retryInterval) {
		return result, false
	}
	// Best effort: a write that fails only costs one duplicate gh call.
	_ = cache.WriteAttempt(attemptPath, now)
	return result, true
}

// Parse reads a cached record. The second value is false when there is no pull
// request to show, which covers an empty record and a malformed one alike.
func Parse(record string) (Info, bool) {
	number, rest := field(record)
	reviewState, rest := field(rest)
	url := strings.Trim(rest, blanks)
	if !isNumber(number) {
		return Info{}, false
	}
	return Info{Number: number, State: reviewState, URL: url}, true
}

// blanks is bash's default field separator.
const blanks = " \t\n"

// field takes the next word the way `read` does — skipping leading separators,
// stopping at the next run of them — and returns the rest untouched, because
// the last variable of a read absorbs whatever is left.
//
// A plain three-way split would differ: a pull request with a null URL leaves a
// trailing space in the record, which read drops and a split would keep.
func field(s string) (string, string) {
	s = strings.TrimLeft(s, blanks)
	i := strings.IndexAny(s, blanks)
	if i < 0 {
		return s, ""
	}
	return s[:i], s[i:]
}

func isNumber(s string) bool {
	if s == "" {
		return false
	}
	for _, r := range s {
		if r < '0' || r > '9' {
			return false
		}
	}
	return true
}

// Refresh asks gh about the current branch and stores the answer. It is meant
// to run detached and reports nothing.
//
// A failure of any kind — no pull request, offline, not authenticated, all of
// which gh reports the same way — is cached as "no pull request". That is what
// keeps an offline machine from calling gh on every redraw.
func Refresh(ctx context.Context, r runner.Runner, cachePath, cacheKey, branch string, now int64) {
	result := ""
	// The default branch may be the head of a release pull request, but it is
	// not a branch-specific working context, so it is skipped before gh is even
	// asked.
	if !isDefaultBranch(ctx, r, branch) {
		result = fetch(ctx, r)
	}
	// Best effort: a write that fails leaves the previous record in place.
	_ = cache.WriteKeyedAtomic(cachePath, cache.Keyed{At: now, Key: cacheKey, Result: result})
}

// fetch returns the record for the current branch's pull request, or the empty
// string when there is none to show.
func fetch(ctx context.Context, r runner.Runner) string {
	out, err := r.Run(ctx, runner.Command{
		Name: "gh",
		Args: []string{"pr", "view", "--json", "number,reviewDecision,state,isDraft,url"},
	})
	if err != nil {
		return ""
	}

	var pr struct {
		Number         int    `json:"number"`
		ReviewDecision string `json:"reviewDecision"`
		State          string `json:"state"`
		IsDraft        bool   `json:"isDraft"`
		URL            string `json:"url"`
	}
	if err := json.Unmarshal(out, &pr); err != nil {
		return ""
	}
	// A merged or closed pull request is not the current work.
	if pr.State != "OPEN" {
		return ""
	}
	return fmt.Sprintf("%d %s %s", pr.Number, state(pr.IsDraft, pr.ReviewDecision), pr.URL)
}

func state(isDraft bool, reviewDecision string) string {
	switch {
	case isDraft:
		return StateDraft
	case reviewDecision == "":
		return StateNoReviewRequested
	default:
		return reviewDecision
	}
}

// isDefaultBranch reports whether branch is the repository's default.
//
// origin/HEAD, which cloning sets, is the answer when it is there; gh is only
// asked when it is not, as it is in a repository that gained its remote by
// hand. When neither knows, the answer is no — showing a badge that should have
// been hidden is the milder failure.
func isDefaultBranch(ctx context.Context, r runner.Runner, branch string) bool {
	out, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"symbolic-ref", "-q", "--short", "refs/remotes/origin/HEAD"},
	})
	def := ""
	if err == nil {
		def = strings.TrimPrefix(shellfmt.Capture(out), "origin/")
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
