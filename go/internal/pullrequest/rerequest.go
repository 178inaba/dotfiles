package pullrequest

import (
	"context"
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// SkipReason is why a login named for a review request was not requested.
type SkipReason string

const (
	// SkipNotAReviewer is a login with no submitted review on the pull
	// request, such as somebody who only commented in the conversation: a
	// request would add a reviewer rather than ask one back.
	SkipNotAReviewer SkipReason = "not_a_reviewer"
	// SkipBot is a reviewer whose account is a bot, which does not come back
	// to review when asked.
	SkipBot SkipReason = "bot"
	// SkipApproved is a reviewer whose standing is an approval, which has
	// nothing left to look at again.
	SkipApproved SkipReason = "approved"
	// SkipReviewsTruncated is every login when the document's reviews were cut
	// short: an approval may sit outside the window, so no standing can be
	// trusted. Raising the review limit and fetching again answers it.
	SkipReviewsTruncated SkipReason = "reviews_truncated"
)

// SkippedReviewer is one login that was named and not requested.
type SkippedReviewer struct {
	Login  string     `json:"login"`
	Reason SkipReason `json:"reason"`
}

// ReviewRequested is who was asked for a review and who was not.
type ReviewRequested struct {
	// The logins a review was requested from, in the order they were named.
	// Empty when nobody was eligible, in which case nothing was sent.
	Requested []string `json:"requested" contract:"required"`
	// The logins that were named and left out, each with the rule that left
	// it out.
	Skipped []SkippedReviewer `json:"skipped" contract:"required"`
}

// PlanReviewRequest settles, from the document alone, which of the named
// logins may be asked for another review.
//
// What the document settles is settled here: the pull request is ours, and
// each login is a person with a submitted review that is not an approval.
// Which logins the run answered is the caller's judgement and is what it
// names. Nothing is sent, so a command refuses before it asks for a client.
func PlanReviewRequest(prContext Context, logins []string) (ReviewRequested, error) {
	if !prContext.IsOwnPR {
		return ReviewRequested{}, fmt.Errorf("%s#%d is not ours, so no review is requested on it", prContext.Repo, prContext.PR.Number)
	}

	standing := map[string]Reviewer{}
	for _, r := range prContext.Reviewers {
		if r.Author != nil {
			standing[*r.Author] = r
		}
	}

	plan := ReviewRequested{Requested: []string{}, Skipped: []SkippedReviewer{}}
	named := map[string]bool{}
	for _, login := range logins {
		if named[login] {
			continue
		}
		named[login] = true

		reviewer, found := standing[login]
		switch {
		case prContext.ReviewsTruncated:
			plan.Skipped = append(plan.Skipped, SkippedReviewer{Login: login, Reason: SkipReviewsTruncated})
		case !found:
			plan.Skipped = append(plan.Skipped, SkippedReviewer{Login: login, Reason: SkipNotAReviewer})
		case reviewer.AuthorType != nil && *reviewer.AuthorType == "Bot":
			plan.Skipped = append(plan.Skipped, SkippedReviewer{Login: login, Reason: SkipBot})
		case reviewer.State == ReviewerApproved:
			plan.Skipped = append(plan.Skipped, SkippedReviewer{Login: login, Reason: SkipApproved})
		default:
			plan.Requested = append(plan.Requested, login)
		}
	}
	return plan, nil
}

// RequestReview sends what a plan requested, in one request.
//
// A plan with nobody to request sends nothing. A failure names every login in
// the request: none of them was requested, and nothing is retried.
func RequestReview(ctx context.Context, c *ghapi.Client, target Target, plan ReviewRequested) error {
	if len(plan.Requested) == 0 {
		return nil
	}
	repo, err := target.repository()
	if err != nil {
		return err
	}
	if err := c.RequestReviewers(ctx, repo, target.Number, plan.Requested); err != nil {
		return fmt.Errorf("failed to request a review from %s: %v", strings.Join(plan.Requested, ", "), err)
	}
	return nil
}
