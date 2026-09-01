// Package reviewprs is the /review-assigned-prs skill's view of GitHub.
//
// The skill runs on a loop: it asks which pull requests are waiting for this
// user's review, hands each to a subagent, and afterwards checks that the
// review it was told about actually reached GitHub. Both questions are here
// because both are the same judgement — whose reviews count as somebody
// else's — asked from opposite ends.
package reviewprs

import (
	"context"
	"fmt"
	"net/url"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// PR is one pull request waiting for a review.
type PR struct {
	Owner  string `json:"owner"`
	Repo   string `json:"repo"`
	Number int    `json:"number"`
	URL    string `json:"url"`
}

// Pending is the answer to "what should I review next".
type Pending struct {
	PRs []PR `json:"prs"`
	// Degraded says that at least one pull request could not be judged, so the
	// list is a subset rather than the answer. The caller loops, and a loop
	// that cannot tell a short list from a complete one stops reviewing.
	Degraded bool     `json:"degraded"`
	Warnings []string `json:"warnings"`
}

// userTTL is how long the authenticated user is cached for, which is what
// `gh api user --cache 24h` asked for. It is the one request in this package
// whose answer does not change between runs.
const userTTL = 24 * time.Hour

// pendingQuery is the search gh sent for
// `gh search prs --state=open --draft=false user-review-requested:@me`.
//
// Drafts are excluded by the server rather than afterwards, because the
// convention is that a draft is not asking for review yet; the author asking
// for one is what Ready for review means.
var pendingQuery = url.Values{
	"q":               {"( user-review-requested:@me ) draft:false state:open type:pr"},
	"advanced_search": {"true"},
	"page":            {"1"},
	"per_page":        {"30"},
}

// ListPending returns the pull requests this user should review.
//
// Two of them qualify: one nobody human has reviewed yet, and one this user has
// already reviewed and been asked for again. A pull request somebody else
// reviewed first is left alone, because piling a second review on top of theirs
// adds nothing the author asked for.
//
// This stays on the REST search endpoint rather than moving to GraphQL. The
// author it returns is matched against the login on each review, and REST
// spells a bot copilot[bot] on both sides where GraphQL spells it copilot on
// one — the two would stop matching, and every bot would start counting as a
// person.
func ListPending(ctx context.Context, c *ghapi.Client) (Pending, error) {
	me, err := currentUser(ctx, c, userTTL)
	if err != nil {
		return Pending{}, err
	}

	var found struct {
		Items []searchItem `json:"items"`
	}
	if err := c.Get(ctx, "search/issues?"+pendingQuery.Encode(), &found); err != nil {
		return Pending{}, fmt.Errorf("search for review requests: %w", err)
	}

	out := Pending{PRs: []PR{}}
	for _, item := range found.Items {
		pr := PR{Number: item.Number, URL: item.HTMLURL}
		pr.Owner, pr.Repo = ownerRepoOf(item.RepositoryURL)

		// A deleted account leaves no author to exclude, and without one every
		// reply the author left would count as somebody else's review. Failing
		// loudly beats silently widening the filter.
		if item.User == nil || item.User.Login == "" {
			out.degrade(fmt.Sprintf("missing author for %s/%s#%d", pr.Owner, pr.Repo, pr.Number))
			continue
		}

		reviews, err := reviewsOf(ctx, c, pr.Owner, pr.Repo, pr.Number)
		if err != nil {
			out.degrade(fmt.Sprintf("failed to fetch reviews for %s/%s#%d", pr.Owner, pr.Repo, pr.Number))
			continue
		}
		if wanted(reviews, me, item.User.Login) {
			out.PRs = append(out.PRs, pr)
		}
	}
	return out, nil
}

func (p *Pending) degrade(warning string) {
	p.Degraded = true
	p.Warnings = append(p.Warnings, warning)
}

// wanted reports whether a pull request is one to review, given every review on
// it, the current user and the author.
//
// The author is not somebody else. GitHub records a reply in a review thread as
// a COMMENTED review, so an author who answered a bot's remark would otherwise
// read as a human reviewer and the pull request would be skipped.
func wanted(reviews []review, me, author string) bool {
	var others, mine bool
	for _, r := range reviews {
		if r.User.Type == "Bot" {
			continue
		}
		switch r.User.Login {
		case me:
			mine = true
		case author:
		default:
			others = true
		}
	}
	return !others || mine
}

// Result is whether one pull request carries a review by this user.
type Result struct {
	Owner  string `json:"owner"`
	Repo   string `json:"repo"`
	Number int    `json:"number"`
	Posted bool   `json:"posted"`
}

// Verification is the answer to "did the reviews I was told about get posted".
type Verification struct {
	Results  []Result `json:"results"`
	Degraded bool     `json:"degraded"`
	Warnings []string `json:"warnings"`
}

// Spec names one pull request on the command line.
type Spec struct {
	Owner  string
	Repo   string
	Number int
}

func (s Spec) String() string { return fmt.Sprintf("%s/%s#%d", s.Owner, s.Repo, s.Number) }

// VerifyPosted checks that each named pull request carries a review by this
// user.
//
// The question it answers is "is there one", not "was one just posted": the
// listing above drops a pull request this user has already reviewed, so a
// review that exists at all is one this run left behind. Without this check a
// subagent that reported success without posting would leave the same pull
// request to be reviewed again on every iteration of the loop.
func VerifyPosted(ctx context.Context, c *ghapi.Client, specs []Spec) (Verification, error) {
	// Not the cached user: this runs once at the end of a loop iteration, and
	// a day-old answer to who is posting is not worth the risk of verifying
	// against the wrong login.
	me, err := currentUser(ctx, c, 0)
	if err != nil {
		return Verification{}, err
	}

	out := Verification{Results: []Result{}}
	for _, s := range specs {
		reviews, err := reviewsOf(ctx, c, s.Owner, s.Repo, s.Number)
		if err != nil {
			out.Degraded = true
			out.Warnings = append(out.Warnings, fmt.Sprintf("failed to fetch reviews for %s", s))
			continue
		}
		out.Results = append(out.Results, Result{
			Owner: s.Owner, Repo: s.Repo, Number: s.Number,
			Posted: posted(reviews, me),
		})
	}
	return out, nil
}

// posted reports whether any of the reviews is a submitted one by me.
//
// A PENDING review is a draft only its author can see, left behind by a POST
// with no event. Counting it would report a review that nobody else can read as
// delivered, which is the failure this check exists to catch.
func posted(reviews []review, me string) bool {
	for _, r := range reviews {
		if r.User.Login == me && r.State != "PENDING" {
			return true
		}
	}
	return false
}

// searchItem is one hit of the issue search. A pull request's own url is
// html_url; url is the api one, and it is the html one the caller opens.
type searchItem struct {
	Number  int    `json:"number"`
	HTMLURL string `json:"html_url"`
	// RepositoryURL is where the owner and the name come from: the search
	// results carry no nameWithOwner of their own.
	RepositoryURL string `json:"repository_url"`
	// User is null for an author whose account is gone.
	User *struct {
		Login string `json:"login"`
	} `json:"user"`
}

// review is one review on a pull request, as much of it as either question
// needs.
type review struct {
	State string `json:"state"`
	User  struct {
		Login string `json:"login"`
		// Type tells a person from a bot. Copilot, github-actions and
		// CodeRabbit all review, and none of them is the human review this
		// skill is deciding whether to add to.
		Type string `json:"type"`
	} `json:"user"`
}

// reviewsOf reads every review on a pull request.
//
// All of them, not the first page: both questions here are "is there a review
// by X", and a pull request with thirty bot reviews would answer no to a
// question that should be yes.
func reviewsOf(ctx context.Context, c *ghapi.Client, owner, repo string, number int) ([]review, error) {
	return ghapi.GetAll[review](ctx, c, fmt.Sprintf("repos/%s/%s/pulls/%d/reviews", owner, repo, number))
}

// currentUser returns the authenticated login, cached for ttl when ttl is
// positive.
func currentUser(ctx context.Context, c *ghapi.Client, ttl time.Duration) (string, error) {
	var user struct {
		Login string `json:"login"`
	}
	var err error
	if ttl > 0 {
		err = c.GetCached(ctx, "user", ttl, &user)
	} else {
		err = c.Get(ctx, "user", &user)
	}
	if err != nil {
		return "", fmt.Errorf("fetch the authenticated user: %w", err)
	}
	if user.Login == "" {
		return "", fmt.Errorf("the authenticated user has no login")
	}
	return user.Login, nil
}

// ownerRepoOf reads the owner and the name out of a repository's api url.
func ownerRepoOf(apiURL string) (owner, name string) {
	u, err := url.Parse(apiURL)
	if err != nil {
		return "", ""
	}
	parts := splitPath(u.Path)
	if len(parts) < 2 {
		return "", ""
	}
	return parts[len(parts)-2], parts[len(parts)-1]
}

// specPattern is the <owner>/<repo>#<number> the caller names a pull request
// with. Neither a slash nor a hash nor whitespace may appear inside a name, so
// the three separators stay unambiguous.
var specPattern = regexp.MustCompile(`^([^/#\s]+)/([^/#\s]+)#([0-9]+)$`)

// ParseSpec reads one <owner>/<repo>#<number>.
func ParseSpec(s string) (Spec, error) {
	m := specPattern.FindStringSubmatch(s)
	if m == nil {
		return Spec{}, fmt.Errorf("invalid PR spec: %s (expected <owner>/<repo>#<number>)", s)
	}
	n, err := strconv.Atoi(m[3])
	if err != nil {
		return Spec{}, fmt.Errorf("invalid PR spec: %s (expected <owner>/<repo>#<number>)", s)
	}
	return Spec{Owner: m[1], Repo: m[2], Number: n}, nil
}

// splitPath returns a url path's non-empty segments.
func splitPath(p string) []string {
	return slices.Collect(strings.SplitSeq(strings.Trim(p, "/"), "/"))
}
