// Package pullrequest gathers what a review needs to know about a pull request.
//
// Comments on a pull request live in three places GitHub keeps separately —
// the conversation, the reviews, and the threads on the diff — and asking for
// one of them is how a review misses what was already said. This fetches all
// three at once and normalises them into one document, which is the whole
// reason it exists rather than being three calls at the point of use.
package pullrequest

import (
	"context"
	"fmt"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// SkillMarker is what /review-response puts at the front of every comment it
// posts.
//
// Exported because the writing side has to agree with it, and a test compares
// this against the skill that does the writing: if the two ever part, this
// stops recognising its own past replies and answers them again as though they
// were new remarks.
const SkillMarker = "<!-- review-response -->"

// PR is the pull request itself.
type PR struct {
	Number  int           `json:"number"`
	Title   string        `json:"title"`
	Body    string        `json:"body"`
	URL     string        `json:"url"`
	State   ghapi.PRState `json:"state"`
	Author  string        `json:"author"`
	HeadRef string        `json:"head_ref"`
	BaseRef string        `json:"base_ref"`
	HeadOID string        `json:"head_oid"`
}

// LinkedIssue is an issue the pull request closes.
type LinkedIssue struct {
	// Repo is null for an issue in this repository, which is how the body
	// wrote it.
	Repo   *string `json:"repo"`
	Number int     `json:"number"`
}

// Comment is one comment in the pull request's conversation.
type Comment struct {
	Author *string `json:"author"`
	// AuthorType is the GraphQL type of the author — User, Bot and so on —
	// which is how a CI comment is told from a person's without a list of bot
	// names to keep up to date.
	AuthorType     *string `json:"author_type"`
	Body           string  `json:"body"`
	CreatedAt      string  `json:"created_at"`
	URL            string  `json:"url"`
	IsSkillComment bool    `json:"is_skill_comment"`
}

// Review is one submitted review.
type Review struct {
	Author      *string `json:"author"`
	State       string  `json:"state"`
	Body        string  `json:"body"`
	URL         string  `json:"url"`
	SubmittedAt string  `json:"submitted_at"`
}

// ThreadComment is one comment inside a review thread.
type ThreadComment struct {
	Author    *string `json:"author"`
	Body      string  `json:"body"`
	CreatedAt string  `json:"created_at"`
	URL       string  `json:"url"`
}

// Thread is one conversation on the diff.
type Thread struct {
	ID         string  `json:"id"`
	IsResolved bool    `json:"is_resolved"`
	IsOutdated bool    `json:"is_outdated"`
	Path       string  `json:"path"`
	Line       *int    `json:"line"`
	ResolvedBy *string `json:"resolved_by"`
	// CommentsTotalCount and CommentsTruncated are what a caller raises the
	// limit against when a thread was cut short.
	CommentsTotalCount int             `json:"comments_total_count"`
	CommentsTruncated  bool            `json:"comments_truncated"`
	Comments           []ThreadComment `json:"comments"`
	// LastComment comes from the other end of the connection, so it is right
	// even where Comments was truncated — which means it is not always the
	// last element of Comments, and must not be treated as one.
	LastComment *ThreadComment `json:"last_comment"`
	// WaitingForResponse is our own pull request, unresolved, with our reply
	// last: the reviewer has the ball. On somebody else's pull request the
	// same shape means the opposite, which is why it is limited to ours.
	WaitingForResponse bool `json:"waiting_for_response"`
	// AwaitingMyConfirmation is a remark of ours, unresolved, that has either
	// been answered or been overtaken by a commit. Resolving is the remarker's
	// act, so this is about our own threads whoever owns the pull request.
	AwaitingMyConfirmation bool `json:"awaiting_my_confirmation"`
}

// Context is everything one review needs, in the order the contract publishes
// it.
type Context struct {
	Repo        string `json:"repo"`
	CurrentUser string `json:"current_user"`
	IsOwnPR     bool   `json:"is_own_pr"`
	PR          PR     `json:"pr"`
	// LinkedIssues are what the body's closing keywords name, which is what
	// GitHub itself would close on merge.
	LinkedIssues []LinkedIssue `json:"linked_issues"`
	// HeadCommittedAt dates the head commit, and is null where it could not be
	// read — in which case the time condition below simply never holds.
	HeadCommittedAt    *string   `json:"head_committed_at"`
	CommentsTotalCount int       `json:"comments_total_count"`
	CommentsTruncated  bool      `json:"comments_truncated"`
	Comments           []Comment `json:"comments"`
	ReviewsTotalCount  int       `json:"reviews_total_count"`
	ReviewsTruncated   bool      `json:"reviews_truncated"`
	Reviews            []Review  `json:"reviews"`
	ThreadsTotalCount  int       `json:"threads_total_count"`
	ThreadsTruncated   bool      `json:"threads_truncated"`
	ReviewThreads      []Thread  `json:"review_threads"`
}

// Limits stop an unusually large pull request from costing an unbounded number
// of round trips and an unbounded output.
//
// Not a correctness device: every connection here terminates on its own. A
// caller that hits one raises it and runs again, which is what the truncation
// flags are for.
type Limits struct {
	Comments int
	Threads  int
	// ThreadComments is per thread rather than across all of them: forty
	// threads of five comments would reach a shared limit in ordinary use, and
	// every thread after it would lose its discussion.
	ThreadComments int
}

// DefaultLimits are what the shell used, and are generous enough that no pull
// request in this repository has reached one.
var DefaultLimits = Limits{Comments: 500, Threads: 300, ThreadComments: 200}

// Fetch gathers the context of one pull request.
//
// pr is its metadata, already resolved by the caller — by number or from the
// branch — because the two ways of getting it fail differently and the caller
// is where those messages belong.
func Fetch(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, pr ghapi.PullRequest, limits Limits) (Context, error) {
	vars := map[string]any{
		"owner": repo.Owner, "name": repo.Name, "number": pr.Number, "headOid": pr.HeadRefOid,
	}
	var b body
	if err := c.GraphQL(ctx, bodyQuery, vars, &b); err != nil {
		return Context{}, fmt.Errorf("failed to fetch PR comments/reviews/threads (GraphQL)")
	}

	me := b.Viewer.Login
	prq := b.Repository.PullRequest

	comments, err := pages(ctx, limits.Comments, prq.Comments.Nodes, prq.Comments.PageInfo,
		func(ctx context.Context, cursor string) ([]commentNode, pageInfo, error) {
			var page body
			vars := map[string]any{"owner": repo.Owner, "name": repo.Name, "number": pr.Number, "cursor": cursor}
			if err := c.GraphQL(ctx, commentsPageQuery, vars, &page); err != nil {
				return nil, pageInfo{}, fmt.Errorf("failed to fetch PR comments page (GraphQL)")
			}
			return page.Repository.PullRequest.Comments.Nodes, page.Repository.PullRequest.Comments.PageInfo, nil
		})
	if err != nil {
		return Context{}, err
	}

	threads, err := pages(ctx, limits.Threads, prq.ReviewThreads.Nodes, prq.ReviewThreads.PageInfo,
		func(ctx context.Context, cursor string) ([]threadNode, pageInfo, error) {
			var page body
			vars := map[string]any{"owner": repo.Owner, "name": repo.Name, "number": pr.Number, "cursor": cursor}
			if err := c.GraphQL(ctx, threadsPageQuery, vars, &page); err != nil {
				return nil, pageInfo{}, fmt.Errorf("failed to fetch review threads page (GraphQL)")
			}
			return page.Repository.PullRequest.ReviewThreads.Nodes, page.Repository.PullRequest.ReviewThreads.PageInfo, nil
		})
	if err != nil {
		return Context{}, err
	}

	var headCommittedAt *string
	if b.Repository.HeadCommit != nil {
		headCommittedAt = &b.Repository.HeadCommit.CommittedDate
	}

	out := Context{
		Repo:        repo.String(),
		CurrentUser: me,
		IsOwnPR:     pr.Author == me,
		PR: PR{
			Number: pr.Number, Title: pr.Title, Body: pr.Body, URL: pr.URL, State: pr.State,
			Author: pr.Author, HeadRef: pr.HeadRefName, BaseRef: pr.BaseRefName, HeadOID: pr.HeadRefOid,
		},
		LinkedIssues:       linkedIssues(pr.Body),
		HeadCommittedAt:    headCommittedAt,
		CommentsTotalCount: prq.Comments.TotalCount,
		CommentsTruncated:  prq.Comments.TotalCount > len(comments),
		Comments:           make([]Comment, 0, len(comments)),
		ReviewsTotalCount:  prq.Reviews.TotalCount,
		// The reviews are a fixed window rather than a paginated connection, so
		// this reports what fell outside it.
		ReviewsTruncated:  prq.Reviews.TotalCount > len(prq.Reviews.Nodes),
		Reviews:           make([]Review, 0, len(prq.Reviews.Nodes)),
		ThreadsTotalCount: prq.ReviewThreads.TotalCount,
		ThreadsTruncated:  prq.ReviewThreads.TotalCount > len(threads),
		ReviewThreads:     make([]Thread, 0, len(threads)),
	}

	for _, n := range comments {
		out.Comments = append(out.Comments, Comment{
			Author:     n.Author.login(),
			AuthorType: n.Author.typename(),
			Body:       n.Body,
			CreatedAt:  n.CreatedAt,
			URL:        n.URL,
			// Prefix rather than contains, so that a reply quoting one of our
			// comments does not read as one.
			IsSkillComment: strings.HasPrefix(n.Body, SkillMarker),
		})
	}
	for _, n := range prq.Reviews.Nodes {
		out.Reviews = append(out.Reviews, Review{
			Author: n.Author.login(), State: n.State, Body: n.Body, URL: n.URL, SubmittedAt: n.SubmittedAt,
		})
	}
	for _, n := range threads {
		t, err := thread(ctx, c, n, me, out.IsOwnPR, headCommittedAt, limits.ThreadComments)
		if err != nil {
			return Context{}, err
		}
		out.ReviewThreads = append(out.ReviewThreads, t)
	}
	return out, nil
}

// thread normalises one review thread, fetching the rest of its comments.
func thread(ctx context.Context, c *ghapi.Client, n threadNode, me string, isOwnPR bool, headCommittedAt *string, max int) (Thread, error) {
	comments, err := pages(ctx, max, n.Comments.Nodes, n.Comments.PageInfo,
		func(ctx context.Context, cursor string) ([]commentNode, pageInfo, error) {
			var page struct {
				Node struct {
					Comments struct {
						PageInfo pageInfo      `json:"pageInfo"`
						Nodes    []commentNode `json:"nodes"`
					} `json:"comments"`
				} `json:"node"`
			}
			vars := map[string]any{"threadId": n.ID, "cursor": cursor}
			if err := c.GraphQL(ctx, threadCommentsPageQuery, vars, &page); err != nil {
				return nil, pageInfo{}, fmt.Errorf("failed to fetch review thread comments page (GraphQL)")
			}
			return page.Node.Comments.Nodes, page.Node.Comments.PageInfo, nil
		})
	if err != nil {
		return Thread{}, err
	}

	t := Thread{
		ID: n.ID, IsResolved: n.IsResolved, IsOutdated: n.IsOutdated, Path: n.Path, Line: n.Line,
		ResolvedBy:         n.ResolvedBy.login(),
		CommentsTotalCount: n.Comments.TotalCount,
		CommentsTruncated:  n.Comments.TotalCount > len(comments),
		Comments:           make([]ThreadComment, 0, len(comments)),
	}
	for _, comment := range comments {
		t.Comments = append(t.Comments, ThreadComment{
			Author: comment.Author.login(), Body: comment.Body, CreatedAt: comment.CreatedAt, URL: comment.URL,
		})
	}
	if len(n.Tail.Nodes) > 0 {
		tail := n.Tail.Nodes[0]
		t.LastComment = &ThreadComment{
			Author: tail.Author.login(), Body: tail.Body, CreatedAt: tail.CreatedAt, URL: tail.URL,
		}
	}

	t.WaitingForResponse = isOwnPR && !n.IsResolved && t.LastComment != nil && isLogin(t.LastComment.Author, me)
	// The opener is comments[0]: the comments are paginated forwards, so the
	// first one survives any truncation.
	opened := len(comments) > 0 && isLogin(comments[0].Author.login(), me)
	t.AwaitingMyConfirmation = !n.IsResolved && opened && t.LastComment != nil &&
		(!isLogin(t.LastComment.Author, me) || movedSince(headCommittedAt, t.LastComment.CreatedAt))
	return t, nil
}

// movedSince reports whether the head commit is newer than a comment.
//
// This is what catches the common case of an author who pushed a fix without
// replying: without it our own remark stays last and the thread never comes
// back to us. It is also what makes the flag idempotent — replying puts our
// comment past the head, and it only returns once a further commit arrives.
//
// A head date that is missing or unreadable makes the condition simply not
// hold, which degrades to the tail test alone rather than marking every thread
// and replying to all of them twice.
func movedSince(headCommittedAt *string, createdAt string) bool {
	if headCommittedAt == nil {
		return false
	}
	head, err := time.Parse(time.RFC3339, *headCommittedAt)
	if err != nil {
		return false
	}
	comment, err := time.Parse(time.RFC3339, createdAt)
	if err != nil {
		return false
	}
	return comment.Before(head)
}

func isLogin(login *string, want string) bool { return login != nil && *login == want }

// closingKeyword matches the references GitHub itself closes an issue on: a
// keyword, then #N or owner/repo#N. A bare #N and a url are deliberately not
// among them, because GitHub does not close on those either.
var closingKeyword = regexp.MustCompile(`(?i)\b(?:close[sd]?|fix(?:es|ed)?|resolve[sd]?):?\s+(?:([\w.-]+/[\w.-]+))?#([0-9]+)`)

// linkedIssues reads the issues a body says it closes.
//
// Sorted by number and then by repository, and deduplicated, because a body may
// name the same issue twice and the order it does so in is not information.
func linkedIssues(body string) []LinkedIssue {
	out := []LinkedIssue{}
	for _, m := range closingKeyword.FindAllStringSubmatch(body, -1) {
		issue := LinkedIssue{Number: mustAtoi(m[2])}
		if m[1] != "" {
			repo := m[1]
			issue.Repo = &repo
		}
		out = append(out, issue)
	}

	slices.SortStableFunc(out, func(a, b LinkedIssue) int {
		if a.Number != b.Number {
			return a.Number - b.Number
		}
		return strings.Compare(repoOf(a), repoOf(b))
	})
	return slices.CompactFunc(out, func(a, b LinkedIssue) bool {
		return a.Number == b.Number && repoOf(a) == repoOf(b)
	})
}

// repoOf flattens the optional repository for comparison. An absent one sorts
// first, which is where a same-repository reference belongs.
func repoOf(i LinkedIssue) string {
	if i.Repo == nil {
		return ""
	}
	return *i.Repo
}

func mustAtoi(s string) int {
	// The pattern matched digits, so this cannot fail on anything that reaches
	// it; a number too large for an int comes back as zero rather than as a
	// reason to abandon the whole context.
	n, _ := strconv.Atoi(s)
	return n
}

// pages walks the rest of a connection, stopping once max elements are in hand.
//
// The limit is checked before each further request rather than applied to the
// result, so the first page always arrives whole and a final count may exceed
// the limit. That is deliberate: the limit bounds the round trips, and the
// truncation flag — the total against what actually arrived — is what tells the
// caller something was left behind.
func pages[T any](ctx context.Context, max int, first []T, info pageInfo,
	next func(context.Context, string) ([]T, pageInfo, error),
) ([]T, error) {
	all := first
	for info.HasNextPage && len(all) < max {
		nodes, page, err := next(ctx, info.EndCursor)
		if err != nil {
			return nil, err
		}
		all = append(all, nodes...)
		info = page
	}
	return all, nil
}
