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
	"net/http"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/issue"
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
	// Null for an issue in this repository, which is how the body
	// wrote it.
	Repo   *string `json:"repo"`
	Number int     `json:"number"`
	// title and body are null where the issue could not be read —
	// deleted, or not ours to see — and warnings says which. An issue with
	// nothing written in it has an empty body rather than a null one, so the
	// two are told apart.
	Title *string `json:"title"`
	Body  *string `json:"body"`
	// The issue this one is a sub-issue of, null where it has none
	// or where the parent could not be read. A Sub is bound by the rules its
	// parent states and cannot be judged without them.
	Parent *IssueParent `json:"parent"`
}

// IssueParent is the issue a linked issue is a sub-issue of.
//
// Its title and body are never null: an unreadable parent is reported as no
// parent at all, since there would be nothing left of it to carry.
type IssueParent struct {
	// Null for a parent in this repository, as the linked issue's own
	// repository is.
	Repo   *string `json:"repo"`
	Number int     `json:"number"`
	Title  string  `json:"title"`
	Body   string  `json:"body"`
}

// Comment is one comment in the pull request's conversation.
type Comment struct {
	Author *string `json:"author"`
	// The GraphQL type of the author — User, Bot and so on —
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
	Author *string `json:"author"`
	// The GraphQL type of the author, as the conversation's comments carry: a
	// bot's review is told from a person's without a list of bot names.
	AuthorType  *string `json:"author_type"`
	State       string  `json:"state"`
	Body        string  `json:"body"`
	URL         string  `json:"url"`
	SubmittedAt string  `json:"submitted_at"`
}

// ThreadComment is one comment inside a review thread.
type ThreadComment struct {
	Author *string `json:"author"`
	// The GraphQL type of the author, as the conversation's comments carry.
	AuthorType *string `json:"author_type"`
	Body       string  `json:"body"`
	CreatedAt  string  `json:"created_at"`
	URL        string  `json:"url"`
}

// Ball is whose move it is on a review thread.
type Ball string

const (
	// BallMine is a thread waiting on us: a remark to answer on our own pull
	// request, or one of ours that has been answered or overtaken by a commit.
	BallMine Ball = "mine"
	// BallTheirs is a thread waiting on somebody else, who has been given
	// everything they need to come back to it.
	BallTheirs Ball = "theirs"
	// BallNone is a thread nobody owes anything on: resolved, or somebody
	// else's remark on somebody else's pull request.
	BallNone Ball = "none"
)

// Thread is one conversation on the diff.
type Thread struct {
	ID         string `json:"id"`
	IsResolved bool   `json:"is_resolved"`
	IsOutdated bool   `json:"is_outdated"`
	Path       string `json:"path"`
	Line       *int   `json:"line"`
	// The line the thread was opened on, which stays put where line
	// goes null the moment the commented lines leave the diff — for the author
	// that is right after the fixing push, which is when the reply is written.
	OriginalLine *int    `json:"original_line"`
	ResolvedBy   *string `json:"resolved_by"`
	// opened_by and opened_by_type are the first comment's author, who
	// is the one the thread belongs to. The first comment is what survives
	// truncation, since the comments are paginated forwards.
	OpenedBy     *string `json:"opened_by"`
	OpenedByType *string `json:"opened_by_type"`
	// comments_total_count and comments_truncated are what a caller raises the
	// limit against when a thread was cut short.
	CommentsTotalCount int             `json:"comments_total_count"`
	CommentsTruncated  bool            `json:"comments_truncated"`
	Comments           []ThreadComment `json:"comments"`
	// Comes from the other end of the connection, so it is right
	// even where Comments was truncated — which means it is not always the
	// last element of Comments, and must not be treated as one.
	LastComment *ThreadComment `json:"last_comment"`
	// Whose move it is. A thread with no comments at all — which
	// GitHub does not produce, and which leaves the opener unknown — falls to
	// none rather than being guessed at.
	Ball Ball `json:"ball"`
	// Whether the thread is ours to mark resolved: we opened
	// it, or it is on our own pull request and nobody but a bot and us is in
	// it. A person's remark is closed by that person, which is the other half
	// of the protocol the reviewing side runs.
	ResolvableByMe bool `json:"resolvable_by_me"`
}

// Context is everything one review needs, in the order the contract publishes
// it.
type Context struct {
	Repo        string `json:"repo"`
	CurrentUser string `json:"current_user"`
	IsOwnPR     bool   `json:"is_own_pr"`
	PR          PR     `json:"pr"`
	// What the body's closing keywords name, which is what
	// GitHub itself would close on merge.
	LinkedIssues []LinkedIssue `json:"linked_issues"`
	// The date of the head commit, null where it could not be
	// read — in which case the time condition below simply never holds.
	HeadCommittedAt *string `json:"head_committed_at"`
	// The pull request's commits, oldest first: every commit of
	// the range GitHub shows on the Commits tab, merge commits included.
	Commits []Commit `json:"commits"`
	// The whole diff at head_oid, as a file and the statistics
	// over it. No limit is applied to either.
	Diff               Diff      `json:"diff"`
	CommentsTotalCount int       `json:"comments_total_count"`
	CommentsTruncated  bool      `json:"comments_truncated"`
	Comments           []Comment `json:"comments"`
	ReviewsTotalCount  int       `json:"reviews_total_count"`
	ReviewsTruncated   bool      `json:"reviews_truncated"`
	Reviews            []Review  `json:"reviews"`
	ThreadsTotalCount  int       `json:"threads_total_count"`
	ThreadsTruncated   bool      `json:"threads_truncated"`
	ReviewThreads      []Thread  `json:"review_threads"`
	// The degradations that did not stop the document being
	// useful: one line per issue that could not be read, as owner/repo#N
	// followed by why. Empty rather than null when everything was read. What
	// was cut short by a limit is not among them — the truncation flags say
	// that, and a caller answers it by raising the limit rather than by
	// reading prose.
	Warnings []string `json:"warnings"`
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

// DefaultLimits are generous enough that no pull request in this repository
// has reached one.
var DefaultLimits = Limits{Comments: 500, Threads: 300, ThreadComments: 200}

// Fetch gathers the context of one pull request.
//
// pr is its metadata, already resolved by the caller — by number or from the
// branch — because the two ways of getting it fail differently and the caller
// is where those messages belong.
//
// change is what ReadChange already read out of git, handed in rather than
// read here: a caller that runs this twice with the limits raised runs git
// once, and the document is assembled in one place either way.
func Fetch(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, pr ghapi.PullRequest, limits Limits, change Change) (Context, error) {
	vars := map[string]any{
		"owner": repo.Owner, "name": repo.Name, "number": pr.Number, "headOid": pr.HeadRefOid,
	}
	var b body
	if err := c.GraphQL(ctx, bodyQuery, vars, &b); err != nil {
		return Context{}, fmt.Errorf("failed to fetch PR comments/reviews/threads (GraphQL): %v", err)
	}

	me := b.Viewer.Login
	prq := b.Repository.PullRequest

	comments, err := pages(ctx, limits.Comments, prq.Comments.Nodes, prq.Comments.PageInfo,
		func(ctx context.Context, cursor string) ([]commentNode, pageInfo, error) {
			var page body
			vars := map[string]any{"owner": repo.Owner, "name": repo.Name, "number": pr.Number, "cursor": cursor}
			if err := c.GraphQL(ctx, commentsPageQuery, vars, &page); err != nil {
				return nil, pageInfo{}, fmt.Errorf("failed to fetch PR comments page (GraphQL): %v", err)
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
				return nil, pageInfo{}, fmt.Errorf("failed to fetch review threads page (GraphQL): %v", err)
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

	issues, warnings, err := readIssues(ctx, c, repo, linkedIssues(pr.Body))
	if err != nil {
		return Context{}, err
	}

	// Empty rather than null where there is nothing, so that a reader walks
	// the same shape whatever the pull request holds.
	if change.Commits == nil {
		change.Commits = []Commit{}
	}
	if change.Diff.Files == nil {
		change.Diff.Files = []DiffFile{}
	}

	out := Context{
		Repo:        repo.String(),
		CurrentUser: me,
		IsOwnPR:     pr.Author == me,
		PR: PR{
			Number: pr.Number, Title: pr.Title, Body: pr.Body, URL: pr.URL, State: pr.State,
			Author: pr.Author, HeadRef: pr.HeadRefName, BaseRef: pr.BaseRefName, HeadOID: pr.HeadRefOid,
		},
		LinkedIssues:       issues,
		HeadCommittedAt:    headCommittedAt,
		Commits:            change.Commits,
		Diff:               change.Diff,
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
		Warnings:          warnings,
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
			Author: n.Author.login(), AuthorType: n.Author.typename(),
			State: n.State, Body: n.Body, URL: n.URL, SubmittedAt: n.SubmittedAt,
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
func thread(ctx context.Context, c *ghapi.Client, n threadNode, me string, isOwnPR bool, headCommittedAt *string, limit int) (Thread, error) {
	comments, err := pages(ctx, limit, n.Comments.Nodes, n.Comments.PageInfo,
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
				return nil, pageInfo{}, fmt.Errorf("failed to fetch review thread comments page (GraphQL): %v", err)
			}
			return page.Node.Comments.Nodes, page.Node.Comments.PageInfo, nil
		})
	if err != nil {
		return Thread{}, err
	}

	t := Thread{
		ID: n.ID, IsResolved: n.IsResolved, IsOutdated: n.IsOutdated, Path: n.Path, Line: n.Line,
		OriginalLine:       n.OriginalLine,
		ResolvedBy:         n.ResolvedBy.login(),
		CommentsTotalCount: n.Comments.TotalCount,
		CommentsTruncated:  n.Comments.TotalCount > len(comments),
		Comments:           make([]ThreadComment, 0, len(comments)),
	}
	for _, comment := range comments {
		t.Comments = append(t.Comments, ThreadComment{
			Author: comment.Author.login(), AuthorType: comment.Author.typename(),
			Body: comment.Body, CreatedAt: comment.CreatedAt, URL: comment.URL,
		})
	}
	if len(n.Tail.Nodes) > 0 {
		tail := n.Tail.Nodes[0]
		t.LastComment = &ThreadComment{
			Author: tail.Author.login(), AuthorType: tail.Author.typename(),
			Body: tail.Body, CreatedAt: tail.CreatedAt, URL: tail.URL,
		}
	}
	// The opener is comments[0]: the comments are paginated forwards, so the
	// first one survives any truncation.
	if len(comments) > 0 {
		t.OpenedBy, t.OpenedByType = comments[0].Author.login(), comments[0].Author.typename()
	}

	botAlone := botOnly(comments, t.CommentsTruncated, me)
	t.Ball = ball(t, isOwnPR, botAlone, me, headCommittedAt)
	t.ResolvableByMe = !n.IsResolved && (isLogin(t.OpenedBy, me) || (isOwnPR && botAlone))
	return t, nil
}

// ball works out whose move a thread is, from who opened it, who spoke last and
// whose pull request it is.
//
// A thread with no comments has no opener and no last comment, so there is
// nobody to hand it to; it falls to none rather than to the branch a missing
// opener would otherwise land in. GitHub does not produce one.
func ball(t Thread, isOwnPR, botAlone bool, me string, headCommittedAt *string) Ball {
	if t.IsResolved || t.LastComment == nil {
		return BallNone
	}
	spokeLast := isLogin(t.LastComment.Author, me)

	if isLogin(t.OpenedBy, me) {
		// Ours to judge once somebody has answered — or once a commit has
		// overtaken our remark, which is how an author who pushes a fix without
		// replying hands it back.
		if !spokeLast || movedSince(headCommittedAt, t.LastComment.CreatedAt) {
			return BallMine
		}
		return BallTheirs
	}
	if !isOwnPR {
		// Somebody else's remark on somebody else's work: not ours to answer
		// and not ours to close.
		return BallNone
	}
	// A bot never comes back to confirm, so a thread only it and we are in is
	// always ours — including after our own reply, which is the "replied, still
	// to resolve" state that leaves threads open for days.
	if botAlone || !spokeLast {
		return BallMine
	}
	return BallTheirs
}

// botOnly reports whether a bot opened the thread and nobody but that kind of
// author and ourselves has spoken in it.
//
// Truncation makes the question unanswerable — a person may sit outside the
// window that was fetched — so a cut-short thread is not one, which is the side
// that keeps a run from closing somebody's remark.
func botOnly(comments []commentNode, truncated bool, me string) bool {
	if truncated || len(comments) == 0 || !comments[0].Author.isBot() {
		return false
	}
	for _, c := range comments {
		if !c.Author.isBot() && !isLogin(c.Author.login(), me) {
			return false
		}
	}
	return true
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

// readIssues fills in what the body only named: each issue's title and body,
// and the parent whose rules a sub-issue is bound by.
//
// The warnings it returns are the issues it could not read. Reading one is not
// what the document is for, so a deleted or invisible issue leaves its fields
// null and is reported rather than stopping the fetch — a review of a pull
// request whose closed issue was since deleted would otherwise be impossible.
// Everything else that goes wrong is returned, because a server error or an
// expired token says nothing about the issue, and a null title would report it
// as gone.
func readIssues(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, issues []LinkedIssue) ([]LinkedIssue, []string, error) {
	warnings := []string{}
	for i, linked := range issues {
		in := repo
		if linked.Repo != nil {
			var err error
			if in, err = ghapi.ParseRepo(*linked.Repo); err != nil {
				warnings = append(warnings, fmt.Sprintf("%s#%d: the repository could not be read", *linked.Repo, linked.Number))
				continue
			}
		}

		var w issueWire
		if err := c.Get(ctx, fmt.Sprintf("repos/%s/issues/%d", in, linked.Number), &w); err != nil {
			status, gone := unreadable(err)
			if !gone {
				return nil, nil, fmt.Errorf("failed to read %s#%d: %v", in, linked.Number, err)
			}
			warnings = append(warnings, fmt.Sprintf("%s#%d: the issue could not be read (HTTP %d)", in, linked.Number, status))
			continue
		}
		issues[i].Title, issues[i].Body = &w.Title, &w.Body

		parent, err := issue.ParentOf(ctx, c, in, linked.Number)
		if err != nil {
			status, gone := unreadable(err)
			if !gone {
				return nil, nil, fmt.Errorf("failed to read the parent of %s#%d: %v", in, linked.Number, err)
			}
			warnings = append(warnings, fmt.Sprintf("%s#%d: the parent issue could not be read (HTTP %d)", in, linked.Number, status))
			continue
		}
		if parent == nil {
			continue
		}
		issues[i].Parent = &IssueParent{
			Repo: elsewhere(parent.Repo, repo), Number: parent.Number,
			Title: parent.Title, Body: parent.Body,
		}
	}
	return issues, warnings, nil
}

// issueWire is as much of a GitHub issue object as the document carries.
type issueWire struct {
	Title string `json:"title"`
	Body  string `json:"body"`
}

// unreadable reports whether GitHub declined to show something in a way that
// says it may no longer be there at all, and with which status.
//
// Not found, forbidden and gone are the three: an issue that was deleted, or
// that this token may not see, is one the document simply cannot carry. Every
// other failure — a server error, an expired token, a network error, which
// carries no status — is about the run rather than about the issue.
func unreadable(err error) (int, bool) {
	status, ok := ghapi.HTTPStatus(err)
	if !ok {
		return 0, false
	}
	switch status {
	case http.StatusNotFound, http.StatusForbidden, http.StatusGone:
		return status, true
	}
	return status, false
}

// elsewhere names a repository only when it is not the one being read, which
// is the same rule the body's own owner/repo#N follows.
func elsewhere(in, repo ghapi.Repo) *string {
	if in == repo || in == (ghapi.Repo{}) {
		return nil
	}
	name := in.String()
	return &name
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

// pages walks the rest of a connection, stopping once limit elements are in
// hand.
//
// The limit is checked before each further request rather than applied to the
// result, so the first page always arrives whole and a final count may exceed
// the limit. That is deliberate: the limit bounds the round trips, and the
// truncation flag — the total against what actually arrived — is what tells the
// caller something was left behind.
func pages[T any](ctx context.Context, limit int, first []T, info pageInfo,
	next func(context.Context, string) ([]T, pageInfo, error),
) ([]T, error) {
	all := first
	for info.HasNextPage && len(all) < limit {
		nodes, page, err := next(ctx, info.EndCursor)
		if err != nil {
			return nil, err
		}
		all = append(all, nodes...)
		info = page
	}
	return all, nil
}
