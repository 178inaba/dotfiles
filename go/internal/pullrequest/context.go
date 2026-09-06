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

	"github.com/178inaba/dotfiles/go/internal/contract"
	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// SkillMarker is what /review-response puts at the front of every comment it
// posts.
//
// Both ends of that are here now: PostComment writes it and the fetch below
// recognises it, so the two cannot part. It stays exported, and the test
// comparing it against the skill stays with it, because the skill's own
// wording still names the marker — until the skill posts through PostComment
// alone, a marker changed here and not there would stop this recognising its
// own past replies and answer them again as though they were new remarks.
const SkillMarker = "<!-- review-response -->"

// PR is the pull request itself.
type PR struct {
	Number  int           `json:"number" contract:"required,positive"`
	Title   string        `json:"title"`
	Body    string        `json:"body"`
	URL     string        `json:"url"`
	State   ghapi.PRState `json:"state"`
	Author  string        `json:"author"`
	HeadRef string        `json:"head_ref" contract:"required,nonempty"`
	BaseRef string        `json:"base_ref" contract:"required,nonempty"`
	HeadOID string        `json:"head_oid" contract:"required,nonempty"`
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
	// comments_total_count and comments_truncated are what a caller raises the
	// per-issue limit against when an issue was cut short, as the pull
	// request's own comments are.
	CommentsTotalCount int  `json:"comments_total_count" contract:"required"`
	CommentsTruncated  bool `json:"comments_truncated" contract:"required"`
	// The discussion the body does not carry, oldest first. Empty for
	// an issue that could not be read, and for one nobody has commented on —
	// never null. A body that is present means the comments were read too,
	// since failing to read them stops the fetch rather than degrading it.
	Comments []IssueComment `json:"comments" contract:"required"`
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
	// The parent is an issue for the limit as much as the sub-issue
	// is: its own share of it, and its own total to raise it to.
	CommentsTotalCount int  `json:"comments_total_count" contract:"required"`
	CommentsTruncated  bool `json:"comments_truncated" contract:"required"`
	// What was settled in the parent's discussion rather than written
	// into its body, oldest first — which a reader of somebody else's issue
	// cannot assume is the same thing.
	Comments []IssueComment `json:"comments" contract:"required"`
}

// IssueComment is one comment on a linked issue or its parent.
type IssueComment struct {
	Author *string `json:"author"`
	// The type REST gives the author — User, Bot and so on — which
	// is how a CI comment is told from a person's without a list of bot names
	// to keep up to date. Null together with author, for an author since
	// deleted.
	AuthorType *string `json:"author_type"`
	Body       string  `json:"body"`
	CreatedAt  string  `json:"created_at"`
	URL        string  `json:"url"`
}

// Comment is one comment in the pull request's conversation.
type Comment struct {
	Author *string `json:"author"`
	// The GraphQL type of the author — User, Bot and so on —
	// which is how a CI comment is told from a person's without a list of bot
	// names to keep up to date.
	AuthorType *string `json:"author_type"`
	Body       string  `json:"body"`
	CreatedAt  string  `json:"created_at"`
	// When the comment was last edited, null for one never edited.
	// What counts as newly arrived is the later of this and the creation date:
	// a remark rewritten after a run had already judged it is a remark again.
	LastEditedAt   *string `json:"last_edited_at"`
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
	// When the review was last edited, null for one never edited,
	// and read the way the conversation's comments read theirs: the later of
	// this and the submission date is when it last had something new to say.
	LastEditedAt *string `json:"last_edited_at"`
}

// ReviewerState is where a reviewer's review stands.
//
// Three of GitHub's five review states, because the other two say nothing
// about a standing: a review the viewer has not submitted is not one yet, and
// a dismissed one has been turned back into a comment.
type ReviewerState string

const (
	// ReviewerApproved is a reviewer whose latest verdict was an approval.
	ReviewerApproved ReviewerState = "APPROVED"
	// ReviewerChangesRequested is a reviewer waiting for changes they asked
	// for, which no later comment of theirs withdraws.
	ReviewerChangesRequested ReviewerState = "CHANGES_REQUESTED"
	// ReviewerCommented is a reviewer who has said something and passed no
	// verdict — including one whose verdict was dismissed, since dismissing a
	// review changes it into a review comment.
	ReviewerCommented ReviewerState = "COMMENTED"
)

// Reviewer is where one reviewer's review stands.
//
// One element per author of a submitted review, so that a skill asking "has
// this reviewer approved" reads the answer rather than walking reviews[] for
// the last verdict — which is the same walk in every skill that asks.
type Reviewer struct {
	// Null together with author_type, for an account that no longer
	// exists — every such review is the same reviewer here, since nothing
	// tells two of them apart.
	Author *string `json:"author"`
	// The GraphQL type of the author, as the reviews carry it: a
	// bot's standing is told from a person's without a list of bot names.
	AuthorType *string `json:"author_type"`
	// Where the reviewer stands.
	State ReviewerState `json:"state" contract:"required"`
	// When the review that settled the state was submitted, or,
	// for a reviewer who passed no verdict, when they last said anything.
	SubmittedAt string `json:"submitted_at" contract:"required"`
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
	// The instant the read began, taken before the first request
	// and truncated to the second, since GitHub's own timestamps carry no
	// finer precision. Anything submitted while the read was in flight is
	// therefore dated at or after it, and is counted again on the next run
	// rather than being lost between the two.
	FetchedAt string `json:"fetched_at" contract:"required"`
	// What is waiting on us, counted from the rest of the document
	// and from what the last run recorded. A reader takes it before anything
	// else: three empty lists is "nothing to judge", and the whole reading
	// below can be skipped.
	Pending     PendingSet `json:"pending" contract:"required"`
	Repo        string     `json:"repo" contract:"required,nonempty"`
	CurrentUser string     `json:"current_user"`
	IsOwnPR     bool       `json:"is_own_pr" contract:"required"`
	PR          PR         `json:"pr" contract:"required"`
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
	// True where the limit left reviews behind, and the ones left
	// behind are the oldest: the connection is read from its newest end, so
	// what a truncated document carries is the most recent of what every
	// reviewer said.
	ReviewsTruncated bool     `json:"reviews_truncated"`
	Reviews          []Review `json:"reviews"`
	// Where each reviewer stands, derived from the reviews above
	// and so from what was fetched: with reviews_truncated an old approval may
	// be outside the window, and this says what the reviews say rather than
	// going null.
	Reviewers         []Reviewer `json:"reviewers" contract:"required"`
	ThreadsTotalCount int        `json:"threads_total_count"`
	ThreadsTruncated  bool       `json:"threads_truncated"`
	ReviewThreads     []Thread   `json:"review_threads" contract:"required"`
	// The degradations that did not stop the document being
	// useful: one line per issue that could not be read, as owner/repo#N
	// followed by why. Empty rather than null when everything was read. What
	// was cut short by a limit is not among them — the truncation flags say
	// that, and a caller answers it by raising the limit rather than by
	// reading prose.
	Warnings []string `json:"warnings"`
}

// ParseContext reads a context file, held to Context's own declaration.
//
// The whole document rather than the part any one caller uses: the three
// commands that read a context file dereference different fields of it, and a
// subset decoded here would be a second declaration of fields Context has
// already published. What each of them goes on to hand its caller is a
// projection off the value this returns.
//
// Which fields carry a tag is settled the same way. A tag is a statement about
// the document rather than about a reader, so what the three between them
// dereference is declared as one set rather than as three overlapping ones. The
// linked issues' comments are in that set ahead of the reader they were added
// for: `ccx pr context` always writes them, so the statement is already true,
// and the reader that arrives finds the declaration rather than adding it.
// Every tag is true of what `ccx pr context` writes, pr included: the document
// always carries the object, and a reader goes straight through it to the
// number. It also has to be said out loud, because a nested declaration is
// read only where the document supplied the object it sits on — the four rules
// under pr are not a substitute for the one on pr itself.
//
// The value rules go on with the presence ones rather than after them: the
// readers refused an empty repo and a zero number before this, and presence
// alone would newly accept both. is_own_pr and review_threads carry none,
// because false and the empty list are answers `ccx pr context` writes.
//
// None of this is on Context or PR because a type's doc comment is rendered
// into a --help, where how a validator walks a document answers nothing a
// reader of one asked.
//
// file is what a refusal names, since the path is the caller's.
func ParseContext(b []byte, file string) (Context, error) {
	var c Context
	if err := contract.Unmarshal(b, &c, file); err != nil {
		return Context{}, err
	}
	return c, nil
}

// Limits stop an unusually large pull request from costing an unbounded number
// of round trips and an unbounded output.
//
// Not a correctness device: every connection here terminates on its own. A
// caller that hits one raises it and runs again, which is what the truncation
// flags are for.
type Limits struct {
	Comments int
	// Reviews bounds what is written rather than only the round trips: the
	// window asked for is what is still wanted, so a limit of two writes two.
	Reviews int
	Threads int
	// ThreadComments is per thread rather than across all of them: forty
	// threads of five comments would reach a shared limit in ordinary use, and
	// every thread after it would lose its discussion.
	ThreadComments int
	// IssueComments is per issue for the same reason, and a parent counts as
	// an issue of its own.
	IssueComments int
}

// DefaultLimits are generous enough that no pull request in this repository
// has reached one.
var DefaultLimits = Limits{Comments: 500, Reviews: 200, Threads: 300, ThreadComments: 200, IssueComments: 200}

// Fetch gathers the context of one pull request.
//
// pr is its metadata, already resolved by the caller — by number or from the
// branch — because the two ways of getting it fail differently and the caller
// is where those messages belong.
//
// change is what ReadChange already read out of git, handed in rather than
// read here: a caller that runs this twice with the limits raised runs git
// once, and the document is assembled in one place either way.
//
// stateHome is where the record of the last judgment is kept, the empty string
// meaning there is nowhere to look and everything counts. It is a parameter
// rather than a read of the environment because the environment is read once,
// at the command line, which is what lets these tests run in parallel.
func Fetch(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, pr ghapi.PullRequest, limits Limits, change Change, stateHome string) (Context, error) {
	// Before the first request rather than after the last: the point of the
	// stamp is that nothing arriving during the read is dated before it.
	fetchedAt := time.Now().UTC().Truncate(time.Second).Format(time.RFC3339)

	vars := map[string]any{
		"owner": repo.Owner, "name": repo.Name, "number": pr.Number, "headOid": pr.HeadRefOid,
		"reviews": window(limits.Reviews, 0),
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

	reviews, err := pagesBefore(ctx, limits.Reviews, prq.Reviews.Nodes, prq.Reviews.PageInfo,
		func(ctx context.Context, cursor string, want int) ([]reviewNode, pageInfo, error) {
			var page body
			vars := map[string]any{"owner": repo.Owner, "name": repo.Name, "number": pr.Number,
				"cursor": cursor, "reviews": want}
			if err := c.GraphQL(ctx, reviewsPageQuery, vars, &page); err != nil {
				return nil, pageInfo{}, fmt.Errorf("failed to fetch PR reviews page (GraphQL): %v", err)
			}
			return page.Repository.PullRequest.Reviews.Nodes, page.Repository.PullRequest.Reviews.PageInfo, nil
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

	issues, warnings, err := readIssues(ctx, c, repo, linkedIssues(pr.Body), limits.IssueComments)
	if err != nil {
		return Context{}, err
	}

	out := Context{
		FetchedAt:   fetchedAt,
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
		// What the limit left behind, which for the reviews is the oldest of
		// them: they are walked from the newest end.
		ReviewsTruncated:  prq.Reviews.TotalCount > len(reviews),
		Reviews:           make([]Review, 0, len(reviews)),
		ThreadsTotalCount: prq.ReviewThreads.TotalCount,
		ThreadsTruncated:  prq.ReviewThreads.TotalCount > len(threads),
		ReviewThreads:     make([]Thread, 0, len(threads)),
		Warnings:          warnings,
	}

	for _, n := range comments {
		out.Comments = append(out.Comments, Comment{
			Author:       n.Author.login(),
			AuthorType:   n.Author.typename(),
			Body:         n.Body,
			CreatedAt:    n.CreatedAt,
			LastEditedAt: n.LastEditedAt,
			URL:          n.URL,
			// Prefix rather than contains, so that a reply quoting one of our
			// comments does not read as one.
			IsSkillComment: strings.HasPrefix(n.Body, SkillMarker),
		})
	}
	for _, n := range reviews {
		out.Reviews = append(out.Reviews, Review{
			Author: n.Author.login(), AuthorType: n.Author.typename(),
			State: n.State, Body: n.Body, URL: n.URL, SubmittedAt: n.SubmittedAt,
			LastEditedAt: n.LastEditedAt,
		})
	}
	// After the reviews are assembled, since it is a projection of them.
	out.Reviewers = Reviewers(out.Reviews)
	for _, n := range threads {
		t, err := thread(ctx, c, n, me, out.IsOwnPR, headCommittedAt, limits.ThreadComments)
		if err != nil {
			return Context{}, err
		}
		out.ReviewThreads = append(out.ReviewThreads, t)
	}

	// Last, because it is a count over everything above it. Here rather than
	// at each caller: one of them fetches twice with the limits raised, and a
	// count taken by the caller would be the first fetch's.
	out.Pending = Pending(out, ReadSeen(stateHome, repo, pr.Number))
	return out, nil
}

// Reviewers is where each reviewer stands, read off the reviews.
//
// A pure function of what the document already carries, so that the answer is
// computed once here rather than in every skill that needs it — and so that a
// caller which fetches twice with the limits raised recomputes it by fetching
// rather than by remembering to.
//
// The rule, in GitHub's own terms: a verdict is an approval or a request for
// changes, and the latest one a reviewer passed is where they stand. A comment
// leaves it alone, and so does a dismissal — dismissing a review "changes the
// status of the review to a review comment", so it is still a review and may
// still be the latest one, but it settles nothing. A review nobody has
// submitted yet is not a review at all and is dropped, which is also what
// keeps a null submitted_at out of the document.
//
// reviews are in the order the document carries them, oldest first, which is
// what makes "the latest" the last one seen rather than a comparison of dates.
func Reviewers(reviews []Review) []Reviewer {
	// An account that no longer exists has no login, and two such reviews are
	// the same reviewer here: nothing in the document tells them apart, and
	// nothing at a consumer could either.
	const deleted = ""

	out := []Reviewer{}
	at := map[string]int{}
	for _, r := range reviews {
		if r.State == "PENDING" {
			continue
		}
		key := deleted
		if r.Author != nil {
			key = *r.Author
		}
		i, seen := at[key]
		if !seen {
			i = len(out)
			at[key] = i
			out = append(out, Reviewer{Author: r.Author, AuthorType: r.AuthorType, State: ReviewerCommented})
		}
		switch r.State {
		case string(ReviewerApproved), string(ReviewerChangesRequested):
			out[i].State, out[i].SubmittedAt = ReviewerState(r.State), r.SubmittedAt
		default:
			// Only where no verdict has been passed: a comment after an
			// approval says nothing newer about where the reviewer stands.
			if out[i].State == ReviewerCommented {
				out[i].SubmittedAt = r.SubmittedAt
			}
		}
	}
	return out
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
func readIssues(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, issues []LinkedIssue, limit int) ([]LinkedIssue, []string, error) {
	warnings := []string{}
	for i, linked := range issues {
		// Set before anything can fail, so that the empty list the document
		// promises is in the value on every path rather than only in the bytes
		// the encoder writes.
		issues[i].Comments = []IssueComment{}

		in := repo
		if linked.Repo != nil {
			var err error
			// The pattern that matched it is owner/name and nothing else, so
			// a failure here is a programmer's rather than an author's.
			if in, err = ghapi.ParseRepo(*linked.Repo); err != nil {
				return nil, nil, fmt.Errorf("failed to read the repository of %s#%d: %v", *linked.Repo, linked.Number, err)
			}
		}

		read, err := c.Issue(ctx, in, linked.Number)
		if err != nil {
			status, gone := unreadable(err)
			if !gone {
				return nil, nil, fmt.Errorf("failed to read %s#%d: %v", in, linked.Number, err)
			}
			warnings = append(warnings, fmt.Sprintf("%s#%d: the issue could not be read (HTTP %d)", in, linked.Number, status))
			continue
		}
		issues[i].Title, issues[i].Body = &read.Title, &read.Body

		// Before the parent lookup, not after it: an issue with no parent and
		// one whose parent could not be read both leave that block early, and
		// their comments would go missing with the body still in hand.
		comments, truncated, err := issueComments(ctx, c, in, read, limit)
		if err != nil {
			return nil, nil, err
		}
		issues[i].Comments = comments
		issues[i].CommentsTotalCount = read.Comments
		issues[i].CommentsTruncated = truncated

		parent, err := c.IssueParent(ctx, in, linked.Number)
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
		// Resolved once, so that the repository the comments come from is the
		// one the document names them as being in. A sub-issue may cross
		// repositories, and where the parent's repository url could not be read
		// the issue's own is a better guess than the empty owner an unparsed one
		// would send.
		if parent.Repo == (ghapi.Repo{}) {
			parent.Repo = in
		}
		parentComments, parentTruncated, err := issueComments(ctx, c, parent.Repo, *parent, limit)
		if err != nil {
			return nil, nil, err
		}
		issues[i].Parent = &IssueParent{
			Repo: elsewhere(parent.Repo, repo), Number: parent.Number,
			Title: parent.Title, Body: parent.Body,
			CommentsTotalCount: parent.Comments,
			CommentsTruncated:  parentTruncated,
			Comments:           parentComments,
		}
	}
	return issues, warnings, nil
}

// issueComments reads one issue's comments into what the document publishes,
// and says whether the limit left any behind.
//
// in is the issue's own repository rather than the pull request's, since a
// linked issue and a parent may each live somewhere else.
//
// A failure is returned rather than recorded as a warning: the body it belongs
// to has already been read, and an issue whose body is present is one a reader
// takes as read whole.
func issueComments(ctx context.Context, c *ghapi.Client, in ghapi.Repo, issue ghapi.Issue, limit int) ([]IssueComment, bool, error) {
	read, err := c.IssueComments(ctx, in, issue.Number, limit)
	if err != nil {
		return nil, false, fmt.Errorf("failed to read the comments of %s#%d (%d of them): %v",
			in, issue.Number, issue.Comments, err)
	}
	out := make([]IssueComment, 0, len(read))
	for _, comment := range read {
		out = append(out, IssueComment(comment))
	}
	return out, issue.Comments > len(out), nil
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

// pagesBefore walks a connection backwards from its end, keeping the whole in
// the order GitHub answers each page in.
//
// The other direction of pages, and deliberately not a mode of it: the limit
// here bounds what is kept rather than the round trips, so each request asks
// for what is still wanted and the count never exceeds the limit. What a limit
// drops is therefore the far end — for the reviews, the oldest, which is the
// end a reader can do without.
//
// first is the window the opening query already asked for, at the size window
// gave it.
func pagesBefore[T any](ctx context.Context, limit int, first []T, info pageInfo,
	next func(ctx context.Context, cursor string, want int) ([]T, pageInfo, error),
) ([]T, error) {
	all := first
	for info.HasPreviousPage && len(all) < limit {
		nodes, page, err := next(ctx, info.StartCursor, window(limit, len(all)))
		if err != nil {
			return nil, err
		}
		all = append(nodes, all...)
		info = page
	}
	return all, nil
}

// window is how many elements one request asks for: what the limit has left,
// capped at the hundred GraphQL allows in one page.
//
// A limit of zero or less asks for a whole page rather than for nothing, which
// with the walk's own guard leaves the opening window and nothing after it —
// what a limit of zero means on the REST side too (ghapi.GetUpTo). The clamp
// is written here rather than borrowed from there because the two hundreds are
// separate limits of separate APIs.
func window(limit, have int) int {
	const page = 100
	if want := limit - have; limit > 0 && want < page {
		return want
	}
	return page
}
