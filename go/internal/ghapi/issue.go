package ghapi

import (
	"context"
	"fmt"
)

// Issue is a GitHub issue: what it says, and which repository it says it in.
//
// Here rather than in one of the packages that read issues because two of them
// do — the sub-issue tree and the pull request context — and a second
// implementation of the same endpoint is how they come to disagree about what
// a missing parent means.
type Issue struct {
	Number int
	// ID is GitHub's own integer for the issue, which is not the number: the
	// sub-issue and dependency endpoints address an issue by this and reject
	// the number. Wide enough for what GitHub issues today, which has passed
	// what an int32 holds.
	ID    int64
	Title string
	Body  string
	State string
	URL   string
	// UpdatedAt is when the issue last changed, in GitHub's own spelling, so
	// that a caller holding a snapshot can tell whether it still describes the
	// issue by comparing the strings.
	UpdatedAt string
	// Labels and Assignees are what the issue carries by name, which is what a
	// caller that has just asked for some compares its request against:
	// GitHub applies neither for a user without push access, and says so only
	// by returning the issue without them.
	Labels    []string
	Assignees []string
	// Repo is the repository the issue lives in, which need not be the one it
	// was asked about: a sub-issue may cross repositories within an owner. The
	// zero value is a repository url that could not be read, which each caller
	// answers for in the shape of its own contract.
	Repo Repo
	// Comments is how many the issue has, which arrives with the body rather
	// than being counted: a caller that fetched a bounded number of them
	// measures the truncation against this without a second request.
	Comments int
}

// IssueComment is one comment on an issue.
type IssueComment struct {
	// Author and AuthorType are nil together, for a comment whose author has
	// since been deleted — which GitHub reports as no user at all, and which a
	// login of "" would be indistinguishable from.
	Author     *string
	AuthorType *string
	Body       string
	CreatedAt  string
	URL        string
}

// Issue reads one issue.
func (c *Client) Issue(ctx context.Context, repo Repo, number int) (Issue, error) {
	var w issueWire
	if err := c.Get(ctx, fmt.Sprintf("repos/%s/issues/%d", repo, number), &w); err != nil {
		return Issue{}, err
	}
	return w.issue(), nil
}

// IssueChange is what a create or an edit asks for.
//
// Every field is a pointer because an edit says what to change and leaves the
// rest alone: an empty body and no body at all are different requests, and a
// nil label set is "do not touch the labels" where an empty one clears them.
type IssueChange struct {
	Title     *string
	Body      *Body
	Labels    *[]string
	Assignees *[]string
}

func (ch IssueChange) request() map[string]any {
	req := map[string]any{}
	if ch.Title != nil {
		req["title"] = *ch.Title
	}
	if ch.Body != nil {
		// The text rather than the Body: an unexported field encodes as
		// nothing at all, and a body silently dropped on the way to GitHub is
		// worse than one refused.
		req["body"] = ch.Body.String()
	}
	if ch.Labels != nil {
		req["labels"] = *ch.Labels
	}
	if ch.Assignees != nil {
		req["assignees"] = *ch.Assignees
	}
	return req
}

// CreateIssue opens an issue and answers with the issue GitHub stored.
//
// The response is the read-back a caller needs: GitHub returns the issue as it
// saved it, so what it declined to apply is visible without a second request.
func (c *Client) CreateIssue(ctx context.Context, repo Repo, ch IssueChange) (Issue, error) {
	var w issueWire
	if err := c.Post(ctx, fmt.Sprintf("repos/%s/issues", repo), ch.request(), &w); err != nil {
		return Issue{}, err
	}
	return w.issue(), nil
}

// EditIssue changes an issue and answers with the issue GitHub stored.
//
// A label set replaces the one on the issue rather than adding to it, which is
// what the endpoint does; a caller adding one sends the existing ones too.
func (c *Client) EditIssue(ctx context.Context, repo Repo, number int, ch IssueChange) (Issue, error) {
	var w issueWire
	if err := c.Patch(ctx, fmt.Sprintf("repos/%s/issues/%d", repo, number), ch.request(), &w); err != nil {
		return Issue{}, err
	}
	return w.issue(), nil
}

// AddSubIssue makes one issue a sub-issue of another and answers with the
// parent as it stands afterwards.
//
// The child is named by its integer id rather than its number, which is what
// the endpoint takes. The response is the parent, so a caller tracking when
// the parent last changed reads it here rather than fetching it again — the
// link moves it.
func (c *Client) AddSubIssue(ctx context.Context, repo Repo, parent int, subID int64) (Issue, error) {
	var w issueWire
	path := fmt.Sprintf("repos/%s/issues/%d/sub_issues", repo, parent)
	if err := c.Post(ctx, path, map[string]any{"sub_issue_id": subID}, &w); err != nil {
		return Issue{}, err
	}
	return w.issue(), nil
}

// AddBlockedBy records that an issue is waiting for another, and answers with
// the waiting issue as it stands afterwards.
//
// Same shape as AddSubIssue: the issue waited for is named by its integer id,
// and the response is the issue in the path.
func (c *Client) AddBlockedBy(ctx context.Context, repo Repo, blocked int, byID int64) (Issue, error) {
	var w issueWire
	path := fmt.Sprintf("repos/%s/issues/%d/dependencies/blocked_by", repo, blocked)
	if err := c.Post(ctx, path, map[string]any{"issue_id": byID}, &w); err != nil {
		return Issue{}, err
	}
	return w.issue(), nil
}

// CreateIssueComment posts a comment on an issue and answers with its url.
//
// A pull request is an issue here: this is the endpoint a comment on either
// one goes to, which is why a comment on a pull request comes through here
// rather than through a writer of its own.
func (c *Client) CreateIssueComment(ctx context.Context, repo Repo, number int, body Body) (string, error) {
	var w issueCommentWire
	path := fmt.Sprintf("repos/%s/issues/%d/comments", repo, number)
	if err := c.Post(ctx, path, map[string]any{"body": body.String()}, &w); err != nil {
		return "", err
	}
	return w.HTMLURL, nil
}

// IssueParent reads the issue an issue is a sub-issue of.
//
// (nil, nil) is an issue that is nobody's child, which the endpoint reports as
// a 404. Every other failure comes back as an error rather than as "has none",
// because the two callers degrade differently: the tree records a warning and
// carries on, while the pull request context decides from the status.
func (c *Client) IssueParent(ctx context.Context, repo Repo, number int) (*Issue, error) {
	var w issueWire
	err := c.Get(ctx, fmt.Sprintf("repos/%s/issues/%d/parent", repo, number), &w)
	if IsNotFound(err) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	issue := w.issue()
	return &issue, nil
}

// IssueComments reads an issue's comments, oldest first, and no more of them
// than limit leaves room for.
//
// Oldest first is GitHub's own order on this endpoint — it sorts by ascending
// id and takes no sort parameter, unlike the repository-wide comment list — so
// nothing here reorders what arrives. What limit means is GetUpTo's to say; the
// page size follows from it, since asking for more than will be kept is a
// larger response for nothing.
func (c *Client) IssueComments(ctx context.Context, repo Repo, number, limit int) ([]IssueComment, error) {
	perPage := 100
	if limit > 0 && limit < perPage {
		perPage = limit
	}
	ws, err := GetUpTo[issueCommentWire](ctx, c,
		fmt.Sprintf("repos/%s/issues/%d/comments?per_page=%d", repo, number, perPage), limit)
	if err != nil {
		return nil, err
	}
	// Never nil: the document this ends up in publishes an empty list, and a
	// caller that had to normalise it would be the second place deciding that.
	out := make([]IssueComment, 0, len(ws))
	for _, w := range ws {
		out = append(out, w.comment())
	}
	return out, nil
}

// issueWire is the GitHub issue object, as much of it as Issue carries. The
// same shape arrives from the issue endpoint, the parent endpoint and both
// list endpoints.
type issueWire struct {
	Number    int    `json:"number"`
	ID        int64  `json:"id"`
	Title     string `json:"title"`
	Body      string `json:"body"`
	State     string `json:"state"`
	UpdatedAt string `json:"updated_at"`
	HTMLURL   string `json:"html_url"`
	// RepositoryURL rather than the repository object, because only this one is
	// required by the issue schema.
	RepositoryURL string `json:"repository_url"`
	Comments      int    `json:"comments"`
	Labels        []struct {
		Name string `json:"name"`
	} `json:"labels"`
	Assignees []struct {
		Login string `json:"login"`
	} `json:"assignees"`
}

func (w issueWire) issue() Issue {
	// An unparseable url leaves the repository at its zero value; see
	// Issue.Repo.
	repo, _ := RepoFromAPIURL(w.RepositoryURL)
	// Nil rather than empty where GitHub sent none, so that "the issue carries
	// no label" and "the response did not say" stay one answer: the endpoint
	// always sends the arrays.
	var labels, assignees []string
	for _, l := range w.Labels {
		labels = append(labels, l.Name)
	}
	for _, a := range w.Assignees {
		assignees = append(assignees, a.Login)
	}
	return Issue{
		Number: w.Number, ID: w.ID, Title: w.Title, Body: w.Body,
		State: w.State, UpdatedAt: w.UpdatedAt, URL: w.HTMLURL,
		Repo: repo, Comments: w.Comments, Labels: labels, Assignees: assignees,
	}
}

// issueCommentWire is the GitHub issue comment object, as much of it as
// IssueComment carries.
type issueCommentWire struct {
	// User is absent for a deleted author, which is the only reason it is a
	// pointer.
	User *struct {
		Login string `json:"login"`
		Type  string `json:"type"`
	} `json:"user"`
	Body      string `json:"body"`
	CreatedAt string `json:"created_at"`
	HTMLURL   string `json:"html_url"`
}

func (w issueCommentWire) comment() IssueComment {
	out := IssueComment{Body: w.Body, CreatedAt: w.CreatedAt, URL: w.HTMLURL}
	if w.User != nil {
		out.Author, out.AuthorType = &w.User.Login, &w.User.Type
	}
	return out
}
