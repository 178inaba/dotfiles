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
	Title  string
	Body   string
	State  string
	URL    string
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
	Number  int    `json:"number"`
	Title   string `json:"title"`
	Body    string `json:"body"`
	State   string `json:"state"`
	HTMLURL string `json:"html_url"`
	// RepositoryURL rather than the repository object, because only this one is
	// required by the issue schema.
	RepositoryURL string `json:"repository_url"`
	Comments      int    `json:"comments"`
}

func (w issueWire) issue() Issue {
	// An unparseable url leaves the repository at its zero value; see
	// Issue.Repo.
	repo, _ := RepoFromAPIURL(w.RepositoryURL)
	return Issue{
		Number: w.Number, Title: w.Title, Body: w.Body,
		State: w.State, URL: w.HTMLURL, Repo: repo, Comments: w.Comments,
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
