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
}

func (w issueWire) issue() Issue {
	// An unparseable url leaves the repository at its zero value; see
	// Issue.Repo.
	repo, _ := RepoFromAPIURL(w.RepositoryURL)
	return Issue{
		Number: w.Number, Title: w.Title, Body: w.Body,
		State: w.State, URL: w.HTMLURL, Repo: repo,
	}
}
