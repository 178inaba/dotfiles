package ghapi

import (
	"context"
	"errors"
	"fmt"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// PRState is the state GitHub reports for a pull request.
type PRState string

const (
	// StateOpen is GraphQL's spelling, as are the other two, and is also what
	// `gh pr view --json state` printed.
	StateOpen PRState = "OPEN"
	// StateClosed is a pull request closed without merging. REST calls a
	// merged one closed as well and puts the distinction in a separate field,
	// so telling these two apart means coming through GraphQL.
	StateClosed PRState = "CLOSED"
	// StateMerged is a pull request that was merged.
	StateMerged PRState = "MERGED"
)

// PullRequest is the metadata every command that starts from a pull request
// reads: the fields the shell asked `gh pr view --json` for, flattened the way
// the output contracts print them.
type PullRequest struct {
	Number int
	Title  string
	Body   string
	URL    string
	State  PRState
	// Author is the login, or empty for an account that no longer exists —
	// which is also what gh produced, since it flattened a null author into a
	// struct of empty strings.
	Author      string
	HeadRefName string
	BaseRefName string
	HeadRefOid  string
	// ReviewDecision is GitHub's summary of the review state: APPROVED,
	// CHANGES_REQUESTED, REVIEW_REQUIRED, or empty where no review is required.
	// A plain string because GraphQL leaves the set open and it is nullable, so
	// a value this module has never heard of arrives intact rather than as an
	// error, and a null arrives as the empty string.
	ReviewDecision string
	// IsDraft reports a pull request that is not asking to be reviewed yet.
	// Separate from ReviewDecision because a draft has one of its own.
	IsDraft bool
}

// prFields is the selection both queries make.
//
// One constant because the two have to agree: a command handed a number and a
// command inferring one from the branch build the same output from what comes
// back, and a field present in only one of them would go missing on whichever
// path the caller happened to take.
const prFields = `
      number
      title
      body
      url
      state
      author { login }
      headRefName
      baseRefName
      headRefOid
      reviewDecision
      isDraft
`

const prByNumberQuery = `
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {` + prFields + `    }
  }
}`

// prForBranchQuery reconstructs what gh does for `gh pr view` with no argument:
// every state rather than only open ones, newest first, and a page large enough
// that a branch's whole history of pull requests is in it.
const prForBranchQuery = `
query($owner: String!, $name: String!, $headRefName: String!) {
  repository(owner: $owner, name: $name) {
    pullRequests(headRefName: $headRefName, first: 30, orderBy: {field: CREATED_AT, direction: DESC}) {
      nodes {` + prFields + `      headRepositoryOwner { login }
      }
    }
  }
}`

// prNode is the GraphQL shape of a pull request, kept apart from PullRequest so
// that the nesting GitHub returns does not become the shape the rest of the
// module passes around.
type prNode struct {
	Number int    `json:"number"`
	Title  string `json:"title"`
	Body   string `json:"body"`
	URL    string `json:"url"`
	State  string `json:"state"`
	Author struct {
		Login string `json:"login"`
	} `json:"author"`
	HeadRefName         string `json:"headRefName"`
	BaseRefName         string `json:"baseRefName"`
	HeadRefOid          string `json:"headRefOid"`
	ReviewDecision      string `json:"reviewDecision"`
	IsDraft             bool   `json:"isDraft"`
	HeadRepositoryOwner struct {
		Login string `json:"login"`
	} `json:"headRepositoryOwner"`
}

func (n prNode) pullRequest() PullRequest {
	return PullRequest{
		Number:         n.Number,
		Title:          n.Title,
		Body:           n.Body,
		URL:            n.URL,
		State:          PRState(n.State),
		Author:         n.Author.Login,
		HeadRefName:    n.HeadRefName,
		BaseRefName:    n.BaseRefName,
		HeadRefOid:     n.HeadRefOid,
		ReviewDecision: n.ReviewDecision,
		IsDraft:        n.IsDraft,
	}
}

// PullRequest returns the metadata of one pull request.
//
// GraphQL, because `gh pr view --json` was a GraphQL query too: its state is
// OPEN, CLOSED or MERGED where REST answers open or closed with a separate
// merged flag, and its url is the html one. Reading this from REST would change
// values the output contracts already publish.
func (c *Client) PullRequest(ctx context.Context, repo Repo, number int) (PullRequest, error) {
	var out struct {
		Repository struct {
			PullRequest prNode `json:"pullRequest"`
		} `json:"repository"`
	}
	vars := map[string]any{"owner": repo.Owner, "name": repo.Name, "number": number}
	if err := c.GraphQL(ctx, prByNumberQuery, vars, &out); err != nil {
		return PullRequest{}, fmt.Errorf("look up %s#%d: %w", repo, number, err)
	}
	return out.Repository.PullRequest.pullRequest(), nil
}

// PullRequestForCurrentBranch is `gh pr view` with no argument: the pull
// request whose head is the branch checked out in dir.
//
// dir is explicit rather than inherited from the process, because a caller that
// was given a repository to work on has to be able to say so — and because a
// default of the process's own directory is one a test forgets to override and
// then passes against whatever repository the test binary happens to run in.
//
// The narrowing against gh is that the local branch name is taken to be the
// head ref, where gh also reads branch.<name>.merge — the two differ only for a
// branch that `gh pr checkout` created from a fork. A caller that reads that
// config itself calls PullRequestForBranch instead.
func (c *Client) PullRequestForCurrentBranch(ctx context.Context, r runner.Runner, dir string, repo Repo) (PullRequest, error) {
	branch, err := currentBranch(ctx, r, dir)
	if err != nil {
		return PullRequest{}, err
	}
	return c.PullRequestForBranch(ctx, repo, branch, repo.Owner)
}

// PullRequestForBranch returns the pull request in repo whose head is
// headRefName.
//
// An open pull request wins over a closed or merged one with the same head, so
// that a branch reused after its first one merged resolves to the one being
// worked on; otherwise the newest wins.
//
// headOwner is the account the head has to belong to, because a fork can have a
// branch of the same name and only one of them is the thing being described.
// Empty accepts any owner, which is what a head that lives on a fork needs —
// the pull request itself is still one of repo's.
func (c *Client) PullRequestForBranch(ctx context.Context, repo Repo, headRefName, headOwner string) (PullRequest, error) {
	var out struct {
		Repository struct {
			PullRequests struct {
				Nodes []prNode `json:"nodes"`
			} `json:"pullRequests"`
		} `json:"repository"`
	}
	vars := map[string]any{"owner": repo.Owner, "name": repo.Name, "headRefName": headRefName}
	if err := c.GraphQL(ctx, prForBranchQuery, vars, &out); err != nil {
		return PullRequest{}, fmt.Errorf("look up the pull request for %s: %w", headRefName, err)
	}

	owned := func(n prNode) bool {
		return headOwner == "" || n.HeadRepositoryOwner.Login == headOwner
	}
	nodes := out.Repository.PullRequests.Nodes
	for _, n := range nodes {
		if owned(n) && n.State == string(StateOpen) {
			return n.pullRequest(), nil
		}
	}
	for _, n := range nodes {
		if owned(n) {
			return n.pullRequest(), nil
		}
	}
	return PullRequest{}, fmt.Errorf("no pull request in %s has %s as its head branch", repo, headRefName)
}

// currentBranch returns the branch a pull request is inferred from.
func currentBranch(ctx context.Context, r runner.Runner, dir string) (string, error) {
	branch, err := runner.Git(ctx, r, dir, "branch", "--show-current")
	if err != nil {
		return "", fmt.Errorf("read the current branch: %w", err)
	}
	// Empty output is how git reports a detached head, and there is no branch
	// to infer a pull request from then.
	if branch == "" {
		return "", errors.New("no branch is checked out, so no pull request can be inferred")
	}
	return branch, nil
}
