package ghapi

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// PRState is the state GitHub reports for a pull request.
type PRState string

const (
	StateOpen   PRState = "OPEN"
	StateClosed PRState = "CLOSED"
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
	HeadRepositoryOwner struct {
		Login string `json:"login"`
	} `json:"headRepositoryOwner"`
}

func (n prNode) pullRequest() PullRequest {
	return PullRequest{
		Number:      n.Number,
		Title:       n.Title,
		Body:        n.Body,
		URL:         n.URL,
		State:       PRState(n.State),
		Author:      n.Author.Login,
		HeadRefName: n.HeadRefName,
		BaseRefName: n.BaseRefName,
		HeadRefOid:  n.HeadRefOid,
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
// request whose head is the branch checked out here.
//
// An open pull request wins over a closed or merged one with the same head, so
// that a branch reused after its first one merged resolves to the one being
// worked on; otherwise the newest wins. The narrowing against gh is that the
// local branch name is taken to be the head ref, where gh also reads
// branch.<name>.merge — the two differ only for a branch that `gh pr checkout`
// created from a fork.
func (c *Client) PullRequestForCurrentBranch(ctx context.Context, r runner.Runner, repo Repo) (PullRequest, error) {
	branch, err := currentBranch(ctx, r)
	if err != nil {
		return PullRequest{}, err
	}

	var out struct {
		Repository struct {
			PullRequests struct {
				Nodes []prNode `json:"nodes"`
			} `json:"pullRequests"`
		} `json:"repository"`
	}
	vars := map[string]any{"owner": repo.Owner, "name": repo.Name, "headRefName": branch}
	if err := c.GraphQL(ctx, prForBranchQuery, vars, &out); err != nil {
		return PullRequest{}, fmt.Errorf("look up the pull request for %s: %w", branch, err)
	}

	// A fork can have a branch of the same name, so the head has to be one this
	// repository owns for the local branch to be the thing being described.
	nodes := out.Repository.PullRequests.Nodes
	for _, n := range nodes {
		if n.HeadRepositoryOwner.Login == repo.Owner && n.State == string(StateOpen) {
			return n.pullRequest(), nil
		}
	}
	for _, n := range nodes {
		if n.HeadRepositoryOwner.Login == repo.Owner {
			return n.pullRequest(), nil
		}
	}
	return PullRequest{}, fmt.Errorf("no pull request in %s has %s as its head branch", repo, branch)
}

// currentBranch returns the branch a pull request is inferred from.
func currentBranch(ctx context.Context, r runner.Runner) (string, error) {
	out, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"branch", "--show-current"}})
	if err != nil {
		return "", fmt.Errorf("read the current branch: %w", err)
	}
	// Empty output is how git reports a detached head, and there is no branch
	// to infer a pull request from then.
	branch := strings.TrimSpace(string(out))
	if branch == "" {
		return "", errors.New("no branch is checked out, so no pull request can be inferred")
	}
	return branch, nil
}
