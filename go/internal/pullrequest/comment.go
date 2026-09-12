package pullrequest

import (
	"context"
	"fmt"
	"path/filepath"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Commented is where the comment ended up.
type Commented struct {
	URL string `json:"url"`
}

// ParseCommentBody reads a comment's body out of the work dir paired with a
// context file.
//
// Here rather than at the command line so that every body a run posts — a
// review's, a reply's, a comment's — is read the same way. What is in it is
// judged exactly as written, since that is the text GitHub renders. A bare
// name for the reason the two documents declare one: a path would reach round
// the directory binding that keeps parallel runs on different pull requests
// out of each other's files.
func ParseCommentBody(workDir, bodyFile string) (string, error) {
	if bodyFile == "" || bodyFile != filepath.Base(bodyFile) {
		return "", fmt.Errorf("the body file must be a bare file name, not a path: %s", bodyFile)
	}
	return resolveBody(nil, &bodyFile, workDir)
}

// PostComment posts one comment on a pull request.
//
// The body is judged before anything else, and the head is confirmed after
// that — see RequirePushedHead for what it compares. A report published from
// a checkout the pull request does not have is about code nobody else can
// see, and nothing undoes it.
func PostComment(ctx context.Context, r runner.Runner, c *ghapi.Client, dir string, target Target, body string) (Commented, error) {
	posted, err := ghapi.NewBody(body)
	if err != nil {
		return Commented{}, fmt.Errorf("the comment body: %w", err)
	}
	repo, err := target.repository()
	if err != nil {
		return Commented{}, err
	}
	if err := RequirePushedHead(ctx, r, c, dir, target, "commenting"); err != nil {
		return Commented{}, err
	}

	url, err := c.CreateIssueComment(ctx, repo, target.Number, posted)
	if err != nil {
		return Commented{}, fmt.Errorf("failed to post the comment: %v", err)
	}
	return Commented{URL: url}, nil
}
