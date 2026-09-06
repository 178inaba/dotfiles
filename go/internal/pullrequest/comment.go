package pullrequest

import (
	"context"
	"fmt"
	"path/filepath"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// The one writer of a marked comment.
//
// Whether a comment on a pull request is one of ours is decided by the marker
// at the front of it, and the reading side keys on a constant. Posting through
// here rather than through gh at the point of use is what keeps the two ends
// of that from parting: the same constant writes it and recognises it.

// Mark names the marker a posted comment carries.
//
// A name rather than the marker itself, so that a caller cannot invent one:
// the reading side recognises exactly one string, and a comment carrying
// anything else would count as somebody else's remark for ever.
type Mark string

// MarkReviewResponse is the only mark, and resolves to SkillMarker.
const MarkReviewResponse Mark = "review-response"

// ParseMark reads what a caller asked to mark a comment with.
//
// Separate from posting so that a caller can settle the mark before it goes
// looking for a body: a run whose mark is wrong reports a missing file
// otherwise, which sends the reader to fix the wrong thing.
func ParseMark(name string) (Mark, error) {
	m := Mark(name)
	if _, err := m.marker(); err != nil {
		return "", err
	}
	return m, nil
}

func (m Mark) marker() (string, error) {
	if m == MarkReviewResponse {
		return SkillMarker, nil
	}
	return "", fmt.Errorf("unknown mark: %s (expected: %s)", m, MarkReviewResponse)
}

// Commented is where the comment ended up.
type Commented struct {
	URL string `json:"url"`
}

// ParseCommentBody reads a comment's body out of the work dir paired with a
// context file.
//
// Here rather than at the command line so that every body a run posts — a
// review's, a reply's, a comment's — is read the same way. What is in it is
// judged where the marker has been prepended, since that is the text GitHub
// renders. A bare name for the reason the two documents declare one: a path
// would reach round the directory binding that keeps parallel runs on
// different pull requests out of each other's files.
func ParseCommentBody(workDir, bodyFile string) (string, error) {
	if bodyFile == "" || bodyFile != filepath.Base(bodyFile) {
		return "", fmt.Errorf("the body file must be a bare file name, not a path: %s", bodyFile)
	}
	return resolveBody(nil, &bodyFile, workDir)
}

// PostComment posts one comment on a pull request, marked as ours.
//
// The marker goes on the first line and a blank line follows it, so that the
// markdown after it renders as it was written rather than being folded into
// the comment's opening line.
//
// The mark is resolved before anything else, then the joined text is judged,
// and the head is confirmed after that — see RequirePushedHead for what it
// compares. A report published from a checkout the pull request does not have
// is about code nobody else can see, and nothing undoes it.
//
// What is judged is the joined text rather than what the caller wrote, because
// that is what GitHub renders. The marker holds nothing the judgement is about,
// so joining first costs the caller no accuracy in the refusal.
func PostComment(ctx context.Context, r runner.Runner, c *ghapi.Client, dir string, target Target, mark Mark, body string) (Commented, error) {
	marker, err := mark.marker()
	if err != nil {
		return Commented{}, err
	}
	marked, err := ghapi.NewBody(marker + "\n\n" + body)
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

	url, err := c.CreateIssueComment(ctx, repo, target.Number, marked)
	if err != nil {
		return Commented{}, fmt.Errorf("failed to post the comment: %v", err)
	}
	return Commented{URL: url}, nil
}
