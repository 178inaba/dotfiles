package ghapi

import (
	"context"
	"fmt"
)

// ReviewComment is one remark anchored to a line of the diff.
type ReviewComment struct {
	Path string
	// Line is the line number on the new side of the diff.
	Line int
	Body Body
}

// ReviewSubmission is a review ready to post.
type ReviewSubmission struct {
	// CommitID is the head the review was written against, so that GitHub
	// anchors the comments to the diff the reviewer read.
	CommitID string
	// Event is APPROVE, REQUEST_CHANGES or COMMENT. Which one a verdict means
	// is the caller's to decide; sending it is this one's.
	Event string
	Body  Body
	// Comments is empty for a review that is all body.
	Comments []ReviewComment
}

// SubmitReview posts a review and answers with where it landed.
//
// Whether the comments still anchor to the diff is checked before this is
// called: by the time GitHub answers 422 the review is half posted, and there
// is nothing to be done about it from here.
func (c *Client) SubmitReview(ctx context.Context, repo Repo, number int, sub ReviewSubmission) (string, error) {
	type comment struct {
		Path string `json:"path"`
		Line int    `json:"line"`
		Body string `json:"body"`
	}
	// The payload carries the text rather than the Body: an unexported field
	// encodes as nothing at all, and a body silently dropped on the way to
	// GitHub is worse than one refused.
	payload := struct {
		CommitID string    `json:"commit_id"`
		Event    string    `json:"event"`
		Body     string    `json:"body"`
		Comments []comment `json:"comments"`
	}{CommitID: sub.CommitID, Event: sub.Event, Body: sub.Body.String(), Comments: []comment{}}
	for _, s := range sub.Comments {
		payload.Comments = append(payload.Comments,
			comment{Path: s.Path, Line: s.Line, Body: s.Body.String()})
	}

	var response struct {
		HTMLURL string `json:"html_url"`
	}
	path := fmt.Sprintf("repos/%s/pulls/%d/reviews", repo, number)
	if err := c.post(ctx, path, payload, &response); err != nil {
		return "", err
	}
	return response.HTMLURL, nil
}

const replyMutation = `
mutation($threadId: ID!, $body: String!) {
  addPullRequestReviewThreadReply(input: {pullRequestReviewThreadId: $threadId, body: $body}) {
    comment { url }
  }
}`

// ReplyToReviewThread answers one review thread and reports where the reply
// landed.
//
// The thread is named by the opaque id GitHub gives it, and nothing here
// checks that it is the thread the caller meant: which thread an entry means
// is resolved from a path and a line before this is reached.
func (c *Client) ReplyToReviewThread(ctx context.Context, threadID string, body Body) (string, error) {
	var out struct {
		AddPullRequestReviewThreadReply struct {
			Comment struct {
				URL string `json:"url"`
			} `json:"comment"`
		} `json:"addPullRequestReviewThreadReply"`
	}
	vars := map[string]any{"threadId": threadID, "body": body.String()}
	if err := c.GraphQL(ctx, replyMutation, vars, &out); err != nil {
		return "", err
	}
	return out.AddPullRequestReviewThreadReply.Comment.URL, nil
}
