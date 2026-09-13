package ghapi

import (
	"context"
	"fmt"
	"time"
)

// Viewer is the login of whoever the token authenticates, cached for ttl when
// ttl is positive.
//
// REST rather than the GraphQL viewer, because the callers that want it are
// making REST requests anyway and a login is all they need; the pull request
// reads ask GraphQL for the same thing only because they are already asking
// for the pull request in the same round trip.
//
// An empty login is an error rather than an answer: every caller goes on to
// compare it against an author or to send it as an assignee, and "" quietly
// matches nobody.
func (c *Client) Viewer(ctx context.Context, ttl time.Duration) (string, error) {
	var w struct {
		Login string `json:"login"`
	}
	var err error
	if ttl > 0 {
		err = c.GetCached(ctx, "user", ttl, &w)
	} else {
		err = c.Get(ctx, "user", &w)
	}
	if err != nil {
		return "", fmt.Errorf("fetch the authenticated user: %w", err)
	}
	if w.Login == "" {
		return "", fmt.Errorf("the authenticated user has no login")
	}
	return w.Login, nil
}
