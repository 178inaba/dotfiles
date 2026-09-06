package ghapi

import "context"

// Viewer is the login of whoever the token authenticates.
//
// REST rather than the GraphQL viewer, because the callers that want it are
// making REST requests anyway and a login is all they need; the pull request
// context asks GraphQL for the same thing only because it is already asking
// for the rest of the pull request in the same round trip.
func (c *Client) Viewer(ctx context.Context) (string, error) {
	var w struct {
		Login string `json:"login"`
	}
	if err := c.Get(ctx, "user", &w); err != nil {
		return "", err
	}
	return w.Login, nil
}
