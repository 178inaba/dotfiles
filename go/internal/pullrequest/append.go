package pullrequest

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// Appended is where the section ended up.
type Appended struct {
	// URL is the pull request the section was added to.
	URL string `json:"url"`
	// SectionFirstLine is the first line of what was appended, which is the
	// heading a write-down opens with: it says which of them this run made,
	// where the section itself is on GitHub to read.
	SectionFirstLine string `json:"section_first_line"`
}

// AppendBody adds one section to the end of a pull request's body.
//
// For the one write-down this module makes: a decision a reviewer contested
// that nothing else records, written where the next reader of the pull request
// will find it. Everything about the existing body is the writer's — see
// ghapi.Client.AppendToPullRequestBody, which reads it at the moment it writes
// — so what is left here is who may write and what may be written.
//
// The head is not confirmed, as it is before a comment or a review: those are
// about lines of a diff, and a body is about the pull request. Nothing said in
// one goes stale because the branch moved, and the write-down happens before
// the run's own push rather than after it.
func AppendBody(ctx context.Context, c *ghapi.Client, target Target, section string) (Appended, error) {
	if err := target.RequireOwn(); err != nil {
		return Appended{}, err
	}
	first := firstLine(section)
	if first == "" {
		return Appended{}, errors.New("the section is empty, so there is nothing to append")
	}
	body, err := ghapi.NewPullRequestBody(section)
	if err != nil {
		return Appended{}, fmt.Errorf("the section: %w", err)
	}
	repo, err := target.repository()
	if err != nil {
		return Appended{}, err
	}

	url, err := c.AppendToPullRequestBody(ctx, repo, target.Number, body)
	if err != nil {
		return Appended{}, fmt.Errorf("failed to append to the body: %v", err)
	}
	return Appended{URL: url, SectionFirstLine: first}, nil
}

// firstLine is the section's first line with anything in it, and the empty
// string for a section that has none.
//
// The first line rather than the first byte, because a file written by hand
// may open with a blank one — and because a section of nothing but blank lines
// is the run whose file was never written, which is worth saying rather than
// appending.
func firstLine(section string) string {
	for line := range strings.Lines(section) {
		if trimmed := strings.TrimSpace(line); trimmed != "" {
			return trimmed
		}
	}
	return ""
}
