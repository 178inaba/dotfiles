package ghapi

// encoding/json rather than the module's usual encoding/json/v2, and in a file
// of its own so that the two never sit side by side under the same name.
//
// This is the decoder go-gh applies to every response it decodes itself, so
// using it here keeps one page of a collection and one whole object arriving
// under the same rules — which matters because v2 rejects two things v1
// accepts, invalid UTF-8 and duplicate object names, and a comment body is not
// ours to be strict about. What this module writes is still encoded with v2.
import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"regexp"

	"github.com/cli/go-gh/v2/pkg/api"
)

// linkRel finds one entry of an RFC 5988 Link header.
var linkRel = regexp.MustCompile(`<([^>]+)>;\s*rel="([^"]+)"`)

// GetAll follows a REST collection to its end and returns every element.
//
// It is `gh api --paginate`: GitHub reports the next page in a Link header, and
// go-gh passes an absolute URL through unchanged, so the header's own value is
// the next request. Callers keep whatever per_page the shell asked for by
// putting it in path, since that is part of how many round trips a command
// makes rather than something to standardise here.
//
// A free function rather than a method because a method cannot introduce a type
// parameter.
func GetAll[T any](ctx context.Context, c *Client, path string) ([]T, error) {
	var all []T
	for path != "" {
		page, next, err := getPage[T](ctx, c, path)
		if err != nil {
			return nil, err
		}
		all = append(all, page...)
		path = next
	}
	return all, nil
}

// GetUpTo is GetAll with room for only so many elements.
//
// The limit is checked before each further request rather than applied to the
// result, so the first page always arrives whole and a final count may exceed
// the limit — the same rule pullrequest.pages follows on the GraphQL side, and
// the reason the two are the same is that MAX_COMMENTS and MAX_ISSUE_COMMENTS
// are read as one set by whoever raises them. A limit of zero or less therefore
// means the first page and nothing after it, rather than no limit at all;
// GetAll is what asks for everything.
//
// The truncation a caller reports is the collection's own total against what
// arrived, not anything this returns: a walk that stopped and a collection that
// ended look alike from here.
func GetUpTo[T any](ctx context.Context, c *Client, path string, limit int) ([]T, error) {
	var all []T
	for path != "" {
		page, next, err := getPage[T](ctx, c, path)
		if err != nil {
			return nil, err
		}
		all = append(all, page...)
		if len(all) >= limit {
			break
		}
		path = next
	}
	return all, nil
}

// getPage is one round trip: the elements it answered with, and where the page
// after it lives.
func getPage[T any](ctx context.Context, c *Client, path string) ([]T, string, error) {
	resp, err := c.rest.RequestWithContext(ctx, http.MethodGet, path, nil)
	if err != nil {
		return nil, "", err
	}
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		defer resp.Body.Close()
		return nil, "", api.HandleHTTPError(resp)
	}
	b, err := bodyOf(resp)
	if err != nil {
		return nil, "", fmt.Errorf("ghapi: read %s: %w", path, err)
	}
	var page []T
	if err := json.Unmarshal(b, &page); err != nil {
		return nil, "", fmt.Errorf("ghapi: decode %s: %w", path, err)
	}
	return page, nextPage(resp.Header.Get("Link")), nil
}

// nextPage returns the URL of the page after the one whose Link header this is,
// or empty at the end of the collection.
func nextPage(link string) string {
	for _, m := range linkRel.FindAllStringSubmatch(link, -1) {
		if m[2] == "next" {
			return m[1]
		}
	}
	return ""
}
