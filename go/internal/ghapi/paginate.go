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
		resp, err := c.rest.RequestWithContext(ctx, http.MethodGet, path, nil)
		if err != nil {
			return nil, err
		}
		if resp.StatusCode < 200 || resp.StatusCode >= 300 {
			defer resp.Body.Close()
			return nil, api.HandleHTTPError(resp)
		}
		b, err := bodyOf(resp)
		if err != nil {
			return nil, fmt.Errorf("ghapi: read %s: %w", path, err)
		}
		var page []T
		if err := json.Unmarshal(b, &page); err != nil {
			return nil, fmt.Errorf("ghapi: decode %s: %w", path, err)
		}
		all = append(all, page...)
		path = nextPage(resp.Header.Get("Link"))
	}
	return all, nil
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
