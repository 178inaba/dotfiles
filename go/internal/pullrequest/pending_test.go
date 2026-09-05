package pullrequest_test

import (
	"encoding/json/v2"
	"slices"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/pullrequest"
)

// The document the count is taken from. Everything the rules turn on is here
// once — an approved review, a dismissed one, one of ours, an empty one, a
// skill comment, a bot's comment, our own comment — so that a case below says
// only what it is about.
func countable() pullrequest.Context {
	user, bot := "User", "Bot"
	return pullrequest.Context{
		CurrentUser: "me",
		Reviews: []pullrequest.Review{
			{Author: new("reviewer"), AuthorType: &user, State: "CHANGES_REQUESTED", Body: "please fix",
				URL: "https://example.com/r1", SubmittedAt: "2026-01-05T00:00:00Z"},
			// Ours: we do not answer our own reviews.
			{Author: new("me"), AuthorType: &user, State: "COMMENTED", Body: "a note of mine",
				URL: "https://example.com/r2", SubmittedAt: "2026-01-05T00:00:00Z"},
			{Author: new("reviewer"), AuthorType: &user, State: "APPROVED", Body: "looks good",
				URL: "https://example.com/r3", SubmittedAt: "2026-01-05T00:00:00Z"},
			{Author: new("reviewer"), AuthorType: &user, State: "DISMISSED", Body: "withdrawn",
				URL: "https://example.com/r4", SubmittedAt: "2026-01-05T00:00:00Z"},
			// "See inline": a review with no body says nothing to answer, its
			// remarks being the threads it left.
			{Author: new("reviewer"), AuthorType: &user, State: "COMMENTED", Body: "",
				URL: "https://example.com/r5", SubmittedAt: "2026-01-05T00:00:00Z"},
		},
		Comments: []pullrequest.Comment{
			{Author: new("reviewer"), AuthorType: &user, Body: "a remark",
				CreatedAt: "2026-01-05T00:00:00Z", URL: "https://example.com/c1"},
			{Author: new("me"), AuthorType: &user, Body: "<!-- review-response -->\ndone",
				CreatedAt: "2026-01-05T00:00:00Z", URL: "https://example.com/c2", IsSkillComment: true},
			{Author: new("ci"), AuthorType: &bot, Body: "the build failed",
				CreatedAt: "2026-01-05T00:00:00Z", URL: "https://example.com/c3"},
			// Ours and unmarked: the author's own follow-up is a remark too,
			// which is why the marker rather than the login is what excludes.
			{Author: new("me"), AuthorType: &user, Body: "one more thing",
				CreatedAt: "2026-01-05T00:00:00Z", URL: "https://example.com/c4"},
		},
		ReviewThreads: []pullrequest.Thread{
			{
				Path: "a.go", Line: new(3), OriginalLine: new(3), OpenedBy: new("reviewer"),
				Ball:     pullrequest.BallMine,
				Comments: []pullrequest.ThreadComment{{Author: new("reviewer"), Body: "here", URL: "https://example.com/t1"}},
			},
			{
				Path: "b.go", Line: new(9), OriginalLine: new(9), OpenedBy: new("reviewer"),
				Ball:     pullrequest.BallTheirs,
				Comments: []pullrequest.ThreadComment{{Author: new("reviewer"), Body: "there", URL: "https://example.com/t2"}},
			},
			{
				Path: "c.go", OriginalLine: new(12), OpenedBy: new("reviewer"),
				Ball:     pullrequest.BallNone,
				Comments: []pullrequest.ThreadComment{{Author: new("reviewer"), Body: "resolved", URL: "https://example.com/t3"}},
			},
		},
	}
}

// urls flattens a list to what it points at, which is what a case is about:
// every element carries the same fields, and comparing whole structs would put
// the fixture in every want.
func urls[T any](list []T, of func(T) string) []string {
	out := make([]string, 0, len(list))
	for _, e := range list {
		out = append(out, of(e))
	}
	return out
}

func reviewURL(r pullrequest.PendingReview) string   { return r.URL }
func commentURL(c pullrequest.PendingComment) string { return c.URL }

func TestPendingCountsWithoutASince(t *testing.T) {
	t.Parallel()

	got := pullrequest.Pending(countable(), nil)

	if got.Since != nil {
		t.Errorf("since = %v, want null: with no state file everything counts", got.Since)
	}
	// r2 is ours, r3 approved, r4 dismissed, r5 has no body.
	if diff := cmp.Diff([]string{"https://example.com/r1"}, urls(got.Reviews, reviewURL)); diff != "" {
		t.Errorf("reviews (-want +got):\n%s", diff)
	}
	// c2 carries the marker; the bot's and our own unmarked one both count.
	want := []string{"https://example.com/c1", "https://example.com/c3", "https://example.com/c4"}
	if diff := cmp.Diff(want, urls(got.Comments, commentURL)); diff != "" {
		t.Errorf("comments (-want +got):\n%s", diff)
	}
}

func TestPendingCountsFromASince(t *testing.T) {
	t.Parallel()

	// A review submitted before the watermark but edited after it, a comment
	// made in the very second of it, and one made after.
	c := countable()
	c.Reviews = append(c.Reviews, pullrequest.Review{
		Author: new("reviewer"), State: "COMMENTED", Body: "rewritten since",
		URL: "https://example.com/r6", SubmittedAt: "2026-01-05T00:00:00Z",
		LastEditedAt: new("2026-01-20T00:00:00Z"),
	})
	c.Comments = append(c.Comments,
		pullrequest.Comment{Author: new("reviewer"), Body: "the same second",
			CreatedAt: "2026-01-10T00:00:00Z", URL: "https://example.com/c5"},
		pullrequest.Comment{Author: new("reviewer"), Body: "after",
			CreatedAt: "2026-01-11T00:00:00Z", URL: "https://example.com/c6"},
	)

	got := pullrequest.Pending(c, new("2026-01-10T00:00:00Z"))

	if got.Since == nil || *got.Since != "2026-01-10T00:00:00Z" {
		t.Errorf("since = %v, want the state file's value", got.Since)
	}
	// r1 was submitted before the watermark and never edited; r6 was edited
	// after it, which is a remark again.
	if diff := cmp.Diff([]string{"https://example.com/r6"}, urls(got.Reviews, reviewURL)); diff != "" {
		t.Errorf("reviews (-want +got):\n%s", diff)
	}
	// A timestamp equal to the watermark counts: the same second is read twice
	// rather than lost.
	want := []string{"https://example.com/c5", "https://example.com/c6"}
	if diff := cmp.Diff(want, urls(got.Comments, commentURL)); diff != "" {
		t.Errorf("comments (-want +got):\n%s", diff)
	}
}

// A timestamp that will not parse is counted rather than dropped, which is the
// direction every other degradation here takes: reading a remark twice costs a
// reading, and losing one costs the answer.
func TestPendingCountsWhatItCannotDate(t *testing.T) {
	t.Parallel()

	c := countable()
	c.Comments = append(c.Comments, pullrequest.Comment{
		Author: new("reviewer"), Body: "undated", CreatedAt: "not a date", URL: "https://example.com/c9",
	})

	got := pullrequest.Pending(c, new("2026-06-01T00:00:00Z"))

	if !slices.Contains(urls(got.Comments, commentURL), "https://example.com/c9") {
		t.Errorf("comments = %v, want the undated one among them", urls(got.Comments, commentURL))
	}
}

func TestPendingTakesTheThreadsItIsOurMoveOn(t *testing.T) {
	t.Parallel()

	got := pullrequest.Pending(countable(), nil)

	want := []pullrequest.PendingThread{{
		Path: "a.go", Line: new(3), OriginalLine: new(3),
		OpenedBy: new("reviewer"), URL: "https://example.com/t1",
	}}
	if diff := cmp.Diff(want, got.Threads); diff != "" {
		t.Errorf("threads (-want +got):\n%s", diff)
	}
}

// The three lists are empty arrays rather than null, so that a reader counts
// them without telling absence from emptiness.
func TestPendingIsNeverNull(t *testing.T) {
	t.Parallel()

	got := pullrequest.Pending(pullrequest.Context{}, nil)

	if got.Threads == nil || got.Reviews == nil || got.Comments == nil {
		t.Errorf("threads/reviews/comments = %v/%v/%v, want empty lists", got.Threads, got.Reviews, got.Comments)
	}
}

// No body text anywhere under the count: whether there is anything to judge is
// answered before anything is read, and a body here would be read to decide.
func TestPendingCarriesNoBodyText(t *testing.T) {
	t.Parallel()

	b, err := json.Marshal(pullrequest.Pending(countable(), nil))
	if err != nil {
		t.Fatalf("marshal the count: %v", err)
	}
	if strings.Contains(string(b), "body") {
		t.Errorf("the count carries a body: %s", b)
	}
}
