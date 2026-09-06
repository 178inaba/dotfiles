package ghapi_test

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
)

var reviewRepo = ghapi.Repo{Owner: "o", Name: "r"}

func TestSubmitReview(t *testing.T) {
	t.Parallel()

	var gotPath, gotMethod string
	var sent struct {
		CommitID string `json:"commit_id"`
		Event    string `json:"event"`
		Body     string `json:"body"`
		Comments []struct {
			Path string `json:"path"`
			Line int    `json:"line"`
			Body string `json:"body"`
		} `json:"comments"`
	}
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath, gotMethod = r.URL.Path, r.Method
		if err := json.UnmarshalRead(r.Body, &sent); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		fmt.Fprint(w, `{"html_url":"https://github.com/o/r/pull/7#pullrequestreview-1"}`)
	}))

	url, err := c.SubmitReview(t.Context(), reviewRepo, 7, ghapi.ReviewSubmission{
		CommitID: "abc123", Event: "COMMENT", Body: ghapitest.Body(t, "the review\n"),
		Comments: []ghapi.ReviewComment{{Path: "a.go", Line: 3, Body: ghapitest.Body(t, "a remark\n")}},
	})
	if err != nil {
		t.Fatalf("SubmitReview: %v", err)
	}

	if want := "/repos/o/r/pulls/7/reviews"; gotPath != want {
		t.Errorf("path = %q, want %q", gotPath, want)
	}
	if gotMethod != http.MethodPost {
		t.Errorf("method = %q, want %q", gotMethod, http.MethodPost)
	}
	if want := "abc123"; sent.CommitID != want {
		t.Errorf("commit_id = %q, want %q", sent.CommitID, want)
	}
	if want := "COMMENT"; sent.Event != want {
		t.Errorf("event = %q, want %q", sent.Event, want)
	}
	if want := "the review\n"; sent.Body != want {
		t.Errorf("body = %q, want %q", sent.Body, want)
	}
	if want := 1; len(sent.Comments) != want {
		t.Fatalf("comments = %d, want %d", len(sent.Comments), want)
	}
	if got := sent.Comments[0]; got.Path != "a.go" || got.Line != 3 || got.Body != "a remark\n" {
		t.Errorf("comments[0] = %+v", got)
	}
	if want := "https://github.com/o/r/pull/7#pullrequestreview-1"; url != want {
		t.Errorf("SubmitReview = %q, want %q", url, want)
	}
}

// A review with no line comments sends an empty list rather than null: GitHub
// takes either, and the shell version this replaces sent the empty one.
func TestSubmitReviewWithNoComments(t *testing.T) {
	t.Parallel()

	var sent struct {
		Comments []struct{} `json:"comments"`
	}
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if err := json.UnmarshalRead(r.Body, &sent); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		fmt.Fprint(w, `{"html_url":"https://example.com/r"}`)
	}))

	if _, err := c.SubmitReview(t.Context(), reviewRepo, 7, ghapi.ReviewSubmission{
		CommitID: "abc123", Event: "APPROVE", Body: ghapitest.Body(t, "looks good\n"),
	}); err != nil {
		t.Fatalf("SubmitReview: %v", err)
	}
	if diff := cmp.Diff([]struct{}{}, sent.Comments); diff != "" {
		t.Errorf("comments (-want +got):\n%s", diff)
	}
}

func TestReplyToReviewThread(t *testing.T) {
	t.Parallel()

	var sent struct {
		Variables struct {
			ThreadID string `json:"threadId"`
			Body     string `json:"body"`
		} `json:"variables"`
		Query string `json:"query"`
	}
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if err := json.UnmarshalRead(r.Body, &sent); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		fmt.Fprint(w, `{"data":{"addPullRequestReviewThreadReply":
			{"comment":{"url":"https://github.com/o/r/pull/7#discussion_r1"}}}}`)
	}))

	url, err := c.ReplyToReviewThread(t.Context(), "PRRT_1", ghapitest.Body(t, "answered\n"))
	if err != nil {
		t.Fatalf("ReplyToReviewThread: %v", err)
	}

	if want := "PRRT_1"; sent.Variables.ThreadID != want {
		t.Errorf("threadId = %q, want %q", sent.Variables.ThreadID, want)
	}
	if want := "answered\n"; sent.Variables.Body != want {
		t.Errorf("body = %q, want %q", sent.Variables.Body, want)
	}
	if want := "https://github.com/o/r/pull/7#discussion_r1"; url != want {
		t.Errorf("ReplyToReviewThread = %q, want %q", url, want)
	}
}
