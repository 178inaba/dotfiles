package pullrequest_test

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"slices"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
)

func ptr(s string) *string { return &s }

// rerequestContext is our own pull request with one reviewer of each standing
// the eligibility rules tell apart.
func rerequestContext() pullrequest.Context {
	return pullrequest.Context{
		Repo: "owner/repo", IsOwnPR: true, PR: pullrequest.PR{Number: 5},
		Reviewers: []pullrequest.Reviewer{
			{Author: ptr("alice"), AuthorType: ptr("User"), State: pullrequest.ReviewerChangesRequested},
			{Author: ptr("bob"), AuthorType: ptr("User"), State: pullrequest.ReviewerCommented},
			{Author: ptr("carol"), AuthorType: ptr("User"), State: pullrequest.ReviewerApproved},
			{Author: ptr("copilot-pull-request-reviewer"), AuthorType: ptr("Bot"), State: pullrequest.ReviewerCommented},
			{State: pullrequest.ReviewerCommented},
		},
	}
}

func TestPlanReviewRequest(t *testing.T) {
	t.Parallel()

	truncated := rerequestContext()
	truncated.ReviewsTruncated = true

	for _, tt := range []struct {
		name          string
		prContext     pullrequest.Context
		logins        []string
		wantRequested []string
		wantSkipped   []pullrequest.SkippedReviewer
	}{
		{
			// Named once however often the caller named them.
			name:          "each standing",
			prContext:     rerequestContext(),
			logins:        []string{"alice", "bob", "carol", "copilot-pull-request-reviewer", "dave", "alice"},
			wantRequested: []string{"alice", "bob"},
			wantSkipped: []pullrequest.SkippedReviewer{
				{Login: "carol", Reason: pullrequest.SkipApproved},
				{Login: "copilot-pull-request-reviewer", Reason: pullrequest.SkipBot},
				{Login: "dave", Reason: pullrequest.SkipNotAReviewer},
			},
		},
		{
			// An approval may sit outside the window, so no standing is trusted.
			name:          "reviews truncated",
			prContext:     truncated,
			logins:        []string{"alice", "dave"},
			wantRequested: []string{},
			wantSkipped: []pullrequest.SkippedReviewer{
				{Login: "alice", Reason: pullrequest.SkipReviewsTruncated},
				{Login: "dave", Reason: pullrequest.SkipReviewsTruncated},
			},
		},
	} {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, err := pullrequest.PlanReviewRequest(tt.prContext, tt.logins)
			if err != nil {
				t.Fatalf("PlanReviewRequest: %v", err)
			}
			if !slices.Equal(got.Requested, tt.wantRequested) {
				t.Errorf("requested = %v, want %v", got.Requested, tt.wantRequested)
			}
			if !slices.Equal(got.Skipped, tt.wantSkipped) {
				t.Errorf("skipped = %v, want %v", got.Skipped, tt.wantSkipped)
			}
		})
	}
}

func TestPlanReviewRequestRefusesSomebodyElsesPullRequest(t *testing.T) {
	t.Parallel()

	prContext := rerequestContext()
	prContext.IsOwnPR = false

	_, err := pullrequest.PlanReviewRequest(prContext, []string{"alice"})
	if err == nil {
		t.Fatal("PlanReviewRequest on somebody else's pull request succeeded, want a refusal")
	}
	if !strings.Contains(err.Error(), "is not ours") {
		t.Errorf("error = %q, want it to say the pull request is not ours", err)
	}
}

func TestRequestReview(t *testing.T) {
	t.Parallel()

	var seenPath string
	var seenReviewers []string
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var req struct {
			Reviewers []string `json:"reviewers"`
		}
		if err := json.UnmarshalRead(r.Body, &req); err != nil {
			t.Errorf("decode the request body: %v", err)
		}
		seenPath, seenReviewers = r.URL.Path, req.Reviewers
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"html_url":"https://github.com/owner/repo/pull/5"}`)
	}))

	plan := pullrequest.ReviewRequested{Requested: []string{"alice", "bob"}}
	if err := pullrequest.RequestReview(t.Context(), c, rerequestContext().Target(), plan); err != nil {
		t.Fatalf("RequestReview: %v", err)
	}
	if want := "/repos/owner/repo/pulls/5/requested_reviewers"; seenPath != want {
		t.Errorf("posted to %q, want %q", seenPath, want)
	}
	if want := []string{"alice", "bob"}; !slices.Equal(seenReviewers, want) {
		t.Errorf("reviewers sent = %v, want %v", seenReviewers, want)
	}
}

// Nothing is sent when nobody is eligible, and that is an answer rather than a
// failure.
func TestRequestReviewWithNobodyToRequest(t *testing.T) {
	t.Parallel()

	c := ghapitest.New(t, http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
		t.Error("a request was sent with nobody to request")
	}))

	if err := pullrequest.RequestReview(t.Context(), c, rerequestContext().Target(), pullrequest.ReviewRequested{}); err != nil {
		t.Fatalf("RequestReview: %v", err)
	}
}

func TestRequestReviewReportsAFailedRequest(t *testing.T) {
	t.Parallel()

	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusUnprocessableEntity)
		fmt.Fprint(w, `{"message":"Reviews may only be requested from collaborators."}`)
	}))

	plan := pullrequest.ReviewRequested{Requested: []string{"alice"}}
	err := pullrequest.RequestReview(t.Context(), c, rerequestContext().Target(), plan)
	if err == nil {
		t.Fatal("RequestReview with GitHub refusing succeeded, want an error")
	}
	if !strings.Contains(err.Error(), "alice") {
		t.Errorf("error = %q, want it to name who was not requested", err)
	}
}
