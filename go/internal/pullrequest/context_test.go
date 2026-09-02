package pullrequest_test

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
)

var repo = ghapi.Repo{Owner: "owner", Name: "repo"}

// meta is the pull request the caller has already resolved, which Fetch takes
// rather than looking up itself.
var meta = ghapi.PullRequest{
	Number: 5,
	Title:  "Test PR",
	Body: "Closes #10\nFIXES: #11\nResolves other/repo#12\nfix #10\nSee #99\n" +
		"See https://github.com/owner/repo/issues/13\nFixes https://github.com/owner/repo/issues/14",
	URL:         "https://github.com/owner/repo/pull/5",
	State:       ghapi.StateOpen,
	Author:      "testuser",
	HeadRefName: "feature/x",
	BaseRefName: "main",
	HeadRefOid:  "abc123",
}

// pages is what one fake GitHub answers with: the first response to each query,
// and then the continuation pages by cursor.
type pages struct {
	// body answers the query that carries everything reachable in one round
	// trip.
	body string
	// comments and threads answer the continuation queries, by the cursor
	// asked for.
	comments map[string]string
	threads  map[string]string
	// threadComments answers per thread id and cursor.
	threadComments map[string]string
	// failAfter names a query whose continuation fails.
	failAfter string
}

func serve(t *testing.T, p pages) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var req struct {
			Query     string `json:"query"`
			Variables struct {
				Cursor   string `json:"cursor"`
				ThreadID string `json:"threadId"`
			} `json:"variables"`
		}
		if err := json.UnmarshalRead(r.Body, &req); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		w.Header().Set("Content-Type", "application/json")

		kind := "body"
		switch {
		case strings.Contains(req.Query, "node(id:"):
			kind = "threadComments"
		case strings.Contains(req.Query, "reviewThreads(first: 100, after:"):
			kind = "threads"
		case strings.Contains(req.Query, "comments(first: 100, after:"):
			kind = "comments"
		}
		if kind == p.failAfter {
			fmt.Fprint(w, `{"errors":[{"message":"page unavailable"}]}`)
			return
		}

		var answer string
		switch kind {
		case "body":
			answer = p.body
		case "comments":
			answer = p.comments[req.Variables.Cursor]
		case "threads":
			answer = p.threads[req.Variables.Cursor]
		case "threadComments":
			answer = p.threadComments[req.Variables.ThreadID+"/"+req.Variables.Cursor]
		}
		if answer == "" {
			t.Errorf("no fixture for the %s query at cursor %q thread %q", kind, req.Variables.Cursor, req.Variables.ThreadID)
			answer = `{"data":null}`
		}
		fmt.Fprint(w, answer)
	}))
}

// The fixture below is, thread by thread: one
// remark of a reviewer's, one resolved, one we answered, one resolved after we
// answered, one of ours that was answered, one of ours we already confirmed,
// one of ours resolved, one of ours the author never replied to, one a bot
// opened that only the bot and we are in, and one a bot opened that a person
// joined.
const fixtureBody = `{"data":{
  "viewer": {"login": "testuser"},
  "repository": {
    "headCommit": {"committedDate": "2026-01-15T00:00:00Z"},
    "pullRequest": {
      "comments": {
        "totalCount": 4,
        "pageInfo": {"hasNextPage": false, "endCursor": "cur-1"},
        "nodes": [
          {"author": {"login": "reviewer1", "__typename": "User"}, "body": "普通のコメント", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/c1"},
          {"author": {"login": "testuser", "__typename": "User"}, "body": "<!-- review-response -->\n対応しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/c2"},
          {"author": {"login": "reviewer1", "__typename": "User"}, "body": "> <!-- review-response -->\n引用返信", "createdAt": "2026-01-03T00:00:00Z", "url": "https://example.com/c3"},
          {"author": null, "body": "CI 通知", "createdAt": "2026-01-04T00:00:00Z", "url": "https://example.com/c4"}
        ]
      },
      "reviews": {
        "totalCount": 1,
        "nodes": [
          {"author": {"login": "reviewer1", "__typename": "User"}, "state": "CHANGES_REQUESTED", "body": "優先度1: テスト不足", "url": "https://example.com/r1", "submittedAt": "2026-01-01T00:00:00Z"}
        ]
      },
      "reviewThreads": {
        "totalCount": 10,
        "pageInfo": {"hasNextPage": false, "endCursor": "tc-1"},
        "nodes": [
          {"id": "PRRT_1", "isResolved": false, "isOutdated": false, "path": "src/main.go", "line": 30, "originalLine": 30, "resolvedBy": null,
           "comments": {"totalCount": 1, "pageInfo": {"hasNextPage": false, "endCursor": "t1"},
             "nodes": [{"author": {"login": "reviewer1", "__typename": "User"}, "body": "ここ直して", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t1"}]},
           "tail": {"nodes": [{"author": {"login": "reviewer1", "__typename": "User"}, "body": "ここ直して", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t1"}]}},
          {"id": "PRRT_2", "isResolved": true, "isOutdated": true, "path": "src/util.go", "line": 10, "originalLine": 10, "resolvedBy": {"login": "testuser"},
           "comments": {"totalCount": 1, "pageInfo": {"hasNextPage": false, "endCursor": "t2"},
             "nodes": [{"author": {"login": "reviewer2", "__typename": "User"}, "body": "解決済み", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t2"}]},
           "tail": {"nodes": [{"author": {"login": "reviewer2", "__typename": "User"}, "body": "解決済み", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t2"}]}},
          {"id": "PRRT_3", "isResolved": false, "isOutdated": false, "path": "src/api.go", "line": 7, "originalLine": 7, "resolvedBy": null,
           "comments": {"totalCount": 2, "pageInfo": {"hasNextPage": false, "endCursor": "t3"},
             "nodes": [{"author": {"login": "reviewer1", "__typename": "User"}, "body": "ここも直して", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t3a"},
                       {"author": {"login": "testuser", "__typename": "User"}, "body": "修正しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t3b"}]},
           "tail": {"nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "修正しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t3b"}]}},
          {"id": "PRRT_4", "isResolved": true, "isOutdated": false, "path": "src/db.go", "line": 42, "originalLine": 42, "resolvedBy": {"login": "testuser"},
           "comments": {"totalCount": 2, "pageInfo": {"hasNextPage": false, "endCursor": "t4"},
             "nodes": [{"author": {"login": "reviewer2", "__typename": "User"}, "body": "ここ確認", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t4a"},
                       {"author": {"login": "testuser", "__typename": "User"}, "body": "対応済みです", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t4b"}]},
           "tail": {"nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "対応済みです", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t4b"}]}},
          {"id": "PRRT_5", "isResolved": false, "isOutdated": false, "path": "src/cache.go", "line": 12, "originalLine": 12, "resolvedBy": null,
           "comments": {"totalCount": 2, "pageInfo": {"hasNextPage": false, "endCursor": "t5"},
             "nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "自分が出した指摘", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t5a"},
                       {"author": {"login": "othercoder", "__typename": "User"}, "body": "直しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t5b"}]},
           "tail": {"nodes": [{"author": {"login": "othercoder", "__typename": "User"}, "body": "直しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t5b"}]}},
          {"id": "PRRT_6", "isResolved": false, "isOutdated": false, "path": "src/cache.go", "line": 20, "originalLine": 20, "resolvedBy": null,
           "comments": {"totalCount": 3, "pageInfo": {"hasNextPage": false, "endCursor": "t6"},
             "nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "自分が出した指摘", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t6a"},
                       {"author": {"login": "othercoder", "__typename": "User"}, "body": "直しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t6b"},
                       {"author": {"login": "testuser", "__typename": "User"}, "body": "確認しました", "createdAt": "2026-02-01T00:00:00Z", "url": "https://example.com/t6c"}]},
           "tail": {"nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "確認しました", "createdAt": "2026-02-01T00:00:00Z", "url": "https://example.com/t6c"}]}},
          {"id": "PRRT_7", "isResolved": true, "isOutdated": false, "path": "src/cache.go", "line": 31, "originalLine": 31, "resolvedBy": {"login": "testuser"},
           "comments": {"totalCount": 2, "pageInfo": {"hasNextPage": false, "endCursor": "t7"},
             "nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "自分が出した指摘", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t7a"},
                       {"author": {"login": "othercoder", "__typename": "User"}, "body": "直しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t7b"}]},
           "tail": {"nodes": [{"author": {"login": "othercoder", "__typename": "User"}, "body": "直しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t7b"}]}},
          {"id": "PRRT_8", "isResolved": false, "isOutdated": true, "path": "src/cache.go", "line": null, "originalLine": 55, "resolvedBy": null,
           "comments": {"totalCount": 1, "pageInfo": {"hasNextPage": false, "endCursor": "t8"},
             "nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "自分が出した指摘（作者は返信せず修正だけ push）", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t8a"}]},
           "tail": {"nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "自分が出した指摘（作者は返信せず修正だけ push）", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t8a"}]}},
          {"id": "PRRT_9", "isResolved": false, "isOutdated": false, "path": "src/bot.go", "line": 3, "originalLine": 3, "resolvedBy": null,
           "comments": {"totalCount": 2, "pageInfo": {"hasNextPage": false, "endCursor": "t9"},
             "nodes": [{"author": {"login": "copilot-pull-request-reviewer", "__typename": "Bot"}, "body": "誤検知の指摘", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t9a"},
                       {"author": {"login": "testuser", "__typename": "User"}, "body": "誤検知なのでこのままにします", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t9b"}]},
           "tail": {"nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "誤検知なのでこのままにします", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t9b"}]}},
          {"id": "PRRT_10", "isResolved": false, "isOutdated": false, "path": "src/bot.go", "line": 9, "originalLine": 9, "resolvedBy": null,
           "comments": {"totalCount": 3, "pageInfo": {"hasNextPage": false, "endCursor": "t10"},
             "nodes": [{"author": {"login": "copilot-pull-request-reviewer", "__typename": "Bot"}, "body": "誤検知の指摘", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t10a"},
                       {"author": {"login": "testuser", "__typename": "User"}, "body": "誤検知なのでこのままにします", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t10b"},
                       {"author": {"login": "reviewer1", "__typename": "User"}, "body": "いや、これは本当のバグでは", "createdAt": "2026-01-03T00:00:00Z", "url": "https://example.com/t10c"}]},
           "tail": {"nodes": [{"author": {"login": "reviewer1", "__typename": "User"}, "body": "いや、これは本当のバグでは", "createdAt": "2026-01-03T00:00:00Z", "url": "https://example.com/t10c"}]}}
        ]
      }
    }
  }
}}`

func fetch(t *testing.T, p pages, pr ghapi.PullRequest, limits pullrequest.Limits) pullrequest.Context {
	t.Helper()

	got, err := pullrequest.Fetch(t.Context(), serve(t, p), repo, pr, limits)
	if err != nil {
		t.Fatalf("Fetch: %v", err)
	}
	return got
}

func TestFetch(t *testing.T) {
	t.Parallel()

	got := fetch(t, pages{body: fixtureBody}, meta, pullrequest.DefaultLimits)

	t.Run("the pull request and who is reading it", func(t *testing.T) {
		want := pullrequest.PR{
			Number: 5, Title: "Test PR", Body: meta.Body, URL: "https://github.com/owner/repo/pull/5",
			State: ghapi.StateOpen, Author: "testuser", HeadRef: "feature/x", BaseRef: "main", HeadOID: "abc123",
		}
		if diff := cmp.Diff(want, got.PR); diff != "" {
			t.Errorf("pr (-want +got):\n%s", diff)
		}
		if got.Repo != "owner/repo" || got.CurrentUser != "testuser" || !got.IsOwnPR {
			t.Errorf("repo/current_user/is_own_pr = %q/%q/%v, want owner/repo, testuser and true", got.Repo, got.CurrentUser, got.IsOwnPR)
		}
		if got.HeadCommittedAt == nil || *got.HeadCommittedAt != "2026-01-15T00:00:00Z" {
			t.Errorf("head_committed_at = %v, want the fixture's date", got.HeadCommittedAt)
		}
	})

	t.Run("the issues the body closes", func(t *testing.T) {
		// A bare #99 and a url are not among them, because GitHub does not
		// close on those either; #10 appears twice and once.
		other := "other/repo"
		want := []pullrequest.LinkedIssue{{Number: 10}, {Number: 11}, {Repo: &other, Number: 12}}
		if diff := cmp.Diff(want, got.LinkedIssues); diff != "" {
			t.Errorf("linked_issues (-want +got):\n%s", diff)
		}
	})

	t.Run("comments", func(t *testing.T) {
		user, reviewer := "User", "reviewer1"
		want := []pullrequest.Comment{
			{Author: &reviewer, AuthorType: &user, Body: "普通のコメント", CreatedAt: "2026-01-01T00:00:00Z", URL: "https://example.com/c1"},
			{Author: new("testuser"), AuthorType: &user, Body: "<!-- review-response -->\n対応しました", CreatedAt: "2026-01-02T00:00:00Z", URL: "https://example.com/c2", IsSkillComment: true},
			// Quoting one of our own replies copies the marker with the rest of
			// the markdown, and the "> " in front is what keeps it from
			// counting as ours.
			{Author: &reviewer, AuthorType: &user, Body: "> <!-- review-response -->\n引用返信", CreatedAt: "2026-01-03T00:00:00Z", URL: "https://example.com/c3"},
			// An account that no longer exists has no login and no type.
			{Body: "CI 通知", CreatedAt: "2026-01-04T00:00:00Z", URL: "https://example.com/c4"},
		}
		if diff := cmp.Diff(want, got.Comments); diff != "" {
			t.Errorf("comments (-want +got):\n%s", diff)
		}
		if got.CommentsTotalCount != 4 || got.CommentsTruncated {
			t.Errorf("comments count = %d truncated %v, want 4 and false", got.CommentsTotalCount, got.CommentsTruncated)
		}
	})

	t.Run("reviews", func(t *testing.T) {
		want := []pullrequest.Review{{
			Author: new("reviewer1"), AuthorType: new("User"), State: "CHANGES_REQUESTED", Body: "優先度1: テスト不足",
			URL: "https://example.com/r1", SubmittedAt: "2026-01-01T00:00:00Z",
		}}
		if diff := cmp.Diff(want, got.Reviews); diff != "" {
			t.Errorf("reviews (-want +got):\n%s", diff)
		}
		if got.ReviewsTotalCount != 1 || got.ReviewsTruncated {
			t.Errorf("reviews count = %d truncated %v, want 1 and false", got.ReviewsTotalCount, got.ReviewsTruncated)
		}
	})

	t.Run("threads", func(t *testing.T) {
		if got.ThreadsTotalCount != 10 || got.ThreadsTruncated || len(got.ReviewThreads) != 10 {
			t.Fatalf("threads = %d of %d, truncated %v; want all 10", len(got.ReviewThreads), got.ThreadsTotalCount, got.ThreadsTruncated)
		}
		first := got.ReviewThreads[0]
		if first.ID != "PRRT_1" || first.Path != "src/main.go" || first.Line == nil || *first.Line != 30 {
			t.Errorf("the first thread = %+v, want the fixture's first", first)
		}
		if second := got.ReviewThreads[1]; !second.IsResolved || !second.IsOutdated || second.ResolvedBy == nil {
			t.Errorf("the second thread = %+v, want it resolved and outdated by somebody", second)
		}
		// A thread whose lines are gone from the diff has no line at all, and
		// original_line is what a selector still reaches it by.
		outdated := got.ReviewThreads[7]
		if outdated.Line != nil {
			t.Errorf("line = %v, want null", outdated.Line)
		}
		if outdated.OriginalLine == nil || *outdated.OriginalLine != 55 {
			t.Errorf("original_line = %v, want 55 even though line is null", outdated.OriginalLine)
		}
		if last := outdated.LastComment; last == nil || last.URL != "https://example.com/t8a" {
			t.Errorf("last_comment = %+v, want the newest comment of the thread", last)
		}
	})

	t.Run("who opened each thread, and of what kind", func(t *testing.T) {
		tests := []struct {
			index    int
			login    string
			typename string
		}{
			{index: 0, login: "reviewer1", typename: "User"},
			{index: 4, login: "testuser", typename: "User"},
			{index: 8, login: "copilot-pull-request-reviewer", typename: "Bot"},
		}
		for _, tc := range tests {
			thread := got.ReviewThreads[tc.index]
			if thread.OpenedBy == nil || *thread.OpenedBy != tc.login {
				t.Errorf("thread %d opened_by = %v, want %q", tc.index, thread.OpenedBy, tc.login)
			}
			if thread.OpenedByType == nil || *thread.OpenedByType != tc.typename {
				t.Errorf("thread %d opened_by_type = %v, want %q", tc.index, thread.OpenedByType, tc.typename)
			}
		}
	})

	t.Run("the type of each comment's author", func(t *testing.T) {
		// The same field the conversation's comments already carry, so that a
		// bot is told from a person inside a thread too.
		bot := got.ReviewThreads[8]
		if len(bot.Comments) != 2 {
			t.Fatalf("the bot thread has %d comments, want 2", len(bot.Comments))
		}
		if bot.Comments[0].AuthorType == nil || *bot.Comments[0].AuthorType != "Bot" {
			t.Errorf("comments[0].author_type = %v, want Bot", bot.Comments[0].AuthorType)
		}
		if bot.LastComment == nil || bot.LastComment.AuthorType == nil || *bot.LastComment.AuthorType != "User" {
			t.Errorf("last_comment.author_type = %+v, want User", bot.LastComment)
		}
		if got.Reviews[0].AuthorType == nil || *got.Reviews[0].AuthorType != "User" {
			t.Errorf("reviews[0].author_type = %v, want User", got.Reviews[0].AuthorType)
		}
	})

	t.Run("whose move it is, and who may close it", func(t *testing.T) {
		tests := []struct {
			name           string
			index          int
			wantBall       pullrequest.Ball
			wantResolvable bool
		}{
			// Somebody else's remark on our own work: ours to answer, theirs
			// to close.
			{name: "a reviewer's remark nobody answered", index: 0, wantBall: pullrequest.BallMine},
			{name: "a resolved thread", index: 1, wantBall: pullrequest.BallNone},
			{name: "we answered a reviewer", index: 2, wantBall: pullrequest.BallTheirs},
			{name: "resolved after we answered", index: 3, wantBall: pullrequest.BallNone},
			// Our own remarks: we close them, whoever owns the pull request.
			{name: "our remark was answered", index: 4, wantBall: pullrequest.BallMine, wantResolvable: true},
			{name: "our remark we already confirmed", index: 5, wantBall: pullrequest.BallTheirs, wantResolvable: true},
			{name: "our remark, resolved", index: 6, wantBall: pullrequest.BallNone},
			{name: "our remark overtaken by a commit", index: 7, wantBall: pullrequest.BallMine, wantResolvable: true},
			// A bot never comes back to confirm, so a thread only it and we are
			// in stays ours to act on even after our own reply — the state that
			// left two threads open for a day.
			{name: "a bot's remark we already answered", index: 8, wantBall: pullrequest.BallMine, wantResolvable: true},
			// One person in the thread and it is a person's remark again.
			{name: "a bot's remark a person joined", index: 9, wantBall: pullrequest.BallMine},
		}

		for _, tc := range tests {
			t.Run(tc.name, func(t *testing.T) {
				thread := got.ReviewThreads[tc.index]
				if thread.Ball != tc.wantBall {
					t.Errorf("ball = %q, want %q", thread.Ball, tc.wantBall)
				}
				if thread.ResolvableByMe != tc.wantResolvable {
					t.Errorf("resolvable_by_me = %v, want %v", thread.ResolvableByMe, tc.wantResolvable)
				}
			})
		}
	})
}

// TestFetchBotThreadWithTruncatedComments is the undecidable case: a
// participant may sit outside the window that was fetched, so the thread is not
// treated as one only a bot and we are in — which would otherwise let a run
// close a person's remark.
func TestFetchBotThreadWithTruncatedComments(t *testing.T) {
	t.Parallel()

	got := fetch(t, pages{body: truncatedBotBody}, meta, pullrequest.Limits{Comments: 500, Threads: 300, ThreadComments: 1})

	thread := got.ReviewThreads[0]
	if !thread.CommentsTruncated {
		t.Fatalf("the fixture's thread is not truncated: %+v", thread)
	}
	if thread.Ball != pullrequest.BallTheirs {
		t.Errorf("ball = %q, want %q: our reply is last and the thread is not known to be a bot's alone", thread.Ball, pullrequest.BallTheirs)
	}
	if thread.ResolvableByMe {
		t.Error("resolvable_by_me = true on a thread whose participants are not all known")
	}
}

// truncatedBotBody is one bot-opened thread whose comments do not all fit in
// the window.
const truncatedBotBody = `{"data":{
  "viewer": {"login": "testuser"},
  "repository": {
    "headCommit": {"committedDate": "2026-01-15T00:00:00Z"},
    "pullRequest": {
      "comments": {"totalCount": 0, "pageInfo": {"hasNextPage": false, "endCursor": ""}, "nodes": []},
      "reviews": {"totalCount": 0, "nodes": []},
      "reviewThreads": {
        "totalCount": 1,
        "pageInfo": {"hasNextPage": false, "endCursor": "tc-1"},
        "nodes": [
          {"id": "PRRT_bot", "isResolved": false, "isOutdated": false, "path": "src/bot.go", "line": 3, "originalLine": 3, "resolvedBy": null,
           "comments": {"totalCount": 3, "pageInfo": {"hasNextPage": true, "endCursor": "bc1"},
             "nodes": [{"author": {"login": "copilot-pull-request-reviewer", "__typename": "Bot"}, "body": "誤検知の指摘", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/tb1"}]},
           "tail": {"nodes": [{"author": {"login": "testuser", "__typename": "User"}, "body": "誤検知なのでこのままにします", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/tb3"}]}}
        ]
      }
    }
  }
}}`

// TestFetchWithoutAHeadDate is the null guard: with no date to compare against,
// the time clause simply never holds, rather than holding for every thread and
// producing a second reply to each of them on every run.
func TestFetchWithoutAHeadDate(t *testing.T) {
	t.Parallel()

	body := strings.Replace(fixtureBody, `"headCommit": {"committedDate": "2026-01-15T00:00:00Z"}`, `"headCommit": null`, 1)
	got := fetch(t, pages{body: body}, meta, pullrequest.DefaultLimits)

	if got.HeadCommittedAt != nil {
		t.Errorf("head_committed_at = %v, want null", got.HeadCommittedAt)
	}
	// The one that only the time clause reached is handed back; the one
	// answered by somebody else stays ours.
	if ball := got.ReviewThreads[7].Ball; ball != pullrequest.BallTheirs {
		t.Errorf("ball = %q, want %q: with no head date the time clause never holds", ball, pullrequest.BallTheirs)
	}
	if ball := got.ReviewThreads[4].Ball; ball != pullrequest.BallMine {
		t.Errorf("ball = %q, want %q: somebody answered, which needs no head date", ball, pullrequest.BallMine)
	}
}

// TestFetchOnAnotherAuthorsPR pins the asymmetry: a remark on somebody else's
// work is theirs to answer and theirs to close, while our own remark means the
// same thing whoever owns the pull request.
func TestFetchOnAnotherAuthorsPR(t *testing.T) {
	t.Parallel()

	others := meta
	others.Author = "othercoder"
	got := fetch(t, pages{body: fixtureBody}, others, pullrequest.DefaultLimits)

	if got.IsOwnPR {
		t.Error("is_own_pr = true on somebody else's pull request")
	}
	// Nothing we did not open is ours to move on, the bot thread included:
	// closing somebody else's remark on their own pull request is not ours to
	// do.
	for i, thread := range got.ReviewThreads {
		opened := thread.OpenedBy != nil && *thread.OpenedBy == "testuser"
		if !opened && thread.Ball != pullrequest.BallNone {
			t.Errorf("thread %d ball = %q, want %q: we did not open it and the pull request is not ours",
				i, thread.Ball, pullrequest.BallNone)
		}
		if !opened && thread.ResolvableByMe {
			t.Errorf("thread %d resolvable_by_me = true on somebody else's pull request", i)
		}
	}
	// Our own remark that the author answered is still ours to judge and to
	// close.
	if ours := got.ReviewThreads[4]; ours.Ball != pullrequest.BallMine || !ours.ResolvableByMe {
		t.Errorf("our own remark = ball %q resolvable %v, want mine and true", ours.Ball, ours.ResolvableByMe)
	}
}

// TestSkillMarkerMatchesTheSkill is a contract in two directions: the skill
// writes this marker and this package recognises it. If one side changes alone,
// every past reply reads as a fresh remark and gets answered again.
func TestSkillMarkerMatchesTheSkill(t *testing.T) {
	t.Parallel()

	// The real skill, not a fixture: a copy would drift with the thing it is
	// supposed to be pinning.
	path := filepath.Join("..", "..", "..", "claude", ".claude", "skills", "review-response", "SKILL.md")
	skill, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile(%q): %v", path, err)
	}
	if !strings.Contains(string(skill), pullrequest.SkillMarker) {
		t.Errorf("%s does not mention %q, which this package detects its comments by", path, pullrequest.SkillMarker)
	}
}

// commentPage renders one page of the conversation.
func commentPage(hasNext bool, cursor string, logins ...string) string {
	nodes := make([]string, 0, len(logins))
	for _, login := range logins {
		nodes = append(nodes, fmt.Sprintf(
			`{"author":{"login":%q,"__typename":"User"},"body":%q,"createdAt":"2026-01-01T00:00:00Z","url":"https://example.com/%s"}`,
			login, login, login))
	}
	return fmt.Sprintf(`{"data":{"repository":{"pullRequest":{"comments":{
		"pageInfo":{"hasNextPage":%v,"endCursor":%q},"nodes":[%s]}}}}}`,
		hasNext, cursor, strings.Join(nodes, ","))
}

// threadCommentPage renders one page of a thread's comments.
func threadCommentPage(hasNext bool, cursor string, bodies ...string) string {
	nodes := make([]string, 0, len(bodies))
	for _, body := range bodies {
		nodes = append(nodes, fmt.Sprintf(
			`{"author":{"login":"reviewer1","__typename":"User"},"body":%q,"createdAt":"2026-01-01T00:00:00Z","url":"https://example.com/%s"}`, body, body))
	}
	return fmt.Sprintf(`{"data":{"node":{"comments":{
		"pageInfo":{"hasNextPage":%v,"endCursor":%q},"nodes":[%s]}}}}`,
		hasNext, cursor, strings.Join(nodes, ","))
}

// pagedBody is a pull request whose conversation and whose two threads all have
// more to fetch, so that one run exercises every continuation.
const pagedBody = `{"data":{
  "viewer": {"login": "testuser"},
  "repository": {
    "headCommit": {"committedDate": "2026-01-15T00:00:00Z"},
    "pullRequest": {
      "comments": {
        "totalCount": 4,
        "pageInfo": {"hasNextPage": true, "endCursor": "c1"},
        "nodes": [{"author": {"login": "first", "__typename": "User"}, "body": "first", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/first"}]
      },
      "reviews": {"totalCount": 0, "nodes": []},
      "reviewThreads": {
        "totalCount": 3,
        "pageInfo": {"hasNextPage": true, "endCursor": "t1"},
        "nodes": [
          {"id": "A", "isResolved": false, "isOutdated": false, "path": "a.go", "line": 1, "originalLine": 1, "resolvedBy": null,
           "comments": {"totalCount": 3, "pageInfo": {"hasNextPage": true, "endCursor": "ac1"},
             "nodes": [{"author": {"login": "reviewer1", "__typename": "User"}, "body": "a1", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/a1"}]},
           "tail": {"nodes": [{"author": {"login": "reviewer1", "__typename": "User"}, "body": "a3", "createdAt": "2026-01-03T00:00:00Z", "url": "https://example.com/a3"}]}},
          {"id": "B", "isResolved": false, "isOutdated": false, "path": "b.go", "line": 1, "originalLine": 1, "resolvedBy": null,
           "comments": {"totalCount": 2, "pageInfo": {"hasNextPage": true, "endCursor": "bc1"},
             "nodes": [{"author": {"login": "reviewer1", "__typename": "User"}, "body": "b1", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/b1"}]},
           "tail": {"nodes": [{"author": {"login": "reviewer1", "__typename": "User"}, "body": "b2", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/b2"}]}}
        ]
      }
    }
  }
}}`

// threadsPage2 is the second page of threads, whose one thread is complete.
const threadsPage2 = `{"data":{"repository":{"pullRequest":{"reviewThreads":{
  "pageInfo": {"hasNextPage": false, "endCursor": "t2"},
  "nodes": [
    {"id": "C", "isResolved": false, "isOutdated": false, "path": "c.go", "line": 1, "originalLine": 1, "resolvedBy": null,
     "comments": {"totalCount": 0, "pageInfo": {"hasNextPage": false, "endCursor": ""}, "nodes": []},
     "tail": {"nodes": []}}
  ]}}}}}`

func pagedFixture() pages {
	return pages{
		body: pagedBody,
		comments: map[string]string{
			"c1": commentPage(true, "c2", "second"),
			"c2": commentPage(false, "c3", "third", "fourth"),
		},
		threads: map[string]string{"t1": threadsPage2},
		threadComments: map[string]string{
			"A/ac1": threadCommentPage(true, "ac2", "a2"),
			"A/ac2": threadCommentPage(false, "ac3", "a3"),
			"B/bc1": threadCommentPage(false, "bc2", "b2"),
		},
	}
}

func TestFetchFollowsEveryConnection(t *testing.T) {
	t.Parallel()

	got := fetch(t, pagedFixture(), meta, pullrequest.DefaultLimits)

	var authors []string
	for _, c := range got.Comments {
		authors = append(authors, *c.Author)
	}
	if diff := cmp.Diff([]string{"first", "second", "third", "fourth"}, authors); diff != "" {
		t.Errorf("comments across pages (-want +got):\n%s", diff)
	}
	if got.CommentsTruncated {
		t.Error("comments_truncated = true, want false once every page arrived")
	}

	var ids []string
	for _, thread := range got.ReviewThreads {
		ids = append(ids, thread.ID)
	}
	if diff := cmp.Diff([]string{"A", "B", "C"}, ids); diff != "" {
		t.Errorf("threads across pages (-want +got):\n%s", diff)
	}

	// Each thread's continuation is keyed by its own node id, so a page of one
	// thread's comments cannot land in another's.
	want := map[string][]string{"A": {"a1", "a2", "a3"}, "B": {"b1", "b2"}, "C": nil}
	for _, thread := range got.ReviewThreads {
		var bodies []string
		for _, c := range thread.Comments {
			bodies = append(bodies, c.Body)
		}
		if diff := cmp.Diff(want[thread.ID], bodies); diff != "" {
			t.Errorf("thread %s comments (-want +got):\n%s", thread.ID, diff)
		}
	}
	// A thread with nothing in it has no last comment rather than an empty one.
	if last := got.ReviewThreads[2].LastComment; last != nil {
		t.Errorf("last_comment = %+v on an empty thread, want null", last)
	}
}

func TestFetchStopsAtItsLimits(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		limits pullrequest.Limits
		// check reads the outcome the limit produced.
		check func(t *testing.T, got pullrequest.Context)
	}{
		{
			// The first page always arrives whole, so a limit below its size
			// still yields it: the limit bounds the round trips, and the flag
			// is what says something was left behind.
			name:   "the conversation",
			limits: pullrequest.Limits{Comments: 1, Threads: 300, ThreadComments: 200},
			check: func(t *testing.T, got pullrequest.Context) {
				if len(got.Comments) != 1 || !got.CommentsTruncated {
					t.Errorf("comments = %d, truncated %v; want 1 and true", len(got.Comments), got.CommentsTruncated)
				}
			},
		},
		{
			name:   "the threads",
			limits: pullrequest.Limits{Comments: 500, Threads: 1, ThreadComments: 200},
			check: func(t *testing.T, got pullrequest.Context) {
				if len(got.ReviewThreads) != 2 || !got.ThreadsTruncated {
					t.Errorf("threads = %d, truncated %v; want 2 and true", len(got.ReviewThreads), got.ThreadsTruncated)
				}
			},
		},
		{
			// Per thread rather than across all of them: a shared limit would
			// be reached in ordinary use and every thread after it would lose
			// its discussion.
			name:   "a thread's comments",
			limits: pullrequest.Limits{Comments: 500, Threads: 300, ThreadComments: 1},
			check: func(t *testing.T, got pullrequest.Context) {
				a := got.ReviewThreads[0]
				if len(a.Comments) != 1 || !a.CommentsTruncated {
					t.Errorf("thread A comments = %d, truncated %v; want 1 and true", len(a.Comments), a.CommentsTruncated)
				}
				// The tail comes from the other end of the connection, so it
				// is right even where the forward pages were cut — which is
				// the whole reason it is asked for separately.
				if a.LastComment == nil || a.LastComment.Body != "a3" {
					t.Errorf("last_comment = %+v, want the newest comment despite the truncation", a.LastComment)
				}
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			tc.check(t, fetch(t, pagedFixture(), meta, tc.limits))
		})
	}
}

func TestFetchFailsOnAnUnreachablePage(t *testing.T) {
	t.Parallel()

	for _, query := range []string{"body", "comments", "threads", "threadComments"} {
		t.Run(query, func(t *testing.T) {
			t.Parallel()

			p := pagedFixture()
			p.failAfter = query
			if got, err := pullrequest.Fetch(t.Context(), serve(t, p), repo, meta, pullrequest.DefaultLimits); err == nil {
				t.Fatalf("Fetch = %+v, want a failure when the %s query fails", got, query)
			}
		})
	}
}
