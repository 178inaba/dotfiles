package pullrequest_test

import (
	"encoding/json/v2"
	"fmt"
	"maps"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

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
	// issues answers the REST issue and parent endpoints by api path, and
	// issueStatus fails one of them; a path in neither answers 404, which is
	// how the parent endpoint says an issue is nobody's child.
	issues      map[string]string
	issueStatus map[string]int
	// issueComments answers the comment endpoints by api path, oldest first,
	// and is paginated the way GitHub paginates it. A path not in it has no
	// comments rather than no endpoint: reading them is not optional, so a
	// missing fixture would fail every issue in every other test.
	issueComments map[string][]string
}

func serve(t *testing.T, p pages) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Routed before the body is touched: the REST calls carry none, and
		// decoding one as a GraphQL query is how a fake answers an issue
		// lookup with a pull request and passes.
		if r.URL.Path != "/graphql" {
			serveIssue(w, r, p)
			return
		}
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

// serveIssue answers the three REST endpoints the linked issues are read from.
func serveIssue(w http.ResponseWriter, r *http.Request, p pages) {
	w.Header().Set("Content-Type", "application/json")
	if s, ok := p.issueStatus[r.URL.Path]; ok {
		w.WriteHeader(s)
		fmt.Fprint(w, `{"message":"unavailable"}`)
		return
	}
	if strings.HasSuffix(r.URL.Path, "/comments") {
		serveIssueComments(w, r, p.issueComments[r.URL.Path])
		return
	}
	body, ok := p.issues[r.URL.Path]
	if !ok {
		w.WriteHeader(http.StatusNotFound)
		fmt.Fprint(w, `{"message":"Not Found"}`)
		return
	}
	fmt.Fprint(w, body)
}

// serveIssueComments answers one page of a comment list, and links to the next
// one where there is one, since what the walk stops on is that link.
func serveIssueComments(w http.ResponseWriter, r *http.Request, list []string) {
	perPage, page := 30, 1
	if n, err := strconv.Atoi(r.URL.Query().Get("per_page")); err == nil {
		perPage = n
	}
	if n, err := strconv.Atoi(r.URL.Query().Get("page")); err == nil {
		page = n
	}

	start := min((page-1)*perPage, len(list))
	end := min(start+perPage, len(list))
	if end < len(list) {
		w.Header().Set("Link", fmt.Sprintf(`<http://%s%s?per_page=%d&page=%d>; rel="next"`,
			r.Host, r.URL.Path, perPage, page+1))
	}
	fmt.Fprintf(w, "[%s]", strings.Join(list[start:end], ","))
}

// issuePath, parentPath and commentsPath name the endpoints one issue is read
// through.
func issuePath(repo string, number int) string {
	return fmt.Sprintf("/repos/%s/issues/%d", repo, number)
}

func parentPath(repo string, number int) string { return issuePath(repo, number) + "/parent" }

func commentsPath(repo string, number int) string { return issuePath(repo, number) + "/comments" }

// issueJSON is a GitHub issue object, as much of it as the context reads.
// comments is the total the truncation flag is measured against, so it is given
// even where the comment fixture is left empty.
func issueJSON(repo string, number int, title, body string, comments int) string {
	return fmt.Sprintf(`{"number":%d,"title":%q,"body":%q,"state":"open","comments":%d,
		"html_url":"https://github.com/%s/issues/%d",
		"repository_url":"https://api.github.com/repos/%s"}`,
		number, title, body, comments, repo, number, repo)
}

// commentJSON is a GitHub issue comment, numbered so that the order it is
// written in is visible in what comes back.
func commentJSON(n int, login, body string) string {
	return fmt.Sprintf(`{"user":{"login":%q,"type":"User"},"body":%q,
		"created_at":"2026-02-0%dT00:00:00Z","html_url":%q}`,
		login, body, n, issueCommentURL(n))
}

func issueCommentURL(n int) string {
	return fmt.Sprintf("https://github.com/owner/repo/issues/10#issuecomment-%d", n)
}

// linkedIssues answers every endpoint the fixture pull request's body reaches:
// #10 with a parent, #11 with none, and other/repo#12 with none.
var linkedIssues = map[string]string{
	issuePath("owner/repo", 10):  issueJSON("owner/repo", 10, "Issue 10", "The tenth body", 3),
	parentPath("owner/repo", 10): issueJSON("owner/repo", 9, "Issue 9", "The parent body", 1),
	issuePath("owner/repo", 11):  issueJSON("owner/repo", 11, "Issue 11", "", 1),
	issuePath("other/repo", 12):  issueJSON("other/repo", 12, "Issue 12", "Elsewhere", 0),
}

// linkedIssueComments gives the issue with a parent three comments and the
// parent one, which is the shape the acceptance criteria name.
//
// #11 has one as well, and no parent at all, which is what catches a fetch
// placed after the parent lookup: every issue with no parent leaves that block
// early, and its comments would go missing with the whole positive case still
// passing on #10.
var linkedIssueComments = map[string][]string{
	commentsPath("owner/repo", 10): {
		commentJSON(1, "178inaba", "first"),
		commentJSON(2, "reviewer1", "second"),
		commentJSON(3, "178inaba", "third"),
	},
	commentsPath("owner/repo", 9):  {commentJSON(4, "reviewer1", "on the parent")},
	commentsPath("owner/repo", 11): {commentJSON(5, "reviewer1", "on the eleventh")},
}

// issue10Comments, issue9Comments and issue11Comments are what the fixture
// issues carry, written once because several tests assert on the same lists.
var issue10Comments = []pullrequest.IssueComment{
	{Author: new("178inaba"), AuthorType: new("User"), Body: "first", CreatedAt: "2026-02-01T00:00:00Z", URL: issueCommentURL(1)},
	{Author: new("reviewer1"), AuthorType: new("User"), Body: "second", CreatedAt: "2026-02-02T00:00:00Z", URL: issueCommentURL(2)},
	{Author: new("178inaba"), AuthorType: new("User"), Body: "third", CreatedAt: "2026-02-03T00:00:00Z", URL: issueCommentURL(3)},
}

var issue9Comments = []pullrequest.IssueComment{
	{Author: new("reviewer1"), AuthorType: new("User"), Body: "on the parent", CreatedAt: "2026-02-04T00:00:00Z", URL: issueCommentURL(4)},
}

var issue11Comments = []pullrequest.IssueComment{
	{Author: new("reviewer1"), AuthorType: new("User"), Body: "on the eleventh", CreatedAt: "2026-02-05T00:00:00Z", URL: issueCommentURL(5)},
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
          {"author": {"login": "reviewer1", "__typename": "User"}, "body": "> <!-- review-response -->\n引用返信", "createdAt": "2026-01-03T00:00:00Z", "lastEditedAt": "2026-01-05T00:00:00Z", "url": "https://example.com/c3"},
          {"author": null, "body": "CI 通知", "createdAt": "2026-01-04T00:00:00Z", "url": "https://example.com/c4"}
        ]
      },
      "reviews": {
        "totalCount": 1,
        "nodes": [
          {"author": {"login": "reviewer1", "__typename": "User"}, "state": "CHANGES_REQUESTED", "body": "優先度1: テスト不足", "url": "https://example.com/r1", "submittedAt": "2026-01-01T00:00:00Z", "lastEditedAt": "2026-02-01T00:00:00Z"}
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

	got, err := pullrequest.Fetch(t.Context(), serve(t, p), repo, pr, limits, noChange())
	if err != nil {
		t.Fatalf("Fetch: %v", err)
	}
	return got
}

func TestFetch(t *testing.T) {
	t.Parallel()

	got := fetch(t, pages{body: fixtureBody, issues: linkedIssues, issueComments: linkedIssueComments}, meta, pullrequest.DefaultLimits)

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

	t.Run("when the read began", func(t *testing.T) {
		// Whole seconds, because a document stamped more precisely than
		// GitHub's own timestamps would sort a comment made in the same second
		// before the read that did not see it.
		at, err := time.Parse(time.RFC3339, got.FetchedAt)
		if err != nil {
			t.Fatalf("fetched_at = %q, which does not parse: %v", got.FetchedAt, err)
		}
		if at.Nanosecond() != 0 {
			t.Errorf("fetched_at = %q, want it truncated to the second", got.FetchedAt)
		}
	})

	t.Run("the issues the body closes", func(t *testing.T) {
		// A bare #99 and a url are not among them, because GitHub does not
		// close on those either; #10 appears twice and once.
		other := "other/repo"
		want := []pullrequest.LinkedIssue{
			{
				Number: 10, Title: new("Issue 10"), Body: new("The tenth body"),
				CommentsTotalCount: 3, Comments: issue10Comments,
				// A parent in this repository writes no repository, the way
				// the linked issue itself does.
				Parent: &pullrequest.IssueParent{
					Number: 9, Title: "Issue 9", Body: "The parent body",
					CommentsTotalCount: 1, Comments: issue9Comments,
				},
			},
			// An empty body is empty rather than null: null is reserved for an
			// issue that could not be read at all.
			{
				Number: 11, Title: new("Issue 11"), Body: new(""),
				CommentsTotalCount: 1, Comments: issue11Comments,
			},
			{Repo: &other, Number: 12, Title: new("Issue 12"), Body: new("Elsewhere"), Comments: []pullrequest.IssueComment{}},
		}
		if diff := cmp.Diff(want, got.LinkedIssues); diff != "" {
			t.Errorf("linked_issues (-want +got):\n%s", diff)
		}
		if len(got.Warnings) != 0 {
			t.Errorf("warnings = %v, want none", got.Warnings)
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
			{Author: &reviewer, AuthorType: &user, Body: "> <!-- review-response -->\n引用返信", CreatedAt: "2026-01-03T00:00:00Z", LastEditedAt: new("2026-01-05T00:00:00Z"), URL: "https://example.com/c3"},
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
			LastEditedAt: new("2026-02-01T00:00:00Z"),
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

	got := fetch(t, pages{body: truncatedBotBody, issues: linkedIssues}, meta, pullrequest.Limits{Comments: 500, Threads: 300, ThreadComments: 1})

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
	got := fetch(t, pages{body: body, issues: linkedIssues}, meta, pullrequest.DefaultLimits)

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
	got := fetch(t, pages{body: fixtureBody, issues: linkedIssues}, others, pullrequest.DefaultLimits)

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

// TestFetchDegradesOnAnUnreadableIssue covers the three answers that mean the
// issue may no longer be there. The run goes on with what it has: a review
// that stopped because a closed issue was deleted would be stopped for good.
func TestFetchDegradesOnAnUnreadableIssue(t *testing.T) {
	t.Parallel()

	one := meta
	one.Body = "Closes #10"

	tests := []struct {
		name        string
		status      map[string]int
		want        pullrequest.LinkedIssue
		wantWarning string
	}{
		{
			name:        "the issue was deleted",
			status:      map[string]int{issuePath("owner/repo", 10): http.StatusNotFound},
			want:        pullrequest.LinkedIssue{Number: 10, Comments: []pullrequest.IssueComment{}},
			wantWarning: "owner/repo#10: the issue could not be read (HTTP 404)",
		},
		{
			name:        "the issue is not ours to see",
			status:      map[string]int{issuePath("owner/repo", 10): http.StatusForbidden},
			want:        pullrequest.LinkedIssue{Number: 10, Comments: []pullrequest.IssueComment{}},
			wantWarning: "owner/repo#10: the issue could not be read (HTTP 403)",
		},
		{
			// Only the parent is lost here, and the body that was read is
			// kept, comments and all: dropping either would throw away what the
			// run came for, and the comments are the half a fetch placed after
			// the parent lookup would lose.
			name:   "only the parent is unreadable",
			status: map[string]int{parentPath("owner/repo", 10): http.StatusGone},
			want: pullrequest.LinkedIssue{
				Number: 10, Title: new("Issue 10"), Body: new("The tenth body"),
				CommentsTotalCount: 3, Comments: issue10Comments,
			},
			wantWarning: "owner/repo#10: the parent issue could not be read (HTTP 410)",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got := fetch(t, pages{body: fixtureBody, issues: linkedIssues, issueComments: linkedIssueComments, issueStatus: tc.status}, one, pullrequest.DefaultLimits)

			if diff := cmp.Diff([]pullrequest.LinkedIssue{tc.want}, got.LinkedIssues); diff != "" {
				t.Errorf("linked_issues (-want +got):\n%s", diff)
			}
			if diff := cmp.Diff([]string{tc.wantWarning}, got.Warnings); diff != "" {
				t.Errorf("warnings (-want +got):\n%s", diff)
			}
		})
	}
}

// TestFetchFailsOnAnUnreadableIssueForAnotherReason is the other side of the
// degradation: a server error or an expired token says nothing about the
// issue, and answering with a null title would report it as gone.
func TestFetchFailsOnAnUnreadableIssueForAnotherReason(t *testing.T) {
	t.Parallel()

	one := meta
	one.Body = "Closes #10"
	c := serve(t, pages{
		body: fixtureBody, issues: linkedIssues,
		issueStatus: map[string]int{issuePath("owner/repo", 10): http.StatusInternalServerError},
	})
	if _, err := pullrequest.Fetch(t.Context(), c, repo, one, pullrequest.DefaultLimits, noChange()); err == nil {
		t.Fatal("Fetch succeeded, want the server error to stop it")
	}
}

// TestFetchFailsOnUnreadableIssueComments is the rule that separates the two:
// a body that is present means the issue was read whole, so comments that could
// not be read stop the run rather than degrading it. Reported as a warning, an
// issue with a body and an empty comment list would be indistinguishable from
// one nobody has commented on.
func TestFetchFailsOnUnreadableIssueComments(t *testing.T) {
	t.Parallel()

	one := meta
	one.Body = "Closes #10"

	for _, status := range []int{http.StatusNotFound, http.StatusForbidden, http.StatusInternalServerError} {
		t.Run(fmt.Sprint(status), func(t *testing.T) {
			t.Parallel()

			c := serve(t, pages{
				body: fixtureBody, issues: linkedIssues, issueComments: linkedIssueComments,
				issueStatus: map[string]int{commentsPath("owner/repo", 10): status},
			})
			if _, err := pullrequest.Fetch(t.Context(), c, repo, one, pullrequest.DefaultLimits, noChange()); err == nil {
				t.Fatal("Fetch succeeded, want the comment failure to stop it")
			}
		})
	}
}

// TestFetchStopsAtTheIssueCommentLimit is MAX_ISSUE_COMMENTS on both an issue
// and a parent: what fits is written oldest first, and the total says what was
// left behind.
func TestFetchStopsAtTheIssueCommentLimit(t *testing.T) {
	t.Parallel()

	one := meta
	one.Body = "Closes #10"
	limits := pullrequest.DefaultLimits
	limits.IssueComments = 2

	// The parent is an issue for the cap as much as the issue is, so it is
	// given three of its own here rather than the one the shared fixture has.
	issues := maps.Clone(linkedIssues)
	issues[parentPath("owner/repo", 10)] = issueJSON("owner/repo", 9, "Issue 9", "The parent body", 3)
	comments := maps.Clone(linkedIssueComments)
	comments[commentsPath("owner/repo", 9)] = []string{
		commentJSON(4, "reviewer1", "on the parent"),
		commentJSON(5, "178inaba", "and again"),
		commentJSON(6, "reviewer1", "once more"),
	}

	got := fetch(t, pages{body: fixtureBody, issues: issues, issueComments: comments}, one, limits)

	issue := got.LinkedIssues[0]
	if len(issue.Comments) != 2 || !issue.CommentsTruncated || issue.CommentsTotalCount != 3 {
		t.Errorf("issue = %d comments, total %d, truncated %v; want 2, 3 and true",
			len(issue.Comments), issue.CommentsTotalCount, issue.CommentsTruncated)
	}
	if want := []string{"first", "second"}; !cmp.Equal(want, bodiesOf(issue.Comments)) {
		t.Errorf("issue comments = %v, want the oldest two %v", bodiesOf(issue.Comments), want)
	}
	parent := issue.Parent
	if len(parent.Comments) != 2 || !parent.CommentsTruncated || parent.CommentsTotalCount != 3 {
		t.Errorf("parent = %d comments, total %d, truncated %v; want 2, 3 and true",
			len(parent.Comments), parent.CommentsTotalCount, parent.CommentsTruncated)
	}
}

// TestFetchReadsCommentsFromTheIssuesOwnRepository pins where the comments are
// asked for, which the shared fixture cannot: an issue and a parent may each
// live somewhere other than the pull request, and asking the wrong repository
// answers 404 rather than anything a caller would notice.
func TestFetchReadsCommentsFromTheIssuesOwnRepository(t *testing.T) {
	t.Parallel()

	one := meta
	one.Body = "Resolves other/repo#12"

	got := fetch(t, pages{
		body: fixtureBody,
		issues: map[string]string{
			issuePath("other/repo", 12):  issueJSON("other/repo", 12, "Issue 12", "Elsewhere", 1),
			parentPath("other/repo", 12): issueJSON("third/repo", 20, "Issue 20", "Elsewhere again", 1),
		},
		issueComments: map[string][]string{
			commentsPath("other/repo", 12): {commentJSON(7, "reviewer1", "in the issue's repository")},
			commentsPath("third/repo", 20): {commentJSON(8, "reviewer1", "in the parent's repository")},
		},
	}, one, pullrequest.DefaultLimits)

	issue := got.LinkedIssues[0]
	if len(issue.Comments) != 1 || issue.Comments[0].Body != "in the issue's repository" {
		t.Errorf("issue comments = %+v, want the one from other/repo", issue.Comments)
	}
	if len(issue.Parent.Comments) != 1 || issue.Parent.Comments[0].Body != "in the parent's repository" {
		t.Errorf("parent comments = %+v, want the one from third/repo", issue.Parent.Comments)
	}
}

// TestFetchLeavesNoIssueCommentsNil keeps the document's promise in the value
// rather than in the bytes: encoding/json/v2 would write a nil slice as [] on
// its own, so only the value tells a caller reading the context back whether
// an empty list was meant.
func TestFetchLeavesNoIssueCommentsNil(t *testing.T) {
	t.Parallel()

	one := meta
	one.Body = "Resolves other/repo#12"

	got := fetch(t, pages{body: fixtureBody, issues: linkedIssues, issueComments: linkedIssueComments}, one, pullrequest.DefaultLimits)

	if got.LinkedIssues[0].Comments == nil {
		t.Error("comments = nil on an issue nobody has commented on, want an empty list")
	}
}

func bodiesOf(comments []pullrequest.IssueComment) []string {
	out := make([]string, 0, len(comments))
	for _, c := range comments {
		out = append(out, c.Body)
	}
	return out
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
			if got, err := pullrequest.Fetch(t.Context(), serve(t, p), repo, meta, pullrequest.DefaultLimits, noChange()); err == nil {
				t.Fatalf("Fetch = %+v, want a failure when the %s query fails", got, query)
			}
		})
	}
}

// fullContext is the smallest document that satisfies every declaration on
// Context: the fields its readers depend on and nothing else. What
// `ccx pr context` writes carries far more, none of which is declared, so
// building the document here rather than fetching one keeps each case below
// about the one field it edits.
func fullContext() map[string]any {
	return map[string]any{
		"fetched_at": "2026-01-10T00:00:00Z",
		"repo":       "owner/repo",
		"is_own_pr": false,
		"pr": map[string]any{
			"number":   5,
			"base_ref": "main",
			"head_ref": "feature/x",
			"head_oid": "abc123",
		},
		"review_threads": []any{},
	}
}

// drop removes what the path names and put writes a value there, walking the
// one nested object the declaration reaches into.
func drop(path ...string) func(map[string]any) {
	return func(m map[string]any) {
		obj, last := walkTo(m, path)
		delete(obj, last)
	}
}

func put(value any, path ...string) func(map[string]any) {
	return func(m map[string]any) {
		obj, last := walkTo(m, path)
		obj[last] = value
	}
}

func walkTo(m map[string]any, path []string) (map[string]any, string) {
	for _, key := range path[:len(path)-1] {
		m = m[key].(map[string]any)
	}
	return m, path[len(path)-1]
}

// edited is one case's document, as the bytes a parser is given.
func edited(t *testing.T, edit func(map[string]any)) []byte {
	t.Helper()

	doc := fullContext()
	if edit != nil {
		edit(doc)
	}
	b, err := json.Marshal(doc)
	if err != nil {
		t.Fatalf("marshal the case document: %v", err)
	}
	return b
}

// TestParseContextRefusesADocumentAgainstItsDeclaration covers what used to be
// three sets of hand-written checks in three parsers. The union is declared
// once on Context, so every reader of a context file refuses the same
// documents whichever fields it goes on to dereference.
func TestParseContextRefusesADocumentAgainstItsDeclaration(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name string
		edit func(map[string]any)
		want string
	}{
		{name: "the whole document"},

		{name: "no fetched_at", edit: drop("fetched_at"), want: "ctx.json is missing fetched_at"},
		{name: "null fetched_at", edit: put(nil, "fetched_at"), want: "ctx.json is missing fetched_at"},

		{name: "no repo", edit: drop("repo"), want: "ctx.json is missing repo"},
		{name: "null repo", edit: put(nil, "repo"), want: "ctx.json is missing repo"},
		{name: "empty repo", edit: put("", "repo"), want: "ctx.json sets repo to an empty string"},

		// The object the four below sit in. Without it they are not absent to
		// a validator that reads a nested declaration only where the document
		// supplied the object, and a reader would find a zero number.
		{name: "no pr", edit: drop("pr"), want: "ctx.json is missing pr"},
		{name: "null pr", edit: put(nil, "pr"), want: "ctx.json is missing pr"},

		{name: "no is_own_pr", edit: drop("is_own_pr"), want: "ctx.json is missing is_own_pr"},
		{name: "null is_own_pr", edit: put(nil, "is_own_pr"), want: "ctx.json is missing is_own_pr"},

		{name: "no review_threads", edit: drop("review_threads"), want: "ctx.json is missing review_threads"},
		{name: "null review_threads", edit: put(nil, "review_threads"), want: "ctx.json is missing review_threads"},

		{name: "no pr.number", edit: drop("pr", "number"), want: "pr is missing number in ctx.json"},
		{name: "null pr.number", edit: put(nil, "pr", "number"), want: "pr is missing number in ctx.json"},
		{name: "zero pr.number", edit: put(0, "pr", "number"), want: "pr sets number to a number that is not positive in ctx.json"},
		{name: "negative pr.number", edit: put(-1, "pr", "number"), want: "pr sets number to a number that is not positive in ctx.json"},

		{name: "no pr.base_ref", edit: drop("pr", "base_ref"), want: "pr is missing base_ref in ctx.json"},
		{name: "null pr.base_ref", edit: put(nil, "pr", "base_ref"), want: "pr is missing base_ref in ctx.json"},
		{name: "empty pr.base_ref", edit: put("", "pr", "base_ref"), want: "pr sets base_ref to an empty string in ctx.json"},

		{name: "no pr.head_ref", edit: drop("pr", "head_ref"), want: "pr is missing head_ref in ctx.json"},
		{name: "null pr.head_ref", edit: put(nil, "pr", "head_ref"), want: "pr is missing head_ref in ctx.json"},
		{name: "empty pr.head_ref", edit: put("", "pr", "head_ref"), want: "pr sets head_ref to an empty string in ctx.json"},

		{name: "no pr.head_oid", edit: drop("pr", "head_oid"), want: "pr is missing head_oid in ctx.json"},
		{name: "null pr.head_oid", edit: put(nil, "pr", "head_oid"), want: "pr is missing head_oid in ctx.json"},
		{name: "empty pr.head_oid", edit: put("", "pr", "head_oid"), want: "pr sets head_oid to an empty string in ctx.json"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			b := edited(t, tc.edit)
			got, err := pullrequest.ParseContext(b, "ctx.json")
			if tc.want == "" {
				if err != nil {
					t.Fatalf("ParseContext(%s) = %v, want it accepted", b, err)
				}
				return
			}
			if err == nil {
				t.Fatalf("ParseContext(%s) = %+v, want the error %q", b, got, tc.want)
			}
			if err.Error() != tc.want {
				t.Errorf("ParseContext(%s) = %q, want %q", b, err, tc.want)
			}
		})
	}
}

// TestParseContextKeepsTheUnconstrainedFieldsWhole is the other half of the
// declaration: false and the empty list are answers `ccx pr context` writes,
// so a rule that read either as absence would refuse a document that says
// exactly what it means.
func TestParseContextKeepsTheUnconstrainedFieldsWhole(t *testing.T) {
	t.Parallel()

	want := pullrequest.Context{
		FetchedAt: "2026-01-10T00:00:00Z",
		Repo:      "owner/repo",
		PR: pullrequest.PR{
			Number: 5, BaseRef: "main", HeadRef: "feature/x", HeadOID: "abc123",
		},
		ReviewThreads: []pullrequest.Thread{},
	}

	got, err := pullrequest.ParseContext(edited(t, nil), "ctx.json")
	if err != nil {
		t.Fatalf("ParseContext: %v", err)
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("ParseContext (-want +got):\n%s", diff)
	}

	own, err := pullrequest.ParseContext(edited(t, put(true, "is_own_pr")), "ctx.json")
	if err != nil {
		t.Fatalf("ParseContext: %v", err)
	}
	if !own.IsOwnPR {
		t.Error("is_own_pr true was read as false")
	}
}

// TestParseContextReportsAMalformedDocument is the decoder's own complaint
// arriving through the same entry point, in the words a violation uses.
func TestParseContextReportsAMalformedDocument(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct{ name, in string }{
		{"not JSON at all", "not json"},
		{"a field of the wrong kind", `{"repo":5}`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			if got, err := pullrequest.ParseContext([]byte(tc.in), "ctx.json"); err == nil {
				t.Fatalf("ParseContext(%s) = %+v, want a failure", tc.in, got)
			} else if !strings.Contains(err.Error(), "ctx.json") {
				t.Errorf("ParseContext(%s) = %q, want it to name the document", tc.in, err)
			}
		})
	}
}
