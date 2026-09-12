package pullrequest_test

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

func TestParseCommentBody(t *testing.T) {
	t.Parallel()

	work := t.TempDir()
	if err := os.WriteFile(filepath.Join(work, "report.md"), []byte("# Done\n"), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	got, err := pullrequest.ParseCommentBody(work, "report.md")
	if err != nil {
		t.Fatalf("ParseCommentBody: %v", err)
	}
	if got != "# Done\n" {
		t.Errorf("body = %q, want the file's content", got)
	}

	for _, tc := range []struct {
		name, file, wantErr string
	}{
		// A path would reach round the directory binding that keeps parallel
		// runs on different pull requests out of each other's files.
		{name: "a path", file: "sub/report.md", wantErr: "bare file name"},
		{name: "nothing named", file: "", wantErr: "bare file name"},
		{name: "not there", file: "nope.md", wantErr: "not found in the work dir"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			if _, err := pullrequest.ParseCommentBody(work, tc.file); err == nil {
				t.Fatalf("ParseCommentBody(%q) = nil, want a refusal", tc.file)
			} else if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
			}
		})
	}
}

// withLiveHead answers the head check's pull request lookup and hands anything
// else to h, so that a case asserting what reached the comment endpoint does
// not carry a second copy of the check's fixture.
func withLiveHead(t *testing.T, liveHead string, h http.Handler) http.Handler {
	t.Helper()

	live := prHandler(t, liveHead)
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/graphql" {
			live.ServeHTTP(w, r)
			return
		}
		h.ServeHTTP(w, r)
	})
}

func TestPostComment(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	head := gittest.Rev(t, repo, "HEAD")
	// The pushed-fixes shape the check is for: the document was fetched at the
	// previous commit, the run pushed, and the comment goes out with no second
	// fetch.
	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: gittest.Rev(t, repo, "HEAD~")}

	var seenPath, seenBody string
	c := ghapitest.New(t, withLiveHead(t, head, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var req struct {
			Body string `json:"body"`
		}
		if err := json.UnmarshalRead(r.Body, &req); err != nil {
			t.Errorf("decode the request body: %v", err)
		}
		seenPath, seenBody = r.URL.Path, req.Body
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"html_url":"https://github.com/owner/repo/pull/5#issuecomment-1"}`)
	})))

	got, err := pullrequest.PostComment(t.Context(), runner.Exec{}, c, repo,
		target, "# Done\n\nEverything is answered.\n")
	if err != nil {
		t.Fatalf("PostComment: %v", err)
	}

	if got.URL != "https://github.com/owner/repo/pull/5#issuecomment-1" {
		t.Errorf("url = %q, want the one GitHub answered with", got.URL)
	}
	if want := "/repos/owner/repo/issues/5/comments"; seenPath != want {
		t.Errorf("posted to %q, want %q", seenPath, want)
	}
	want := "# Done\n\nEverything is answered.\n"
	if seenBody != want {
		t.Errorf("body = %q, want %q", seenBody, want)
	}
}

// A body GitHub would autolink into notifications on unrelated issues is
// refused before the head is even confirmed, so nothing is posted.
func TestPostCommentRefusesABodyThatNumbersItsItems(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: gittest.Rev(t, repo, "HEAD")}
	c := ghapitest.New(t, http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
		t.Error("something was posted despite the bare #N numbering")
	}))

	_, err := pullrequest.PostComment(t.Context(), runner.Exec{}, c, repo,
		target, "#1 one\n#2 two\n#3 three\n")
	if err == nil {
		t.Fatal("PostComment with bare #N numbering succeeded, want a refusal")
	}
	if !strings.Contains(err.Error(), "3 distinct bare #N") {
		t.Errorf("error = %q, want it to name what was found", err)
	}
}

// The checkout is what GitHub holds, but nothing the report was written
// against leads to it — a rebase or a force-push. The report is about a state
// the pull request no longer has, so nothing is posted.
func TestPostCommentRefusesADocumentOffTheBranch(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: "0000000000000000000000000000000000000000"}
	c := ghapitest.New(t, withLiveHead(t, gittest.Rev(t, repo, "HEAD"), http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
		t.Error("something was posted despite the document being off the branch")
	})))

	_, err := pullrequest.PostComment(t.Context(), runner.Exec{}, c, repo,
		target, "anything")
	if err == nil {
		t.Fatal("PostComment from a document off the branch succeeded, want a refusal")
	}
	if !strings.Contains(err.Error(), "ccx pr context") {
		t.Errorf("error = %q, want it to say to fetch the document again", err)
	}
}
