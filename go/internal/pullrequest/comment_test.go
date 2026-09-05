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

// posted captures what reached the comment endpoint, so that a case can assert
// the body without a second copy of the marker.
type posted struct {
	path string
	body string
}

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

func TestPostComment(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: gittest.Rev(t, repo, "HEAD")}

	var seen posted
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var req struct {
			Body string `json:"body"`
		}
		if err := json.UnmarshalRead(r.Body, &req); err != nil {
			t.Errorf("decode the request body: %v", err)
		}
		seen = posted{path: r.URL.Path, body: req.Body}
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"html_url":"https://github.com/owner/repo/pull/5#issuecomment-1"}`)
	}))

	got, err := pullrequest.PostComment(t.Context(), runner.Exec{}, c, repo,
		target, pullrequest.MarkReviewResponse, "# Done\n\nEverything is answered.\n")
	if err != nil {
		t.Fatalf("PostComment: %v", err)
	}

	if got.URL != "https://github.com/owner/repo/pull/5#issuecomment-1" {
		t.Errorf("url = %q, want the one GitHub answered with", got.URL)
	}
	if want := "/repos/owner/repo/issues/5/comments"; seen.path != want {
		t.Errorf("posted to %q, want %q", seen.path, want)
	}
	// The marker, a blank line, then the file's content — so that what decides
	// is_skill_comment is the constant that wrote it, and the markdown after it
	// renders as written.
	want := pullrequest.SkillMarker + "\n\n# Done\n\nEverything is answered.\n"
	if seen.body != want {
		t.Errorf("body = %q, want %q", seen.body, want)
	}
}

// A name the command does not own is refused before anything is sent: the
// marker is what the reading side keys on, and one it does not recognise would
// leave the comment counting as somebody else's remark for ever.
func TestPostCommentRefusesAnUnknownMark(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: gittest.Rev(t, repo, "HEAD")}
	c := ghapitest.New(t, http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
		t.Error("something was posted despite the unknown mark")
	}))

	_, err := pullrequest.PostComment(t.Context(), runner.Exec{}, c, repo, target, "other", "anything")
	if err == nil {
		t.Fatal("PostComment with an unknown mark succeeded, want a refusal")
	}
	if !strings.Contains(err.Error(), "review-response") {
		t.Errorf("error = %q, want it to name the mark it does own", err)
	}
}

// A checkout that has moved on is a run whose report is about code the pull
// request no longer holds, so nothing is posted.
func TestPostCommentRefusesAMovedHead(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: "0000000000000000000000000000000000000000"}
	c := ghapitest.New(t, http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
		t.Error("something was posted despite the moved head")
	}))

	_, err := pullrequest.PostComment(t.Context(), runner.Exec{}, c, repo,
		target, pullrequest.MarkReviewResponse, "anything")
	if err == nil {
		t.Fatal("PostComment from a moved head succeeded, want a refusal")
	}
	if !strings.Contains(err.Error(), "differs from PR head") {
		t.Errorf("error = %q, want it to say the head moved", err)
	}
}
