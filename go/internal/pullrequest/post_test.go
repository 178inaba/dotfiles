package pullrequest_test

import (
	"encoding/json/v2"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

func TestWorkDir(t *testing.T) {
	t.Parallel()

	// The identifier is the context file's own name with its prefix and
	// extension taken off, so that the two commands that write into the
	// directory and the one that hands it out cannot disagree about it.
	got := pullrequest.WorkDir("/scratch/pr-context-owner@repo-5.json")
	if want := "/scratch/deep-review-owner@repo-5"; got != want {
		t.Errorf("WorkDir = %q, want %q", got, want)
	}
}

func TestRequireInWorkDir(t *testing.T) {
	t.Parallel()

	scratch := t.TempDir()
	contextFile := filepath.Join(scratch, "pr-context-owner@repo-5.json")
	work := pullrequest.WorkDir(contextFile)
	if err := os.MkdirAll(work, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	if err := pullrequest.RequireInWorkDir(filepath.Join(work, "review.json"), "review_path", contextFile); err != nil {
		t.Errorf("a file in the work dir was rejected: %v", err)
	}
	// A fixed name in the shared scratch directory is what a parallel review of
	// another pull request overwrites.
	err := pullrequest.RequireInWorkDir(filepath.Join(scratch, "review.json"), "review_path", contextFile)
	if err == nil {
		t.Fatal("a file outside the work dir was accepted")
	}
	if !strings.Contains(err.Error(), "review_path") {
		t.Errorf("error = %q, want it to name the field that would have been right", err)
	}
}

func TestParseSubmission(t *testing.T) {
	t.Parallel()

	work := t.TempDir()
	if err := os.WriteFile(filepath.Join(work, "body.md"), []byte("# From a file\n"), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	tests := []struct {
		name    string
		in      string
		want    pullrequest.Submission
		wantErr string
	}{
		{
			name: "inline bodies",
			in:   `{"assessment":"Approve可能","body":"looks good","comments":[{"path":"a.go","line":3,"body":"here"}]}`,
			want: pullrequest.Submission{
				Assessment: pullrequest.AssessmentApprove, Body: "looks good",
				Comments: []pullrequest.SubmissionComment{{Path: "a.go", Line: 3, Body: "here"}},
			},
		},
		{
			// Long prose written as a JSON string loses its meaning to one
			// missed escape, so naming a plain markdown file is supported.
			name: "a named body",
			in:   `{"assessment":"要議論","body_file":"body.md","comments":[]}`,
			want: pullrequest.Submission{
				Assessment: pullrequest.AssessmentDiscuss, Body: "# From a file\n",
				Comments: []pullrequest.SubmissionComment{},
			},
		},
		{name: "no assessment", in: `{"body":"x","comments":[]}`, wantErr: "assessment missing"},
		{name: "both forms of body", in: `{"assessment":"要議論","body":"x","body_file":"body.md","comments":[]}`, wantErr: "exactly one of body"},
		{name: "neither form of body", in: `{"assessment":"要議論","comments":[]}`, wantErr: "exactly one of body"},
		{name: "an empty body_file", in: `{"assessment":"要議論","body_file":"","comments":[]}`, wantErr: "exactly one of body"},
		// Allowing a path would reach round the directory binding.
		{name: "a body_file with a path", in: `{"assessment":"要議論","body_file":"sub/x.md","comments":[]}`, wantErr: "bare filename"},
		{name: "a body_file that is not there", in: `{"assessment":"要議論","body_file":"nope.md","comments":[]}`, wantErr: "not found in the review work dir"},
		{name: "comments missing", in: `{"assessment":"要議論","body":"x"}`, wantErr: "comments must be an array"},
		{name: "comments not an array", in: `{"assessment":"要議論","body":"x","comments":{}}`, wantErr: "comments must be an array"},
		{name: "a comment without a line", in: `{"assessment":"要議論","body":"x","comments":[{"path":"a.go","body":"y"}]}`, wantErr: "comments must be an array"},
		{name: "a comment whose line is not a number", in: `{"assessment":"要議論","body":"x","comments":[{"path":"a.go","line":"3","body":"y"}]}`, wantErr: "comments must be an array"},
		{name: "not json at all", in: `not json`, wantErr: "invalid JSON"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := pullrequest.ParseSubmission([]byte(tc.in), work, "review.json")
			if tc.wantErr != "" {
				if err == nil {
					t.Fatalf("ParseSubmission = %+v, want an error mentioning %q", got, tc.wantErr)
				}
				if !strings.Contains(err.Error(), tc.wantErr) {
					t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseSubmission: %v", err)
			}
			if diff := cmp.Diff(tc.want, got); diff != "" {
				t.Errorf("ParseSubmission (-want +got):\n%s", diff)
			}
		})
	}
}

// diffRepo builds a repository with a diff against origin/main: one added line
// at the end of a file, and one added line whose own text begins with "++".
func diffRepo(t *testing.T) string {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	bare := filepath.Join(base, "origin.git")
	repo := filepath.Join(base, "repo")
	gittest.Init(t, bare, "--bare", "-b", "main")
	gittest.Clone(t, bare, repo)
	gittest.Write(t, filepath.Join(repo, "file.txt"), "one\ntwo\nthree\n")
	gittest.Run(t, repo, "add", "file.txt")
	gittest.Run(t, repo, "commit", "-qm", "init")
	gittest.Run(t, repo, "push", "-q", "-u", "origin", "main")

	gittest.Run(t, repo, "switch", "-qc", "feature/x")
	// The second line added here renders as "+++ still added", which a diff
	// reader that checked for file headers first would take for one.
	gittest.Write(t, filepath.Join(repo, "file.txt"), "one\ntwo\nthree\nfour\n++ still added\n")
	gittest.Run(t, repo, "commit", "-qam", "add lines")
	return repo
}

func TestPost(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: gittest.Rev(t, repo, "HEAD")}

	var gotPath, gotBody string
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath = r.URL.Path
		b, err := io.ReadAll(r.Body)
		if err != nil {
			t.Errorf("read the request body: %v", err)
			return
		}
		gotBody = string(b)
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"html_url":"https://github.com/owner/repo/pull/5#pullrequestreview-1"}`)
	}))

	sub := pullrequest.Submission{
		Assessment: pullrequest.AssessmentChanges,
		Body:       "needs work",
		// Line 4 is the added "four", line 5 the one beginning with "++".
		Comments: []pullrequest.SubmissionComment{
			{Path: "file.txt", Line: 4, Body: "this one"},
			{Path: "file.txt", Line: 5, Body: "and this"},
		},
	}
	got, err := pullrequest.Post(t.Context(), runner.Exec{}, c, repo, target, sub)
	if err != nil {
		t.Fatalf("Post: %v", err)
	}

	if want := "https://github.com/owner/repo/pull/5#pullrequestreview-1"; got.URL != want {
		t.Errorf("url = %q, want %q", got.URL, want)
	}
	if want := "/repos/owner/repo/pulls/5/reviews"; gotPath != want {
		t.Errorf("posted to %q, want %q", gotPath, want)
	}

	var payload struct {
		CommitID string `json:"commit_id"`
		Event    string `json:"event"`
		Body     string `json:"body"`
		Comments []struct {
			Path string `json:"path"`
			Line int    `json:"line"`
			Body string `json:"body"`
		} `json:"comments"`
	}
	if err := json.Unmarshal([]byte(gotBody), &payload); err != nil {
		t.Fatalf("decode the payload: %v\n%s", err, gotBody)
	}
	if payload.CommitID != target.HeadOID || payload.Event != "REQUEST_CHANGES" || payload.Body != "needs work" {
		t.Errorf("payload = %+v, want the head, REQUEST_CHANGES and the body", payload)
	}
	if len(payload.Comments) != 2 || payload.Comments[1].Line != 5 {
		t.Errorf("comments = %+v, want both, including the line that reads like a diff header", payload.Comments)
	}
}

// TestPostMapsTheAssessment pins the decision table. It lives here rather than
// in the prompt so that a reviewer's politeness cannot change what GitHub is
// told the review was.
func TestPostMapsTheAssessment(t *testing.T) {
	t.Parallel()

	tests := []struct {
		assessment pullrequest.Assessment
		want       string
	}{
		{assessment: pullrequest.AssessmentApprove, want: "APPROVE"},
		{assessment: pullrequest.AssessmentChanges, want: "REQUEST_CHANGES"},
		{assessment: pullrequest.AssessmentDiscuss, want: "COMMENT"},
	}

	repo := diffRepo(t)
	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: gittest.Rev(t, repo, "HEAD")}
	for _, tc := range tests {
		t.Run(string(tc.assessment), func(t *testing.T) {
			t.Parallel()

			var event string
			c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				var payload struct {
					Event string `json:"event"`
				}
				if err := json.UnmarshalRead(r.Body, &payload); err != nil {
					t.Errorf("decode the payload: %v", err)
				}
				event = payload.Event
				w.Header().Set("Content-Type", "application/json")
				fmt.Fprint(w, `{"html_url":"https://example.com/r"}`)
			}))

			if _, err := pullrequest.Post(t.Context(), runner.Exec{}, c, repo, target,
				pullrequest.Submission{Assessment: tc.assessment, Body: "x"}); err != nil {
				t.Fatalf("Post: %v", err)
			}
			if event != tc.want {
				t.Errorf("event = %q, want %q", event, tc.want)
			}
		})
	}
}

func TestPostRefuses(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	at := gittest.Rev(t, repo, "HEAD")

	tests := []struct {
		name    string
		target  pullrequest.Target
		sub     pullrequest.Submission
		wantErr string
	}{
		{
			name:    "an assessment that is not one of the three",
			target:  pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: at},
			sub:     pullrequest.Submission{Assessment: "なんとなく", Body: "x"},
			wantErr: "invalid assessment",
		},
		{
			// Posting from a moved head puts comments on line numbers that
			// have shifted, which GitHub rejects with a 422 after the review
			// is already half made.
			name:    "a head that has moved",
			target:  pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: "0000000"},
			sub:     pullrequest.Submission{Assessment: pullrequest.AssessmentApprove, Body: "x"},
			wantErr: "rerun the freshness check",
		},
		{
			name:   "a comment on a line the diff does not have",
			target: pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: at},
			sub: pullrequest.Submission{
				Assessment: pullrequest.AssessmentApprove, Body: "x",
				Comments: []pullrequest.SubmissionComment{{Path: "file.txt", Line: 99, Body: "y"}},
			},
			wantErr: "file.txt:99",
		},
		{
			// A removed line has no number on the new side, so it cannot be
			// commented on however plainly it appears in the diff.
			name:   "a comment on a file the diff does not have",
			target: pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: at},
			sub: pullrequest.Submission{
				Assessment: pullrequest.AssessmentApprove, Body: "x",
				Comments: []pullrequest.SubmissionComment{{Path: "other.txt", Line: 1, Body: "y"}},
			},
			wantErr: "other.txt:1",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			// A server that fails the test if it is reached: none of these may
			// post anything.
			c := ghapitest.New(t, http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
				t.Error("a review was posted despite the check")
			}))
			got, err := pullrequest.Post(t.Context(), runner.Exec{}, c, repo, tc.target, tc.sub)
			if err == nil {
				t.Fatalf("Post = %+v, want a failure", got)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
			}
		})
	}
}

func TestPostWithoutAURL(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{}`)
	}))

	target := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: gittest.Rev(t, repo, "HEAD")}
	_, err := pullrequest.Post(t.Context(), runner.Exec{}, c, repo, target,
		pullrequest.Submission{Assessment: pullrequest.AssessmentApprove, Body: "x"})
	if err == nil || !strings.Contains(err.Error(), "html_url missing") {
		t.Errorf("Post error = %v, want it to report the missing url", err)
	}
}

func TestParseTarget(t *testing.T) {
	t.Parallel()

	const full = `{"repo":"owner/repo","pr":{"number":5,"base_ref":"main","head_oid":"abc"}}`
	tests := []struct {
		name    string
		in      string
		wantErr string
	}{
		{name: "every field", in: full},
		{name: "no repo", in: `{"pr":{"number":5,"base_ref":"main","head_oid":"abc"}}`, wantErr: "repo missing"},
		{name: "no number", in: `{"repo":"owner/repo","pr":{"base_ref":"main","head_oid":"abc"}}`, wantErr: "pr.number missing"},
		{name: "no base_ref", in: `{"repo":"owner/repo","pr":{"number":5,"head_oid":"abc"}}`, wantErr: "pr.base_ref missing"},
		{name: "no head_oid", in: `{"repo":"owner/repo","pr":{"number":5,"base_ref":"main"}}`, wantErr: "pr.head_oid missing"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := pullrequest.ParseTarget([]byte(tc.in))
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("ParseTarget = %+v, %v; want the error %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseTarget: %v", err)
			}
			want := pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", HeadOID: "abc"}
			if got != want {
				t.Errorf("ParseTarget = %+v, want %+v", got, want)
			}
		})
	}
}
