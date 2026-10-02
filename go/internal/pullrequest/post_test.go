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

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

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
				Assessment: pullrequest.AssessmentApprove, Body: ghapitest.Body(t, "looks good"),
				Comments: []ghapi.ReviewComment{{Path: "a.go", Line: 3, Body: ghapitest.Body(t, "here")}},
			},
		},
		{
			// Long prose written as a JSON string loses its meaning to one
			// missed escape, so naming a plain markdown file is supported.
			name: "a named body",
			in:   `{"assessment":"要議論","body_file":"body.md","comments":[]}`,
			want: pullrequest.Submission{
				Assessment: pullrequest.AssessmentDiscuss, Body: ghapitest.Body(t, "# From a file\n"),
				Comments: []ghapi.ReviewComment{},
			},
		},
		{name: "no assessment", in: `{"body":"x","comments":[]}`, wantErr: "review.json is missing assessment"},
		{name: "both forms of body", in: `{"assessment":"要議論","body":"x","body_file":"body.md","comments":[]}`, wantErr: "review.json sets both body and body_file"},
		{name: "neither form of body", in: `{"assessment":"要議論","comments":[]}`, wantErr: "review.json sets neither body nor body_file"},
		{name: "an empty body_file", in: `{"assessment":"要議論","body_file":"","comments":[]}`, wantErr: "review.json sets body_file to an empty string"},
		// Allowing a path would reach round the directory binding.
		{name: "a body_file with a path", in: `{"assessment":"要議論","body_file":"sub/x.md","comments":[]}`, wantErr: "review.json sets body_file to a path, not a bare file name"},
		{name: "a body_file that is not there", in: `{"assessment":"要議論","body_file":"nope.md","comments":[]}`, wantErr: "not found in the work dir"},
		{name: "comments missing", in: `{"assessment":"要議論","body":"x"}`, wantErr: "review.json is missing comments"},
		// A null key is the writer saying "not this one", and a file that
		// forgot the key has not said there are no comments either.
		{name: "comments null", in: `{"assessment":"要議論","body":"x","comments":null}`, wantErr: "review.json is missing comments"},
		{name: "comments not an array", in: `{"assessment":"要議論","body":"x","comments":{}}`, wantErr: "comments must be an array in review.json"},
		{name: "a comment without a line", in: `{"assessment":"要議論","body":"x","comments":[{"path":"a.go","body":"y"}]}`, wantErr: "comments[0] is missing line in review.json"},
		{name: "a comment with both forms of body", in: `{"assessment":"要議論","body":"x","comments":[{"path":"a.go","line":3,"body":"y","body_file":"body.md"}]}`, wantErr: "comments[0] sets both body and body_file in review.json"},
		{name: "a comment with neither form of body", in: `{"assessment":"要議論","body":"x","comments":[{"path":"a.go","line":3}]}`, wantErr: "comments[0] sets neither body nor body_file in review.json"},
		// The group is satisfied — one key was supplied — so what is left is
		// the value itself, which the field declares the rules for. This is
		// the one of the three body_file fields with no doc comment, and the
		// declaration reaches it the same way it reaches the other two.
		{name: "a comment with an empty body_file", in: `{"assessment":"要議論","body":"x","comments":[{"path":"a.go","line":3,"body_file":""}]}`, wantErr: "comments[0] sets body_file to an empty string in review.json"},
		{name: "a comment with a body_file with a path", in: `{"assessment":"要議論","body":"x","comments":[{"path":"a.go","line":3,"body_file":"sub/x.md"}]}`, wantErr: "comments[0] sets body_file to a path, not a bare file name in review.json"},
		{name: "a comment whose line is not a number", in: `{"assessment":"要議論","body":"x","comments":[{"path":"a.go","line":"3","body":"y"}]}`, wantErr: "comments[0].line must be a number in review.json"},
		{name: "not json at all", in: `not json`, wantErr: "invalid JSON in review.json"},
		// A root that is the wrong kind is a decode failure like the one above,
		// but the decoder has a field-shaped complaint for it and no field to
		// hang it on, so the document is what the message names.
		{name: "the root is not an object", in: `[]`, wantErr: "review.json must be an object"},

		// The distinctions below were not covered when the fields were read as
		// raw JSON and checked by hand. They are here so that giving them
		// their real Go types cannot change any of them by accident.
		{
			name: "a zero line and an empty path",
			in:   `{"assessment":"要議論","body":"x","comments":[{"path":"","line":0,"body":"y"}]}`,
			want: pullrequest.Submission{
				Assessment: pullrequest.AssessmentDiscuss, Body: ghapitest.Body(t, "x"),
				Comments: []ghapi.ReviewComment{{Path: "", Line: 0, Body: ghapitest.Body(t, "y")}},
			},
		},
		{
			// A zero value is a value: what the parser rejects is a field that
			// is not there, and the anchors are checked against the diff later.
			name: "an empty assessment",
			in:   `{"assessment":"","body":"x","comments":[]}`,
			want: pullrequest.Submission{
				Assessment: "", Body: ghapitest.Body(t, "x"), Comments: []ghapi.ReviewComment{},
			},
		},
		{
			// An explicit null is the writer saying "not this one", which is
			// what omitting the field says too.
			name: "a null body beside a named one",
			in:   `{"assessment":"要議論","body":null,"body_file":"body.md","comments":[]}`,
			want: pullrequest.Submission{
				Assessment: pullrequest.AssessmentDiscuss, Body: ghapitest.Body(t, "# From a file\n"),
				Comments: []ghapi.ReviewComment{},
			},
		},
		{name: "an assessment that is not a string", in: `{"assessment":5,"body":"x","comments":[]}`, wantErr: "assessment must be a string in review.json"},
		// A member of the exclusive group, whose key sits in the same object
		// its parent was read from.
		{name: "a body that is not a string", in: `{"assessment":"要議論","body":3,"comments":[]}`, wantErr: "body must be a string in review.json"},
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
			if diff := cmp.Diff(tc.want, got, ghapitest.CmpBody); diff != "" {
				t.Errorf("ParseSubmission (-want +got):\n%s", diff)
			}
		})
	}
}

// A review is read before it is posted, so a body GitHub would autolink into
// notifications on unrelated issues stops the whole submission — the body and
// every line comment alike — with nothing sent.
func TestParseSubmissionRefusesABodyThatNumbersItsItems(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name, in, wantErr string
	}{
		{
			name:    "the review body",
			in:      `{"assessment":"要議論","body":"#1 one\n#2 two\n#3 three\n","comments":[]}`,
			wantErr: "the review body: 3 distinct bare #N",
		},
		{
			name: "one line comment",
			in: `{"assessment":"要議論","body":"x","comments":[` +
				`{"path":"a.go","line":3,"body":"#1 one\n#2 two\n#3 three\n"}]}`,
			wantErr: "comments[0] (a.go:3): 3 distinct bare #N",
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := pullrequest.ParseSubmission([]byte(tc.in), t.TempDir(), "review.json")
			if err == nil {
				t.Fatalf("ParseSubmission = %+v, want an error mentioning %q", got, tc.wantErr)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
			}
		})
	}
}

// documentTarget writes the patch of HEAD in dir over base the way a document's
// is written, and takes the target out of a context carrying it.
// prMergeBase is the merge base GitHub took the range from, empty where git's
// one is the only one.
func documentTarget(t *testing.T, dir, base, prMergeBase string) pullrequest.Target {
	t.Helper()

	change, warning, err := pullrequest.ReadLocalChange(t.Context(), runner.Exec{}, dir, base, prMergeBase, filepath.Join(t.TempDir(), "diff.patch"))
	if err != nil {
		t.Fatalf("ReadLocalChange: %v", err)
	}
	if warning != "" {
		t.Fatalf("ReadLocalChange took the range from a merge base it had to guess: %s", warning)
	}
	return pullrequest.Context{
		Repo: "owner/repo", PR: pullrequest.PR{Number: 5, HeadOID: gittest.Rev(t, dir, "HEAD")}, Diff: change.Diff,
	}.Target()
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

// TestPostKeepsTheAPIsRefusal is the regression guard for the message the shell
// version got for free: it never suppressed gh's standard error, so a 422 from
// the review endpoint — the one that says which line GitHub would not accept —
// reached the operator alongside the script's own wording. Reporting only the
// fixed sentence sends them to debug a request they cannot see.
func TestPostKeepsTheAPIsRefusal(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	target := documentTarget(t, repo, "main", "")
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusUnprocessableEntity)
		fmt.Fprint(w, `{"message":"pull_request_review.line must be part of the diff"}`)
	}))

	sub := pullrequest.Submission{
		Assessment: pullrequest.AssessmentChanges,
		Body:       ghapitest.Body(t, "needs work"),
		Comments:   []ghapi.ReviewComment{{Path: "file.txt", Line: 4, Body: ghapitest.Body(t, "this one")}},
	}
	_, err := pullrequest.Post(t.Context(), runner.Exec{}, c, repo, target, sub)
	if err == nil {
		t.Fatal("Post succeeded, want the refusal reported")
	}
	for _, want := range []string{"failed to post review", "422", "must be part of the diff"} {
		if !strings.Contains(err.Error(), want) {
			t.Errorf("error = %q, want it to mention %q", err, want)
		}
	}
}

func TestPost(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	target := documentTarget(t, repo, "main", "")

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
		Body:       ghapitest.Body(t, "needs work"),
		// Line 4 is the added "four", line 5 the one beginning with "++".
		Comments: []ghapi.ReviewComment{
			{Path: "file.txt", Line: 4, Body: ghapitest.Body(t, "this one")},
			{Path: "file.txt", Line: 5, Body: ghapitest.Body(t, "and this")},
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
	target := documentTarget(t, repo, "main", "")
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
				pullrequest.Submission{Assessment: tc.assessment, Body: ghapitest.Body(t, "x")}); err != nil {
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
	target := documentTarget(t, repo, "main", "")
	moved := target
	moved.HeadOID = "0000000"
	missing := target
	missing.DiffPath = filepath.Join(t.TempDir(), "diff.patch")
	// Another run on the same pull request wrote the patch after this
	// document was: the file is there, and it is not the one the document
	// was written with.
	foreign := target
	foreign.DiffSHA256 = strings.Repeat("0", 64)
	anchored := []ghapi.ReviewComment{{Path: "file.txt", Line: 4, Body: ghapitest.Body(t, "y")}}

	tests := []struct {
		name    string
		target  pullrequest.Target
		sub     pullrequest.Submission
		wantErr string
	}{
		{
			name:    "an assessment that is not one of the three",
			target:  target,
			sub:     pullrequest.Submission{Assessment: "なんとなく", Body: ghapitest.Body(t, "x")},
			wantErr: "invalid assessment",
		},
		{
			// Posting from a moved head puts comments on line numbers that
			// have shifted, which GitHub rejects with a 422 after the review
			// is already half made.
			name:    "a head that has moved",
			target:  moved,
			sub:     pullrequest.Submission{Assessment: pullrequest.AssessmentApprove, Body: ghapitest.Body(t, "x")},
			wantErr: "rerun the freshness check",
		},
		{
			name:   "a comment on a line the diff does not have",
			target: target,
			sub: pullrequest.Submission{
				Assessment: pullrequest.AssessmentApprove, Body: ghapitest.Body(t, "x"),
				Comments: []ghapi.ReviewComment{{Path: "file.txt", Line: 99, Body: ghapitest.Body(t, "y")}},
			},
			wantErr: "file.txt:99",
		},
		{
			// A removed line has no number on the new side, so it cannot be
			// commented on however plainly it appears in the diff.
			name:   "a comment on a file the diff does not have",
			target: target,
			sub: pullrequest.Submission{
				Assessment: pullrequest.AssessmentApprove, Body: ghapitest.Body(t, "x"),
				Comments: []ghapi.ReviewComment{{Path: "other.txt", Line: 1, Body: ghapitest.Body(t, "y")}},
			},
			wantErr: "other.txt:1",
		},
		{
			name:   "a patch that is not there",
			target: missing,
			sub: pullrequest.Submission{
				Assessment: pullrequest.AssessmentApprove, Body: ghapitest.Body(t, "x"), Comments: anchored,
			},
			wantErr: "failed to read the pull request's patch",
		},
		{
			name:   "a patch the document was not written with",
			target: foreign,
			sub: pullrequest.Submission{
				Assessment: pullrequest.AssessmentApprove, Body: ghapitest.Body(t, "x"), Comments: anchored,
			},
			wantErr: "is not the one the pull request context was written with\nrerun `ccx pr context` or `ccx pr prepare-review`",
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

// TestPostAnchorsToThePullRequestsRange is a criss-cross history, where the
// document's patch starts from the merge base GitHub named and git, asked for
// one, would pick the other. Each of the two adds a file the other does not,
// so a comment on either is accepted by one reading and refused by the other.
func TestPostAnchorsToThePullRequestsRange(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, crissCross(t))
	github := notGitsPick(t, r.author, "main", "HEAD")
	// The file each merge base has not seen yet is in the diff from it.
	inRange, outOfRange := "main.txt", "feature.txt"
	if strings.TrimSpace(gittest.Run(t, r.author, "log", "-1", "--format=%s", github)) == "On main" {
		inRange, outOfRange = outOfRange, inRange
	}
	target := documentTarget(t, r.author, "main", github)
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"html_url":"https://example.com/r"}`)
	}))
	post := func(path string) error {
		_, err := pullrequest.Post(t.Context(), runner.Exec{}, c, r.author, target, pullrequest.Submission{
			Assessment: pullrequest.AssessmentDiscuss, Body: ghapitest.Body(t, "x"),
			Comments: []ghapi.ReviewComment{{Path: path, Line: 1, Body: ghapitest.Body(t, "y")}},
		})
		return err
	}

	if err := post(inRange); err != nil {
		t.Errorf("a comment on %s:1, which the pull request's diff has, was refused: %v", inRange, err)
	}
	err := post(outOfRange)
	if err == nil || !strings.Contains(err.Error(), outOfRange+":1") {
		t.Errorf("Post error = %v, want it to refuse %s:1, which only git's pick has", err, outOfRange)
	}
}

// TestPostAnchorsAsGitHubDoes is a change holding copies of a file it also
// edits and renames on either side of the similarity GitHub calls a rename,
// and each case is the answer GitHub's review endpoint gave on a pull request
// built the same way: a rename keeps its old lines out of the diff, and a
// copy, which GitHub does not detect even byte for byte, is a new file whose
// every line was added.
func TestPostAnchorsAsGitHubDoes(t *testing.T) {
	t.Parallel()
	gittest.SkipWithoutGit(t)

	lines := func(name string) string {
		var b strings.Builder
		for i := 1; i <= 20; i++ {
			fmt.Fprintf(&b, "%s line %d\n", name, i)
		}
		return b.String()
	}
	// rewritten is a file of twenty lines of equal length whose last n are
	// rewritten: 9 of them leave it 53% like the original, which GitHub still
	// reads as a rename, and 10 leave it below the line GitHub draws.
	rewritten := func(n, last int) string {
		var b strings.Builder
		for i := 1; i <= 20; i++ {
			word := "original"
			if i > 20-last {
				word = "REWRITTEN"
			}
			fmt.Fprintf(&b, "file %d %s line %02d\n", n, word, i)
		}
		return b.String()
	}
	repo := t.TempDir()
	gittest.Init(t, repo, "-b", "main")
	gittest.Write(t, filepath.Join(repo, "source.txt"), lines("source"))
	gittest.Write(t, filepath.Join(repo, "moved.txt"), lines("moved"))
	gittest.Write(t, filepath.Join(repo, "sim9.txt"), rewritten(9, 0))
	gittest.Write(t, filepath.Join(repo, "sim10.txt"), rewritten(10, 0))
	gittest.Run(t, repo, "add", ".")
	gittest.Run(t, repo, "commit", "-qm", "init")

	gittest.Run(t, repo, "switch", "-qc", "feature/x")
	// The source is edited too: that is what makes the copies ones git could
	// match against it.
	gittest.Write(t, filepath.Join(repo, "source.txt"), lines("source")+"source line 21\n")
	gittest.Write(t, filepath.Join(repo, "copy.txt"), strings.Replace(lines("source"), "source line 20\n", "source line twenty\n", 1))
	gittest.Write(t, filepath.Join(repo, "exact-copy.txt"), lines("source"))
	gittest.Run(t, repo, "mv", "moved.txt", "renamed.txt")
	gittest.Write(t, filepath.Join(repo, "renamed.txt"), strings.Replace(lines("moved"), "moved line 20\n", "moved line twenty\n", 1))
	for _, n := range []int{9, 10} {
		from, to := fmt.Sprintf("sim%d.txt", n), fmt.Sprintf("sim%d-renamed.txt", n)
		gittest.Run(t, repo, "mv", from, to)
		gittest.Write(t, filepath.Join(repo, to), rewritten(n, n))
	}
	gittest.Run(t, repo, "add", ".")
	gittest.Run(t, repo, "commit", "-qm", "copy and rename")

	target := documentTarget(t, repo, "main", "")
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"html_url":"https://example.com/r"}`)
	}))

	for _, tc := range []struct {
		path     string
		line     int
		accepted bool
	}{
		{path: "copy.txt", line: 1, accepted: true},
		{path: "copy.txt", line: 20, accepted: true},
		{path: "exact-copy.txt", line: 1, accepted: true},
		{path: "sim9-renamed.txt", line: 1, accepted: false},
		{path: "sim10-renamed.txt", line: 1, accepted: true},
		{path: "renamed.txt", line: 1, accepted: false},
		{path: "renamed.txt", line: 20, accepted: true},
		{path: "source.txt", line: 1, accepted: false},
	} {
		at := fmt.Sprintf("%s:%d", tc.path, tc.line)
		_, err := pullrequest.Post(t.Context(), runner.Exec{}, c, repo, target, pullrequest.Submission{
			Assessment: pullrequest.AssessmentDiscuss, Body: ghapitest.Body(t, "x"),
			Comments: []ghapi.ReviewComment{{Path: tc.path, Line: tc.line, Body: ghapitest.Body(t, "y")}},
		})
		switch {
		case tc.accepted && err != nil:
			t.Errorf("a comment on %s, which GitHub accepts, was refused: %v", at, err)
		case !tc.accepted && (err == nil || !strings.Contains(err.Error(), at)):
			t.Errorf("Post error = %v, want it to refuse %s, which GitHub refuses", err, at)
		}
	}
}

func TestPostWithoutAURL(t *testing.T) {
	t.Parallel()

	repo := diffRepo(t)
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{}`)
	}))

	target := documentTarget(t, repo, "main", "")
	_, err := pullrequest.Post(t.Context(), runner.Exec{}, c, repo, target,
		pullrequest.Submission{Assessment: pullrequest.AssessmentApprove, Body: ghapitest.Body(t, "x")})
	if err == nil || !strings.Contains(err.Error(), "html_url missing") {
		t.Errorf("Post error = %v, want it to report the missing url", err)
	}
}

// TestContextTarget pins the projection alone.
func TestContextTarget(t *testing.T) {
	t.Parallel()

	c := pullrequest.Context{
		Repo: "owner/repo",
		PR: pullrequest.PR{
			Number: 5, Title: "Test PR", BaseRef: "main", HeadRef: "feature/x", HeadOID: "abc",
		},
		Diff: pullrequest.Diff{Path: "/work/diff.patch", MergeBaseOID: "def", SHA256: "0f1e2d"},
	}
	want := pullrequest.Target{Repo: "owner/repo", Number: 5, DiffPath: "/work/diff.patch", DiffSHA256: "0f1e2d", HeadOID: "abc"}
	if got := c.Target(); got != want {
		t.Errorf("Target = %+v, want %+v", got, want)
	}
}
