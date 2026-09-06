package pullrequest_test

import (
	"fmt"
	"net/http"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
)

// ourPR is the target every case here starts from: a pull request of ours, so
// that what a case changes is the one thing it is about.
func ourPR() pullrequest.Target {
	return pullrequest.Target{Repo: "owner/repo", Number: 5, BaseRef: "main", IsOwnPR: true}
}

// appendServer answers the read of the pull request and captures the body sent
// back, so that a case can tell a refusal from a write.
func appendServer(t *testing.T, live string, sent *string) http.Handler {
	t.Helper()

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		if r.Method == http.MethodPatch {
			*sent = "edited"
			fmt.Fprint(w, `{"html_url":"https://github.com/owner/repo/pull/5"}`)
			return
		}
		fmt.Fprintf(w, `{"data":{"repository":{"pullRequest":{"number":5,"body":%q}}}}`, live)
	})
}

// refuses runs one case that must write nothing, and answers with the refusal.
func refuses(t *testing.T, target pullrequest.Target, section string) error {
	t.Helper()

	var sent string
	c := ghapitest.New(t, appendServer(t, "The original description.\n", &sent))

	_, err := pullrequest.AppendBody(t.Context(), c, target, section)
	if err == nil {
		t.Fatal("AppendBody succeeded, want a refusal")
	}
	if sent != "" {
		t.Error("the body was edited despite the refusal")
	}
	return err
}

func TestAppendBody(t *testing.T) {
	t.Parallel()

	var sent string
	c := ghapitest.New(t, appendServer(t, "The original description.\n", &sent))

	got, err := pullrequest.AppendBody(t.Context(), c, ourPR(),
		"## The decision\n\nKept as it is, because the issue records it.\n")
	if err != nil {
		t.Fatalf("AppendBody: %v", err)
	}

	if want := "https://github.com/owner/repo/pull/5"; got.URL != want {
		t.Errorf("url = %q, want %q", got.URL, want)
	}
	// The heading rather than the whole section: the run's output says which
	// write-down this was, and the section itself is in the pull request.
	if want := "## The decision"; got.SectionFirstLine != want {
		t.Errorf("section_first_line = %q, want %q", got.SectionFirstLine, want)
	}
}

// The body of somebody else's pull request is theirs. The document says whose
// it is, so the refusal costs no request.
func TestAppendBodyRefusesSomebodyElsesPullRequest(t *testing.T) {
	t.Parallel()

	target := ourPR()
	target.IsOwnPR = false

	err := refuses(t, target, "## The decision\n\nKept as it is.\n")
	for _, want := range []string{"is not ours", "code comment"} {
		if !strings.Contains(err.Error(), want) {
			t.Errorf("error = %q, want it to mention %q", err, want)
		}
	}
}

// The section is judged before the body is read, by the rules every body and
// every pull request body are judged by.
func TestAppendBodyRefusesASectionThatCannotBeSent(t *testing.T) {
	t.Parallel()

	for _, tt := range []struct {
		name, section, wantErr string
	}{
		{
			name:    "item numbering notifies unrelated issues",
			section: "#1 one\n#2 two\n#3 three\n",
			wantErr: "3 distinct bare #N",
		},
		{
			name:    "a quoted closing keyword closes nothing",
			section: "as agreed: `Closes #656`\n",
			wantErr: "leaves the issue open",
		},
		// Nothing to append is not an empty append: it is a run whose section
		// file was never written, and a blank line at the end of the body is
		// all it would leave behind.
		{name: "nothing to append", section: "\n  \n", wantErr: "empty"},
	} {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			if err := refuses(t, ourPR(), tt.section); !strings.Contains(err.Error(), tt.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tt.wantErr)
			}
		})
	}
}
