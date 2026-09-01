package pullrequest_test

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
)

func TestParseThreadActions(t *testing.T) {
	t.Parallel()

	body := "looks fixed"
	tests := []struct {
		name    string
		in      string
		want    []pullrequest.ThreadAction
		wantErr string
	}{
		{
			name: "a reply and a resolve",
			in:   `{"threads":[{"id":"PRRT_1","resolve":true,"body":"looks fixed"}]}`,
			want: []pullrequest.ThreadAction{{ID: "PRRT_1", Body: &body, Resolve: true}},
		},
		{
			// No body means resolving without replying, which is how a repeat
			// run avoids saying the same thing twice.
			name: "a resolve alone",
			in:   `{"threads":[{"id":"PRRT_1","resolve":true}]}`,
			want: []pullrequest.ThreadAction{{ID: "PRRT_1", Resolve: true}},
		},
		{name: "no threads at all", in: `{}`, wantErr: "threads must be an array"},
		{name: "threads not an array", in: `{"threads":{}}`, wantErr: "threads must be an array"},
		{name: "no id", in: `{"threads":[{"resolve":true}]}`, wantErr: "{id: string, resolve: boolean"},
		{name: "resolve is a string", in: `{"threads":[{"id":"a","resolve":"yes"}]}`, wantErr: "{id: string, resolve: boolean"},
		{name: "body is a number", in: `{"threads":[{"id":"a","resolve":true,"body":3}]}`, wantErr: "{id: string, resolve: boolean"},
		{name: "not json at all", in: "nope", wantErr: "invalid JSON"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := pullrequest.ParseThreadActions([]byte(tc.in), "threads.json")
			if tc.wantErr != "" {
				if err == nil {
					t.Fatalf("ParseThreadActions = %+v, want an error mentioning %q", got, tc.wantErr)
				}
				if !strings.Contains(err.Error(), tc.wantErr) {
					t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseThreadActions: %v", err)
			}
			if diff := cmp.Diff(tc.want, got); diff != "" {
				t.Errorf("ParseThreadActions (-want +got):\n%s", diff)
			}
		})
	}
}

// TestReplyRefuses drives the checks through Reply with a client that fails the
// test if it is reached: that a refusal happens is the point, but that nothing
// was posted first is what the all-or-nothing promise is made of.
func TestReplyRefuses(t *testing.T) {
	t.Parallel()

	body, blank := "fixed", "   "
	eligible := []string{"PRRT_ok", "PRRT_ok2"}

	tests := []struct {
		name    string
		actions []pullrequest.ThreadAction
		// posted is what an earlier run of the same file recorded.
		posted  []string
		wantErr string
	}{
		{
			name:    "a body of only whitespace",
			actions: []pullrequest.ThreadAction{{ID: "PRRT_ok", Body: &blank, Resolve: true}},
			wantErr: "reply body is present but blank",
		},
		{
			name:    "neither a reply nor a resolve",
			actions: []pullrequest.ThreadAction{{ID: "PRRT_ok"}},
			wantErr: "do nothing",
		},
		{
			name: "the same thread twice",
			actions: []pullrequest.ThreadAction{
				{ID: "PRRT_ok", Body: &body, Resolve: true},
				{ID: "PRRT_ok", Body: &body, Resolve: true},
			},
			wantErr: "duplicate thread id",
		},
		{
			// Whose thread it is and whether it is still open was decided when
			// the context was fetched; deciding it again here from different
			// information is how the two would come apart.
			name:    "a thread the context does not flag",
			actions: []pullrequest.ThreadAction{{ID: "PRRT_other", Body: &body, Resolve: true}},
			wantErr: "not awaiting our confirmation",
		},
		{
			// Eligibility is frozen in the context, so running the same file
			// again would pass every check and reply a second time.
			name:    "a thread an earlier run already replied to",
			actions: []pullrequest.ThreadAction{{ID: "PRRT_ok", Body: &body, Resolve: true}},
			posted:  []string{"PRRT_ok"},
			wantErr: "already replied to in an earlier run",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			threadsFile := filepath.Join(t.TempDir(), "threads.json")
			if len(tc.posted) > 0 {
				if err := os.WriteFile(pullrequest.PostedLog(threadsFile), []byte(strings.Join(tc.posted, "\n")+"\n"), 0o644); err != nil {
					t.Fatalf("WriteFile: %v", err)
				}
			}

			// Only the refusals reach here; the accepting case is covered by
			// TestReply, which does let the mutations through.
			unreachable := ghapitest.New(t, http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
				t.Error("a mutation was sent despite the refusal")
			}))
			_, err := pullrequest.Reply(t.Context(), unreachable, pullrequest.ReplyRequest{
				Actions: tc.actions, Eligible: eligible, ContextFile: "context.json", ThreadsFile: threadsFile,
			})
			if err == nil {
				t.Fatalf("Reply succeeded, want an error mentioning %q", tc.wantErr)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
			}
		})
	}
}

func TestParseEligible(t *testing.T) {
	t.Parallel()

	const context = `{"pr":{"head_oid":"abc"},"review_threads":[
		{"id":"PRRT_1","awaiting_my_confirmation":true},
		{"id":"PRRT_2","awaiting_my_confirmation":false},
		{"id":"PRRT_3","awaiting_my_confirmation":true}]}`

	ids, head, err := pullrequest.ParseEligible([]byte(context))
	if err != nil {
		t.Fatalf("ParseEligible: %v", err)
	}
	if diff := cmp.Diff([]string{"PRRT_1", "PRRT_3"}, ids); diff != "" {
		t.Errorf("eligible (-want +got):\n%s", diff)
	}
	if head != "abc" {
		t.Errorf("head = %q, want abc", head)
	}

	if _, _, err := pullrequest.ParseEligible([]byte(`{"pr":{"head_oid":"abc"}}`)); err == nil {
		t.Error("a context with no review_threads was accepted")
	}
}

// mutations answers the two mutations, recording what it was asked and failing
// whichever the test says.
type mutations struct {
	// failReply and failResolve name the thread whose mutation fails.
	failReply, failResolve string
	replied, resolved      []string
	// urlless names a thread whose reply comes back with no url.
	urlless string
}

func (m *mutations) client(t *testing.T) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var req struct {
			Query     string `json:"query"`
			Variables struct {
				ThreadID string `json:"threadId"`
			} `json:"variables"`
		}
		if err := json.UnmarshalRead(r.Body, &req); err != nil {
			t.Errorf("decode the request body: %v", err)
			return
		}
		id := req.Variables.ThreadID
		w.Header().Set("Content-Type", "application/json")

		if strings.Contains(req.Query, "addPullRequestReviewThreadReply") {
			if id == m.failReply {
				fmt.Fprint(w, `{"errors":[{"message":"reply refused"}]}`)
				return
			}
			m.replied = append(m.replied, id)
			if id == m.urlless {
				fmt.Fprint(w, `{"data":{"addPullRequestReviewThreadReply":{"comment":{}}}}`)
				return
			}
			fmt.Fprintf(w, `{"data":{"addPullRequestReviewThreadReply":{"comment":{"url":"https://example.com/%s"}}}}`, id)
			return
		}
		if id == m.failResolve {
			fmt.Fprint(w, `{"errors":[{"message":"must have write access"}]}`)
			return
		}
		m.resolved = append(m.resolved, id)
		fmt.Fprint(w, `{"data":{"resolveReviewThread":{"thread":{"isResolved":true}}}}`)
	}))
}

func threadsFile(t *testing.T) string {
	t.Helper()
	return filepath.Join(t.TempDir(), "threads.json")
}

func TestReply(t *testing.T) {
	t.Parallel()

	body := "confirmed"
	file := threadsFile(t)
	m := &mutations{}
	actions := []pullrequest.ThreadAction{
		{ID: "PRRT_1", Body: &body, Resolve: true},
		{ID: "PRRT_2", Resolve: true},
	}

	got, err := pullrequest.Reply(t.Context(), m.client(t), pullrequest.ReplyRequest{
		Actions: actions, Eligible: []string{"PRRT_1", "PRRT_2"}, ThreadsFile: file,
	})
	if err != nil {
		t.Fatalf("Reply: %v", err)
	}

	want := pullrequest.ThreadReplies{
		Replied:       []pullrequest.RepliedThread{{ID: "PRRT_1", URL: "https://example.com/PRRT_1"}},
		Resolved:      []string{"PRRT_1", "PRRT_2"},
		ResolveFailed: []pullrequest.FailedResolve{},
		Warnings:      []string{},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Reply (-want +got):\n%s", diff)
	}
	// Only the thread with a body was replied to; the other was resolved
	// without one.
	if diff := cmp.Diff([]string{"PRRT_1"}, m.replied); diff != "" {
		t.Errorf("replies posted (-want +got):\n%s", diff)
	}
	// The record is what a second run of the same file consults.
	if posted := read(t, pullrequest.PostedLog(file)); posted != "PRRT_1\n" {
		t.Errorf("the record holds %q, want the thread that was replied to", posted)
	}
}

// TestReplyDegradesOnResolve is the asymmetry: a reply that fails stops
// everything, a resolve that fails does not. Resolving needs write access,
// which a review of somebody else's fork does not have, and the replies are the
// point of the exercise.
func TestReplyDegradesOnResolve(t *testing.T) {
	t.Parallel()

	body := "confirmed"
	m := &mutations{failResolve: "PRRT_1"}
	got, err := pullrequest.Reply(t.Context(), m.client(t), pullrequest.ReplyRequest{
		Actions: []pullrequest.ThreadAction{
			{ID: "PRRT_1", Body: &body, Resolve: true},
			{ID: "PRRT_2", Body: &body, Resolve: true},
		},
		Eligible:    []string{"PRRT_1", "PRRT_2"},
		ThreadsFile: threadsFile(t),
	})
	if err != nil {
		t.Fatalf("Reply: %v", err)
	}

	if len(got.Replied) != 2 {
		t.Errorf("replied = %+v, want both", got.Replied)
	}
	if diff := cmp.Diff([]string{"PRRT_2"}, got.Resolved); diff != "" {
		t.Errorf("resolved (-want +got):\n%s", diff)
	}
	if len(got.ResolveFailed) != 1 || got.ResolveFailed[0].ID != "PRRT_1" {
		t.Errorf("resolve_failed = %+v, want the one that could not be resolved", got.ResolveFailed)
	}
	if len(got.Warnings) != 1 || !strings.Contains(got.Warnings[0], "PRRT_1") {
		t.Errorf("warnings = %v, want one naming the thread", got.Warnings)
	}
}

func TestReplyStops(t *testing.T) {
	t.Parallel()

	body := "confirmed"
	actions := []pullrequest.ThreadAction{
		{ID: "PRRT_1", Body: &body, Resolve: true},
		{ID: "PRRT_2", Body: &body, Resolve: true},
		{ID: "PRRT_3", Body: &body, Resolve: true},
	}

	tests := []struct {
		name string
		m    *mutations
		// wantReplied is what the record should hold afterwards, and
		// wantLeft what the message should call unprocessed.
		wantReplied []string
		wantLeft    string
	}{
		{
			name:        "the reply is refused",
			m:           &mutations{failReply: "PRRT_2"},
			wantReplied: []string{"PRRT_1"},
			wantLeft:    "not processed: PRRT_2,PRRT_3",
		},
		{
			// The reply landed even though the answer was unusable, so it has
			// to be on the record or a retry would post it again.
			name:        "the reply lands without a url",
			m:           &mutations{urlless: "PRRT_2"},
			wantReplied: []string{"PRRT_1", "PRRT_2"},
			wantLeft:    "not processed: PRRT_3",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			file := threadsFile(t)
			_, err := pullrequest.Reply(t.Context(), tc.m.client(t), pullrequest.ReplyRequest{
				Actions: actions, Eligible: []string{"PRRT_1", "PRRT_2", "PRRT_3"}, ThreadsFile: file,
			})
			if err == nil {
				t.Fatal("Reply succeeded, want it to stop")
			}

			// Nothing after the failure was touched: a run acts on its input in
			// order and stops, rather than skipping past what went wrong.
			if slices.Contains(tc.m.replied, "PRRT_3") {
				t.Errorf("replies posted = %v, want nothing after the failure", tc.m.replied)
			}
			for _, want := range []string{"already replied", tc.wantLeft} {
				if !strings.Contains(err.Error(), want) {
					t.Errorf("error = %q, want it to say %q", err, want)
				}
			}
			got := strings.Fields(read(t, pullrequest.PostedLog(file)))
			if diff := cmp.Diff(tc.wantReplied, got); diff != "" {
				t.Errorf("the record (-want +got):\n%s", diff)
			}
		})
	}
}

func read(t *testing.T, path string) string {
	t.Helper()

	b, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return ""
		}
		t.Fatalf("ReadFile(%q): %v", path, err)
	}
	return string(b)
}
