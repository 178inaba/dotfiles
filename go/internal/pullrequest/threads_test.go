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

// lastURL is the newest comment a context recorded for a thread, and what the
// live read is compared against.
func lastURL(id string) string { return "https://example.com/last/" + id }

// replyURL is where the mutations mock puts a reply of ours, and so the newest
// comment a context re-fetched after one reports.
func replyURL(id string) string { return "https://example.com/" + id }

// contextThreads is one context's review_threads, covering every shape a
// selector has to tell apart: two threads on one line, one whose line has left
// the diff, two waiting on somebody else and one that is nobody's move.
var contextThreads = []pullrequest.KnownThread{
	{
		ID: "PRRT_bot", Path: "src/a.go", Line: new(10), OriginalLine: new(10),
		OpenedBy: new("copilot-pull-request-reviewer"), Ball: pullrequest.BallMine,
		ResolvableByMe: true, LastCommentURL: lastURL("PRRT_bot"),
	},
	{
		// A person's remark: ours to answer, theirs to close.
		ID: "PRRT_person", Path: "src/a.go", Line: new(20), OriginalLine: new(20),
		OpenedBy: new("reviewer1"), Ball: pullrequest.BallMine,
		LastCommentURL: lastURL("PRRT_person"),
	},
	{
		// The lines are gone from the diff, which is the state the author is in
		// right after the fixing push.
		ID: "PRRT_outdated", Path: "src/b.go", OriginalLine: new(55),
		OpenedBy: new("testuser"), Ball: pullrequest.BallMine,
		ResolvableByMe: true, LastCommentURL: lastURL("PRRT_outdated"),
	},
	{
		ID: "PRRT_dup1", Path: "src/dup.go", Line: new(7), OriginalLine: new(7),
		OpenedBy: new("reviewer1"), Ball: pullrequest.BallMine,
		ResolvableByMe: true, LastCommentURL: lastURL("PRRT_dup1"),
	},
	{
		ID: "PRRT_dup2", Path: "src/dup.go", Line: new(7), OriginalLine: new(7),
		OpenedBy: new("reviewer2"), Ball: pullrequest.BallMine,
		ResolvableByMe: true, LastCommentURL: lastURL("PRRT_dup2"),
	},
	{
		// Ours, answered by us: the reviewer has everything they need, and we
		// may still add to it.
		ID: "PRRT_theirs", Path: "src/waiting.go", Line: new(5), OriginalLine: new(5),
		OpenedBy: new("testuser"), Ball: pullrequest.BallTheirs,
		ResolvableByMe: true, LastCommentURL: lastURL("PRRT_theirs"),
	},
	{
		// A person's remark we have already answered: ours to add to after a
		// change of design, theirs to close.
		ID: "PRRT_answered", Path: "src/answered.go", Line: new(30), OriginalLine: new(30),
		OpenedBy: new("reviewer1"), Ball: pullrequest.BallTheirs,
		LastCommentURL: lastURL("PRRT_answered"),
	},
	{
		ID: "PRRT_none", Path: "src/settled.go", Line: new(6), OriginalLine: new(6),
		OpenedBy: new("reviewer1"), Ball: pullrequest.BallNone,
		LastCommentURL: lastURL("PRRT_none"),
	},
}

func TestParseThreadActions(t *testing.T) {
	t.Parallel()

	work := t.TempDir()
	if err := os.WriteFile(filepath.Join(work, "r1.md"), []byte("a long reply"), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	tests := []struct {
		name    string
		in      string
		want    []pullrequest.ThreadAction
		wantErr string
	}{
		{
			name: "a reply and a resolve",
			in:   `{"threads":[{"path":"src/a.go","line":10,"resolve":true,"body":"looks fixed"}]}`,
			want: []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(10), Body: new("looks fixed"), Resolve: true}},
		},
		{
			// No body means resolving without replying, which is how a repeat
			// run avoids saying the same thing twice.
			name: "a resolve alone",
			in:   `{"threads":[{"path":"src/a.go","resolve":true}]}`,
			want: []pullrequest.ThreadAction{{Path: "src/a.go", Resolve: true}},
		},
		{
			// The id only narrows: the path is still what names the thread.
			name: "an id to break a tie",
			in:   `{"threads":[{"path":"src/dup.go","line":7,"id":"PRRT_dup2","resolve":false,"body":"x"}]}`,
			want: []pullrequest.ThreadAction{{Path: "src/dup.go", Line: new(7), ID: new("PRRT_dup2"), Body: new("x")}},
		},
		{
			// A long reply is written as markdown beside the file rather than
			// escaped into a JSON string, exactly as a review body is.
			name: "a body in a file",
			in:   `{"threads":[{"path":"src/a.go","resolve":true,"body_file":"r1.md"}]}`,
			want: []pullrequest.ThreadAction{{Path: "src/a.go", Body: new("a long reply"), Resolve: true}},
		},

		{name: "no threads at all", in: `{}`, wantErr: "threads.json is missing threads"},
		// An explicit null is not an empty array: only the array says none.
		{name: "threads null", in: `{"threads":null}`, wantErr: "threads.json is missing threads"},
		{name: "threads not an array", in: `{"threads":{}}`, wantErr: "threads must be an array in threads.json"},
		{name: "no path", in: `{"threads":[{"resolve":true}]}`, wantErr: "threads[0] is missing path in threads.json"},
		{name: "resolve missing", in: `{"threads":[{"path":"a","body":"x"}]}`, wantErr: "threads[0] is missing resolve in threads.json"},
		// A mistyped field names itself, as a missing one does: what the entry
		// as a whole should look like is a --help away.
		{name: "resolve is a string", in: `{"threads":[{"path":"a","resolve":"yes"}]}`, wantErr: "threads[0].resolve must be a boolean in threads.json"},
		{name: "line is a string", in: `{"threads":[{"path":"a","line":"7","resolve":true}]}`, wantErr: "threads[0].line must be a number in threads.json"},
		{name: "body is a number", in: `{"threads":[{"path":"a","resolve":true,"body":3}]}`, wantErr: "threads[0].body must be a string in threads.json"},
		{name: "not json at all", in: "nope", wantErr: "invalid JSON in threads.json"},
		// The root has no field to name, so the document is what is named.
		{name: "the root is not an object", in: `[]`, wantErr: "threads.json must be an object"},
		{
			name:    "both a body and a file",
			in:      `{"threads":[{"path":"a","resolve":true,"body":"x","body_file":"r1.md"}]}`,
			wantErr: "threads[0] sets both body and body_file in threads.json",
		},
		{
			// A path would let an entry reach round the directory binding that
			// keeps parallel runs on different pull requests apart.
			name:    "a body file outside the work dir",
			in:      `{"threads":[{"path":"a","resolve":true,"body_file":"../r1.md"}]}`,
			wantErr: "threads[0] sets body_file to a path, not a bare file name in threads.json",
		},
		{
			// The group is satisfied — one key was supplied — so what is left
			// is the value, which the field declares the rules for.
			name:    "an empty body file",
			in:      `{"threads":[{"path":"a","resolve":true,"body_file":""}]}`,
			wantErr: "threads[0] sets body_file to an empty string in threads.json",
		},
		{
			name:    "a body file that is not there",
			in:      `{"threads":[{"path":"a","resolve":true,"body_file":"missing.md"}]}`,
			wantErr: "body_file not found",
		},
		{
			// An explicit null says "not this one", which is what leaving the
			// field out says: resolve without replying.
			name: "a null body",
			in:   `{"threads":[{"path":"a","resolve":true,"body":null}]}`,
			want: []pullrequest.ThreadAction{{Path: "a", Resolve: true}},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := pullrequest.ParseThreadActions([]byte(tc.in), work, "threads.json")
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
// was sent first — not even the live read — is what the all-or-nothing promise
// is made of.
func TestReplyRefuses(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		actions []pullrequest.ThreadAction
		// wantErr is every fragment the message has to carry.
		wantErr []string
	}{
		{
			name:    "a body of only whitespace",
			actions: []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(10), Body: new("   "), Resolve: true}},
			wantErr: []string{"reply body is present but blank", "src/a.go:10"},
		},
		{
			name:    "neither a reply nor a resolve",
			actions: []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(10)}},
			wantErr: []string{"do nothing", "src/a.go:10"},
		},
		{
			// The transcription error the whole selector exists to catch: the
			// id belongs to a thread on another file entirely.
			name: "an id from another thread",
			actions: []pullrequest.ThreadAction{
				{Path: "src/a.go", Line: new(10), ID: new("PRRT_outdated"), Body: new("fixed"), Resolve: true},
			},
			wantErr: []string{"PRRT_outdated", "src/b.go", "testuser", "mine"},
		},
		{
			name: "an id no thread has",
			actions: []pullrequest.ThreadAction{
				{Path: "src/a.go", Line: new(10), ID: new("PRRT_typo"), Body: new("fixed"), Resolve: true},
			},
			wantErr: []string{"PRRT_typo", "context.json"},
		},
		{
			// Two reviewers on one line: the path cannot say which, so the run
			// stops and asks rather than guessing.
			name:    "two threads on the line",
			actions: []pullrequest.ThreadAction{{Path: "src/dup.go", Line: new(7), Body: new("fixed"), Resolve: true}},
			wantErr: []string{"PRRT_dup1", "PRRT_dup2", `"id"`},
		},
		{
			// The same, with the line left out: a path with several threads is
			// never resolved by taking the first.
			name:    "two threads at the path and no line",
			actions: []pullrequest.ThreadAction{{Path: "src/dup.go", Body: new("fixed"), Resolve: true}},
			wantErr: []string{"PRRT_dup1", "PRRT_dup2", `"id"`},
		},
		{
			// The thread is there, it is simply settled — which the message
			// says by naming it and its ball, rather than reading as "nothing
			// here" and sending the caller to try another line.
			name:    "a path with nothing we may reach",
			actions: []pullrequest.ThreadAction{{Path: "src/settled.go", Body: new("fixed"), Resolve: true}},
			wantErr: []string{"src/settled.go", "PRRT_none", "ball none"},
		},
		{
			// A thread waiting on the reviewer is reachable — the author adds
			// to it after a change of design — but closing it is still theirs.
			name:    "resolving a person's remark we have answered",
			actions: []pullrequest.ThreadAction{{Path: "src/answered.go", Body: new("the design changed"), Resolve: true}},
			wantErr: []string{"resolve", "PRRT_answered", "src/answered.go:30"},
		},
		{
			name:    "a path no thread is on at all",
			actions: []pullrequest.ThreadAction{{Path: "src/absent.go", Body: new("fixed"), Resolve: true}},
			wantErr: []string{"no thread at all is recorded at src/absent.go"},
		},
		{
			name:    "a line no thread is on",
			actions: []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(99), Body: new("fixed"), Resolve: true}},
			wantErr: []string{"src/a.go", "PRRT_bot", "PRRT_person"},
		},
		{
			// A person's remark is closed by that person; replying to it is
			// fine, resolving it is not.
			name:    "resolving a person's remark",
			actions: []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(20), Body: new("fixed"), Resolve: true}},
			wantErr: []string{"resolve", "PRRT_person", "src/a.go:20"},
		},
		{
			// Two selectors, one thread: without the check the reply lands
			// twice.
			name: "two entries on one thread",
			actions: []pullrequest.ThreadAction{
				{Path: "src/a.go", Line: new(10), Body: new("fixed"), Resolve: true},
				{Path: "src/a.go", ID: new("PRRT_bot"), Body: new("fixed again"), Resolve: true},
			},
			wantErr: []string{"duplicate", "PRRT_bot"},
		},
		{
			// The body rule reaches a reply too, and the refusal lands with
			// the selector refusals rather than after the first reply of the
			// run has already been posted.
			name: "a reply that numbers its items with bare #N",
			actions: []pullrequest.ThreadAction{
				{Path: "src/a.go", Line: new(10), Body: new("fixed"), Resolve: true},
				{Path: "src/b.go", Body: new("#1 one\n#2 two\n#3 three\n"), Resolve: true},
			},
			wantErr: []string{"src/b.go", "3 distinct bare #N"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			// Only the refusals reach here; the accepting cases are covered by
			// TestReply, which does let the requests through.
			unreachable := ghapitest.New(t, http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
				t.Error("a request was sent despite the refusal")
			}))
			req := pullrequest.ReplyRequest{
				Actions: tc.actions, Threads: contextThreads, ContextFile: "context.json", ThreadsFile: threadsFile(t),
				CurrentUser: "testuser",
			}
			_, err := pullrequest.Reply(t.Context(), unreachable, req)
			if err == nil {
				t.Fatalf("Reply succeeded, want an error mentioning %q", tc.wantErr)
			}
			for _, want := range tc.wantErr {
				if !strings.Contains(err.Error(), want) {
					t.Errorf("error = %q, want it to mention %q", err, want)
				}
			}
			// A dry run refuses the same way, since it shares every check.
			if _, err := pullrequest.DryRun(t.Context(), unreachable, req); err == nil {
				t.Error("DryRun accepted what Reply refused")
			}
		})
	}
}

// TestReplyResolvesSelectors is the accepting half: what a path, a line and an
// id pick out when they are unambiguous.
func TestReplyResolvesSelectors(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		action pullrequest.ThreadAction
		wantID string
	}{
		{
			name:   "a path and a line",
			action: pullrequest.ThreadAction{Path: "src/a.go", Line: new(10), Body: new("fixed"), Resolve: true},
			wantID: "PRRT_bot",
		},
		{
			// The only thread at the path, so the line is not needed.
			name:   "a path alone",
			action: pullrequest.ThreadAction{Path: "src/b.go", Body: new("fixed"), Resolve: true},
			wantID: "PRRT_outdated",
		},
		{
			// line is null on this thread, so the number the skill has in hand
			// after the fixing push only matches original_line.
			name:   "a line that only original_line has",
			action: pullrequest.ThreadAction{Path: "src/b.go", Line: new(55), Body: new("fixed"), Resolve: true},
			wantID: "PRRT_outdated",
		},
		{
			name:   "an id breaking a tie",
			action: pullrequest.ThreadAction{Path: "src/dup.go", Line: new(7), ID: new("PRRT_dup2"), Body: new("fixed"), Resolve: true},
			wantID: "PRRT_dup2",
		},
		{
			// Ours, on somebody else's pull request, answered by us: the
			// follow-up a change of design calls for, and ours to close.
			name:   "a thread we opened and answered",
			action: pullrequest.ThreadAction{Path: "src/waiting.go", Line: new(5), Body: new("the design changed"), Resolve: true},
			wantID: "PRRT_theirs",
		},
		{
			// The same on our own pull request, where the remark is a person's:
			// the reply lands and the thread stays open.
			name:   "a person's remark we have answered",
			action: pullrequest.ThreadAction{Path: "src/answered.go", Body: new("the design changed")},
			wantID: "PRRT_answered",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			m := &mutations{}
			got, err := pullrequest.Reply(t.Context(), m.client(t), pullrequest.ReplyRequest{
				Actions: []pullrequest.ThreadAction{tc.action}, Threads: contextThreads,
				ContextFile: "context.json", ThreadsFile: threadsFile(t), CurrentUser: "testuser",
			})
			if err != nil {
				t.Fatalf("Reply: %v", err)
			}
			if diff := cmp.Diff([]string{tc.wantID}, m.replied); diff != "" {
				t.Errorf("replies posted (-want +got):\n%s", diff)
			}
			if len(got.Replied) != 1 || got.Replied[0].Path != tc.action.Path {
				t.Errorf("replied = %+v, want one carrying the thread's path", got.Replied)
			}
		})
	}
}

// TestReplyAfterAnEarlierReply covers the ways a thread already carrying one
// of our replies comes back legitimately.
//
// Every one of these posts something the resend check has no business
// refusing — which matters because the skills tell the caller to do exactly
// this, into the same threads file, and the refusal's advice is to drop the
// entry.
func TestReplyAfterAnEarlierReply(t *testing.T) {
	t.Parallel()

	// step is one run against a thread the first run already replied to: the
	// reply it writes (none for a run that only resolves), and the newest
	// comment's url, body and author as the re-fetched context and the live
	// read agree on it. What the run should post follows from the body, so it
	// is derived rather than written down.
	type step struct {
		body                              *string
		lastComment, lastBody, lastAuthor string
	}
	ourReply := replyURL("PRRT_bot")

	tests := []struct {
		name  string
		steps []step
	}{
		{
			// The resolve half failed the first time — write access, a fork —
			// and the skills say to retry it with the body left out. It cannot
			// repeat a reply, because it posts none.
			name: "a resolve retried without the reply",
			steps: []step{
				{lastComment: ourReply, lastBody: "fixed", lastAuthor: "testuser"},
			},
		},
		{
			// A later /loop iteration on a thread somebody has spoken in since:
			// a genuinely new remark to answer, not the old one resent.
			name: "the thread was answered since we replied",
			steps: []step{
				{
					body: new("fixed again"), lastComment: "https://example.com/reviewer-came-back",
					lastBody: "a fresh remark", lastAuthor: "reviewer1",
				},
			},
		},
		{
			// Nobody has spoken since, and there is still something new to
			// say: the design changed under the answer we already gave.
			name: "a follow-up saying something else",
			steps: []step{
				{body: new("the design changed"), lastComment: ourReply, lastBody: "fixed", lastAuthor: "testuser"},
			},
		},
		{
			// And again on top of that one: the newest comment is now our own
			// follow-up, and what is being written is neither of our two
			// earlier replies.
			name: "a follow-up on a follow-up",
			steps: []step{
				{body: new("the design changed"), lastComment: ourReply, lastBody: "fixed", lastAuthor: "testuser"},
				{body: new("and once more"), lastComment: ourReply, lastBody: "the design changed", lastAuthor: "testuser"},
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			file := threadsFile(t)
			// The first run, driven rather than faked, so the reply the later
			// steps compare against is whatever this package actually posts.
			first := &mutations{failResolve: "PRRT_bot"}
			if _, err := pullrequest.Reply(t.Context(), first.client(t), pullrequest.ReplyRequest{
				Actions:     []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(10), Body: new("fixed"), Resolve: true}},
				Threads:     contextThreads,
				ContextFile: "context.json", ThreadsFile: file, CurrentUser: "testuser",
			}); err != nil {
				t.Fatalf("the first Reply: %v", err)
			}

			for i, s := range tc.steps {
				action := pullrequest.ThreadAction{Path: "src/a.go", Line: new(10), Body: s.body, Resolve: true}
				var wantReplied []string
				if s.body != nil {
					wantReplied = []string{"PRRT_bot"}
				}
				threads := replacing(contextThreads, "PRRT_bot", func(t pullrequest.KnownThread) pullrequest.KnownThread {
					t.LastCommentURL = s.lastComment
					return t
				})
				// The live read has to agree with the context it is handed, or
				// the staleness check stops the run before the resend check is
				// reached.
				m := &mutations{liveURL: s.lastComment, liveBody: s.lastBody, liveAuthor: s.lastAuthor}
				_, err := pullrequest.Reply(t.Context(), m.client(t), pullrequest.ReplyRequest{
					Actions: []pullrequest.ThreadAction{action}, Threads: threads,
					ContextFile: "context.json", ThreadsFile: file, CurrentUser: "testuser",
				})
				if err != nil {
					t.Fatalf("Reply %d: %v", i+1, err)
				}
				if diff := cmp.Diff(wantReplied, m.replied); diff != "" {
					t.Errorf("replies posted by run %d (-want +got):\n%s", i+1, diff)
				}
				if diff := cmp.Diff([]string{"PRRT_bot"}, m.resolved); diff != "" {
					t.Errorf("threads resolved by run %d (-want +got):\n%s", i+1, diff)
				}
			}
		})
	}
}

// replacing is one thread of a context swapped for a changed copy, so a case
// can say what moved without restating the rest.
func replacing(threads []pullrequest.KnownThread, id string,
	change func(pullrequest.KnownThread) pullrequest.KnownThread,
) []pullrequest.KnownThread {
	out := slices.Clone(threads)
	for i, t := range out {
		if t.ID == id {
			out[i] = change(t)
		}
	}
	return out
}

// TestReplyRefusesOnAStaleView is the last guard before the first mutation: the
// context is a snapshot, and a thread that has been resolved or answered since
// it was fetched is one the reply no longer belongs on. One such thread stops
// every entry, not only its own.
func TestReplyRefusesOnAStaleView(t *testing.T) {
	t.Parallel()

	actions := []pullrequest.ThreadAction{
		{Path: "src/a.go", Line: new(10), Body: new("fixed"), Resolve: true},
		{Path: "src/b.go", Body: new("fixed"), Resolve: true},
		// Resolve-only entries are read too: resolving a thread somebody has
		// since answered discards the answer from the author's list.
		{Path: "src/dup.go", Line: new(7), ID: new("PRRT_dup1"), Resolve: true},
	}

	tests := []struct {
		name string
		m    *mutations
		want string
	}{
		{
			name: "a thread gained a comment",
			m:    &mutations{movedNow: "PRRT_outdated"},
			want: "PRRT_outdated",
		},
		{
			name: "a thread is already resolved",
			m:    &mutations{resolvedNow: "PRRT_dup1"},
			want: "PRRT_dup1",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			req := pullrequest.ReplyRequest{
				Actions: actions, Threads: contextThreads, ContextFile: "context.json", ThreadsFile: threadsFile(t),
				CurrentUser: "testuser",
			}
			_, err := pullrequest.Reply(t.Context(), tc.m.client(t), req)
			if err == nil {
				t.Fatal("Reply succeeded over a stale context")
			}
			for _, want := range []string{tc.want, "ccx pr context"} {
				if !strings.Contains(err.Error(), want) {
					t.Errorf("error = %q, want it to mention %q", err, want)
				}
			}
			if len(tc.m.replied)+len(tc.m.resolved) > 0 {
				t.Errorf("replied %v and resolved %v, want nothing sent for any of the three", tc.m.replied, tc.m.resolved)
			}
		})
	}
}

// TestDryRun is the promise that the plan shown is the plan executed: every
// check runs, the live read included, and only the first mutation is held back.
func TestDryRun(t *testing.T) {
	t.Parallel()

	m := &mutations{}
	got, err := pullrequest.DryRun(t.Context(), m.client(t), pullrequest.ReplyRequest{
		Actions: []pullrequest.ThreadAction{
			{Path: "src/a.go", Line: new(10), Body: new("fixed"), Resolve: true},
			{Path: "src/b.go", Line: new(55), Resolve: true},
			{Path: "src/answered.go", Body: new("the design changed")},
		},
		Threads: contextThreads, ContextFile: "context.json", ThreadsFile: threadsFile(t), CurrentUser: "testuser",
	})
	if err != nil {
		t.Fatalf("DryRun: %v", err)
	}

	want := pullrequest.ReplyPlan{Plan: []pullrequest.PlannedThread{
		{
			ID: "PRRT_bot", Path: "src/a.go", Line: new(10), OriginalLine: new(10),
			OpenedBy: new("copilot-pull-request-reviewer"), Reply: true, Resolve: true,
		},
		{
			ID: "PRRT_outdated", Path: "src/b.go", OriginalLine: new(55),
			OpenedBy: new("testuser"), Resolve: true,
		},
		{
			ID: "PRRT_answered", Path: "src/answered.go", Line: new(30), OriginalLine: new(30),
			OpenedBy: new("reviewer1"), Reply: true,
		},
	}}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("DryRun (-want +got):\n%s", diff)
	}
	if len(m.replied)+len(m.resolved) > 0 {
		t.Errorf("replied %v and resolved %v, want a dry run to send neither", m.replied, m.resolved)
	}
	// The live read did happen, which is what makes the plan trustworthy.
	if diff := cmp.Diff([]string{"PRRT_bot", "PRRT_outdated", "PRRT_answered"}, m.read); diff != "" {
		t.Errorf("threads read (-want +got):\n%s", diff)
	}
}

// TestContextKnownThreads pins the projection alone.
func TestContextKnownThreads(t *testing.T) {
	t.Parallel()

	c := pullrequest.Context{
		PR: pullrequest.PR{HeadOID: "abc"},
		ReviewThreads: []pullrequest.Thread{
			{
				ID: "PRRT_1", Path: "a.go", Line: new(3), OriginalLine: new(3),
				OpenedBy: new("me"), Ball: pullrequest.BallMine, ResolvableByMe: true,
				LastComment: &pullrequest.ThreadComment{URL: "https://example.com/1"},
			},
			{ID: "PRRT_2", Path: "b.go", OriginalLine: new(9), Ball: pullrequest.BallNone},
		},
	}

	want := []pullrequest.KnownThread{
		{
			ID: "PRRT_1", Path: "a.go", Line: new(3), OriginalLine: new(3), OpenedBy: new("me"),
			Ball: pullrequest.BallMine, ResolvableByMe: true, LastCommentURL: "https://example.com/1",
		},
		// A thread with no comments has no url to compare the live read
		// against, which the empty string is.
		{ID: "PRRT_2", Path: "b.go", OriginalLine: new(9), Ball: pullrequest.BallNone},
	}
	if diff := cmp.Diff(want, c.KnownThreads()); diff != "" {
		t.Errorf("KnownThreads (-want +got):\n%s", diff)
	}

	// A pull request with no threads: the empty list is what `ccx pr context`
	// writes for one, and an empty list is what a run then acts on.
	if got := (pullrequest.Context{ReviewThreads: []pullrequest.Thread{}}).KnownThreads(); len(got) != 0 {
		t.Errorf("KnownThreads on a context with no threads = %+v, want none", got)
	}
}

// mutations answers the two mutations and the live read, recording what it was
// asked and failing whichever the test says.
type mutations struct {
	// failReply and failResolve name the thread whose mutation fails.
	failReply, failResolve string
	replied, resolved      []string
	// read is every thread the live check looked at, which is what says the
	// check ran at all.
	read []string
	// urlless names a thread whose reply comes back with no url.
	urlless string
	// resolvedNow and movedNow name the thread the live read finds already
	// resolved, or holding a comment the context never saw.
	resolvedNow, movedNow string
	// liveURL overrides the newest comment the live read reports, for a case
	// that hands Reply a context somebody has already spoken in.
	liveURL string
	// liveBody and liveAuthor are the newest comment's body and login the live
	// read reports. Left unset, they default to a body and a login that match
	// neither a reply this package would post nor our own login, so a case
	// only has to set them to exercise the resend check.
	liveBody, liveAuthor string
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

		if strings.Contains(req.Query, "node(id:") {
			m.read = append(m.read, id)
			url := lastURL(id)
			if m.liveURL != "" {
				url = m.liveURL
			}
			if id == m.movedNow {
				url += "-newer"
			}
			body := "please fix this"
			if m.liveBody != "" {
				body = m.liveBody
			}
			author := "reviewer1"
			if m.liveAuthor != "" {
				author = m.liveAuthor
			}
			fmt.Fprintf(w, `{"data":{"node":{"isResolved":%v,"comments":{"nodes":[{"author":{"login":%q,"__typename":"User"},"body":%q,"url":%q}]}}}}`,
				id == m.resolvedNow, author, body, url)
			return
		}
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
			fmt.Fprintf(w, `{"data":{"addPullRequestReviewThreadReply":{"comment":{"url":%q}}}}`, replyURL(id))
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

	m := &mutations{}
	actions := []pullrequest.ThreadAction{
		{Path: "src/a.go", Line: new(10), Body: new("confirmed"), Resolve: true},
		{Path: "src/b.go", Resolve: true},
	}

	got, err := pullrequest.Reply(t.Context(), m.client(t), pullrequest.ReplyRequest{
		Actions: actions, Threads: contextThreads, ContextFile: "context.json", ThreadsFile: threadsFile(t),
		CurrentUser: "testuser",
	})
	if err != nil {
		t.Fatalf("Reply: %v", err)
	}

	want := pullrequest.ThreadReplies{
		Replied: []pullrequest.RepliedThread{{
			ID: "PRRT_bot", Path: "src/a.go", Line: new(10), OriginalLine: new(10),
			URL: replyURL("PRRT_bot"),
		}},
		Resolved:      []string{"PRRT_bot", "PRRT_outdated"},
		ResolveFailed: []pullrequest.FailedResolve{},
		Warnings:      []string{},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Reply (-want +got):\n%s", diff)
	}
	// Only the thread with a body was replied to; the other was resolved
	// without one.
	if diff := cmp.Diff([]string{"PRRT_bot"}, m.replied); diff != "" {
		t.Errorf("replies posted (-want +got):\n%s", diff)
	}
}

// TestReplyDegradesOnResolve is the asymmetry: a reply that fails stops
// everything, a resolve that fails does not. Resolving needs write access,
// which a review of somebody else's fork does not have, and the replies are the
// point of the exercise.
func TestReplyDegradesOnResolve(t *testing.T) {
	t.Parallel()

	m := &mutations{failResolve: "PRRT_bot"}
	got, err := pullrequest.Reply(t.Context(), m.client(t), pullrequest.ReplyRequest{
		Actions: []pullrequest.ThreadAction{
			{Path: "src/a.go", Line: new(10), Body: new("confirmed"), Resolve: true},
			{Path: "src/b.go", Body: new("confirmed"), Resolve: true},
		},
		Threads: contextThreads, ContextFile: "context.json", ThreadsFile: threadsFile(t), CurrentUser: "testuser",
	})
	if err != nil {
		t.Fatalf("Reply: %v", err)
	}

	if len(got.Replied) != 2 {
		t.Errorf("replied = %+v, want both", got.Replied)
	}
	if diff := cmp.Diff([]string{"PRRT_outdated"}, got.Resolved); diff != "" {
		t.Errorf("resolved (-want +got):\n%s", diff)
	}
	if len(got.ResolveFailed) != 1 || got.ResolveFailed[0].ID != "PRRT_bot" {
		t.Errorf("resolve_failed = %+v, want the one that could not be resolved", got.ResolveFailed)
	}
	if len(got.Warnings) != 1 || !strings.Contains(got.Warnings[0], "PRRT_bot") {
		t.Errorf("warnings = %v, want one naming the thread", got.Warnings)
	}
}

func TestReplyStops(t *testing.T) {
	t.Parallel()

	actions := []pullrequest.ThreadAction{
		{Path: "src/a.go", Line: new(10), Body: new("confirmed"), Resolve: true},
		{Path: "src/b.go", Body: new("confirmed"), Resolve: true},
		{Path: "src/dup.go", ID: new("PRRT_dup1"), Body: new("confirmed"), Resolve: true},
	}

	tests := []struct {
		name string
		m    *mutations
		// wantAlreadyReplied is what the message's "already replied" line
		// should list, and wantLeft what it should call unprocessed.
		wantAlreadyReplied string
		wantLeft           string
	}{
		{
			name:               "the reply is refused",
			m:                  &mutations{failReply: "PRRT_outdated"},
			wantAlreadyReplied: "already replied (do NOT resend on retry): PRRT_bot",
			// The outdated thread is named by its original_line, which is the
			// number a retry would write against it.
			wantLeft: "not processed: src/b.go:55 (PRRT_outdated), src/dup.go:7 (PRRT_dup1)",
		},
		{
			// The reply landed even though the answer was unusable, so it is
			// reported as already replied: a retry must not resend it.
			name:               "the reply lands without a url",
			m:                  &mutations{urlless: "PRRT_outdated"},
			wantAlreadyReplied: "already replied (do NOT resend on retry): PRRT_bot, PRRT_outdated",
			wantLeft:           "not processed: src/dup.go:7 (PRRT_dup1)",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			_, err := pullrequest.Reply(t.Context(), tc.m.client(t), pullrequest.ReplyRequest{
				Actions: actions, Threads: contextThreads, ContextFile: "context.json", ThreadsFile: threadsFile(t),
				CurrentUser: "testuser",
			})
			if err == nil {
				t.Fatal("Reply succeeded, want it to stop")
			}

			// Nothing after the failure was touched: a run acts on its input in
			// order and stops, rather than skipping past what went wrong.
			if slices.Contains(tc.m.replied, "PRRT_dup1") {
				t.Errorf("replies posted = %v, want nothing after the failure", tc.m.replied)
			}
			for _, want := range []string{tc.wantAlreadyReplied, tc.wantLeft} {
				if !strings.Contains(err.Error(), want) {
					t.Errorf("error = %q, want it to say %q", err, want)
				}
			}
		})
	}
}

// TestReplyReachesAThreadWhoseReplyURLNeverCame is what a reply whose comment
// url never comes back does not do: keep a later reply off the thread. The
// first run's mutation posts without a url and aborts; a second run naming a
// different body still reaches the thread and posts, because what a resend
// check is judged against is the live newest comment, not anything a run
// failed to learn.
func TestReplyReachesAThreadWhoseReplyURLNeverCame(t *testing.T) {
	t.Parallel()

	file := threadsFile(t)
	first := &mutations{urlless: "PRRT_bot"}
	_, err := pullrequest.Reply(t.Context(), first.client(t), pullrequest.ReplyRequest{
		Actions:     []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(10), Body: new("confirmed"), Resolve: true}},
		Threads:     contextThreads,
		ContextFile: "context.json", ThreadsFile: file, CurrentUser: "testuser",
	})
	if err == nil {
		t.Fatal("the first Reply succeeded, want it to stop on the missing url")
	}

	second := &mutations{}
	got, err := pullrequest.Reply(t.Context(), second.client(t), pullrequest.ReplyRequest{
		Actions:     []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(10), Body: new("saying something else"), Resolve: true}},
		Threads:     contextThreads,
		ContextFile: "context.json", ThreadsFile: file, CurrentUser: "testuser",
	})
	if err != nil {
		t.Fatalf("the second Reply: %v", err)
	}
	if diff := cmp.Diff([]string{"PRRT_bot"}, second.replied); diff != "" {
		t.Errorf("replies posted (-want +got):\n%s", diff)
	}
	if len(got.Replied) != 1 {
		t.Errorf("replied = %+v, want the one reply", got.Replied)
	}
}

// TestReplyRefusesARepeat is the live-read half of stopping a resend: an
// entry whose reply would say, again, what our own newest comment on that
// thread already says is refused before anything is sent, and a follow-up
// that says something else, or the same words from somebody else, is not a
// resend and reaches the thread.
func TestReplyRefusesARepeat(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		body       string
		liveBody   string
		liveAuthor string
		// wantRefused is false for every case that should reach the thread.
		wantRefused bool
	}{
		{
			name: "the same body from us is refused", body: "fixed",
			liveBody: "fixed", liveAuthor: "testuser", wantRefused: true,
		},
		{
			name: "a different body from us passes", body: "said something else",
			liveBody: "fixed", liveAuthor: "testuser",
		},
		{
			name: "a further different body from us still passes", body: "and once more",
			liveBody: "said something else", liveAuthor: "testuser",
		},
		{
			// It is not our reply repeating itself, since we never said it.
			name: "the same words from somebody else are not our repeat", body: "fixed",
			liveBody: "fixed", liveAuthor: "reviewer1",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			req := pullrequest.ReplyRequest{
				Actions: []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(10), Body: new(tc.body), Resolve: false}},
				Threads: contextThreads, ContextFile: "context.json", ThreadsFile: threadsFile(t),
				CurrentUser: "testuser",
			}

			m := &mutations{liveBody: tc.liveBody, liveAuthor: tc.liveAuthor}
			_, err := pullrequest.Reply(t.Context(), m.client(t), req)
			if !tc.wantRefused {
				if err != nil {
					t.Fatalf("Reply: %v", err)
				}
				if diff := cmp.Diff([]string{"PRRT_bot"}, m.replied); diff != "" {
					t.Errorf("replies posted (-want +got):\n%s", diff)
				}
				return
			}

			if err == nil {
				t.Fatal("Reply succeeded, want it to refuse a repeat")
			}
			for _, want := range []string{"the reply this entry would post again", "src/a.go:10 (PRRT_bot)"} {
				if !strings.Contains(err.Error(), want) {
					t.Errorf("error = %q, want it to mention %q", err, want)
				}
			}
			if len(m.replied) > 0 {
				t.Errorf("replied %v, want nothing sent for a refused repeat", m.replied)
			}
			// A dry run refuses the same way, since it shares every check.
			dry := &mutations{liveBody: tc.liveBody, liveAuthor: tc.liveAuthor}
			if _, err := pullrequest.DryRun(t.Context(), dry.client(t), req); err == nil {
				t.Error("DryRun accepted what Reply refused")
			}
		})
	}
}

// TestReplyIgnoresAStrayFileBesideTheThreadsFile is the check against a file
// happening to sit where the old on-disk record used to live: nothing in this
// package writes one, and nothing reads one either.
func TestReplyIgnoresAStrayFileBesideTheThreadsFile(t *testing.T) {
	t.Parallel()

	file := threadsFile(t)
	stray := file + ".posted"
	// The old record format, naming the very reply this run is about to post:
	// under the old rule this content would have refused it.
	old := "PRRT_bot " + lastURL("PRRT_bot") + " 0000000000000000000000000000000000000000000000000000000000000000\n"
	if err := os.WriteFile(stray, []byte(old), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	m := &mutations{}
	got, err := pullrequest.Reply(t.Context(), m.client(t), pullrequest.ReplyRequest{
		Actions:     []pullrequest.ThreadAction{{Path: "src/a.go", Line: new(10), Body: new("fixed"), Resolve: true}},
		Threads:     contextThreads,
		ContextFile: "context.json", ThreadsFile: file, CurrentUser: "testuser",
	})
	if err != nil {
		t.Fatalf("Reply: %v", err)
	}
	if len(got.Replied) != 1 {
		t.Errorf("replied = %+v, want the one reply", got.Replied)
	}

	b, err := os.ReadFile(stray)
	if err != nil {
		t.Fatalf("ReadFile(%q): %v", stray, err)
	}
	if string(b) != old {
		t.Errorf("the stray file changed from %q to %q, want it untouched", old, string(b))
	}
}
