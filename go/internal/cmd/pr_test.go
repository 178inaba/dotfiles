package cmd

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/contract"
	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
)

// TestStoreSeen is the writing half of the record: the bytes are the ones its
// declaration publishes, and the temporary file it went through is gone, so no
// reader finds a half-written record where a whole one is expected.
func TestStoreSeen(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	path := filepath.Join(dir, "5.json")
	if err := storeSeen(path, pullrequest.Seen{SeenAt: "2026-01-10T00:00:00Z"}); err != nil {
		t.Fatalf("storeSeen: %v", err)
	}

	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if !strings.Contains(string(b), `"seen_at": "2026-01-10T00:00:00Z"`) {
		t.Errorf("the record reads %s, want the instant under seen_at", b)
	}

	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	if len(entries) != 1 {
		t.Errorf("%s holds %d files, want only the record", dir, len(entries))
	}
}

// contextDocument is the smallest document `ccx pr seen` can be given: what
// Context declares, and the two fields the command reads out of it.
//
// isOwnPR is a parameter because it is a gate rather than a detail: the body
// of somebody else's pull request is not edited, and a case about that refusal
// says so where it is read. headOID is one for the same reason: the head the
// document was fetched at is what the check before posting compares against.
func contextDocument(t *testing.T, fetchedAt string, isOwnPR bool, headOID string) string {
	t.Helper()

	path := filepath.Join(t.TempDir(), "pr-context-owner@repo-5.json")
	doc := fmt.Sprintf(`{"fetched_at":%q,
		"pending":{"since":null,"threads":[],"reviews":[],"comments":[]},
		"repo":"owner/repo","is_own_pr":%t,
		"pr":{"number":5,"base_ref":"main","head_ref":"feature/x","head_oid":%q},
		"reviewers":[],"review_threads":[]}`, fetchedAt, isOwnPR, headOID)
	if err := os.WriteFile(path, []byte(doc), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	return path
}

// TestPRSeen drives the command the way a skill does: it records, it says
// where, and a second run against an older document is refused rather than
// moving the mark backwards.
func TestPRSeen(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)

	var out, errOut bytes.Buffer
	if code := run(t.Context(), []string{"pr", "seen", contextDocument(t, "2026-01-11T00:00:00Z", true, "abc123")},
		strings.NewReader(""), &out, &errOut, Deps{}); code != 0 {
		t.Fatalf("`ccx pr seen` = %d, want 0: %s", code, errOut.String())
	}

	want := pullrequest.SeenPath(state, ghapi.Repo{Owner: "owner", Name: "repo"}, 5)
	if !strings.Contains(out.String(), want) || !strings.Contains(out.String(), "2026-01-11T00:00:00Z") {
		t.Errorf("`ccx pr seen` printed %s, want the path %s and the instant", out.String(), want)
	}
	if at := pullrequest.ReadSeen(state, ghapi.Repo{Owner: "owner", Name: "repo"}, 5); at == nil {
		t.Error("nothing was recorded")
	}

	out.Reset()
	errOut.Reset()
	if code := run(t.Context(), []string{"pr", "seen", contextDocument(t, "2026-01-10T00:00:00Z", true, "abc123")},
		strings.NewReader(""), &out, &errOut, Deps{}); code == 0 {
		t.Error("`ccx pr seen` on an older document = 0, want a refusal")
	}
}

// TestPRSeenWithoutTheVariable is the other half of where the record goes: with
// the variable unset the whole command, not just the resolution, has to land
// under ~/.local/state. Asserting the resolution alone would pass on a command
// that then wrote somewhere else.
func TestPRSeenWithoutTheVariable(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("XDG_STATE_HOME", "")

	var out, errOut bytes.Buffer
	if code := run(t.Context(), []string{"pr", "seen", contextDocument(t, "2026-01-11T00:00:00Z", true, "abc123")},
		strings.NewReader(""), &out, &errOut, Deps{}); code != 0 {
		t.Fatalf("`ccx pr seen` = %d, want 0: %s", code, errOut.String())
	}

	want := filepath.Join(home, ".local", "state", "ccx", "seen", "owner", "repo", "5.json")
	if _, err := os.Stat(want); err != nil {
		t.Errorf("no record at %s: %v", want, err)
	}
}

// noClient is what a command that must not reach GitHub is given: a
// constructor that fails the test by name rather than the nil one a bare Deps
// carries, so that a run which does reach it says what it broke.
func noClient(t *testing.T) Deps {
	t.Helper()

	return Deps{NewClient: func() (*ghapi.Client, error) {
		t.Error("the command asked for a client, want the refusal to come first")
		return nil, errors.New("no client")
	}}
}

// The two refusals `ccx pr body-append` makes before it could reach GitHub,
// which is why they can be run without a server: the document says whose pull
// request it is, and the file name says whether it is in the work dir. Both
// come before the body is read, so a run at fault for one is not told about
// the other.
func TestPRBodyAppendRefusesBeforeItReachesGitHub(t *testing.T) {
	t.Parallel()

	for _, tt := range []struct {
		name     string
		isOwnPR  bool
		bodyFile string
		wantErr  string
	}{
		{name: "somebody else's pull request", bodyFile: "section.md", wantErr: "is not ours"},
		{name: "a body file outside the work dir", isOwnPR: true, bodyFile: "sub/section.md", wantErr: "bare file name"},
	} {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var out, errOut bytes.Buffer
			code := run(t.Context(), []string{
				"pr", "body-append", contextDocument(t, "2026-01-11T00:00:00Z", tt.isOwnPR, "abc123"),
				"--body-file", tt.bodyFile,
			}, strings.NewReader(""), &out, &errOut, noClient(t))

			if code == 0 {
				t.Fatalf("`ccx pr body-append` = 0, want a refusal")
			}
			if !strings.Contains(errOut.String(), tt.wantErr) {
				t.Errorf("stderr = %q, want it to mention %q", errOut.String(), tt.wantErr)
			}
		})
	}
}

// publishedLimits is what each fetch limit owes a reader: the variable that
// raises it, the cap it raises, and the flag that reports the cap was reached.
//
// Written out by hand, and deliberately not read from the table the command and
// the help are rendered from — a test taking the pairing from that table would
// agree with it however it was wired, which is the mistake both tests below
// exist to catch.
var publishedLimits = []struct {
	variable string
	cap      func(pullrequest.Limits) int
	flag     string
}{
	{"MAX_COMMENTS", func(l pullrequest.Limits) int { return l.Comments }, "comments_truncated"},
	{"MAX_REVIEWS", func(l pullrequest.Limits) int { return l.Reviews }, "reviews_truncated"},
	{"MAX_THREADS", func(l pullrequest.Limits) int { return l.Threads }, "threads_truncated"},
	{"MAX_THREAD_COMMENTS", func(l pullrequest.Limits) int { return l.ThreadComments }, "review_threads[].comments_truncated"},
	{"MAX_ISSUE_COMMENTS", func(l pullrequest.Limits) int { return l.IssueComments }, "linked_issues[].comments_truncated"},
}

// TestContextLimitsBindEachVariableToItsOwnCap pins the pairing an accessor
// makes easy to get wrong: nothing about a row says the closure beside a
// variable reaches that variable's own cap, and a swapped pair would quietly
// raise the wrong collection.
func TestContextLimitsBindEachVariableToItsOwnCap(t *testing.T) {
	for _, l := range publishedLimits {
		t.Run(l.variable, func(t *testing.T) {
			t.Setenv(l.variable, "7")

			got, err := contextLimits()
			if err != nil {
				t.Fatalf("contextLimits: %v", err)
			}
			if l.cap(got) != 7 {
				t.Errorf("%s left %+v, want it to raise its own cap to 7", l.variable, got)
			}
		})
	}
}

// TestPRContextHelpPublishesEachLimit is what lets the skills point at the help
// instead of copying the numbers into a table of their own: a reader answering
// a truncation finds the variable to raise, what it is now, and which flag sent
// them there, in one place. A help carrying its own copy of a number fails here
// as soon as the struct moves, which is how the table drifted while it lived in
// a skill.
func TestPRContextHelpPublishesEachLimit(t *testing.T) {
	t.Parallel()

	text := longFor("pr context")
	rows := map[string]string{}
	for _, line := range strings.Split(text, "\n") {
		if cols := strings.Fields(line); len(cols) > 0 && strings.HasPrefix(cols[0], "MAX_") {
			rows[cols[0]] = line
		}
	}
	if len(rows) != len(publishedLimits) {
		t.Fatalf("%d rows name a variable, want one each for %d:\n%s", len(rows), len(publishedLimits), text)
	}

	// Every path the document walks, so a flag renamed in the contract is not
	// left behind here: transcribing a json tag into a help is the arrangement
	// the rendered contract exists to end.
	published, err := contract.Paths(reflect.TypeFor[pullrequest.Context]())
	if err != nil {
		t.Fatalf("Paths: %v", err)
	}

	for _, l := range publishedLimits {
		t.Run(l.variable, func(t *testing.T) {
			// By column rather than by substring: comments_truncated is a
			// substring of both per-item flags, so a row showing the wrong one
			// would pass a contains check — the very mix-up this catches.
			cols := strings.Fields(rows[l.variable])
			want := []string{l.variable, strconv.Itoa(l.cap(pullrequest.DefaultLimits)), l.flag}
			if len(cols) < len(want) || !slices.Equal(cols[:len(want)], want) {
				t.Errorf("row is %q, want it to open with %q", rows[l.variable], strings.Join(want, " "))
			}
			// The whole path, not its segments: comments_truncated is a key on
			// four of the document's types, so a renamed review_threads would
			// leave the path in front of it stale while every segment of it
			// still stood somewhere.
			if !slices.Contains(published, l.flag) {
				t.Errorf("the document walks no %q, so %s sends a reader to a path nobody has", l.flag, l.variable)
			}
		})
	}
}

// TestStateHome pins where a judged pull request is recorded. Not parallel,
// and here rather than in pullrequest, for the reason the clone workspace's
// equivalent is: t.Setenv changes the whole process, so the package that keeps
// the record takes the directory as a parameter and only this reader touches
// the environment.
func TestStateHome(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)

	t.Run("XDG_STATE_HOME wins", func(t *testing.T) {
		xdg := t.TempDir()
		t.Setenv("XDG_STATE_HOME", xdg)
		if got := stateHome(); got != xdg {
			t.Errorf("stateHome() = %q, want %q", got, xdg)
		}
	})

	t.Run("without it the home directory", func(t *testing.T) {
		t.Setenv("XDG_STATE_HOME", "")
		want := filepath.Join(home, ".local", "state")
		if got := stateHome(); got != want {
			t.Errorf("stateHome() = %q, want %q", got, want)
		}
	})
}

// commitRef names one of the commits a head check case is written against:
// the two the checkout stands on, and one it cannot resolve at all. A type of
// its own so that a case cannot name a commit the fixture does not hold.
type commitRef int

const (
	refFirst commitRef = iota
	refSecond
	refElsewhere
	numCommitRefs
)

// headCheckFixture is the checkout the two posting commands run in, and the
// commits their cases name. Built once per test rather than per case: nothing
// a case does writes to the checkout, and building it each time is four git
// processes for three strings that never differ.
type headCheckFixture struct {
	repo string
	oid  [numCommitRefs]string
}

func newHeadCheckFixture(t *testing.T) headCheckFixture {
	t.Helper()

	gittest.SkipWithoutGit(t)
	repo := gittest.InitWithCommit(t, filepath.Join(t.TempDir(), "repo"))
	first := gittest.Rev(t, repo, "HEAD")
	gittest.Write(t, filepath.Join(repo, "file.txt"), "second\n")
	gittest.Run(t, repo, "commit", "-aqm", "second")

	// A commit in a repository of its own, so that it names a head this
	// checkout cannot resolve. It is the second commit, and its content
	// differs from the one above, because two repositories built from the same
	// content, the same identity and the same message within one second hash
	// alike — the first commits of these two do, and taking one of them here
	// would make the case about an unresolvable head into a case about first.
	other := gittest.InitWithCommit(t, filepath.Join(t.TempDir(), "elsewhere"))
	gittest.Write(t, filepath.Join(other, "file.txt"), "elsewhere\n")
	gittest.Run(t, other, "commit", "-aqm", "elsewhere")

	return headCheckFixture{repo: repo, oid: [3]string{
		refFirst:     first,
		refSecond:    gittest.Rev(t, repo, "HEAD"),
		refElsewhere: gittest.Rev(t, other, "HEAD"),
	}}
}

// headCheckHandler answers the live head lookup, and the comment a run that passes
// the check goes on to post. posted counts what arrived there, which is how a
// run that was accepted is told from one that only failed later.
func headCheckHandler(t *testing.T, liveHead string, posted *int) http.Handler {
	t.Helper()

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		switch {
		case r.URL.Path == "/graphql":
			fmt.Fprintf(w, `{"data":{"repository":{"pullRequest":{"number":5,"headRefOid":%q}}}}`, liveHead)
		case r.Method == http.MethodPost && r.URL.Path == "/repos/owner/repo/issues/5/comments":
			*posted++
			fmt.Fprint(w, `{"html_url":"https://github.com/owner/repo/pull/5#issuecomment-1"}`)
		default:
			t.Errorf("unexpected request to %s %s", r.Method, r.URL.Path)
		}
	})
}

// headCheckCase is one of the five states RequirePushedHead tells apart, named
// by what the checkout, the pull request's live head and the document's head
// are to each other.
type headCheckCase struct {
	name string
	// live is the head GitHub reports and docHead the one the document was
	// fetched at, each naming one of the fixture's commits.
	live, docHead commitRef
	// liveFails serves the lookup a 500 instead, since a head that could not
	// be read is not a head that matched.
	liveFails bool
	// wantErr is empty where the run is expected to go through.
	wantErr string
}

func headCheckCases() []headCheckCase {
	return []headCheckCase{
		{
			name:    "the local commit has not been pushed",
			live:    refFirst,
			docHead: refFirst,
			wantErr: "push before",
		},
		{
			name:    "the live head is not in this checkout",
			live:    refElsewhere,
			docHead: refFirst,
			wantErr: "sync the checkout before",
		},
		{
			name:    "the document's head was rewritten away",
			live:    refSecond,
			docHead: refElsewhere,
			wantErr: "not an ancestor",
		},
		{
			name:      "the live head cannot be read",
			live:      refSecond,
			docHead:   refFirst,
			liveFails: true,
			wantErr:   "failed to read the pull request's current head",
		},
		{
			name:    "the push landed with the document behind it",
			live:    refSecond,
			docHead: refFirst,
		},
	}
}

// run drives one command through one case and answers with the exit code, what
// went to stderr, and how many comments reached the server.
//
// args is given the document and the work dir the case implies, so that the two
// commands differ only in what they are asked to do once the check has passed.
// It has to write whatever it names into the work dir, which is what brings
// that directory into being.
func (f headCheckFixture) run(t *testing.T, tt headCheckCase,
	args func(contextFile, workDir string) []string) (int, string, int) {
	t.Helper()

	contextFile := contextDocument(t, "2026-01-11T00:00:00Z", true, f.oid[tt.docHead])

	posted := 0
	h := headCheckHandler(t, f.oid[tt.live], &posted)
	if tt.liveFails {
		h = http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
			w.WriteHeader(http.StatusInternalServerError)
		})
	}
	deps := Deps{NewClient: func() (*ghapi.Client, error) { return ghapitest.New(t, h), nil }, Dir: f.repo}

	var errOut bytes.Buffer
	code := run(t.Context(), args(contextFile, pullrequest.WorkDir(contextFile)),
		strings.NewReader(""), io.Discard, &errOut, deps)
	return code, errOut.String(), posted
}

// assertHeadCheck holds one run to what its case expects: an acceptance, or a
// refusal that names its own reason — and either way, exactly the requests the
// command had to make.
func assertHeadCheck(t *testing.T, cmd string, tt headCheckCase, code int, stderr string, posted, wantPosted int) {
	t.Helper()

	if tt.wantErr == "" {
		if code != 0 {
			t.Fatalf("`ccx %s` = %d, want 0: %s", cmd, code, stderr)
		}
	} else {
		if code == 0 {
			t.Fatalf("`ccx %s` = 0, want a refusal", cmd)
		}
		if !strings.Contains(stderr, tt.wantErr) {
			t.Errorf("stderr = %q, want it to mention %q", stderr, tt.wantErr)
		}
	}
	if posted != wantPosted {
		t.Errorf("%d comments reached the server, want %d", posted, wantPosted)
	}
}

// TestPRCommentChecksTheLiveHead is the check at the boundary a skill calls it
// from: the command is driven through run with the network and the checkout
// both standing in, and the five states are told apart by what each refuses.
//
// The accepted case goes all the way to the posting path and the request
// arrives at the handler, which is the property the injected client is for.
func TestPRCommentChecksTheLiveHead(t *testing.T) {
	t.Parallel()

	f := newHeadCheckFixture(t)

	for _, tt := range headCheckCases() {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			code, stderr, posted := f.run(t, tt, func(contextFile, workDir string) []string {
				gittest.Write(t, filepath.Join(workDir, "body.md"), "The fixes are in.\n")
				return []string{"pr", "comment", contextFile, "--body-file", "body.md"}
			})

			wantPosted := 0
			if tt.wantErr == "" {
				wantPosted = 1
			}
			assertHeadCheck(t, "pr comment", tt, code, stderr, posted, wantPosted)
		})
	}
}

// TestPRReplyThreadsChecksTheLiveHead is the same five states for the other
// command. Nothing is posted in any of them, the accepted case included: a run
// with nothing to say is still held to the check, and then has nothing to
// send. Where the check sits relative to parsing the threads file is not what
// these cases tell apart — every one of them is given a threads file that
// parses.
func TestPRReplyThreadsChecksTheLiveHead(t *testing.T) {
	t.Parallel()

	f := newHeadCheckFixture(t)

	for _, tt := range headCheckCases() {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			code, stderr, posted := f.run(t, tt, func(contextFile, workDir string) []string {
				threadsFile := filepath.Join(workDir, "threads.json")
				gittest.Write(t, threadsFile, `{"threads":[]}`+"\n")
				return []string{"pr", "reply-threads", contextFile, threadsFile}
			})

			assertHeadCheck(t, "pr reply-threads", tt, code, stderr, posted, 0)
		})
	}
}
