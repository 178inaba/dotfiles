package cmd

import (
	"bytes"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

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

// A mark the command does not own is refused on its own terms, before the body
// is looked for. Resolving the body first reports a missing file for a run
// whose real fault is the mark, which sends the reader to fix the wrong thing.
func TestPRCommentRefusesAnUnknownMarkFirst(t *testing.T) {
	t.Parallel()

	var out, errOut bytes.Buffer
	code := run(t.Context(), []string{
		"pr", "comment", contextDocument(t, "2026-01-11T00:00:00Z", true, "abc123"),
		"--mark", "other", "--body-file", "nowhere.md",
	}, strings.NewReader(""), &out, &errOut, Deps{})

	if code == 0 {
		t.Fatal("`ccx pr comment --mark other` = 0, want a refusal")
	}
	if !strings.Contains(errOut.String(), "unknown mark") {
		t.Errorf("stderr = %q, want it to name the mark as the fault", errOut.String())
	}
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
			}, strings.NewReader(""), &out, &errOut, Deps{})

			if code == 0 {
				t.Fatalf("`ccx pr body-append` = 0, want a refusal")
			}
			if !strings.Contains(errOut.String(), tt.wantErr) {
				t.Errorf("stderr = %q, want it to mention %q", errOut.String(), tt.wantErr)
			}
		})
	}
}

// TestContextLimitsBindEachVariableToItsOwnCap pins the pairing a positional
// list makes easy to get wrong: the variable names and the caps they raise are
// two lists kept aligned by hand, and a swapped pair would quietly raise the
// wrong collection.
func TestContextLimitsBindEachVariableToItsOwnCap(t *testing.T) {
	for name, raised := range map[string]func(pullrequest.Limits) int{
		"MAX_COMMENTS":        func(l pullrequest.Limits) int { return l.Comments },
		"MAX_REVIEWS":         func(l pullrequest.Limits) int { return l.Reviews },
		"MAX_THREADS":         func(l pullrequest.Limits) int { return l.Threads },
		"MAX_THREAD_COMMENTS": func(l pullrequest.Limits) int { return l.ThreadComments },
		"MAX_ISSUE_COMMENTS":  func(l pullrequest.Limits) int { return l.IssueComments },
	} {
		t.Run(name, func(t *testing.T) {
			t.Setenv(name, "7")

			got, err := contextLimits()
			if err != nil {
				t.Fatalf("contextLimits: %v", err)
			}
			if raised(got) != 7 {
				t.Errorf("%s left %+v, want it to raise its own cap to 7", name, got)
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

// headCheckRepo is the checkout the two posting commands run in: two commits,
// so that a document can name one while the checkout stands on the other.
func headCheckRepo(t *testing.T) (dir, first, second string) {
	t.Helper()

	dir = gittest.InitWithCommit(t, filepath.Join(t.TempDir(), "repo"))
	first = gittest.Rev(t, dir, "HEAD")
	gittest.Write(t, filepath.Join(dir, "file.txt"), "second\n")
	gittest.Run(t, dir, "commit", "-aqm", "second")
	return dir, first, gittest.Rev(t, dir, "HEAD")
}

// elsewhereCommit is a commit no checkout of headCheckRepo can resolve: what a
// head belonging to a branch nothing here ever fetched looks like. Its content
// differs from headCheckRepo's so that the two cannot hash alike.
func elsewhereCommit(t *testing.T) string {
	t.Helper()

	dir := gittest.InitWithCommit(t, filepath.Join(t.TempDir(), "elsewhere"))
	gittest.Write(t, filepath.Join(dir, "file.txt"), "elsewhere\n")
	gittest.Run(t, dir, "commit", "-aqm", "elsewhere")
	return gittest.Rev(t, dir, "HEAD")
}

// prHandler answers the live head lookup, and the comment a run that passes
// the check goes on to post. posted counts what arrived there, which is how a
// run that was accepted is told from one that only failed later.
func prHandler(t *testing.T, liveHead string, posted *int) http.Handler {
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
// are to each other. The two heads are chosen from the fixture's commits, which
// is why they arrive as functions rather than as strings.
type headCheckCase struct {
	name          string
	live, docHead func(first, second, elsewhere string) string
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
			live:    func(first, _, _ string) string { return first },
			docHead: func(first, _, _ string) string { return first },
			wantErr: "push before",
		},
		{
			name:    "the live head is not in this checkout",
			live:    func(_, _, elsewhere string) string { return elsewhere },
			docHead: func(first, _, _ string) string { return first },
			wantErr: "sync the checkout before",
		},
		{
			name:    "the document's head was rewritten away",
			live:    func(_, second, _ string) string { return second },
			docHead: func(_, _, elsewhere string) string { return elsewhere },
			wantErr: "not an ancestor",
		},
		{
			name:      "the live head cannot be read",
			live:      func(_, second, _ string) string { return second },
			docHead:   func(first, _, _ string) string { return first },
			liveFails: true,
			wantErr:   "failed to read the pull request's current head",
		},
		{
			name:    "the push landed with the document behind it",
			live:    func(_, second, _ string) string { return second },
			docHead: func(first, _, _ string) string { return first },
		},
	}
}

// runHeadCheckCase drives one command through one case and answers with the
// exit code, what went to stderr, and how many comments reached the server.
//
// args is given the document and the work dir the case implies, so that the two
// commands differ only in what they are asked to do once the check has passed.
func runHeadCheckCase(t *testing.T, tt headCheckCase, args func(contextFile, workDir string) []string) (int, string, int) {
	t.Helper()

	gittest.SkipWithoutGit(t)
	repo, first, second := headCheckRepo(t)
	elsewhere := elsewhereCommit(t)
	t.Chdir(repo)

	contextFile := contextDocument(t, "2026-01-11T00:00:00Z", true, tt.docHead(first, second, elsewhere))
	workDir := pullrequest.WorkDir(contextFile)
	if err := os.MkdirAll(workDir, 0o755); err != nil {
		t.Fatalf("MkdirAll %s: %v", workDir, err)
	}

	posted := 0
	h := prHandler(t, tt.live(first, second, elsewhere), &posted)
	if tt.liveFails {
		h = http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
			w.WriteHeader(http.StatusInternalServerError)
		})
	}
	deps := Deps{NewClient: func() (*ghapi.Client, error) { return ghapitest.New(t, h), nil }}

	var out, errOut bytes.Buffer
	code := run(t.Context(), args(contextFile, workDir), strings.NewReader(""), &out, &errOut, deps)
	return code, errOut.String(), posted
}

// TestPRCommentChecksTheLiveHead is the check at the boundary a skill calls
// it from: the command is driven through run with the network and the checkout
// both standing in, and the five states are told apart by what each refuses.
//
// Not parallel, because these commands read the checkout out of the working
// directory rather than a flag, and t.Chdir moves the whole process.
func TestPRCommentChecksTheLiveHead(t *testing.T) {
	for _, tt := range headCheckCases() {
		t.Run(tt.name, func(t *testing.T) {
			code, stderr, posted := runHeadCheckCase(t, tt, func(contextFile, workDir string) []string {
				gittest.Write(t, filepath.Join(workDir, "body.md"), "The fixes are in.\n")
				return []string{"pr", "comment", contextFile,
					"--mark", string(pullrequest.MarkReviewResponse), "--body-file", "body.md"}
			})

			if tt.wantErr == "" {
				if code != 0 {
					t.Fatalf("`ccx pr comment` = %d, want 0: %s", code, stderr)
				}
				// What the seam is for: the run went through to the posting
				// path, and what it posted arrived where a test can see it.
				if posted != 1 {
					t.Errorf("%d comments reached the server, want the one the run posted", posted)
				}
				return
			}
			if code == 0 {
				t.Fatal("`ccx pr comment` = 0, want a refusal")
			}
			if !strings.Contains(stderr, tt.wantErr) {
				t.Errorf("stderr = %q, want it to mention %q", stderr, tt.wantErr)
			}
			if posted != 0 {
				t.Errorf("%d comments reached the server, want none from a refused run", posted)
			}
		})
	}
}

// TestPRReplyThreadsChecksTheLiveHead is the same five states for the other
// command. Its accepted case posts nothing: the check keeps its place ahead of
// the threads file, so a run with nothing to say still makes it.
func TestPRReplyThreadsChecksTheLiveHead(t *testing.T) {
	for _, tt := range headCheckCases() {
		t.Run(tt.name, func(t *testing.T) {
			code, stderr, posted := runHeadCheckCase(t, tt, func(contextFile, workDir string) []string {
				threadsFile := filepath.Join(workDir, "threads.json")
				gittest.Write(t, threadsFile, `{"threads":[]}`+"\n")
				return []string{"pr", "reply-threads", contextFile, threadsFile}
			})

			if tt.wantErr == "" {
				if code != 0 {
					t.Fatalf("`ccx pr reply-threads` = %d, want 0: %s", code, stderr)
				}
				return
			}
			if code == 0 {
				t.Fatal("`ccx pr reply-threads` = 0, want a refusal")
			}
			if !strings.Contains(stderr, tt.wantErr) {
				t.Errorf("stderr = %q, want it to mention %q", stderr, tt.wantErr)
			}
			if posted != 0 {
				t.Errorf("%d comments reached the server, want none from a refused run", posted)
			}
		})
	}
}
