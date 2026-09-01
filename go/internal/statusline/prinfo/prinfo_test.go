package prinfo

import (
	"context"
	"encoding/json/v2"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

var now = time.Date(2026, 8, 31, 12, 0, 0, 0, time.UTC)

const (
	key     = "/Users/x/repo:feat"
	repoDir = "/Users/x/repo"
)

// fakeRunner answers by git subcommand and records the whole argv.
//
// Keying on the subcommand rather than on the first argument is what runner.Git
// forces: it puts `-C <dir>` in front of everything, so the first argument is
// the same for every call.
type fakeRunner struct {
	out   map[string]string
	fail  map[string]bool
	calls [][]string
}

func (f *fakeRunner) Run(_ context.Context, c runner.Command) ([]byte, error) {
	f.calls = append(f.calls, append([]string{c.Name}, c.Args...))

	sub := ""
	for i, a := range c.Args {
		// Skip `-C <dir>`; what follows is the subcommand and then, for git
		// config, the setting being read.
		if a == "-C" {
			continue
		}
		if i > 0 && c.Args[i-1] == "-C" {
			continue
		}
		sub = a
		if sub == "config" && i+2 < len(c.Args) {
			sub = "config " + c.Args[i+2]
		}
		break
	}
	if f.fail[sub] {
		return nil, &runner.Error{Name: c.Name, Err: os.ErrInvalid}
	}
	out, ok := f.out[sub]
	if !ok {
		// git config exits non-zero for a setting that is not there, and the
		// caller has to see that rather than an empty value.
		return nil, &runner.Error{Name: c.Name, Err: os.ErrNotExist}
	}
	return []byte(out), nil
}

// gitOnly fails the test if anything other than git was started, which is the
// property this package exists to keep.
func (f *fakeRunner) gitOnly(t *testing.T) {
	t.Helper()

	for _, c := range f.calls {
		if c[0] != "git" {
			t.Errorf("started %q, want git and nothing else (calls: %v)", c[0], f.calls)
		}
	}
}

// dirOf returns the directory a recorded call was made in, so a test can check
// that the passed directory reached git rather than the process's own.
func dirOf(call []string) string {
	for i, a := range call {
		if a == "-C" && i+1 < len(call) {
			return call[i+1]
		}
	}
	return ""
}

const prNode = `{
	"number": %d,
	"state": %q,
	"url": "https://e/%d",
	"reviewDecision": %q,
	"isDraft": %t,
	"headRepositoryOwner": {"login": %q}
}`

// github serves the two endpoints the badge can reach and counts them.
type github struct {
	rest, graphQL int
	// node is the pull request the branch query answers with, or empty for a
	// branch that has none.
	node string
	// defaultBranch is what the repository lookup reports.
	defaultBranch string
	// vars captures the GraphQL variables, which is how a test checks which
	// head ref was asked about.
	vars map[string]any
}

func (g *github) handler(t *testing.T) http.Handler {
	t.Helper()

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/repos/178inaba/dotfiles":
			g.rest++
			fmt.Fprintf(w, `{"default_branch":%q}`, g.defaultBranch)
		case "/graphql":
			g.graphQL++
			g.vars = graphQLVars(t, r)
			fmt.Fprintf(w, `{"data":{"repository":{"pullRequests":{"nodes":[%s]}}}}`, g.node)
		default:
			t.Errorf("unexpected request to %s", r.URL.Path)
			http.NotFound(w, r)
		}
	})
}

func TestLookup(t *testing.T) {
	t.Parallel()

	open := Info{Number: 123, State: StateNoReviewRequested, URL: "https://e/1"}

	tests := []struct {
		name string
		// seed is the record to write, with the key and time to write it under.
		seed     *Info
		seedKey  string
		at       time.Time
		attempt  time.Time
		wantInfo Info
		wantRef  bool
	}{
		{
			name: "a fresh record is used and no refresh is started",
			seed: &open, seedKey: key, at: now,
			wantInfo: open,
		},
		{
			name:    "no record renders nothing and starts a refresh",
			wantRef: true,
		},
		{
			// The badge stays put while the refresh runs, exactly as the
			// exchange rate does.
			name: "a stale record is still rendered",
			seed: &open, seedKey: key, at: now.Add(-time.Hour),
			wantInfo: open, wantRef: true,
		},
		{
			// "no pull request" is a real answer and is cached like any other,
			// which is what keeps the refresh from running on every redraw
			// offline.
			name: "an empty record is a fresh answer",
			seed: &Info{}, seedKey: key, at: now,
		},
		{
			// Deep directories can share a cache file once the name is cut to
			// length; the key inside decides whether the record is ours.
			name: "a record for another key is discarded",
			seed: &open, seedKey: "/Users/x/other:main", at: now,
			wantRef: true,
		},
		{
			name:    "a recent attempt suppresses the refresh",
			attempt: now.Add(-10 * time.Second),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "pr")
			if tt.seed != nil {
				if err := cache.Write(dir, tt.seedKey, tt.at, *tt.seed); err != nil {
					t.Fatalf("seed: %v", err)
				}
			}
			if !tt.attempt.IsZero() {
				// Recorded the way the foreground records it, since that is the
				// only way in from outside the package.
				cache.ShouldAttempt(dir, tt.attempt, retryInterval)
			}

			info, refresh := Lookup(dir, key, now)

			if diff := cmp.Diff(tt.wantInfo, info); diff != "" {
				t.Errorf("Info mismatch (-want +got):\n%s", diff)
			}
			if refresh != tt.wantRef {
				t.Errorf("refresh = %t, want %t", refresh, tt.wantRef)
			}
		})
	}
}

func TestRefresh(t *testing.T) {
	t.Parallel()

	const remotes = "origin\tgit@github.com:178inaba/dotfiles.git (fetch)\n"
	originHEAD := map[string]string{"symbolic-ref": "origin/main\n", "remote": remotes}

	tests := []struct {
		name   string
		branch string
		out    map[string]string
		fail   map[string]bool
		node   string
		// defaultBranch is what the repository lookup reports; empty leaves the
		// REST endpoint answering with no default branch at all.
		defaultBranch string
		want          Info
		wantREST      int
		wantGraphQL   int
	}{
		{
			name:        "an open pull request awaiting review",
			branch:      "feat",
			out:         originHEAD,
			node:        fmt.Sprintf(prNode, 123, "OPEN", 123, "", false, "178inaba"),
			want:        Info{Number: 123, State: StateNoReviewRequested, URL: "https://e/123"},
			wantGraphQL: 1,
		},
		{
			name:        "a draft outranks its review decision",
			branch:      "feat",
			out:         originHEAD,
			node:        fmt.Sprintf(prNode, 126, "OPEN", 126, "", true, "178inaba"),
			want:        Info{Number: 126, State: StateDraft, URL: "https://e/126"},
			wantGraphQL: 1,
		},
		{
			name:        "an approved pull request",
			branch:      "feat",
			out:         originHEAD,
			node:        fmt.Sprintf(prNode, 124, "OPEN", 124, "APPROVED", false, "178inaba"),
			want:        Info{Number: 124, State: StateApproved, URL: "https://e/124"},
			wantGraphQL: 1,
		},
		{
			name:        "a pull request with changes requested",
			branch:      "feat",
			out:         originHEAD,
			node:        fmt.Sprintf(prNode, 125, "OPEN", 125, "CHANGES_REQUESTED", false, "178inaba"),
			want:        Info{Number: 125, State: StateChangesRequested, URL: "https://e/125"},
			wantGraphQL: 1,
		},
		{
			// A merged pull request is history, not the current work.
			name:        "a merged pull request is not shown",
			branch:      "feat",
			out:         originHEAD,
			node:        fmt.Sprintf(prNode, 127, "MERGED", 127, "APPROVED", false, "178inaba"),
			wantGraphQL: 1,
		},
		{
			name:        "a branch with no pull request",
			branch:      "feat",
			out:         originHEAD,
			wantGraphQL: 1,
		},
		{
			// The default branch is not a branch-specific context, so nothing
			// is asked and no client is even built.
			name:   "the default branch is skipped without asking GitHub",
			branch: "main",
			out:    originHEAD,
		},
		{
			// A repository whose remote was added by hand has no origin/HEAD,
			// and GitHub knows the answer instead.
			name:          "GitHub supplies the default branch when origin/HEAD is missing",
			branch:        "main",
			out:           map[string]string{"remote": remotes},
			fail:          map[string]bool{"symbolic-ref": true},
			defaultBranch: "main",
			wantREST:      1,
		},
		{
			// Neither source knows, so the badge is shown rather than hidden:
			// a badge too many beats a badge missing.
			name:        "an unknown default branch still shows the pull request",
			branch:      "main",
			out:         map[string]string{"remote": remotes},
			fail:        map[string]bool{"symbolic-ref": true},
			node:        fmt.Sprintf(prNode, 133, "OPEN", 133, "", false, "178inaba"),
			want:        Info{Number: 133, State: StateNoReviewRequested, URL: "https://e/133"},
			wantREST:    1,
			wantGraphQL: 1,
		},
		{
			// No remote names a repository, so both lookups would fail; the
			// answer is the same one `gh pr view` gave on that condition.
			name:   "a repository with no usable remote is cached as no pull request",
			branch: "feat",
			out:    map[string]string{"symbolic-ref": "origin/main\n", "remote": "origin\t/srv/git/bare.git (fetch)\n"},
		},
		{
			name:   "a git failure is cached as no pull request",
			branch: "feat",
			fail:   map[string]bool{"symbolic-ref": true, "remote": true},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "pr")
			r := &fakeRunner{out: tt.out, fail: tt.fail}
			gh := &github{node: tt.node, defaultBranch: tt.defaultBranch}
			c := ghapitest.New(t, gh.handler(t))

			built := 0
			newClient := func() (*ghapi.Client, error) {
				built++
				return c, nil
			}

			if err := Refresh(t.Context(), r, newClient, dir, key, tt.branch, repoDir, now); err != nil {
				t.Fatalf("Refresh: %v", err)
			}

			rec, ok := cache.Read[Info](dir, key)
			if !ok {
				t.Fatal("no record written")
			}
			if diff := cmp.Diff(tt.want, rec.Value); diff != "" {
				t.Errorf("Info mismatch (-want +got):\n%s", diff)
			}
			if gh.rest != tt.wantREST {
				t.Errorf("repository lookups = %d, want %d", gh.rest, tt.wantREST)
			}
			if gh.graphQL != tt.wantGraphQL {
				t.Errorf("pull request lookups = %d, want %d", gh.graphQL, tt.wantGraphQL)
			}
			// The client is built only where GitHub is actually reached, which
			// is what keeps the default branch free of go-gh's option
			// resolution and the `gh auth token` it can run.
			if wantBuilt := tt.wantREST + tt.wantGraphQL; (built > 0) != (wantBuilt > 0) {
				t.Errorf("client built %d times, want it built only when GitHub is reached (%d requests)", built, wantBuilt)
			}
			r.gitOnly(t)
			for _, call := range r.calls {
				if got := dirOf(call); got != repoDir {
					t.Errorf("ran %v in %q, want %q", call, got, repoDir)
				}
			}
		})
	}
}

// TestRefreshWithoutAClient covers the machine that go-gh cannot build a client
// for, which is an unauthenticated one. The record still has to be written:
// leaving it out would strand whatever badge is on screen, where today a
// failure clears it.
func TestRefreshWithoutAClient(t *testing.T) {
	t.Parallel()

	dir := filepath.Join(t.TempDir(), "pr")
	r := &fakeRunner{out: map[string]string{
		"symbolic-ref": "origin/main\n",
		"remote":       "origin\tgit@github.com:178inaba/dotfiles.git (fetch)\n",
	}}
	newClient := func() (*ghapi.Client, error) { return nil, os.ErrPermission }

	if err := Refresh(t.Context(), r, newClient, dir, key, "feat", repoDir, now); err != nil {
		t.Fatalf("Refresh: %v", err)
	}

	rec, ok := cache.Read[Info](dir, key)
	if !ok {
		t.Fatal("no record written")
	}
	if diff := cmp.Diff(Info{}, rec.Value); diff != "" {
		t.Errorf("Info mismatch (-want +got):\n%s", diff)
	}
}

// TestRefreshFollowsTheBranchConfig is the fork checkout `gh pr view` resolves
// through branch.<name>.merge and branch.<name>.remote. Without reading both,
// the local branch name finds no pull request and the badge disappears.
func TestRefreshFollowsTheBranchConfig(t *testing.T) {
	t.Parallel()

	const remotes = "origin\tgit@github.com:178inaba/dotfiles.git (fetch)\n"

	tests := []struct {
		name      string
		out       map[string]string
		headOwner string
		wantRef   string
		wantInfo  Info
	}{
		{
			// gh pr checkout on a fork's pull request: the ref lives on the
			// fork under another name, and its head is not this account's.
			name: "a head on a fork is found",
			out: map[string]string{
				"symbolic-ref":              "origin/main\n",
				"remote":                    remotes,
				"config branch.feat.merge":  "refs/heads/their-branch\n",
				"config branch.feat.remote": "git@github.com:someone/dotfiles.git\n",
			},
			headOwner: "someone",
			wantRef:   "their-branch",
			wantInfo:  Info{Number: 140, State: StateNoReviewRequested, URL: "https://e/140"},
		},
		{
			// An ordinary tracking branch: the settings are there, but they
			// name this repository, so the narrowing stays and a fork's branch
			// of the same name cannot answer for it.
			name: "a head on origin keeps the owner narrowing",
			out: map[string]string{
				"symbolic-ref":              "origin/main\n",
				"remote":                    remotes,
				"config branch.feat.merge":  "refs/heads/feat\n",
				"config branch.feat.remote": "origin\n",
				"config remote.origin.url":  "git@github.com:178inaba/dotfiles.git\n",
			},
			headOwner: "someone",
			wantRef:   "feat",
			// The only candidate is a fork's, and it is rejected.
		},
		{
			// No branch config at all, which is the branch that was never
			// pushed. The local name is the head ref.
			name: "no branch config falls back to the local name",
			out: map[string]string{
				"symbolic-ref": "origin/main\n",
				"remote":       remotes,
			},
			headOwner: "178inaba",
			wantRef:   "feat",
			wantInfo:  Info{Number: 140, State: StateNoReviewRequested, URL: "https://e/140"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			dir := filepath.Join(t.TempDir(), "pr")
			r := &fakeRunner{out: tt.out}
			gh := &github{node: fmt.Sprintf(prNode, 140, "OPEN", 140, "", false, tt.headOwner)}
			c := ghapitest.New(t, gh.handler(t))

			err := Refresh(t.Context(), r, func() (*ghapi.Client, error) { return c, nil },
				dir, key, "feat", repoDir, now)
			if err != nil {
				t.Fatalf("Refresh: %v", err)
			}

			rec, _ := cache.Read[Info](dir, key)
			if diff := cmp.Diff(tt.wantInfo, rec.Value); diff != "" {
				t.Errorf("Info mismatch (-want +got):\n%s", diff)
			}
			if got := gh.vars["headRefName"]; got != tt.wantRef {
				t.Errorf("headRefName = %v, want %q", got, tt.wantRef)
			}
			r.gitOnly(t)
		})
	}
}

// TestState pins the mapping the display depends on, including the two states
// that are this package's rather than GitHub's.
func TestState(t *testing.T) {
	t.Parallel()

	tests := []struct {
		isDraft  bool
		decision string
		want     State
	}{
		{isDraft: true, want: StateDraft},
		{isDraft: true, decision: "APPROVED", want: StateDraft},
		{decision: "", want: StateNoReviewRequested},
		{decision: "APPROVED", want: StateApproved},
		{decision: "CHANGES_REQUESTED", want: StateChangesRequested},
		{decision: "REVIEW_REQUIRED", want: StateReviewRequired},
		// GraphQL leaves the set open, so an unknown value reaches the display
		// rather than being flattened into one of the known ones.
		{decision: "SOMETHING_NEW", want: State("SOMETHING_NEW")},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("draft=%t/%s", tt.isDraft, tt.decision), func(t *testing.T) {
			t.Parallel()

			if got := state(tt.isDraft, tt.decision); got != tt.want {
				t.Errorf("state(%t, %q) = %q, want %q", tt.isDraft, tt.decision, got, tt.want)
			}
		})
	}
}

// graphQLVars reads the variables out of a GraphQL request body.
func graphQLVars(t *testing.T, r *http.Request) map[string]any {
	t.Helper()

	var req struct {
		Variables map[string]any `json:"variables"`
	}
	if err := json.UnmarshalRead(r.Body, &req); err != nil {
		t.Errorf("decode the request body: %v", err)
		return nil
	}
	return req.Variables
}
