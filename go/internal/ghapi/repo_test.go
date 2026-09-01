package ghapi_test

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// fakeRunner answers one command with canned output and records what it was
// asked, so that a test can assert the git invocation as well as the result.
type fakeRunner struct {
	out   string
	fail  bool
	calls [][]string
}

func (f *fakeRunner) Run(_ context.Context, c runner.Command) ([]byte, error) {
	f.calls = append(f.calls, append([]string{c.Name}, c.Args...))
	if f.fail {
		return nil, &runner.Error{Name: c.Name, Err: os.ErrNotExist}
	}
	return []byte(f.out), nil
}

func TestParseRepo(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		in      string
		want    ghapi.Repo
		wantErr bool
	}{
		{name: "owner and name", in: "178inaba/dotfiles", want: ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}},
		{name: "host, owner and name", in: "github.com/178inaba/dotfiles", want: ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}},
		{
			// The form a remote takes here, since ~/.gitconfig rewrites the
			// https URLs GitHub hands out into ssh ones.
			name: "an scp-like ssh remote", in: "git@github.com:178inaba/dotfiles.git",
			want: ghapi.Repo{Owner: "178inaba", Name: "dotfiles"},
		},
		{name: "an https remote", in: "https://github.com/178inaba/dotfiles.git", want: ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}},
		{name: "an ssh url", in: "ssh://git@github.com/178inaba/dotfiles.git", want: ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}},
		{name: "empty", in: "", wantErr: true},
		{name: "a name with no owner", in: "dotfiles", wantErr: true},
		{name: "a trailing separator", in: "178inaba/", wantErr: true},
		{name: "too many parts", in: "github.com/178inaba/dotfiles/extra", wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := ghapi.ParseRepo(tc.in)
			if tc.wantErr {
				if err == nil {
					t.Fatalf("ParseRepo(%q) = %v, want an error", tc.in, got)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseRepo(%q): %v", tc.in, err)
			}
			if got != tc.want {
				t.Errorf("ParseRepo(%q) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}

func TestRepoString(t *testing.T) {
	t.Parallel()

	if got, want := (ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}).String(), "178inaba/dotfiles"; got != want {
		t.Errorf("String() = %q, want %q", got, want)
	}
}

func TestCurrentRepo(t *testing.T) {
	t.Parallel()

	const origin = "origin\tgit@github.com:178inaba/dotfiles.git (fetch)\norigin\tgit@github.com:178inaba/dotfiles.git (push)\n"

	tests := []struct {
		name string
		// remotes is what `git remote -v` prints.
		remotes  string
		wantPath string
	}{
		{name: "the only remote", remotes: origin, wantPath: "/repos/178inaba/dotfiles"},
		{
			// A fork: the repository the work is about is the one it was forked
			// from, which is where its pull requests and issues live.
			name: "upstream over origin",
			remotes: "origin\tgit@github.com:178inaba/dotfiles.git (fetch)\n" +
				"upstream\tgit@github.com:someone/dotfiles.git (fetch)\n",
			wantPath: "/repos/someone/dotfiles",
		},
		{
			name: "github over origin",
			remotes: "github\tgit@github.com:someone/dotfiles.git (fetch)\n" +
				"origin\tgit@github.com:178inaba/dotfiles.git (fetch)\n",
			wantPath: "/repos/someone/dotfiles",
		},
		{
			// No recognised name, so git's own order decides.
			name: "the first of two unrecognised remotes",
			remotes: "alpha\tgit@github.com:someone/dotfiles.git (fetch)\n" +
				"beta\tgit@github.com:other/dotfiles.git (fetch)\n",
			wantPath: "/repos/someone/dotfiles",
		},
		{
			// The push url of a remote pushed elsewhere must not displace the
			// fetch url that identifies it.
			name:     "a remote whose push url differs",
			remotes:  "origin\tgit@github.com:178inaba/dotfiles.git (fetch)\norigin\tgit@github.com:fork/dotfiles.git (push)\n",
			wantPath: "/repos/178inaba/dotfiles",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			var gotPath string
			c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				gotPath = r.URL.Path
				fmt.Fprint(w, `{"name":"dotfiles","owner":{"login":"178inaba"}}`)
			}))

			run := &fakeRunner{out: tc.remotes}
			got, err := c.CurrentRepo(t.Context(), run, "/repo")
			if err != nil {
				t.Fatalf("CurrentRepo: %v", err)
			}

			if want := (ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}); got != want {
				t.Errorf("CurrentRepo = %v, want %v", got, want)
			}
			if gotPath != tc.wantPath {
				t.Errorf("CurrentRepo looked up %q, want %q", gotPath, tc.wantPath)
			}
			wantCalls := [][]string{{"git", "-C", "/repo", "remote", "-v"}}
			if diff := cmp.Diff(wantCalls, run.calls); diff != "" {
				t.Errorf("commands run (-want +got):\n%s", diff)
			}
		})
	}
}

// TestCurrentRepoPrefersTheApiName is why the lookup is not merely a check that
// the repository exists: a remote may spell the repository in another case, or
// name one that has since been renamed, and every later round trip has to use
// the name the API itself answers with.
func TestCurrentRepoPrefersTheApiName(t *testing.T) {
	t.Parallel()

	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		fmt.Fprint(w, `{"name":"dotfiles","owner":{"login":"178inaba"}}`)
	}))

	got, err := c.CurrentRepo(t.Context(), &fakeRunner{out: "origin\tgit@github.com:178INABA/DotFiles.git (fetch)\n"}, "/repo")
	if err != nil {
		t.Fatalf("CurrentRepo: %v", err)
	}
	if want := (ghapi.Repo{Owner: "178inaba", Name: "dotfiles"}); got != want {
		t.Errorf("CurrentRepo = %v, want %v", got, want)
	}
}

func TestCurrentRepoFailures(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		remotes string
		gitFail bool
		status  int
	}{
		{name: "git could not run", gitFail: true},
		{name: "no remotes at all", remotes: ""},
		{name: "a remote that names no repository", remotes: "origin\t/srv/git/bare.git (fetch)\n"},
		{name: "the repository is gone", remotes: "origin\tgit@github.com:178inaba/gone.git (fetch)\n", status: http.StatusNotFound},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				w.WriteHeader(tc.status)
				fmt.Fprint(w, `{"message":"Not Found"}`)
			}))

			got, err := c.CurrentRepo(t.Context(), &fakeRunner{out: tc.remotes, fail: tc.gitFail}, "/repo")
			if err == nil {
				t.Fatalf("CurrentRepo = %v, want an error", got)
			}
		})
	}
}
