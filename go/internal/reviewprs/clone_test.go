package reviewprs_test

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/reviewprs"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// gitFake stands in for git, so that a test can say what a clone does — finish,
// fail, or fail after somebody else has published — without a server to clone
// from.
type gitFake struct {
	// clone runs in place of `git clone <url> <dir>`.
	clone func(dir string) error
	// fetchErr is what `git -C <dir> fetch --prune` returns.
	fetchErr error

	calls [][]string
}

func (g *gitFake) Run(_ context.Context, c runner.Command) ([]byte, error) {
	g.calls = append(g.calls, c.Args)
	if len(c.Args) > 0 && c.Args[0] == "clone" {
		if g.clone == nil {
			return nil, nil
		}
		return nil, g.clone(c.Args[2])
	}
	return nil, g.fetchErr
}

func (g *gitFake) ran(verb string) bool {
	return slices.ContainsFunc(g.calls, func(args []string) bool {
		return slices.Contains(args, verb)
	})
}

// cloned makes dir look like a finished clone.
func cloned(dir string) error { return os.MkdirAll(filepath.Join(dir, ".git"), 0o755) }

// options points a clone at a throwaway workspace and a gh configuration that
// asks for ssh.
func options(t *testing.T, protocol string) reviewprs.CloneOptions {
	t.Helper()

	config := t.TempDir()
	if protocol != "" {
		if err := os.WriteFile(filepath.Join(config, "hosts.yml"),
			[]byte("github.com:\n    git_protocol: "+protocol+"\n"), 0o644); err != nil {
			t.Fatalf("write hosts.yml: %v", err)
		}
	}
	return reviewprs.CloneOptions{DataHome: t.TempDir(), ConfigDir: config, Host: "github.com"}
}

var acmeFoo = reviewprs.OwnerRepo{Owner: "acme", Name: "foo"}

func wantPath(o reviewprs.CloneOptions) string {
	return filepath.Join(o.DataHome, "claude-review-prs", "acme", "foo")
}

// residue reports the hidden clone-in-progress directories left in dir, which
// must be none: a crash can leave one, but a return must not.
func residue(t *testing.T, dir string) []string {
	t.Helper()

	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir(%q): %v", dir, err)
	}
	var left []string
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), "."+acmeFoo.Name+".") {
			left = append(left, e.Name())
		}
	}
	return left
}

func TestEnsureCloneFresh(t *testing.T) {
	t.Parallel()

	o := options(t, "ssh")
	git := &gitFake{clone: cloned}

	got, err := reviewprs.EnsureClone(t.Context(), git, o, acmeFoo)
	if err != nil {
		t.Fatalf("EnsureClone: %v", err)
	}

	if want := wantPath(o); got.Path != want {
		t.Errorf("EnsureClone path = %q, want %q", got.Path, want)
	}
	if _, err := os.Stat(filepath.Join(got.Path, ".git")); err != nil {
		t.Errorf("the published clone has no .git: %v", err)
	}
	// Cloned into a hidden directory beside the destination and moved in one
	// step, so a concurrent caller never sees a half-finished clone.
	if left := residue(t, filepath.Dir(got.Path)); left != nil {
		t.Errorf("temporary directories left behind: %v", left)
	}
	if len(git.calls) != 1 || git.calls[0][0] != "clone" || git.calls[0][1] != "git@github.com:acme/foo.git" {
		t.Errorf("git was run as %v, want one ssh clone", git.calls)
	}
}

func TestEnsureCloneProtocol(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		protocol string
		want     string
	}{
		{name: "ssh", protocol: "ssh", want: "git@github.com:acme/foo.git"},
		{name: "https", protocol: "https", want: "https://github.com/acme/foo.git"},
		// gh's own default, which is what an installation that has never been
		// configured clones with.
		{name: "unconfigured", want: "https://github.com/acme/foo.git"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			git := &gitFake{clone: cloned}
			if _, err := reviewprs.EnsureClone(t.Context(), git, options(t, tc.protocol), acmeFoo); err != nil {
				t.Fatalf("EnsureClone: %v", err)
			}
			if got := git.calls[0][1]; got != tc.want {
				t.Errorf("cloned from %q, want %q", got, tc.want)
			}
		})
	}
}

// TestEnsureCloneProtocolPrefersTheHost pins the precedence: gh's per-host
// setting wins over the global one, which is the shape of this machine's own
// configuration.
func TestEnsureCloneProtocolPrefersTheHost(t *testing.T) {
	t.Parallel()

	o := options(t, "ssh")
	if err := os.WriteFile(filepath.Join(o.ConfigDir, "config.yml"), []byte("git_protocol: https\n"), 0o644); err != nil {
		t.Fatalf("write config.yml: %v", err)
	}

	git := &gitFake{clone: cloned}
	if _, err := reviewprs.EnsureClone(t.Context(), git, o, acmeFoo); err != nil {
		t.Fatalf("EnsureClone: %v", err)
	}
	if got, want := git.calls[0][1], "git@github.com:acme/foo.git"; got != want {
		t.Errorf("cloned from %q, want %q", got, want)
	}
}

func TestEnsureCloneExisting(t *testing.T) {
	t.Parallel()

	o := options(t, "ssh")
	if _, err := reviewprs.EnsureClone(t.Context(), &gitFake{clone: cloned}, o, acmeFoo); err != nil {
		t.Fatalf("EnsureClone: %v", err)
	}

	git := &gitFake{}
	got, err := reviewprs.EnsureClone(t.Context(), git, o, acmeFoo)
	if err != nil {
		t.Fatalf("EnsureClone: %v", err)
	}

	if want := wantPath(o); got.Path != want {
		t.Errorf("EnsureClone path = %q, want %q", got.Path, want)
	}
	if git.ran("clone") {
		t.Errorf("git was run as %v, want no second clone", git.calls)
	}
	want := [][]string{{"-C", got.Path, "fetch", "--prune"}}
	if diff := cmp.Diff(want, git.calls); diff != "" {
		t.Errorf("commands run (-want +got):\n%s", diff)
	}
}

func TestEnsureCloneFetchFailure(t *testing.T) {
	t.Parallel()

	o := options(t, "ssh")
	if _, err := reviewprs.EnsureClone(t.Context(), &gitFake{clone: cloned}, o, acmeFoo); err != nil {
		t.Fatalf("EnsureClone: %v", err)
	}

	if got, err := reviewprs.EnsureClone(t.Context(), &gitFake{fetchErr: errors.New("offline")}, o, acmeFoo); err == nil {
		t.Fatalf("EnsureClone = %+v, want a failure", got)
	}
}

func TestEnsureCloneFailure(t *testing.T) {
	t.Parallel()

	o := options(t, "ssh")
	git := &gitFake{clone: func(string) error { return errors.New("no such repository") }}

	if got, err := reviewprs.EnsureClone(t.Context(), git, o, acmeFoo); err == nil {
		t.Fatalf("EnsureClone = %+v, want a failure", got)
	}
	path := wantPath(o)
	if _, err := os.Stat(path); !os.IsNotExist(err) {
		t.Errorf("a failed clone left %q behind", path)
	}
	if left := residue(t, filepath.Dir(path)); left != nil {
		t.Errorf("temporary directories left behind: %v", left)
	}
}

// TestEnsureCloneAdoptsAWinner covers both halves of the race two subagents
// reviewing one repository can lose: somebody else publishing while this clone
// runs, and somebody else publishing before it can be moved into place. Either
// way the published clone has to survive untouched — it is what the winner is
// already reviewing in.
func TestEnsureCloneAdoptsAWinner(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// ownCloneFails says whether this caller's own clone failed before it
		// noticed the winner.
		ownCloneFails bool
	}{
		{name: "the winner publishes while this clone fails", ownCloneFails: true},
		{name: "the winner publishes before this clone is moved in"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			o := options(t, "ssh")
			path := wantPath(o)
			marker := filepath.Join(path, "winner-marker")
			git := &gitFake{clone: func(dir string) error {
				// The winner finishes mid-clone.
				if err := cloned(path); err != nil {
					return err
				}
				if err := os.WriteFile(marker, nil, 0o644); err != nil {
					return err
				}
				if tc.ownCloneFails {
					return errors.New("clone failed")
				}
				return cloned(dir)
			}}

			got, err := reviewprs.EnsureClone(t.Context(), git, o, acmeFoo)
			if err != nil {
				t.Fatalf("EnsureClone: %v", err)
			}
			if got.Path != path {
				t.Errorf("EnsureClone path = %q, want %q", got.Path, path)
			}
			if _, err := os.Stat(marker); err != nil {
				t.Errorf("the winner's clone was destroyed: %v", err)
			}
			if left := residue(t, path); left != nil {
				t.Errorf("temporary directories left inside the published clone: %v", left)
			}
			if left := residue(t, filepath.Dir(path)); left != nil {
				t.Errorf("temporary directories left behind: %v", left)
			}
		})
	}
}

// TestEnsureCloneRemovesDebris covers what the previous implementation, which
// cloned straight into the destination, left behind when it was interrupted.
func TestEnsureCloneRemovesDebris(t *testing.T) {
	t.Parallel()

	o := options(t, "ssh")
	path := wantPath(o)
	if err := os.MkdirAll(filepath.Join(path, "partial-stuff"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	got, err := reviewprs.EnsureClone(t.Context(), &gitFake{clone: cloned}, o, acmeFoo)
	if err != nil {
		t.Fatalf("EnsureClone: %v", err)
	}
	if _, err := os.Stat(filepath.Join(got.Path, ".git")); err != nil {
		t.Errorf("the debris was not replaced by a clone: %v", err)
	}
	if _, err := os.Stat(filepath.Join(got.Path, "partial-stuff")); !os.IsNotExist(err) {
		t.Errorf("the debris survived the fresh clone")
	}
}

func TestParseOwnerRepo(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		in      string
		want    reviewprs.OwnerRepo
		wantErr bool
	}{
		{name: "owner and name", in: "acme/foo", want: reviewprs.OwnerRepo{Owner: "acme", Name: "foo"}},
		// Dots are ordinary in a repository name; only a whole component of
		// them is not.
		{name: "a name containing dots", in: "acme/foo.bar.baz", want: reviewprs.OwnerRepo{Owner: "acme", Name: "foo.bar.baz"}},
		{name: "empty", in: "", wantErr: true},
		{name: "no slash", in: "no-slash", wantErr: true},
		{name: "too many slashes", in: "too/many/slashes", wantErr: true},
		{name: "no owner", in: "/missing-owner", wantErr: true},
		{name: "no name", in: "missing-repo/", wantErr: true},
		// Each of these would put the clone, and the removal that cleans up
		// after a failed one, outside the review workspace.
		{name: "a parent owner", in: "../evil", wantErr: true},
		{name: "a parent name", in: "evil/..", wantErr: true},
		{name: "a dot owner", in: "./evil", wantErr: true},
		{name: "a dot name", in: "evil/.", wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := reviewprs.ParseOwnerRepo(tc.in)
			if tc.wantErr {
				if err == nil {
					t.Fatalf("ParseOwnerRepo(%q) = %v, want an error", tc.in, got)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseOwnerRepo(%q): %v", tc.in, err)
			}
			if got != tc.want {
				t.Errorf("ParseOwnerRepo(%q) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}
