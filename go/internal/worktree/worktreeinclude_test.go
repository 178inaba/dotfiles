package worktree

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// origin builds the repository every case clones, with one branch per shape of
// .worktreeinclude a worktree can be created at.
//
// outside is a directory beyond any worktree, which the two escape branches
// point at: the guards are what keeps a gitignored file from being written
// there.
func origin(t *testing.T) (bare, outside string) {
	t.Helper()
	gittest.SkipWithoutGit(t)

	base := t.TempDir()
	bare = filepath.Join(base, "origin.git")
	outside = filepath.Join(base, "outside")
	if err := os.MkdirAll(outside, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	gittest.Init(t, bare, "--bare", "-b", "main")

	seed := gittest.Clone(t, bare, filepath.Join(base, "seed"))
	gittest.Run(t, seed, "commit", "-q", "--allow-empty", "-m", "initial")

	// The ignore rules belong to the repository being copied out of, so they
	// live on main; the list belongs to the commit being checked out, so it
	// lives on the branches below.
	gittest.Write(t, filepath.Join(seed, ".gitignore"), ".env\nconfig/secrets.json\n")
	gittest.Run(t, seed, "add", ".gitignore")
	gittest.Run(t, seed, "commit", "-qm", "add gitignore")
	gittest.Run(t, seed, "push", "-q", "origin", "main")

	// not-ignored.txt matches the list without being gitignored, and must not
	// be copied: the worktree already has whatever the commit tracks.
	branch(t, seed, "include", func() {
		gittest.Write(t, filepath.Join(seed, ".worktreeinclude"), ".env\nconfig/secrets.json\nnot-ignored.txt\n")
		gittest.Run(t, seed, "add", ".worktreeinclude")
	})

	// A directory on the way to the destination is a committed symlink out of
	// the worktree.
	branch(t, seed, "escape", func() {
		gittest.Write(t, filepath.Join(seed, ".worktreeinclude"), "config/secrets.json\n")
		symlink(t, outside, filepath.Join(seed, "config"))
		gittest.Run(t, seed, "add", ".worktreeinclude", "config")
	})

	// The destination itself is a committed symlink out of the worktree. Added
	// with -f because the path is gitignored, which is how somebody would have
	// to commit one.
	branch(t, seed, "leaf-escape", func() {
		gittest.Write(t, filepath.Join(seed, ".worktreeinclude"), ".env\n")
		symlink(t, filepath.Join(outside, "stolen"), filepath.Join(seed, ".env"))
		gittest.Run(t, seed, "add", ".worktreeinclude")
		gittest.Run(t, seed, "add", "-f", ".env")
	})

	// The list itself is a committed symlink.
	branch(t, seed, "wtinc-symlink", func() {
		gittest.Write(t, filepath.Join(seed, "real-include"), ".env\n")
		symlink(t, "real-include", filepath.Join(seed, ".worktreeinclude"))
		gittest.Run(t, seed, "add", "real-include", ".worktreeinclude")
	})

	return bare, outside
}

// branch commits what add does onto a fresh branch off main and pushes it.
func branch(t *testing.T, seed, name string, add func()) {
	t.Helper()

	gittest.Run(t, seed, "switch", "-qc", name, "main")
	add()
	gittest.Run(t, seed, "commit", "-qm", name+" fixture")
	gittest.Run(t, seed, "push", "-q", "origin", name)
	gittest.Run(t, seed, "switch", "-q", "main")
}

func symlink(t *testing.T, target, name string) {
	t.Helper()

	if err := os.Symlink(target, name); err != nil {
		t.Fatalf("Symlink(%q, %q): %v", target, name, err)
	}
}

// checkout clones the origin and adds a worktree at the given branch, returning
// the source root and the worktree.
func checkout(t *testing.T, bare, ref string) (srcRoot, worktreePath string) {
	t.Helper()

	base := t.TempDir()
	srcRoot = gittest.Clone(t, bare, filepath.Join(base, "src"))

	worktreePath = filepath.Join(srcRoot, ".claude", "worktrees", "wt")
	gittest.Run(t, srcRoot, "worktree", "add", "-q", "--detach", worktreePath, "origin/"+ref)
	return srcRoot, worktreePath
}

func TestCopyWorktreeInclude(t *testing.T) {
	t.Parallel()

	bare, outside := origin(t)

	tests := []struct {
		name string
		ref  string
		// setUp puts the gitignored files in place in the source root.
		setUp func(t *testing.T, srcRoot string)
		// want is the files the worktree should hold afterwards, by path and
		// content.
		want         map[string]string
		wantAbsent   []string
		wantWarnings []string
		// wantOutside is a path beyond the worktree that must not exist.
		wantOutside string
	}{
		{
			name: "the gitignored files the list names",
			ref:  "include",
			setUp: func(t *testing.T, src string) {
				gittest.Write(t, filepath.Join(src, ".env"), "SECRET=1\n")
				gittest.Write(t, filepath.Join(src, "config", "secrets.json"), "{}\n")
				gittest.Write(t, filepath.Join(src, "not-ignored.txt"), "plain\n")
			},
			want:       map[string]string{".env": "SECRET=1\n", "config/secrets.json": "{}\n"},
			wantAbsent: []string{"not-ignored.txt"},
		},
		{
			name: "a symlink is not followed out of the source",
			ref:  "include",
			setUp: func(t *testing.T, src string) {
				gittest.Write(t, filepath.Join(src, "real-file"), "real\n")
				symlink(t, "real-file", filepath.Join(src, ".env"))
			},
			wantAbsent:   []string{".env"},
			wantWarnings: []string{"skipped symlink in .worktreeinclude: .env"},
		},
		{
			name: "another worktree's files are not a source",
			ref:  "include",
			setUp: func(t *testing.T, src string) {
				gittest.Write(t, filepath.Join(src, ".env"), "SECRET=1\n")
				gittest.Write(t, filepath.Join(src, ".claude", "worktrees", "other", ".env"), "LEAK=1\n")
			},
			want:       map[string]string{".env": "SECRET=1\n"},
			wantAbsent: []string{".claude/worktrees/other/.env"},
		},
		{
			name: "a directory that leaves the worktree",
			ref:  "escape",
			setUp: func(t *testing.T, src string) {
				gittest.Write(t, filepath.Join(src, "config", "secrets.json"), "SECRET=1\n")
			},
			wantWarnings: []string{"skipped .worktreeinclude entry (destination escapes worktree): config/secrets.json"},
			wantOutside:  "secrets.json",
		},
		{
			// The directory check does not look at the last component, and a
			// destination that is itself a symlink is how a pull request branch
			// would collect the secrets of whoever reviews it.
			name: "a destination that is itself a symlink out of the worktree",
			ref:  "leaf-escape",
			setUp: func(t *testing.T, src string) {
				gittest.Write(t, filepath.Join(src, ".env"), "SECRET=1\n")
			},
			wantWarnings: []string{"skipped .worktreeinclude entry (destination is a committed symlink): .env"},
			wantOutside:  "stolen",
		},
		{
			name: "the commit has no list",
			ref:  "main",
			setUp: func(t *testing.T, src string) {
				gittest.Write(t, filepath.Join(src, ".env"), "SECRET=1\n")
			},
			wantAbsent: []string{".env"},
		},
		{
			name: "the list is a symlink",
			ref:  "wtinc-symlink",
			setUp: func(t *testing.T, src string) {
				gittest.Write(t, filepath.Join(src, ".env"), "SECRET=1\n")
			},
			wantAbsent: []string{".env"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			srcRoot, worktreePath := checkout(t, bare, tc.ref)
			tc.setUp(t, srcRoot)

			copied, warnings, err := copyWorktreeInclude(t.Context(), runner.Exec{}, srcRoot, worktreePath)
			if err != nil {
				t.Fatalf("copyWorktreeInclude: %v", err)
			}

			if copied != len(tc.want) {
				t.Errorf("copied %d files, want %d", copied, len(tc.want))
			}
			for name, want := range tc.want {
				got, err := os.ReadFile(filepath.Join(worktreePath, name))
				if err != nil {
					t.Errorf("ReadFile(%q): %v", name, err)
					continue
				}
				if string(got) != want {
					t.Errorf("%s = %q, want %q", name, got, want)
				}
			}
			for _, name := range tc.wantAbsent {
				if _, err := os.Lstat(filepath.Join(worktreePath, name)); !os.IsNotExist(err) {
					t.Errorf("%s was copied into the worktree, want it left out", name)
				}
			}
			if diff := cmp.Diff(tc.wantWarnings, warnings); diff != "" {
				t.Errorf("warnings (-want +got):\n%s", diff)
			}
			if tc.wantOutside != "" {
				if _, err := os.Lstat(filepath.Join(outside, tc.wantOutside)); !os.IsNotExist(err) {
					t.Errorf("%s was written outside the worktree", tc.wantOutside)
				}
			}
		})
	}
}

// TestCopyWorktreeIncludeKeepsTheMode is what `cp -p` was for: a secret must
// not arrive in the worktree more readable than it was in the repository.
func TestCopyWorktreeIncludeKeepsTheMode(t *testing.T) {
	t.Parallel()

	bare, _ := origin(t)
	srcRoot, worktreePath := checkout(t, bare, "include")
	gittest.Write(t, filepath.Join(srcRoot, ".env"), "SECRET=1\n")
	if err := os.Chmod(filepath.Join(srcRoot, ".env"), 0o600); err != nil {
		t.Fatalf("Chmod: %v", err)
	}

	if _, _, err := copyWorktreeInclude(t.Context(), runner.Exec{}, srcRoot, worktreePath); err != nil {
		t.Fatalf("copyWorktreeInclude: %v", err)
	}

	info, err := os.Stat(filepath.Join(worktreePath, ".env"))
	if err != nil {
		t.Fatalf("Stat: %v", err)
	}
	if got, want := info.Mode().Perm(), os.FileMode(0o600); got != want {
		t.Errorf("mode = %v, want %v", got, want)
	}
}
