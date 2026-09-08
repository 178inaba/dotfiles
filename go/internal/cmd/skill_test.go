package cmd

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/skill"
)

// TestPublishedNamesTheContract keeps the set from quietly emptying, which
// would turn `ccx skill contract` into a check that passes on anything.
func TestPublishedNamesTheContract(t *testing.T) {
	got := published()

	if len(got.Commands) != len(contracts) {
		t.Errorf("published names %d commands, want %d", len(got.Commands), len(contracts))
	}
	// One from each place an identifier comes from.
	for _, want := range []string{"head_oid", "in_use_by_process", "release_manual_steps", "missing_section"} {
		if !slices.Contains(got.Identifiers, want) {
			t.Errorf("published does not name %q", want)
		}
	}
}

// TestSkillContractOnThisRepository is the case a fixture cannot make: what
// this repository's skills actually name holds together. Here rather than
// beside CheckContract because the contract it checks against is assembled
// here.
func TestSkillContractOnThisRepository(t *testing.T) {
	skills := filepath.Join("..", "..", "..", "claude", ".claude", "skills")
	if _, err := os.Stat(skills); err != nil {
		t.Skipf("the repository's skills are not there: %v", err)
	}

	got, err := skill.CheckContract(skills, published())
	if err != nil {
		t.Fatalf("CheckContract: %v", err)
	}
	for _, v := range got.Violations {
		t.Errorf("%s:%d %s %s", v.File, v.Line, v.Type, v.Ref)
	}
}

// TestSkillFrontmatterDefaultsToTheCheckout is what the argument-less form is
// for: the copy being edited. A linked worktree is a checkout like any other,
// and reading the main tree from one is a pass that never looked at the file
// that changed.
func TestSkillFrontmatterDefaultsToTheCheckout(t *testing.T) {
	gittest.SkipWithoutGit(t)

	repo := gittest.Init(t, filepath.Join(t.TempDir(), "repo"))
	gittest.Write(t, filepath.Join(repo, "claude", ".claude", "skills", "demo", "SKILL.md"),
		"---\nname: not-demo\ndescription: a skill whose name does not match its directory\n---\n\nbody\n")
	// From a subdirectory, because that is where a run starts as often as not
	// and the answer must not depend on standing at the top.
	sub := filepath.Join(repo, "claude", ".claude")

	var stdout, stderr bytes.Buffer
	if code := run(t.Context(), []string{"skill", "frontmatter"},
		strings.NewReader(""), &stdout, &stderr, Deps{Dir: sub}); code != 0 {
		t.Fatalf("`ccx skill frontmatter` = %d, want 0: %s", code, stderr.String())
	}

	var got skill.Frontmatter
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("decode: %v\n%s", err, stdout.String())
	}
	// git rev-parse answers with symlinks resolved, which on macOS is what a
	// temporary directory under /var is not.
	want, err := filepath.EvalSymlinks(filepath.Join(repo, "claude", ".claude", "skills"))
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	if got.Target != want {
		t.Errorf("target = %q, want %q", got.Target, want)
	}
	if len(got.Violations) != 1 || got.Violations[0].Type != skill.NameMismatch {
		t.Errorf("violations = %+v, want one %s", got.Violations, skill.NameMismatch)
	}
}

// TestSkillFrontmatterFallsBackToTheStowedRepository holds the other half: run
// from outside any checkout — the home directory is the case the help text
// describes — the stowed copy is still the one meant.
func TestSkillFrontmatterFallsBackToTheStowedRepository(t *testing.T) {
	repo := stowedRepository(t)

	var stdout, stderr bytes.Buffer
	if code := run(t.Context(), []string{"skill", "frontmatter"},
		strings.NewReader(""), &stdout, &stderr, Deps{Dir: t.TempDir()}); code != 0 {
		t.Fatalf("`ccx skill frontmatter` = %d, want 0: %s", code, stderr.String())
	}

	var got skill.Frontmatter
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("decode: %v\n%s", err, stdout.String())
	}
	if want := filepath.Join(repo, "claude", ".claude", "skills"); got.Target != want {
		t.Errorf("target = %q, want %q", got.Target, want)
	}
}

// stowedRepository points HOME at a checkout stowed the way selfbuild reads
// one, and returns that checkout. The layout is selfbuild_test.go's newHarness,
// built here because that one is unexported to its own package.
//
// A fake rather than the machine's own, so that the fallback is checked on a
// machine where this repository is not stowed rather than skipped there.
func stowedRepository(t *testing.T) string {
	t.Helper()

	home := t.TempDir()
	repo := filepath.Join(home, ".dotfiles")
	gittest.Write(t, filepath.Join(repo, "claude", ".claude", "settings.json"), "{}")
	// The sentinel selfbuild checks before believing what the link resolved to.
	gittest.Write(t, filepath.Join(repo, "go", "go.mod"), "module example\n\ngo 1.27\n")
	// One skill, because a skills directory holding no SKILL.md is an error
	// rather than a clean check.
	gittest.Write(t, filepath.Join(repo, "claude", ".claude", "skills", "demo", "SKILL.md"),
		"---\nname: demo\ndescription: a skill the fallback has something to check\n---\n\nbody\n")

	// stow links relatively, which is the case the resolution has to handle.
	if err := os.MkdirAll(filepath.Join(home, ".claude"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.Symlink(filepath.Join("..", ".dotfiles", "claude", ".claude", "settings.json"),
		filepath.Join(home, ".claude", "settings.json")); err != nil {
		t.Fatalf("Symlink: %v", err)
	}

	t.Setenv("HOME", home)
	return repo
}
