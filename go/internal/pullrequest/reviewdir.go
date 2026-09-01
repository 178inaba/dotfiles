package pullrequest

import (
	"context"
	"encoding/json/v2"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// A review gets a directory of its own, and the files a review works with have
// to be inside it.
//
// Parallel subagents share one scratch directory, so a fixed file name in the
// shared root is overwritten by whichever review of whichever pull request
// wrote last. Binding by directory rather than by file name is what covers the
// working files a review makes for itself as well as the two this command
// hands out.

// ContextFileName is what a fetched context is stored as.
//
// The owner and the name are separated by an @, which neither may contain:
// with a hyphen, a-b/c and a/b-c would collapse onto one name, and the
// uniqueness the file's whole purpose rests on would have a hole in it.
// Composed here rather than where the file is written, because WorkDir below
// takes the name apart again — one format, one owner.
func ContextFileName(repo ghapi.Repo, number int) string {
	return fmt.Sprintf("pr-context-%s@%s-%d.json", repo.Owner, repo.Name, number)
}

// WorkDir is the directory paired with a pull request context file.
//
// The identifier comes from the context file's own name rather than being
// rebuilt from the repository and the number: the name is where that format is
// defined, and rebuilding it in each consumer is how they stop agreeing when it
// changes.
func WorkDir(contextFile string) string {
	token := strings.TrimSuffix(strings.TrimPrefix(filepath.Base(contextFile), "pr-context-"), ".json")
	return filepath.Join(filepath.Dir(contextFile), "deep-review-"+token)
}

// RequireInWorkDir checks that an input file sits directly in the work dir the
// context file is paired with.
//
// field names the output of `ccx pr prepare-review` that would have been the
// right path, because that is what the caller does about it.
func RequireInWorkDir(file, field, contextFile string) error {
	expected := WorkDir(contextFile)
	if info, err := os.Stat(expected); err != nil || !info.IsDir() {
		return fmt.Errorf("review work dir not found: %s\nrerun `ccx pr prepare-review` to create it", expected)
	}
	// Both sides resolved, so that the same directory named relatively or
	// through a symlink does not read as a different one.
	want, err := filepath.EvalSymlinks(expected)
	if err != nil {
		return fmt.Errorf("resolve %s: %w", expected, err)
	}
	got, err := filepath.EvalSymlinks(filepath.Dir(file))
	if err != nil {
		return fmt.Errorf("resolve %s: %w", filepath.Dir(file), err)
	}
	if got != want {
		return fmt.Errorf(
			"input file must be in the review work dir paired with %s: %s\nuse the %s emitted by `ccx pr prepare-review` (files outside it are overwritten by parallel reviews of other PRs)",
			contextFile, file, field)
	}
	return nil
}

// Target is the pull request a review is being posted to.
type Target struct {
	Repo    string
	Number  int
	BaseRef string
	HeadOID string
}

// ParseTarget reads the fields of a pull request context that posting needs.
func ParseTarget(b []byte) (Target, error) {
	var wire struct {
		Repo string `json:"repo"`
		PR   struct {
			Number  int    `json:"number"`
			BaseRef string `json:"base_ref"`
			HeadOID string `json:"head_oid"`
		} `json:"pr"`
	}
	if err := json.Unmarshal(b, &wire); err != nil {
		return Target{}, fmt.Errorf("decode the pull request context: %w", err)
	}
	if wire.Repo == "" {
		return Target{}, fmt.Errorf("repo missing")
	}
	if wire.PR.Number == 0 {
		return Target{}, fmt.Errorf("pr.number missing")
	}
	if wire.PR.BaseRef == "" {
		return Target{}, fmt.Errorf("pr.base_ref missing")
	}
	if wire.PR.HeadOID == "" {
		return Target{}, fmt.Errorf("pr.head_oid missing")
	}
	return Target{Repo: wire.Repo, Number: wire.PR.Number, BaseRef: wire.PR.BaseRef, HeadOID: wire.PR.HeadOID}, nil
}

// RequireHead checks that the checkout has not moved since the review was
// prepared.
//
// Posting from a head that has moved is how a comment lands on a line number
// that no longer means what it did, which GitHub rejects outright — and a
// thread resolved against a diff that has since been undone is worse, because
// nothing rejects that.
func RequireHead(ctx context.Context, r runner.Runner, dir, headOID, before string) error {
	local, err := runner.Git(ctx, r, dir, "rev-parse", "HEAD")
	if err != nil {
		return fmt.Errorf("not inside a git repository")
	}
	if local != headOID {
		return fmt.Errorf("local HEAD (%s) differs from PR head (%s); rerun the freshness check before %s", local, headOID, before)
	}
	return nil
}

// ParseCheckout reads the four fields of a context that the freshness check
// depends on.
//
// A subset of what a context holds, deliberately: naming only what is read
// keeps a field the check never looks at from becoming a reason it fails. It
// lives here rather than in worktree because the document is this package's,
// and a renamed field should not have to be found in two of them.
func ParseCheckout(b []byte) (worktree.PullRequest, error) {
	var wire struct {
		PR struct {
			HeadOID string `json:"head_oid"`
			HeadRef string `json:"head_ref"`
			BaseRef string `json:"base_ref"`
		} `json:"pr"`
		// A pointer, because false is a meaningful answer and its absence is
		// not: reading a missing flag as false would treat the author's own
		// unpushed commits as somebody else's history.
		IsOwnPR *bool `json:"is_own_pr"`
	}
	if err := json.Unmarshal(b, &wire); err != nil {
		return worktree.PullRequest{}, fmt.Errorf("decode the pull request context: %w", err)
	}

	for _, field := range []struct{ name, value string }{
		{"pr.head_oid", wire.PR.HeadOID},
		{"pr.head_ref", wire.PR.HeadRef},
		{"pr.base_ref", wire.PR.BaseRef},
	} {
		if field.value == "" {
			return worktree.PullRequest{}, fmt.Errorf("%s missing", field.name)
		}
	}
	if wire.IsOwnPR == nil {
		return worktree.PullRequest{}, fmt.Errorf("is_own_pr missing")
	}
	return worktree.PullRequest{
		HeadRef: wire.PR.HeadRef,
		HeadOID: wire.PR.HeadOID,
		BaseRef: wire.PR.BaseRef,
		IsOwnPR: *wire.IsOwnPR,
	}, nil
}
