package pullrequest

import (
	"context"
	"encoding/json/v2"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// A review gets a directory of its own, and the files a review works with have
// to be inside it.
//
// Parallel subagents share one scratch directory, so a fixed file name in the
// shared root is overwritten by whichever review of whichever pull request
// wrote last. Binding by directory rather than by file name is what covers the
// working files a review makes for itself as well as the two this command
// hands out.

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
		return fmt.Errorf("review work dir not found: %s\nrerun prepare-review.sh to create it", expected)
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
			"input file must be in the review work dir paired with %s: %s\nuse the %s emitted by prepare-review.sh (files outside it are overwritten by parallel reviews of other PRs)",
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
	out, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"-C", dir, "rev-parse", "HEAD"}})
	if err != nil {
		return fmt.Errorf("not inside a git repository")
	}
	local := strings.TrimSpace(string(out))
	if local != headOID {
		return fmt.Errorf("local HEAD (%s) differs from PR head (%s); rerun the freshness check before %s", local, headOID, before)
	}
	return nil
}
