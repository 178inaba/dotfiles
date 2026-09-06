package pullrequest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// A pull request gets a directory of its own, and the files a run works with
// have to be inside it.
//
// Parallel subagents share one scratch directory, so a fixed file name in the
// shared root is overwritten by whichever run on whichever pull request wrote
// last. Binding by directory rather than by file name is what covers the
// working files a run makes for itself as well as the ones handed out.

// Stored is where a fetched context and the directory paired with it were
// written.
//
// The document itself never goes to standard output: on a pull request with a
// busy conversation it runs to hundreds of kilobytes, and the caller reads it
// with a tool that takes a path.
type Stored struct {
	// The absolute path of the file holding the context document.
	Path string `json:"path"`
	// work_dir and threads_path are handed out rather than left to the caller
	// to name, which is what binds a run's working files to one pull request.
	WorkDir     string `json:"work_dir"`
	ThreadsPath string `json:"threads_path"`
}

// repoToken is how a repository is spelled inside one of these names.
//
// The owner and the name are separated by an @, which neither may contain:
// with a hyphen, a-b/c and a/b-c would collapse onto one name, and the
// uniqueness these names' whole purpose rests on would have a hole in it. One
// owner for that rule, since both names below carry it.
func repoToken(repo ghapi.Repo) string {
	return repo.Owner + "@" + repo.Name
}

// ContextFileName is what a fetched context is stored as.
//
// Composed here rather than where the file is written, because WorkDir below
// takes the name apart again — one format, one owner.
func ContextFileName(repo ghapi.Repo, number int) string {
	return fmt.Sprintf("pr-context-%s-%d.json", repoToken(repo), number)
}

// ContextPath is where one pull request's context file goes under outDir.
//
// Composed here rather than by whoever writes the file, because the document
// carries the path of the diff file beside it: where it will be written has to
// be known before it is built, and by more than one caller.
func ContextPath(outDir string, repo ghapi.Repo, number int) string {
	return filepath.Join(outDir, ContextFileName(repo, number))
}

// WorkDir is the directory paired with a pull request context file.
//
// The identifier comes from the context file's own name rather than being
// rebuilt from the repository and the number: the name is where that format is
// defined, and rebuilding it in each consumer is how they stop agreeing when it
// changes.
func WorkDir(contextFile string) string {
	token := strings.TrimSuffix(strings.TrimPrefix(filepath.Base(contextFile), "pr-context-"), ".json")
	return filepath.Join(filepath.Dir(contextFile), "pr-"+token)
}

// BranchWorkDir is the directory a run works in where there is no pull request
// to pair one with.
//
// Beside WorkDir rather than composed by the command that needs it, so that
// the work dir has one owner whichever of the two states a run is in. The
// branch stands where the number does, and its slashes are folded onto hyphens
// because a directory name is one path segment: two branches whose names
// differ only in that — feature/x and feature-x — share a directory, which is
// the one hole left in what the directory is for.
func BranchWorkDir(outDir string, repo ghapi.Repo, branch string) string {
	// branch- rather than pr-: a branch may be named after a number, and the
	// shared prefix would put it in that pull request's directory.
	return filepath.Join(outDir, "branch-"+repoToken(repo)+"-"+strings.ReplaceAll(branch, "/", "-"))
}

// WorkFiles is the directory a run works in and the four files handed out with
// it.
type WorkFiles struct {
	Dir         string
	ReviewPath  string
	ThreadsPath string
	// DiffPath is where the patch goes. Named here with the rest rather than
	// by whoever writes it, for the reason above: a name composed at the point
	// of use is one two runs on two pull requests can share.
	DiffPath string
	// LocalDiffPath is where the patch of the change in this checkout goes,
	// which is a second patch rather than the same one: the document's diff is
	// taken at the pull request's head, and the local one runs past it.
	LocalDiffPath string
}

// ensureWorkFiles creates dir and names what goes in it.
//
// The one place the four names are spelled, so that the commands handing a
// directory out cannot come to disagree about where a caller should write.
func ensureWorkFiles(dir string) (WorkFiles, error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return WorkFiles{}, fmt.Errorf("failed to create the work dir: %s", dir)
	}
	return WorkFiles{
		Dir:           dir,
		ReviewPath:    filepath.Join(dir, "review.json"),
		ThreadsPath:   filepath.Join(dir, "threads.json"),
		DiffPath:      filepath.Join(dir, "diff.patch"),
		LocalDiffPath: filepath.Join(dir, "local.patch"),
	}, nil
}

// EnsureWorkFiles creates the directory paired with a context file and names
// the documents inside it.
//
// One implementation for both commands that hand the directory out: fetching a
// context and preparing a review each produce it.
func EnsureWorkFiles(contextFile string) (WorkFiles, error) {
	return ensureWorkFiles(WorkDir(contextFile))
}

// EnsureBranchWorkFiles is the same for a branch with no pull request, whose
// run has no context file to pair a directory with.
func EnsureBranchWorkFiles(outDir string, repo ghapi.Repo, branch string) (WorkFiles, error) {
	return ensureWorkFiles(BranchWorkDir(outDir, repo, branch))
}

// Document is where one pull request's context file and working files go, and
// the change already read into the directory beside it.
type Document struct {
	Path   string
	Work   WorkFiles
	Change Change
}

// OpenDocument settles where a pull request's document goes and reads its
// change into the directory paired with it.
//
// Both writers of the document open it this way, and in this order: a head
// that moved has to stop the run while there is still no document, since one
// whose head_oid and diff disagree is something no reader could detect. dir is
// the checkout git runs against; outDir is where the document goes.
func OpenDocument(ctx context.Context, r runner.Runner, dir, outDir string, repo ghapi.Repo, pr ghapi.PullRequest) (Document, error) {
	path := ContextPath(outDir, repo, pr.Number)
	work, err := EnsureWorkFiles(path)
	if err != nil {
		return Document{}, err
	}
	// Whatever an earlier run left goes now, before the patch beside it is
	// overwritten. A run that stops partway would otherwise leave that run's
	// patch under the previous run's document, which points at it by path and
	// says nothing about which head it was taken at.
	if err := os.Remove(path); err != nil && !os.IsNotExist(err) {
		return Document{}, fmt.Errorf("failed to remove the previous context file: %s", path)
	}
	change, err := ReadChange(ctx, r, dir, pr, work.DiffPath)
	if err != nil {
		return Document{}, err
	}
	return Document{Path: path, Work: work, Change: change}, nil
}

// RequireInWorkDir checks that an input file sits directly in the work dir the
// context file is paired with.
//
// field names the output of the command that fetched the context that would
// have been the right path, because that is what the caller does about it.
func RequireInWorkDir(file, field, contextFile string) error {
	expected := WorkDir(contextFile)
	if info, err := os.Stat(expected); err != nil || !info.IsDir() {
		return fmt.Errorf("work dir not found: %s\nrerun `ccx pr context` or `ccx pr prepare-review` to create it", expected)
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
			"input file must be in the work dir paired with %s: %s\nuse the %s emitted by `ccx pr context` or `ccx pr prepare-review` (files outside it are overwritten by parallel runs on other PRs)",
			contextFile, file, field)
	}
	return nil
}

// Target is the pull request a run is writing to.
type Target struct {
	Repo    string
	Number  int
	BaseRef string
	HeadOID string
	// IsOwnPR is whether the pull request is the current user's, which is what
	// decides whether its body may be edited at all. A field of the target
	// rather than an argument beside it, so that a writer added later cannot
	// be called without it.
	IsOwnPR bool
}

// Target is what writing needs out of a pull request context.
func (c Context) Target() Target {
	return Target{
		Repo: c.Repo, Number: c.PR.Number, BaseRef: c.PR.BaseRef,
		HeadOID: c.PR.HeadOID, IsOwnPR: c.IsOwnPR,
	}
}

// repository is what the target names, in the shape the API writers address it
// by. The context is a document, so owner/name is what it holds rather than
// something already parsed.
func (t Target) repository() (ghapi.Repo, error) {
	repo, err := ghapi.ParseRepo(t.Repo)
	if err != nil {
		return ghapi.Repo{}, fmt.Errorf("the pull request context names %q as its repository: %v", t.Repo, err)
	}
	return repo, nil
}

// RequireOwn checks that the pull request is one whose own text we may edit.
//
// A method rather than a check each caller writes, so that the wording and the
// verdict stay one thing: a command refuses early, before it goes looking for
// a body file, and the writer refuses again for a caller that came another
// way.
func (t Target) RequireOwn() error {
	if t.IsOwnPR {
		return nil
	}
	return fmt.Errorf(
		"%s#%d is not ours, so its body is not edited: leave a code comment instead, or escalate the decision",
		t.Repo, t.Number)
}

// RequireHead checks that the checkout is still the exact state the document
// describes, which is what posting a review is held to.
//
// A review is written against one document and is posted on that document's
// head, and the tie is mechanical: checkAnchors matches every remark against
// the diff it reads out of this very checkout, so the local tree has to be the
// state the remarks were written against or the anchoring check is judging the
// wrong diff. The commands that reply to and comment on a pull request are held
// to something different, since their run pushes between fetching the document
// and posting: see RequirePushedHead.
func RequireHead(ctx context.Context, r runner.Runner, dir, headOID, before string) error {
	local, err := localHead(ctx, r, dir)
	if err != nil {
		return err
	}
	if local != headOID {
		return fmt.Errorf("local HEAD (%s) differs from PR head (%s); rerun the freshness check before %s", local, headOID, before)
	}
	return nil
}

// RequirePushedHead checks that the run's push landed and that the document it
// posts from is behind where the checkout now stands.
//
// This is what replying and commenting need, and it is deliberately not what
// RequireHead asks. Such a run fetches the document, judges, pushes its fixes,
// and only then posts; holding it to the document's own head would mean
// fetching the whole document a second time to satisfy a check about one
// commit. What matters instead is that the local HEAD is the pull request's
// head as GitHub holds it now — so a reply is about code the pull request
// really has — and that the document the replies were written against is on
// the way to it.
//
// The live head is read here rather than taken as an argument so that the two
// callers cannot disagree about what an unreadable one means: it refuses,
// dry runs included, because a head that could not be read is not a head that
// matched, and nothing undoes a published reply.
func RequirePushedHead(ctx context.Context, r runner.Runner, c *ghapi.Client, dir string, target Target, before string) error {
	local, err := localHead(ctx, r, dir)
	if err != nil {
		return err
	}
	repo, err := target.repository()
	if err != nil {
		return err
	}
	pr, err := c.PullRequest(ctx, repo, target.Number)
	if err != nil {
		return fmt.Errorf("failed to read the pull request's current head, so %s is refused: %v", before, err)
	}

	if local != pr.HeadRefOid {
		// Asked in this direction on purpose. The live head is usually not in
		// this repository at all — nothing fetches it between the document and
		// the post — and a commit it cannot resolve answers false, which here
		// means "the checkout does not contain it" and lands on the sync side.
		// Asked the other way round, that same absence would read as "the
		// local HEAD is not behind" and tell a reviewer who cannot push to
		// push.
		if worktree.IsAncestor(ctx, r, dir, pr.HeadRefOid, local) {
			return fmt.Errorf(
				"local HEAD (%s) is ahead of the pull request's head on GitHub (%s); push before %s",
				local, pr.HeadRefOid, before)
		}
		return fmt.Errorf(
			"local HEAD (%s) is not the pull request's head on GitHub (%s); sync the checkout before %s",
			local, pr.HeadRefOid, before)
	}
	if !worktree.IsAncestor(ctx, r, dir, target.HeadOID, local) {
		return fmt.Errorf(
			"the document was fetched at %s, which is not an ancestor of local HEAD (%s) — the branch was rebased or force-pushed; "+
				"sync the checkout and fetch the document again with `ccx pr context` before %s",
			target.HeadOID, local, before)
	}
	return nil
}

// localHead is the commit the checkout stands on.
//
// Two calls rather than one, as the shell version had: reading HEAD fails both
// for a directory that is no repository and for a repository with no commits,
// and answering the second with the first's wording sends the reader somewhere
// there is nothing to find.
func localHead(ctx context.Context, r runner.Runner, dir string) (string, error) {
	if _, err := runner.Git(ctx, r, dir, "rev-parse", "--git-dir"); err != nil {
		return "", fmt.Errorf("not inside a git repository")
	}
	local, err := runner.Git(ctx, r, dir, "rev-parse", "HEAD")
	if err != nil {
		return "", fmt.Errorf("failed to read HEAD in %s: %v", dir, err)
	}
	return local, nil
}

// Checkout is what the freshness check needs out of a pull request context.
//
// It lives here rather than in worktree because the document is this
// package's, and a renamed field should not have to be found in two of them.
// is_own_pr is read as the plain bool it is: false is a meaningful answer and
// its absence is not, and what tells the two apart is the declaration, which
// reads presence from the document rather than from the decoded value.
func (c Context) Checkout() worktree.PullRequest {
	return worktree.PullRequest{
		HeadRef: c.PR.HeadRef,
		HeadOID: c.PR.HeadOID,
		BaseRef: c.PR.BaseRef,
		IsOwnPR: c.IsOwnPR,
	}
}
