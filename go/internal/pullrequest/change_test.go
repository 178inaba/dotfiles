package pullrequest_test

import (
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// changeRepo builds the fixture the change is read from: an origin holding
// only main and refs/pull/7/head, an author's clone the history was made in,
// and a reader's clone that has never seen any of it.
//
// The reader is cloned from a bare repository that has no branch for the head,
// which is a pull request from a fork — and, more to the point here, is what
// makes the fetch load bearing. Building the history in the reader's own clone
// would leave the objects there whatever the fetch did, and every assertion
// below would pass with the fetch removed.
type changeRepo struct {
	author string
	reader string
	head   string
	base   string
}

func changeFixture(t *testing.T, build func(author string)) changeRepo {
	t.Helper()
	gittest.SkipWithoutGit(t)

	dir := t.TempDir()
	bare := gittest.Init(t, filepath.Join(dir, "origin.git"), "--bare", "-b", "main")
	author := gittest.Clone(t, bare, filepath.Join(dir, "author"))

	gittest.Write(t, filepath.Join(author, "base.txt"), "on main\n")
	gittest.Write(t, filepath.Join(author, "old.txt"), "a\nb\nc\n")
	gittest.Write(t, filepath.Join(author, "bin.dat"), "\x00\x01\x02")
	gittest.Run(t, author, "add", ".")
	gittest.Run(t, author, "commit", "-qm", "init")
	gittest.Run(t, author, "push", "-q", "origin", "main")

	// The reader clones before the head is pushed, so that nothing of the
	// pull request reaches it except through the fetch under test.
	reader := gittest.Clone(t, bare, filepath.Join(dir, "reader"))

	gittest.Run(t, author, "switch", "-qc", "feature/x")
	build(author)
	gittest.Run(t, author, "push", "-q", "origin", "HEAD:refs/pull/7/head")

	return changeRepo{
		author: author, reader: reader,
		head: gittest.Rev(t, author, "HEAD"),
		// The feature branch was cut from main and main has not moved, so
		// main's tip is the merge base ReadChange has to find.
		base: gittest.Rev(t, author, "main"),
	}
}

// history is the fixture pull request: a file added, a file renamed and
// changed, a binary file changed, and a merge commit — with a message that has
// a body under its headline, since the headline alone is not what the document
// carries.
func history(t *testing.T) func(string) {
	t.Helper()

	return func(author string) {
		gittest.Write(t, filepath.Join(author, "added.txt"), "x\n")
		gittest.Run(t, author, "add", ".")
		gittest.Run(t, author, "commit", "-qm", "Add a file\n\nAnd say why, in a paragraph the headline does not carry.")

		gittest.Run(t, author, "mv", "old.txt", "new.txt")
		gittest.Write(t, filepath.Join(author, "new.txt"), "a\nb\nc\nd\n")
		gittest.Run(t, author, "rm", "-q", "base.txt")
		gittest.Run(t, author, "add", ".")
		gittest.Run(t, author, "commit", "-qm", "Rename and extend, and drop the other file")

		gittest.Write(t, filepath.Join(author, "bin.dat"), "\x00\x03\x04\x05")
		gittest.Run(t, author, "add", ".")
		gittest.Run(t, author, "commit", "-qm", "Change the binary")

		gittest.Run(t, author, "switch", "-qc", "side", "HEAD~2")
		gittest.Write(t, filepath.Join(author, "side.txt"), "s\n")
		gittest.Run(t, author, "add", ".")
		gittest.Run(t, author, "commit", "-qm", "Add the side file")

		gittest.Run(t, author, "switch", "-q", "feature/x")
		gittest.Run(t, author, "merge", "-q", "--no-ff", "-m", "Merge the side branch", "side")
	}
}

// noChange is what ReadChange answers with for a pull request that changes
// nothing: empty rather than nil, which is the shape the document publishes.
// Used where a test is about the conversation and not about the change.
func noChange() pullrequest.Change {
	return pullrequest.Change{Commits: []pullrequest.Commit{}, Diff: pullrequest.Diff{Files: []pullrequest.DiffFile{}}}
}

// prFor is the metadata ReadChange takes, already resolved by its caller.
func prFor(r changeRepo) ghapi.PullRequest {
	return ghapi.PullRequest{Number: 7, BaseRefName: "main", HeadRefName: "feature/x", HeadRefOid: r.head}
}

func TestReadChange(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, history(t))
	patch := filepath.Join(t.TempDir(), "diff.patch")

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), patch)
	if err != nil {
		t.Fatalf("ReadChange: %v", err)
	}

	t.Run("the head is present afterwards", func(t *testing.T) {
		// The promise the skills that follow rely on: they read the diff from
		// the document and the surrounding code from the checkout.
		gittest.Run(t, r.reader, "cat-file", "-e", r.head+"^{commit}")
	})

	t.Run("the commits, oldest first", func(t *testing.T) {
		// Compared against git's own answer in the repository that has the
		// whole history, so that this pins the order and the range rather
		// than restating the fixture.
		want := strings.Fields(gittest.Run(t, r.author, "log", "--format=%H", r.base+".."+r.head))
		slices.Reverse(want)
		oids := make([]string, 0, len(got.Commits))
		for _, c := range got.Commits {
			oids = append(oids, c.OID)
		}
		if diff := cmp.Diff(want, oids); diff != "" {
			t.Errorf("commits (-want +got):\n%s", diff)
		}
		// A merge commit is in the range, which is the set GitHub shows on the
		// Commits tab; --first-parent would drop the side branch's own commit.
		if len(want) != 5 {
			t.Errorf("the range holds %d commits, want 5 including the merge", len(want))
		}
		// Found by its headline rather than by position: commits made in the
		// same second tie, and which of them git puts first is not what this
		// is about.
		const whole = "Add a file\n\nAnd say why, in a paragraph the headline does not carry."
		if !slices.ContainsFunc(got.Commits, func(c pullrequest.Commit) bool { return c.Message == whole }) {
			t.Errorf("commits = %+v, want one carrying the whole message %q", got.Commits, whole)
		}
	})

	t.Run("the patch file", func(t *testing.T) {
		content, err := os.ReadFile(patch)
		if err != nil {
			t.Fatalf("read the patch: %v", err)
		}
		if want := gittest.Run(t, r.author, "diff", r.base+"..."+r.head); string(content) != want {
			t.Errorf("the patch differs from git diff %s...%s:\n%s", r.base, r.head, cmp.Diff(want, string(content)))
		}
		if got.Diff.Path != patch {
			t.Errorf("diff.path = %q, want %q", got.Diff.Path, patch)
		}
	})

	t.Run("the files and the totals", func(t *testing.T) {
		old := "old.txt"
		want := []pullrequest.DiffFile{
			{Path: "added.txt", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0)},
			{Path: "base.txt", Status: pullrequest.StatusDeleted, Additions: new(0), Deletions: new(1)},
			// A binary file is counted in neither the file's own numbers nor
			// the totals: git counts no lines in one.
			{Path: "bin.dat", Status: pullrequest.StatusModified},
			{Path: "new.txt", PreviousPath: &old, Status: pullrequest.StatusRenamed, Additions: new(1), Deletions: new(0)},
			{Path: "side.txt", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0)},
		}
		if diff := cmp.Diff(want, got.Diff.Files); diff != "" {
			t.Errorf("diff.files (-want +got):\n%s", diff)
		}
		if got.Diff.Additions != 3 || got.Diff.Deletions != 1 {
			t.Errorf("diff totals = +%d -%d, want +3 -1", got.Diff.Additions, got.Diff.Deletions)
		}
	})
}

// TestReadChangeReportsCopiesAndTypechanges covers the two statuses the main
// fixture cannot produce.
//
// A copy is only found where its source was itself touched in the range, and
// only when copy detection is asked for — which is why the command asks for it
// rather than leaving the two statuses the contract publishes unreachable. A
// typechange has no name of its own among the five, and is a modification.
func TestReadChangeReportsCopiesAndTypechanges(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, func(author string) {
		// A copy of a file that is modified in the same range, which is what
		// git matches one against.
		gittest.Write(t, filepath.Join(author, "copy.txt"), "a\nb\nc\n")
		gittest.Write(t, filepath.Join(author, "old.txt"), "a\nb\nc\nd\n")
		// A regular file replaced by a symlink: same path, different kind.
		link := filepath.Join(author, "base.txt")
		if err := os.Remove(link); err != nil {
			t.Fatalf("remove %s: %v", link, err)
		}
		if err := os.Symlink("old.txt", link); err != nil {
			t.Fatalf("symlink %s: %v", link, err)
		}
		gittest.Run(t, author, "add", "-A")
		gittest.Run(t, author, "commit", "-qm", "Copy a file and turn another into a link")
	})

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), filepath.Join(t.TempDir(), "diff.patch"))
	if err != nil {
		t.Fatalf("ReadChange: %v", err)
	}

	source := "old.txt"
	want := []pullrequest.DiffFile{
		{Path: "base.txt", Status: pullrequest.StatusModified, Additions: new(1), Deletions: new(1)},
		{Path: "copy.txt", PreviousPath: &source, Status: pullrequest.StatusCopied, Additions: new(0), Deletions: new(0)},
		{Path: "old.txt", Status: pullrequest.StatusModified, Additions: new(1), Deletions: new(0)},
	}
	if diff := cmp.Diff(want, got.Diff.Files); diff != "" {
		t.Errorf("diff.files (-want +got):\n%s", diff)
	}
}

// TestReadChangeWritesALargeDiffWhole is requirement 7 in one test: no line,
// file or byte limit is applied, so a diff far past any of the context's own
// limits arrives entire.
func TestReadChangeWritesALargeDiffWhole(t *testing.T) {
	t.Parallel()

	const lines = 5000
	r := changeFixture(t, func(author string) {
		var b strings.Builder
		for i := range lines {
			fmt.Fprintf(&b, "line %d\n", i)
		}
		gittest.Write(t, filepath.Join(author, "big.txt"), b.String())
		gittest.Run(t, author, "add", ".")
		gittest.Run(t, author, "commit", "-qm", "Add a large file")
	})
	patch := filepath.Join(t.TempDir(), "diff.patch")

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), patch)
	if err != nil {
		t.Fatalf("ReadChange: %v", err)
	}

	if got.Diff.Additions != lines {
		t.Errorf("diff.additions = %d, want %d", got.Diff.Additions, lines)
	}
	content, err := os.ReadFile(patch)
	if err != nil {
		t.Fatalf("read the patch: %v", err)
	}
	if n := strings.Count(string(content), "\n+line "); n != lines {
		t.Errorf("the patch holds %d added lines, want %d", n, lines)
	}
}

// TestReadChangeRefusesAMovedHead is the alternative to a document whose
// head_oid and diff disagree, which nothing downstream could detect.
func TestReadChangeRefusesAMovedHead(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, history(t))
	pr := prFor(r)
	pr.HeadRefOid = "0000000000000000000000000000000000000001"
	patch := filepath.Join(t.TempDir(), "diff.patch")

	_, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, pr, patch)
	if err == nil {
		t.Fatal("ReadChange succeeded, want it to refuse a head that is not there")
	}
	if !strings.Contains(err.Error(), "run this again") {
		t.Errorf("ReadChange error = %v, want it to say to run the command again", err)
	}
	if _, err := os.Stat(patch); err == nil {
		t.Error("a patch file was written for a head that could not be resolved")
	}
}
