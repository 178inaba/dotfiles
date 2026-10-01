package pullrequest_test

import (
	"context"
	"crypto/sha256"
	"errors"
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

// wantHistory is what the fixture above must answer, in git's order.
//
// Shared by the two readings of that range, as wantGenerated is by the two of
// the other one: a file added to history has to be added to one expectation,
// not to two that then drift.
func wantHistory() []pullrequest.DiffFile {
	old := "old.txt"
	return []pullrequest.DiffFile{
		{Path: "added.txt", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0)},
		{Path: "base.txt", Status: pullrequest.StatusDeleted, Additions: new(0), Deletions: new(1)},
		// A binary file is counted in neither the file's own numbers nor the
		// totals: git counts no lines in one.
		{Path: "bin.dat", Status: pullrequest.StatusModified},
		{Path: "new.txt", PreviousPath: &old, Status: pullrequest.StatusRenamed, Additions: new(1), Deletions: new(0)},
		{Path: "side.txt", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0)},
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

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), r.base, patch)
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
		if want := gittest.Run(t, r.author, "diff", r.base, r.head); string(content) != want {
			t.Errorf("the patch differs from git diff %s %s:\n%s", r.base, r.head, cmp.Diff(want, string(content)))
		}
		if got.Diff.Path != patch {
			t.Errorf("diff.path = %q, want %q", got.Diff.Path, patch)
		}
		if got.Diff.MergeBaseOID != r.base {
			t.Errorf("diff.merge_base_oid = %q, want %q", got.Diff.MergeBaseOID, r.base)
		}
		if want := fmt.Sprintf("%x", sha256.Sum256(content)); got.Diff.SHA256 != want {
			t.Errorf("diff.sha256 = %q, want %q, the digest of the patch file", got.Diff.SHA256, want)
		}
		// The patch is written beside its final name and renamed onto it, and
		// nothing of that is left behind for a reader of the work dir.
		entries, err := os.ReadDir(filepath.Dir(patch))
		if err != nil {
			t.Fatalf("read the work dir: %v", err)
		}
		if len(entries) != 1 || entries[0].Name() != filepath.Base(patch) {
			names := make([]string, 0, len(entries))
			for _, e := range entries {
				names = append(names, e.Name())
			}
			t.Errorf("work dir holds %v, want only %s", names, filepath.Base(patch))
		}
	})

	t.Run("the files and the totals", func(t *testing.T) {
		if diff := cmp.Diff(wantHistory(), got.Diff.Files); diff != "" {
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

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), r.base, filepath.Join(t.TempDir(), "diff.patch"))
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

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), r.base, patch)
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

	_, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, pr, r.base, patch)
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

// TestReadChangeTakesTheMergeBaseItIsGiven is the reason the merge base comes
// from GitHub: under a criss-cross history git has two answers and picks one
// without a word, and the pull request's own range may be the other.
func TestReadChangeTakesTheMergeBaseItIsGiven(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, crissCross(t))
	other := notGitsPick(t, r.author, "main", r.head)
	patch := filepath.Join(t.TempDir(), "diff.patch")

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), other, patch)
	if err != nil {
		t.Fatalf("ReadChange: %v", err)
	}

	if got.Diff.MergeBaseOID != other {
		t.Errorf("diff.merge_base_oid = %q, want %q", got.Diff.MergeBaseOID, other)
	}
	content, err := os.ReadFile(patch)
	if err != nil {
		t.Fatalf("read the patch: %v", err)
	}
	if want := gittest.Run(t, r.author, "diff", other, r.head); string(content) != want {
		t.Errorf("the patch differs from git diff %s %s:\n%s", other, r.head, cmp.Diff(want, string(content)))
	}
	want := strings.Fields(gittest.Run(t, r.author, "log", "--format=%H", other+".."+r.head))
	slices.Reverse(want)
	oids := make([]string, 0, len(got.Commits))
	for _, c := range got.Commits {
		oids = append(oids, c.OID)
	}
	if diff := cmp.Diff(want, oids); diff != "" {
		t.Errorf("commits (-want +got):\n%s", diff)
	}
}

// TestReadChangeRefusesAnAbsentMergeBase is a merge base GitHub named that the
// fetch did not bring, which leaves the range unreadable rather than up to git.
func TestReadChangeRefusesAnAbsentMergeBase(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, history(t))
	patch := filepath.Join(t.TempDir(), "diff.patch")

	_, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), "0000000000000000000000000000000000000001", patch)
	if err == nil {
		t.Fatal("ReadChange succeeded, want it to refuse a merge base that is not there")
	}
	if !strings.Contains(err.Error(), "run this again") {
		t.Errorf("ReadChange error = %v, want it to say to run the command again", err)
	}
	if _, err := os.Stat(patch); err == nil {
		t.Error("a patch file was written for a merge base that could not be resolved")
	}
}

// generatedHistory is the fixture for the generated flag: a head that adds a
// .gitattributes the base branch has never had, marking files in the three
// spellings the attribute takes, one path that no longer exists at head, and
// one pattern anchored to a directory.
//
// The anchored pattern is the only one that can tell where git measured the
// paths from. An unanchored pattern matches at any level below the
// .gitattributes it is written in, so a path handed to check-attr with a
// directory prefixed onto it would still match *.lock — and a run asking about
// the wrong directory would answer correctly by accident.
func generatedHistory(t *testing.T) func(string) {
	t.Helper()

	return func(author string) {
		gittest.Write(t, filepath.Join(author, ".gitattributes"), strings.Join([]string{
			"*.lock linguist-generated",
			"*.pb.go linguist-generated=true",
			"hand.txt linguist-generated=false",
			"old.txt linguist-generated",
			"gen/anchored.txt linguist-generated",
			// A value nobody anticipated, on a binary file: neither is a case
			// the flag has an answer of its own for, and saying so is what
			// keeps it to one meaning.
			"*.dat linguist-generated=maybe",
			"",
		}, "\n"))
		gittest.Write(t, filepath.Join(author, "bin.dat"), "\x00\x01\x02\x03")
		gittest.Write(t, filepath.Join(author, "deps.lock"), "locked\n")
		gittest.Write(t, filepath.Join(author, "api.pb.go"), "package api\n")
		gittest.Write(t, filepath.Join(author, "hand.txt"), "written by hand\n")
		gittest.Write(t, filepath.Join(author, "plain.txt"), "nothing marks this\n")
		gittest.Write(t, filepath.Join(author, "gen", "anchored.txt"), "under gen\n")
		// Deleted at head while the head still marks it, which is what asks
		// whether the removed path was the one looked up.
		gittest.Run(t, author, "rm", "-q", "old.txt")
		gittest.Run(t, author, "add", "-A")
		gittest.Run(t, author, "commit", "-qm", "Add generated files and drop one that was marked")
	}
}

// wantGenerated is what the fixture above must answer, in git's order.
func wantGenerated() []pullrequest.DiffFile {
	return []pullrequest.DiffFile{
		{Path: ".gitattributes", Status: pullrequest.StatusAdded, Additions: new(6), Deletions: new(0)},
		{Path: "api.pb.go", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0), Generated: true},
		// A binary file, whose line counts are null and whose flag is not.
		{Path: "bin.dat", Status: pullrequest.StatusModified, Generated: true},
		{Path: "deps.lock", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0), Generated: true},
		{Path: "gen/anchored.txt", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0), Generated: true},
		{Path: "hand.txt", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0)},
		{Path: "old.txt", Status: pullrequest.StatusDeleted, Additions: new(0), Deletions: new(3), Generated: true},
		{Path: "plain.txt", Status: pullrequest.StatusAdded, Additions: new(1), Deletions: new(0)},
	}
}

// TestReadChangeFlagsGeneratedFilesAtTheHead reads the attribute out of the
// pull request's head rather than out of the checkout, which here has never
// seen a .gitattributes at all.
func TestReadChangeFlagsGeneratedFilesAtTheHead(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, generatedHistory(t))
	patch := filepath.Join(t.TempDir(), "diff.patch")

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, r.reader, prFor(r), r.base, patch)
	if err != nil {
		t.Fatalf("ReadChange: %v", err)
	}

	if diff := cmp.Diff(wantGenerated(), got.Diff.Files); diff != "" {
		t.Errorf("diff.files (-want +got):\n%s", diff)
	}
	// The flag is the only addition: the patch still holds every hunk, the
	// generated files included, and the totals count their lines.
	if got.Diff.Additions != 11 || got.Diff.Deletions != 3 {
		t.Errorf("diff totals = +%d -%d, want +11 -3", got.Diff.Additions, got.Diff.Deletions)
	}
	content, err := os.ReadFile(patch)
	if err != nil {
		t.Fatalf("read the patch: %v", err)
	}
	if want := gittest.Run(t, r.author, "diff", r.base+"..."+r.head); string(content) != want {
		t.Errorf("the patch differs from git diff %s...%s:\n%s", r.base, r.head, cmp.Diff(want, string(content)))
	}
}

// TestReadChangeAsksAboutRepositoryRelativePaths pins where the attribute
// lookup measures a path from.
//
// The file list is relative to the repository, whatever directory the command
// was started in — readDiff pins --no-relative for that. check-attr, on the
// other hand, resolves what it reads against git's own working directory, so a
// lookup made where the command stands would prefix that directory onto every
// path and quietly find nothing.
func TestReadChangeAsksAboutRepositoryRelativePaths(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, generatedHistory(t))
	sub := filepath.Join(r.reader, "somewhere", "below")
	if err := os.MkdirAll(sub, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	got, err := pullrequest.ReadChange(t.Context(), runner.Exec{}, sub, prFor(r), r.base, filepath.Join(t.TempDir(), "diff.patch"))
	if err != nil {
		t.Fatalf("ReadChange: %v", err)
	}
	if diff := cmp.Diff(wantGenerated(), got.Diff.Files); diff != "" {
		t.Errorf("diff.files from a subdirectory (-want +got):\n%s", diff)
	}
}

// TestReadChangeRefusesAGitWithoutCheckAttrSource covers the one git version
// that cannot answer the question at all.
//
// Through the runner rather than through a git of that age: what the command
// has to do with the refusal is name the version, and an old git is not
// something a test can install.
func TestReadChangeRefusesAGitWithoutCheckAttrSource(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, generatedHistory(t))
	old := runnerFunc(func(ctx context.Context, c runner.Command) ([]byte, error) {
		if !slices.Contains(c.Args, "check-attr") {
			return runner.Exec{}.Run(ctx, c)
		}
		// The phrase matched below is one git puts through gettext, so the
		// refusal only reaches a reader on a machine told to answer in
		// English. A stub cannot reproduce a locale; what it can pin is that
		// the call asks for one.
		if !slices.Contains(c.Env, "LC_ALL=C") {
			t.Errorf("check-attr ran with env %q, want it to ask git for untranslated messages", c.Env)
		}
		return nil, &runner.Error{
			Name:   "git",
			Err:    errors.New("exit status 129"),
			Stderr: []byte("error: unknown option `source'\nusage: git check-attr [-a | --all | <attr>...] [--] <pathname>...\n"),
		}
	})

	_, err := pullrequest.ReadChange(t.Context(), old, r.reader, prFor(r), r.base, filepath.Join(t.TempDir(), "diff.patch"))
	if err == nil {
		t.Fatal("ReadChange succeeded, want it to refuse a git that cannot read the head's attributes")
	}
	if !strings.Contains(err.Error(), "2.40.0") {
		t.Errorf("ReadChange error = %v, want it to name git 2.40.0", err)
	}
}

// TestReadLocalChange reads the range out of the checkout it is given rather
// than out of a pull request, which is what the two states with a change the
// document cannot carry are reviewed from.
//
// Run against the author's own clone, since that is where the commits are: a
// branch with no pull request has pushed nothing, and an author ahead of the
// pull request has pushed only part.
func TestReadLocalChange(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, history(t))
	patch := filepath.Join(t.TempDir(), "local.patch")

	got, warning, err := pullrequest.ReadLocalChange(t.Context(), runner.Exec{}, r.author, "origin/main", "", patch)
	if err != nil {
		t.Fatalf("ReadLocalChange: %v", err)
	}
	if warning != "" {
		t.Errorf("warning = %q, want none for a single merge base", warning)
	}
	if got.Diff.MergeBaseOID != r.base {
		t.Errorf("diff.merge_base_oid = %q, want %q", got.Diff.MergeBaseOID, r.base)
	}

	// The same range the pull request's own reading covers.
	want := strings.Fields(gittest.Run(t, r.author, "log", "--format=%H", "origin/main..HEAD"))
	slices.Reverse(want)
	oids := make([]string, 0, len(got.Commits))
	for _, c := range got.Commits {
		oids = append(oids, c.OID)
	}
	if diff := cmp.Diff(want, oids); diff != "" {
		t.Errorf("commits (-want +got):\n%s", diff)
	}

	content, err := os.ReadFile(patch)
	if err != nil {
		t.Fatalf("read the patch: %v", err)
	}
	if wantPatch := gittest.Run(t, r.author, "diff", r.base, "HEAD"); string(content) != wantPatch {
		t.Errorf("the patch differs from the same range read by hand:\n%s", cmp.Diff(wantPatch, string(content)))
	}
	if got.Diff.Path != patch {
		t.Errorf("diff.path = %q, want %q", got.Diff.Path, patch)
	}

	if diff := cmp.Diff(wantHistory(), got.Diff.Files); diff != "" {
		t.Errorf("diff.files (-want +got):\n%s", diff)
	}
}

// TestReadLocalChangeFlagsGeneratedFiles is the reason the range is read here
// rather than by whoever asks for it: the local diff carries the same flag the
// document's does, so the one exclusion a reader of the whole diff is given
// applies to both alike.
//
// The attribute is read at HEAD, which for a local range is the commit whose
// change is being described — the same rule the document's reading follows.
func TestReadLocalChangeFlagsGeneratedFiles(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, generatedHistory(t))

	got, _, err := pullrequest.ReadLocalChange(
		t.Context(), runner.Exec{}, r.author, "origin/main", "", filepath.Join(t.TempDir(), "local.patch"))
	if err != nil {
		t.Fatalf("ReadLocalChange: %v", err)
	}
	if diff := cmp.Diff(wantGenerated(), got.Diff.Files); diff != "" {
		t.Errorf("diff.files (-want +got):\n%s", diff)
	}
}

// crissCross is a history whose base and head have two merge bases: each side
// commits once, each merges the other's commit, and both move on. Neither of
// the two is a better answer than the other, so git picks one without a word.
//
// main is pushed as well, so that the reader's fetch of the base brings it.
func crissCross(t *testing.T) func(string) {
	t.Helper()

	return func(author string) {
		commit := func(file, message string) {
			gittest.Write(t, filepath.Join(author, file), message+"\n")
			gittest.Run(t, author, "add", ".")
			gittest.Run(t, author, "commit", "-qm", message)
		}
		gittest.Run(t, author, "switch", "-q", "main")
		commit("main.txt", "On main")
		onMain := gittest.Rev(t, author, "HEAD")
		gittest.Run(t, author, "switch", "-q", "feature/x")
		commit("feature.txt", "On the feature")
		onFeature := gittest.Rev(t, author, "HEAD")

		gittest.Run(t, author, "switch", "-q", "main")
		gittest.Run(t, author, "merge", "-q", "--no-ff", "-m", "Merge the feature into main", onFeature)
		commit("main-later.txt", "Later on main")
		gittest.Run(t, author, "push", "-q", "origin", "main")

		gittest.Run(t, author, "switch", "-q", "feature/x")
		gittest.Run(t, author, "merge", "-q", "--no-ff", "-m", "Merge main into the feature", onMain)
		commit("feature-later.txt", "Later on the feature")
	}
}

// mergeBases is every merge base git finds for the two, in git's own order —
// which git does not document, so a test pins whatever it prints rather than
// sorting it.
func mergeBases(t *testing.T, dir, a, b string) []string {
	t.Helper()

	bases := strings.Fields(gittest.Run(t, dir, "merge-base", "--all", a, b))
	if len(bases) != 2 {
		t.Fatalf("the fixture has %d merge bases, want 2: %v", len(bases), bases)
	}
	return bases
}

// notGitsPick is the one of the two merge bases git does not settle on by
// itself, which is what a test has to hand in for a range git chose to show.
func notGitsPick(t *testing.T, dir, a, b string) string {
	t.Helper()

	bases := mergeBases(t, dir, a, b)
	if bases[0] == strings.TrimSpace(gittest.Run(t, dir, "merge-base", a, b)) {
		return bases[1]
	}
	return bases[0]
}

// TestReadLocalChangeWithSeveralMergeBases is the one place a local change
// has to choose a merge base of its own, there being no pull request whose
// range GitHub already defined — or one whose answer is among the candidates,
// in which case that is the range the document has too.
func TestReadLocalChangeWithSeveralMergeBases(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		// pick chooses the pull request's merge base out of git's
		// candidates, empty for a branch with no pull request.
		pick func(bases []string) string
		// want is the candidate the range is taken from, by its place in
		// git's list.
		want        int
		wantWarning bool
	}{
		{
			name:        "no pull request: git's first, said out loud",
			pick:        func([]string) string { return "" },
			want:        0,
			wantWarning: true,
		},
		{
			// The second rather than the first, so that a reading which
			// ignored the pull request's answer would be caught.
			name: "the pull request's merge base among them",
			pick: func(bases []string) string { return bases[1] },
			want: 1,
		},
		{
			name:        "the pull request's merge base not among them",
			pick:        func([]string) string { return "0000000000000000000000000000000000000001" },
			want:        0,
			wantWarning: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			r := changeFixture(t, crissCross(t))
			bases := mergeBases(t, r.author, "main", "HEAD")
			patch := filepath.Join(t.TempDir(), "local.patch")

			got, warning, err := pullrequest.ReadLocalChange(t.Context(), runner.Exec{}, r.author, "main", tc.pick(bases), patch)
			if err != nil {
				t.Fatalf("ReadLocalChange: %v", err)
			}

			want := bases[tc.want]
			if got.Diff.MergeBaseOID != want {
				t.Errorf("diff.merge_base_oid = %q, want %q", got.Diff.MergeBaseOID, want)
			}
			content, err := os.ReadFile(patch)
			if err != nil {
				t.Fatalf("read the patch: %v", err)
			}
			if wantPatch := gittest.Run(t, r.author, "diff", want, "HEAD"); string(content) != wantPatch {
				t.Errorf("the patch differs from git diff %s HEAD:\n%s", want, cmp.Diff(wantPatch, string(content)))
			}

			if !tc.wantWarning {
				if warning != "" {
					t.Errorf("warning = %q, want none", warning)
				}
				return
			}
			for _, b := range bases {
				if !strings.Contains(warning, b) {
					t.Errorf("warning = %q, want it to name the candidate %s", warning, b)
				}
			}
			if !strings.Contains(warning, "taken from "+want) {
				t.Errorf("warning = %q, want it to name %s as the one used", warning, want)
			}
		})
	}
}

// TestReadLocalChangeWithoutAMergeBase is a base that shares no history with
// the checkout, which leaves nothing to take a range from.
func TestReadLocalChangeWithoutAMergeBase(t *testing.T) {
	t.Parallel()

	r := changeFixture(t, history(t))
	gittest.Run(t, r.author, "switch", "-q", "--orphan", "unrelated")
	gittest.Run(t, r.author, "commit", "-q", "--allow-empty", "-m", "unrelated")
	gittest.Run(t, r.author, "switch", "-q", "feature/x")

	if _, _, err := pullrequest.ReadLocalChange(
		t.Context(), runner.Exec{}, r.author, "unrelated", "", filepath.Join(t.TempDir(), "local.patch")); err == nil {
		t.Fatal("ReadLocalChange succeeded, want it to refuse a base with no merge base")
	}
}

// runnerFunc is a Runner made of one function, so that a test can let every
// call through and stand in for a single one.
type runnerFunc func(context.Context, runner.Command) ([]byte, error)

func (f runnerFunc) Run(ctx context.Context, c runner.Command) ([]byte, error) { return f(ctx, c) }
