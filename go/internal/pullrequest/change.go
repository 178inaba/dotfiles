package pullrequest

import (
	"context"
	"fmt"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Commit is one commit of the pull request.
type Commit struct {
	OID string `json:"oid"`
	// The whole message, headline and paragraphs together. Not the
	// headline alone: what a change was for is written under it, and that is
	// what a run citing the intent has to read.
	Message string `json:"message"`
}

// FileStatus is what became of one file of the diff, as git reports it.
type FileStatus string

const (
	// StatusAdded is a file the pull request creates.
	StatusAdded FileStatus = "added"
	// StatusModified is a file whose content changed. A file whose type
	// changed — a regular file replaced by a symlink, say — is reported as
	// this: it is a modification, and there is no third answer to give.
	StatusModified FileStatus = "modified"
	// StatusDeleted is a file the pull request removes.
	StatusDeleted FileStatus = "deleted"
	// StatusRenamed is a file that moved, and is the reason previous_path
	// exists: the patch shows the two paths, and a reader matching the file
	// list against it needs both.
	StatusRenamed FileStatus = "renamed"
	// StatusCopied is a file git matched against another one it left in place.
	StatusCopied FileStatus = "copied"
)

// DiffFile is one file the pull request changes.
type DiffFile struct {
	// The path on the new side; for a deletion, the path that was
	// removed.
	Path string `json:"path"`
	// Null except for a rename or a copy, where it is the path
	// the file came from.
	PreviousPath *string    `json:"previous_path"`
	Status       FileStatus `json:"status"`
	// additions and deletions are null for a binary file, in
	// which git counts no lines.
	Additions *int `json:"additions"`
	Deletions *int `json:"deletions"`
}

// Diff is the whole diff of the pull request.
//
// The patch goes to a file rather than into the document: it is unbounded, and
// a reader takes it with a tool that reads a path.
type Diff struct {
	// The absolute path of the file holding the patch. It sits
	// directly in the work dir, is named with the rest of what goes there, and
	// is overwritten on every run — a caller composing the name is how two
	// runs on two pull requests come to write over each other.
	Path  string     `json:"path"`
	Files []DiffFile `json:"files"`
	// additions and deletions are the lines across the text
	// files. A binary file counts towards neither, which is why they can be
	// less than the patch appears to hold.
	Additions int `json:"additions"`
	Deletions int `json:"deletions"`
}

// Change is what a pull request changes: the commits that made it and the diff
// they add up to.
//
// Read apart from the conversation because the two fail differently and are
// retried differently, and because a caller that fetches the conversation
// twice with the limits raised still reads this once.
//
// ReadChange is where one comes from: both lists are empty rather than nil on
// every path out of it, which is what the document publishes.
type Change struct {
	Commits []Commit
	Diff    Diff
}

// ReadChange reads a pull request's commits and diff out of git.
//
// The objects are made present by fetching refs/pull/<n>/head, which the base
// repository carries for every pull request — including one from a fork, whose
// head branch it does not. The diff is then taken from head_oid itself rather
// than from what the fetch brought back, so that a pull request which moved
// between the metadata being read and the fetch is refused rather than
// described wrongly: a document whose head_oid and diff disagree is something
// no reader could detect.
//
// git runs against dir, which is the checkout the command was invoked in.
func ReadChange(ctx context.Context, r runner.Runner, dir string, pr ghapi.PullRequest, diffPath string) (Change, error) {
	// Absolute before it reaches git: -C moves git's own working directory, so
	// a relative --output would land under dir rather than beside the
	// document. It is also what diff.path promises its reader.
	patch, err := filepath.Abs(diffPath)
	if err != nil {
		return Change{}, fmt.Errorf("failed to resolve the diff path %s: %v", diffPath, err)
	}

	head := fmt.Sprintf("refs/pull/%d/head", pr.Number)
	if _, err := r.Run(ctx, runner.Command{
		Name: "git", Args: []string{"-C", dir, "fetch", "-q", "origin", pr.BaseRefName, head},
	}); err != nil {
		// Not degraded to what is already local: with a stale origin/<base>
		// the merge base moves and the diff quietly widens to commits the base
		// branch already has, which nothing downstream can tell from the real
		// thing.
		return Change{}, fmt.Errorf("git fetch origin %s %s failed in %s: %v", pr.BaseRefName, head, dir, err)
	}
	if _, err := runner.Git(ctx, r, dir, "cat-file", "-e", pr.HeadRefOid+"^{commit}"); err != nil {
		return Change{}, fmt.Errorf(
			"the pull request head %s is not in %s after fetching %s; it moved while the pull request was being read — run this again",
			pr.HeadRefOid, dir, head)
	}
	base, err := runner.Git(ctx, r, dir, "merge-base", "origin/"+pr.BaseRefName, pr.HeadRefOid)
	if err != nil {
		return Change{}, fmt.Errorf("failed to find the merge base of origin/%s and %s: %v", pr.BaseRefName, pr.HeadRefOid, err)
	}

	commits, err := readCommits(ctx, r, dir, base+".."+pr.HeadRefOid)
	if err != nil {
		return Change{}, err
	}
	diff, err := readDiff(ctx, r, dir, base+"..."+pr.HeadRefOid, patch)
	if err != nil {
		return Change{}, err
	}
	return Change{Commits: commits, Diff: diff}, nil
}

// readCommits lists the range oldest first.
//
// Every commit in it, merge commits included: that is the set GitHub shows on
// the Commits tab, and --first-parent would drop the work done on a branch
// that was merged in.
func readCommits(ctx context.Context, r runner.Runner, dir, span string) ([]Commit, error) {
	// -z rather than a chosen separator: a message may hold any line the
	// author wrote, and the one byte it cannot hold is the one git separates
	// records with here.
	out, err := r.Run(ctx, runner.Command{
		Name: "git", Args: []string{"-C", dir, "log", "--reverse", "-z", "--format=%H%n%B", span},
	})
	if err != nil {
		return nil, fmt.Errorf("git log %s failed in %s: %v", span, dir, err)
	}

	commits := []Commit{}
	for _, record := range split(string(out)) {
		oid, message, ok := strings.Cut(record, "\n")
		if !ok {
			return nil, fmt.Errorf("unexpected commit record %q from git log %s", record, span)
		}
		// Only the newline git puts after the message is dropped; the blank
		// line between a headline and its body is part of what was written.
		commits = append(commits, Commit{OID: oid, Message: strings.TrimSuffix(message, "\n")})
	}
	return commits, nil
}

// readDiff writes the patch and reads the statistics over the same range.
//
// The statistics come from git rather than from GitHub's file list so that a
// reader can check one against the other; three invocations rather than one
// because --output keeps the patch out of the captured standard output, which
// matters once it is unbounded.
//
// The flags are all pinned rather than left to the configuration, because each
// of them is something a local setting could otherwise change about what the
// contract publishes: -M and -C ask for the rename and copy detection the two
// statuses of that name depend on, --no-relative keeps a run started in a
// subdirectory from silently reporting only that subdirectory, and
// --no-ext-diff --no-color shut out a configured external differ and a colour
// setting, either of which would corrupt the patch file itself.
func readDiff(ctx context.Context, r runner.Runner, dir, span, patch string) (Diff, error) {
	git := func(args ...string) (string, error) {
		full := append([]string{
			"-C", dir, "diff", "-M", "-C", "--no-relative", "--no-ext-diff", "--no-color",
		}, args...)
		out, err := r.Run(ctx, runner.Command{Name: "git", Args: full})
		if err != nil {
			return "", fmt.Errorf("git diff %s failed in %s: %v", span, dir, err)
		}
		return string(out), nil
	}

	if _, err := git("--output="+patch, span); err != nil {
		return Diff{}, err
	}
	numstat, err := git("--numstat", "-z", span)
	if err != nil {
		return Diff{}, err
	}
	nameStatus, err := git("--name-status", "-z", span)
	if err != nil {
		return Diff{}, err
	}

	counts, err := parseNumstat(numstat)
	if err != nil {
		return Diff{}, err
	}
	files, err := parseNameStatus(nameStatus)
	if err != nil {
		return Diff{}, err
	}

	diff := Diff{Path: patch}
	for i := range files {
		c, ok := counts[files[i].Path]
		if !ok {
			return Diff{}, fmt.Errorf("git counted no lines for %s over %s", files[i].Path, span)
		}
		files[i].Additions, files[i].Deletions = c.additions, c.deletions
		if c.additions != nil {
			diff.Additions += *c.additions
			diff.Deletions += *c.deletions
		}
	}
	diff.Files = files
	return diff, nil
}

// lineCount is one file's pair of counts, both absent for a binary file.
type lineCount struct{ additions, deletions *int }

// parseNumstat reads the counts, by the path on the new side.
//
// A record is "<added>\t<deleted>\t<path>", except for a rename or a copy,
// where the path field is empty and the old and new paths follow as the next
// two fields.
func parseNumstat(out string) (map[string]lineCount, error) {
	counts := make(map[string]lineCount)
	fields := &nulFields{all: split(out)}
	for fields.more() {
		record := strings.SplitN(fields.next(), "\t", 3)
		if len(record) != 3 {
			return nil, fmt.Errorf("unexpected numstat record %q", strings.Join(record, "\t"))
		}
		path := record[2]
		if path == "" {
			if _, ok := fields.take(); !ok {
				return nil, fmt.Errorf("numstat record for a rename names no old path")
			}
			var ok bool
			if path, ok = fields.take(); !ok {
				return nil, fmt.Errorf("numstat record for a rename names no new path")
			}
		}
		c, err := lineCounts(record[0], record[1])
		if err != nil {
			return nil, err
		}
		counts[path] = c
	}
	return counts, nil
}

// lineCounts reads one pair. A pair of dashes is git saying it counted no
// lines in a binary file, which is not the same as counting none.
func lineCounts(added, deleted string) (lineCount, error) {
	if added == "-" && deleted == "-" {
		return lineCount{}, nil
	}
	a, err := strconv.Atoi(added)
	if err != nil {
		return lineCount{}, fmt.Errorf("unexpected numstat addition count %q", added)
	}
	d, err := strconv.Atoi(deleted)
	if err != nil {
		return lineCount{}, fmt.Errorf("unexpected numstat deletion count %q", deleted)
	}
	return lineCount{additions: &a, deletions: &d}, nil
}

// parseNameStatus reads the files and what became of them, in git's order.
//
// Each entry is a status field followed by a path, and a rename or a copy is
// followed by two: the old path and then the new one.
func parseNameStatus(out string) ([]DiffFile, error) {
	files := []DiffFile{}
	fields := &nulFields{all: split(out)}
	for fields.more() {
		code := fields.next()
		status, err := fileStatus(code)
		if err != nil {
			return nil, err
		}
		path, ok := fields.take()
		if !ok {
			return nil, fmt.Errorf("name-status entry %q names no path", code)
		}
		file := DiffFile{Path: path, Status: status}
		if status == StatusRenamed || status == StatusCopied {
			previous := file.Path
			if file.Path, ok = fields.take(); !ok {
				return nil, fmt.Errorf("name-status entry %q names only one path", code)
			}
			file.PreviousPath = &previous
		}
		files = append(files, file)
	}
	return files, nil
}

// fileStatus reads git's letter. A rename or a copy carries a similarity score
// after it, which says how alike the two files are and not what happened.
func fileStatus(field string) (FileStatus, error) {
	if field == "" {
		return "", fmt.Errorf("empty name-status entry")
	}
	switch field[0] {
	case 'A':
		return StatusAdded, nil
	case 'M', 'T':
		return StatusModified, nil
	case 'D':
		return StatusDeleted, nil
	case 'R':
		return StatusRenamed, nil
	case 'C':
		return StatusCopied, nil
	}
	return "", fmt.Errorf("unexpected name-status entry %q", field)
}

// nulFields walks the fields of one -z output.
//
// A cursor rather than an index, because both formats above are records of
// however many fields the record before said they had: reading them by hand
// puts the arithmetic, and four spellings of "ran out", in each parser.
type nulFields struct {
	all []string
	at  int
}

func (f *nulFields) more() bool { return f.at < len(f.all) }

// next is the field a record starts on, which more has just said is there.
func (f *nulFields) next() string {
	field := f.all[f.at]
	f.at++
	return field
}

// take is one further field of the record in hand, false where it is missing.
func (f *nulFields) take() (string, bool) {
	if !f.more() {
		return "", false
	}
	return f.next(), true
}

// split cuts git's -z output, which terminates every field rather than
// separating them — so the last one is followed by a NUL and not by nothing.
func split(out string) []string {
	trimmed := strings.TrimSuffix(out, "\x00")
	if trimmed == "" {
		return nil
	}
	return strings.Split(trimmed, "\x00")
}
