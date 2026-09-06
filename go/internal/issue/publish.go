package issue

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// Writing a set of issues that name each other is a sequence, and a sequence
// that is followed by hand skips a step: an issue went out with a placeholder
// still in its body because one substitution was never made, and nothing
// noticed for a day. So the sequence is here, the skill declares what it wants
// rather than how to get it, and a run that stops halfway can be re-run
// without writing anything twice.

// PublishManifest is the document `ccx issue publish` reads.
type PublishManifest struct {
	// The repository every issue in the run belongs to, as owner/name. There
	// is no flag for it: the manifest is what a re-run is repeated from, and a
	// repository given beside it could differ between the two.
	Repo *string `json:"repo" contract:"required,nonempty"`
	// One row per issue the run writes, in the order they are created. A
	// parent's subs are ordered by the parent's composition section, which
	// this does not read.
	Issues []PublishManifestIssue `json:"issues" contract:"required"`
	// The dependencies to register, once every issue exists.
	BlockedBy []PublishManifestBlockedBy `json:"blocked_by"`
}

// PublishManifestIssue is one issue the run writes.
type PublishManifestIssue struct {
	// An existing issue's number, or a placeholder name in A-Z and
	// underscores for one the run creates. A body refers to a placeholder as
	// #{NAME}, and the run replaces it with the number it received.
	Key *string `json:"key" contract:"required,nonempty"`
	// The name of the file holding the body, in the manifest's own directory.
	// A bare name rather than a path, so that a manifest cannot reach out of
	// the directory it was written in.
	Draft *string `json:"draft" contract:"required,nonempty,barefilename"`
	// The title to give the issue.
	Title *string `json:"title" contract:"required,nonempty"`
	// The labels the issue ends up with — all of them, not the ones being
	// added: an edit replaces the whole set, so a row that adds one names the
	// existing ones too.
	Labels []string `json:"labels" contract:"required"`
	// The issue this one is a sub of, by number or by placeholder. Absent for
	// an issue with no parent. A number needs no row of its own.
	Parent *string `json:"parent"`
	// The language the body is written in, for the section check.
	Locale *string `json:"locale" contract:"required,nonempty"`
	// What the issue is in the parent/child scheme, for the section check.
	Kind *string `json:"kind" contract:"required,nonempty"`
	// For a row keyed by an existing number: when that issue last changed, as
	// the draft was being written against it. The run refuses to overwrite an
	// issue that has moved since. Absent for a placeholder, which names
	// nothing yet.
	UpdatedAt *string `json:"updated_at"`
	// The name of a file holding a comment to post after the edit, telling
	// whoever has already read the issue what changed. Absent where there is
	// nobody to tell.
	Comment *string `json:"comment" contract:"nonempty,barefilename"`
}

// PublishManifestBlockedBy is one dependency to register: the issue that
// waits, and the issue it waits for. Either may be a placeholder.
type PublishManifestBlockedBy struct {
	Blocked *string `json:"blocked" contract:"required,nonempty"`
	By      *string `json:"by" contract:"required,nonempty"`
}

var (
	// placeholderKey and numberKey are the two spellings a key may take.
	placeholderKey = regexp.MustCompile(`^[A-Z_]+$`)
	numberKey      = regexp.MustCompile(`^[0-9]+$`)
)

// publishSet is a manifest with its files read and its values parsed.
type publishSet struct {
	repo   ghapi.Repo
	rows   []publishRow
	blocks []publishBlock
	// byKey is rows indexed by their key, which is how every reference in the
	// manifest — a parent, a dependency, a placeholder in a body — is
	// followed. A row is absent for a key that names an issue this run does
	// not write, which is the one thing every follower has to handle.
	byKey map[string]publishRow
	// file is the manifest itself: named in refusals, and where the record of
	// what has been written lives.
	file string
}

// publishRow is one row of the manifest, resolved.
type publishRow struct {
	key string
	// number is the issue the key names, or 0 for a placeholder.
	number int
	draft  string
	body   string
	title  string
	labels []string
	// parent is a key, empty where the row names no parent. It need not be a
	// key any row carries: an existing issue can be a parent without being
	// written to.
	parent      string
	locale      Locale
	kind        Kind
	mapping     []Mapping
	updatedAt   string
	commentFile string
	comment     string
}

// target reports whether the row names an issue that already exists, which is
// the half of the manifest that is edited rather than created.
func (r publishRow) target() bool { return r.number != 0 }

// publishBlock is one dependency, by key.
type publishBlock struct{ blocked, by string }

// parsePublishManifest resolves a decoded manifest and reads the files it
// names.
//
// The decoding itself is the caller's, through contract.Unmarshal, because
// internal/contract imports this package for the two types that serialise
// themselves and so cannot be imported by it. The document still goes through
// the same contract the help renders, so the two cannot drift; what it costs
// is that the other document-taking commands decode inside their own domain
// package and this one does not.
//
// Only what can be decided from the document itself is decided here; the
// checks that need the whole set, or GitHub, are the plan's.
//
// The drafts are read from the manifest's own directory rather than one given
// alongside it: the two would have to agree, and nothing would make them.
func parsePublishManifest(wire PublishManifest, file string) (publishSet, error) {
	dir := filepath.Dir(file)
	set := publishSet{file: file, byKey: map[string]publishRow{}}
	var bad violations
	repo, err := ghapi.ParseRepo(*wire.Repo)
	if err != nil {
		bad.add("repo: %v", err)
	}
	set.repo = repo

	for i, e := range wire.Issues {
		row, err := parsePublishRow(e, dir)
		if err != nil {
			bad.add("issues[%d]: %v", i, err)
			continue
		}
		if _, dup := set.byKey[row.key]; dup {
			bad.add("key %s appears more than once", row.key)
		}
		set.rows = append(set.rows, row)
		set.byKey[row.key] = row
	}
	for _, d := range wire.BlockedBy {
		set.blocks = append(set.blocks, publishBlock{blocked: *d.Blocked, by: *d.By})
	}

	if err := bad.err(file); err != nil {
		return publishSet{}, err
	}
	return set, nil
}

func parsePublishRow(e PublishManifestIssue, dir string) (publishRow, error) {
	row := publishRow{
		key: *e.Key, draft: *e.Draft, title: *e.Title, labels: e.Labels,
		locale: Locale(*e.Locale), kind: Kind(*e.Kind),
	}
	switch {
	case numberKey.MatchString(row.key):
		n, err := strconv.Atoi(row.key)
		if err != nil || n == 0 {
			return publishRow{}, fmt.Errorf("key is not an issue number: %s", row.key)
		}
		row.number = n
	case placeholderKey.MatchString(row.key):
	default:
		return publishRow{}, fmt.Errorf(
			"key %q is neither an issue number nor a placeholder name in A-Z and underscores", row.key)
	}

	if err := validLocale(row.locale); err != nil {
		return publishRow{}, err
	}
	if err := validKind(row.kind); err != nil {
		return publishRow{}, err
	}
	if e.Parent != nil {
		row.parent = *e.Parent
	}
	if e.UpdatedAt != nil {
		row.updatedAt = *e.UpdatedAt
	}

	body, err := os.ReadFile(filepath.Join(dir, row.draft))
	if err != nil {
		return publishRow{}, fmt.Errorf("draft not found beside the manifest: %s", row.draft)
	}
	row.body = string(body)

	// Beside the draft and named after it, so that the mapping is found again
	// without being declared, and a draft carries its own.
	if m, err := os.ReadFile(filepath.Join(dir, row.draft+".mapping")); err == nil {
		row.mapping, err = ParseMapping(string(m))
		if err != nil {
			return publishRow{}, fmt.Errorf("%s.mapping: %v", row.draft, err)
		}
	}

	if e.Comment != nil {
		row.commentFile = *e.Comment
		c, err := os.ReadFile(filepath.Join(dir, row.commentFile))
		if err != nil {
			return publishRow{}, fmt.Errorf("comment not found beside the manifest: %s", row.commentFile)
		}
		row.comment = string(c)
	}
	return row, nil
}

// violations collects what is wrong with a run so that a refusal names all of
// it. Stopping at the first would make fixing a manifest a sequence of runs,
// and every one of those runs reads the live issues again.
type violations struct{ found []string }

func (v *violations) add(format string, args ...any) {
	v.found = append(v.found, fmt.Sprintf(format, args...))
}

func (v *violations) err(file string) error {
	if len(v.found) == 0 {
		return nil
	}
	msg := fmt.Sprintf("%s cannot be published as it stands (nothing was written):", file)
	for _, f := range v.found {
		msg += "\n  " + f
	}
	return fmt.Errorf("%s", msg)
}
