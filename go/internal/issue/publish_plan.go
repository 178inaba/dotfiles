package issue

import (
	"context"
	"fmt"
	"slices"
	"strconv"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

// Every check a run makes is made before its first write, so that a manifest
// either publishes or is refused whole. A dry run is the same function with
// the writing left off, which is what makes "the plan shown is the plan
// executed" a fact rather than a promise.

// PublishPlan is what a run would do, in the order it would do it.
type PublishPlan struct {
	// The issues that would be created, in creation order.
	Create []PlannedIssue `json:"create"`
	// The sub-issue links that would be made.
	Link []PlannedLink `json:"link"`
	// The substitutions that would be made in the bodies, once the numbers are
	// known. A number of 0 is one this run would assign.
	Substitute []PlannedSubstitution `json:"substitute"`
	// The issues whose body, title and labels would be written.
	Edit []PlannedIssue `json:"edit"`
	// The issues that would receive an edit-notification comment.
	Comment []int `json:"comment"`
	// The dependencies that would be registered.
	BlockedBy []PlannedLink `json:"blocked_by"`
}

// PlannedIssue is one issue a run would write.
type PlannedIssue struct {
	// The manifest key, so that a plan reads as the manifest that produced it.
	Key string `json:"key"`
	// The issue's number, or 0 for one this run would create.
	Number int      `json:"number"`
	Title  string   `json:"title"`
	Labels []string `json:"labels"`
}

// PlannedLink is one parent/sub link or one dependency, by manifest key.
type PlannedLink struct {
	// For a sub-issue link, the sub and its parent; for a dependency, the
	// issue that waits and the issue it waits for.
	From string `json:"from"`
	To   string `json:"to"`
}

// PlannedSubstitution is one placeholder a run would replace.
type PlannedSubstitution struct {
	// The key of the issue whose body holds it.
	In string `json:"in"`
	// The line of that body the placeholder is on.
	Line int `json:"line"`
	// The placeholder's name, and the number it would become — 0 where this
	// run has yet to create the issue it names.
	Name   string `json:"name"`
	Number int    `json:"number"`
}

// publishPlan is one run's whole intent, resolved.
type publishPlan struct {
	set    publishSet
	record publishRecord
	// live is every issue the run reads before writing, by key: the targets it
	// checks for freshness, and the existing issues it only needs an id for.
	live   map[string]ghapi.Issue
	viewer string
}

// plan runs every check, up to but not including the first write.
//
// Shared whole by Publish and PublishDryRun.
func plan(ctx context.Context, c *ghapi.Client, wire PublishManifest, dir, file string) (publishPlan, error) {
	set, err := parsePublishManifest(wire, dir, file)
	if err != nil {
		return publishPlan{}, err
	}
	record := readPublishRecord(PublishedLog(file))

	if err := checkPublishSet(set); err != nil {
		return publishPlan{}, err
	}
	live, err := readPublishTargets(ctx, c, set, record)
	if err != nil {
		return publishPlan{}, err
	}
	viewer, err := c.Viewer(ctx)
	if err != nil {
		return publishPlan{}, fmt.Errorf("failed to resolve who the token authenticates: %v", err)
	}
	return publishPlan{set: set, record: record, live: live, viewer: viewer}, nil
}

// checkPublishSet rejects what the manifest says about itself, with GitHub not
// yet consulted.
func checkPublishSet(set publishSet) error {
	var bad violations

	rows := map[string]publishRow{}
	for _, row := range set.rows {
		if _, dup := rows[row.key]; dup {
			bad.add("key %s appears more than once", row.key)
		}
		rows[row.key] = row
	}

	for _, row := range set.rows {
		switch {
		case row.target() && row.updatedAt == "":
			bad.add("%s: a row keyed by an issue number needs updated_at, the snapshot the draft was written against", row.key)
		case !row.target() && row.updatedAt != "":
			bad.add("%s: a placeholder names no issue yet, so it cannot carry updated_at", row.key)
		case !row.target() && row.commentFile != "":
			bad.add("%s: an issue this run creates has no reader to notify, so it cannot carry a comment", row.key)
		}
		if row.parent != "" {
			bad.addAll(checkPublishRef(rows, row.key+": parent", row.parent))
		}
		bad.addAll(checkPublishBody(rows, row))
	}

	for _, b := range set.blocks {
		bad.addAll(checkPublishRef(rows, "blocked_by: blocked", b.blocked))
		bad.addAll(checkPublishRef(rows, "blocked_by: by", b.by))
	}

	return bad.err(set.file)
}

// checkPublishRef checks one reference to an issue by key.
//
// A placeholder has to be a row, because nothing else can tell the run what
// number to put in its place. A number does not: an issue can be a parent, or
// something a new sub waits for, without this run writing to it — and giving
// it a row would mean a draft, a snapshot and an edit of an issue nobody
// approved editing.
func checkPublishRef(rows map[string]publishRow, where, key string) []string {
	switch {
	case numberKey.MatchString(key):
		if n, err := strconv.Atoi(key); err != nil || n == 0 {
			return []string{fmt.Sprintf("%s: %s is not an issue number", where, key)}
		}
		return nil
	case !placeholderKey.MatchString(key):
		return []string{fmt.Sprintf(
			"%s: %q is neither an issue number nor a placeholder name in A-Z and underscores", where, key)}
	case rows[key].key == "":
		return []string{fmt.Sprintf("%s: no row defines the placeholder %s", where, key)}
	}
	return nil
}

// checkPublishBody checks a row's draft, and the comment beside it, for what
// must not reach GitHub.
func checkPublishBody(rows map[string]publishRow, row publishRow) []string {
	var found []string

	vs, err := Check(row.body, row.locale, row.kind, row.mapping)
	if err != nil {
		found = append(found, fmt.Sprintf("%s: %v", row.draft, err))
	}
	for _, v := range vs {
		found = append(found, fmt.Sprintf("%s: %s", row.draft, v.Message))
	}

	for _, f := range []struct{ name, body string }{
		{row.draft, row.body}, {row.commentFile, row.comment},
	} {
		if f.name == "" {
			continue
		}
		// Scanned as written, before any substitution: what the gh shim would
		// have refused had this gone out through it must be refused here too,
		// from the same judgement rather than a copy of it.
		if n := ghmd.BareHashRefs(f.body); n >= ghmd.BareHashRefLimit {
			found = append(found, fmt.Sprintf(
				"%s: %d distinct bare #N look like item numbering, which GitHub would autolink to unrelated issues"+
					" (number the items with an ordered list)", f.name, n))
		}
	}

	for _, s := range substitutionsIn(row) {
		if rows[s.Name].key == "" {
			found = append(found, fmt.Sprintf("%s line %d: no row defines the placeholder %s",
				row.draft, s.Line, s.Name))
		}
	}
	return found
}

// substitutionsIn finds the placeholders in a row's body, outside the code a
// draft may quote them in: #{NAME} is string interpolation in Ruby and Elixir,
// and a draft that shows some is not naming an issue.
func substitutionsIn(row publishRow) []PlannedSubstitution {
	var out []PlannedSubstitution
	for s := range ghmd.Segments(row.body) {
		if s.Kind != ghmd.Prose {
			continue
		}
		for _, m := range placeholderRef.FindAllStringSubmatch(row.body[s.Start:s.End], -1) {
			out = append(out, PlannedSubstitution{In: row.key, Line: s.Line, Name: m[1]})
		}
	}
	return out
}

// readPublishTargets reads every issue the run has to know something about
// before it writes: what a target's updated_at is now, and what integer id an
// issue on either end of a link has.
func readPublishTargets(ctx context.Context, c *ghapi.Client, set publishSet, record publishRecord) (map[string]ghapi.Issue, error) {
	var bad violations
	live := map[string]ghapi.Issue{}

	read := func(key string) (ghapi.Issue, bool) {
		if got, ok := live[key]; ok {
			return got, true
		}
		n, err := strconv.Atoi(key)
		if err != nil {
			return ghapi.Issue{}, false
		}
		got, err := c.Issue(ctx, set.repo, n)
		if err != nil {
			bad.add("#%d could not be read: %v", n, err)
			return ghapi.Issue{}, false
		}
		live[key] = got
		return got, true
	}

	for _, row := range set.rows {
		if !row.target() {
			continue
		}
		got, ok := read(row.key)
		if !ok {
			continue
		}
		if want := record.baseline(row); got.UpdatedAt != want {
			bad.add("#%d has changed since the draft was written (%s, now %s):"+
				" re-fetch it, carry the change into %s, and update updated_at",
				row.number, want, got.UpdatedAt, row.draft)
		}
	}
	// An issue named only as a parent or a blocker is read for its id alone.
	// Nothing is written to it, so nothing about it can be stale.
	for _, key := range referencedNumbers(set) {
		read(key)
	}

	if err := bad.err(set.file); err != nil {
		return nil, err
	}
	return live, nil
}

// referencedNumbers is every existing issue a link names, in a stable order so
// that a refusal reads the same way twice.
func referencedNumbers(set publishSet) []string {
	var out []string
	for _, row := range set.rows {
		if numberKey.MatchString(row.parent) {
			out = append(out, row.parent)
		}
	}
	for _, b := range set.blocks {
		for _, key := range []string{b.blocked, b.by} {
			if numberKey.MatchString(key) {
				out = append(out, key)
			}
		}
	}
	slices.Sort(out)
	return slices.Compact(out)
}

// addAll records a set of violations found elsewhere.
func (v *violations) addAll(found []string) {
	v.found = append(v.found, found...)
}

// PublishDryRun runs every check, sends no write, and answers with what a run
// would do.
func PublishDryRun(ctx context.Context, c *ghapi.Client, m PublishManifest, dir, file string) (PublishPlan, error) {
	p, err := plan(ctx, c, m, dir, file)
	if err != nil {
		return PublishPlan{}, err
	}
	return p.render(), nil
}

// render projects the plan into what the command prints, leaving out the steps
// the record says have already been taken.
func (p publishPlan) render() PublishPlan {
	out := PublishPlan{
		Create: []PlannedIssue{}, Link: []PlannedLink{}, Substitute: []PlannedSubstitution{},
		Edit: []PlannedIssue{}, Comment: []int{}, BlockedBy: []PlannedLink{},
	}
	for _, row := range p.set.rows {
		planned := PlannedIssue{Key: row.key, Number: p.numberOf(row.key), Title: row.title, Labels: row.labels}
		switch {
		case !row.target() && p.record.numbered[row.key].number == 0:
			out.Create = append(out.Create, planned)
		case row.target() && !p.record.patched[row.key]:
			out.Edit = append(out.Edit, planned)
		}
		if row.parent != "" && !p.record.linked[row.key] {
			out.Link = append(out.Link, PlannedLink{From: row.key, To: row.parent})
		}
		if row.commentFile != "" && !p.record.commented[row.key] {
			out.Comment = append(out.Comment, row.number)
		}
		for _, s := range substitutionsIn(row) {
			s.Number = p.numberOf(s.Name)
			out.Substitute = append(out.Substitute, s)
		}
	}
	for _, b := range p.set.blocks {
		if !p.record.blocked[b] {
			out.BlockedBy = append(out.BlockedBy, PlannedLink{From: b.blocked, To: b.by})
		}
	}
	return out
}

// numberOf is the issue a key names: the number it always had, or the one this
// run has already given it, or 0 for one still to be created.
func (p publishPlan) numberOf(key string) int {
	if n, err := strconv.Atoi(key); err == nil {
		return n
	}
	return p.record.numbered[key].number
}
