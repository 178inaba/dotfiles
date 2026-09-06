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
//
// It answers "what is left to do" for the run and for a dry run alike, from
// the record it read once. Nothing asks that question a second way: a plan
// that reported one thing while the run did another would be a plan shown for
// approval that does not describe what happens.
type publishPlan struct {
	set    publishSet
	record *publishRecord
	// ids is the integer GitHub addresses each existing issue by — the targets
	// and the issues named only as a parent or a blocker — which is what the
	// two link endpoints take in place of the number.
	ids map[string]int64
	// forwardRefs is the rows this run creates whose bodies will still hold a
	// placeholder afterwards, and so need a second write.
	forwardRefs map[string]bool
	// viewer is who the token authenticates, resolved only where an issue is
	// created: it is the assignee, and nothing else wants it.
	viewer string
}

// plan runs every check, up to but not including the first write.
//
// Shared whole by Publish and PublishDryRun.
func plan(ctx context.Context, c *ghapi.Client, wire PublishManifest, file string) (publishPlan, error) {
	set, err := parsePublishManifest(wire, file)
	if err != nil {
		return publishPlan{}, err
	}
	record, err := readPublishRecord(PublishedLog(file))
	if err != nil {
		return publishPlan{}, err
	}
	if err := record.describes(set); err != nil {
		return publishPlan{}, err
	}

	if err := checkPublishSet(set); err != nil {
		return publishPlan{}, err
	}
	ids, err := readPublishTargets(ctx, c, set, record)
	if err != nil {
		return publishPlan{}, err
	}

	p := publishPlan{set: set, record: record, ids: ids, forwardRefs: forwardRefs(set, record)}
	for _, row := range set.rows {
		if !p.needsCreate(row) {
			continue
		}
		if p.viewer, err = c.Viewer(ctx, 0); err != nil {
			return publishPlan{}, fmt.Errorf("failed to resolve who the token authenticates: %v", err)
		}
		break
	}
	return p, nil
}

// The five questions a run and a dry run both ask of every row, answered from
// the record in one place. A stage skips what is already recorded, and the
// plan lists what a stage would not skip.

func (p publishPlan) needsCreate(row publishRow) bool {
	return !row.target() && p.record.numbered[row.key].number == 0
}

func (p publishPlan) needsLink(row publishRow) bool {
	return row.parent != "" && !p.record.linked[publishBlock{blocked: row.key, by: row.parent}]
}

// needsWrite covers a target's edit and the body of an issue this run creates
// holding a forward reference. Both are the same request.
//
// An issue created with nothing left to fill in needs no second write, and is
// recorded as final at the moment it is created. Before the run starts, which
// of the two a created row will be is worked out by walking the rows in
// creation order — the same walk the create stage makes — so that the plan a
// dry run prints is the one that happens.
func (p publishPlan) needsWrite(row publishRow) bool {
	if p.record.final[row.key] {
		return false
	}
	if row.target() || p.record.numbered[row.key].number != 0 {
		// A target always gets its edit; an issue an earlier run created and
		// did not record as final went out holding a forward reference, which
		// is the whole of what "not final" says about it.
		return true
	}
	return p.forwardRefs[row.key]
}

// forwardRefs marks each row this run creates whose body will still hold a
// placeholder once it is created, by numbering the rows in the order the
// create stage does.
func forwardRefs(set publishSet, record *publishRecord) map[string]bool {
	numbered := map[string]bool{}
	for key, row := range set.byKey {
		if row.target() || record.numbered[key].number != 0 {
			numbered[key] = true
		}
	}
	out := map[string]bool{}
	for _, row := range set.rows {
		if numbered[row.key] {
			continue
		}
		for _, s := range placeholdersIn(row.body) {
			if !numbered[s.Name] {
				out[row.key] = true
				break
			}
		}
		numbered[row.key] = true
	}
	return out
}

func (p publishPlan) needsComment(row publishRow) bool {
	return row.commentFile != "" && !p.record.commented[row.key]
}

func (p publishPlan) needsBlock(b publishBlock) bool { return !p.record.blocked[b] }

// isTarget reports whether a freshness check applies to a key. An existing
// issue named only as a parent or a blocker is not one: nothing is written to
// it, so nothing about it can go stale under the run.
func (p publishPlan) isTarget(key string) bool { return p.set.byKey[key].target() }

// checkPublishSet rejects what the manifest says about itself, with GitHub not
// yet consulted.
func checkPublishSet(set publishSet) error {
	var bad violations

	for _, row := range set.rows {
		// Separate conditions rather than one switch, so that a row breaking
		// two of them is told about both.
		if row.target() && row.updatedAt == "" {
			bad.add("%s: a row keyed by an issue number needs updated_at, the snapshot the draft was written against", row.key)
		}
		if !row.target() && row.updatedAt != "" {
			bad.add("%s: a placeholder names no issue yet, so it cannot carry updated_at", row.key)
		}
		if !row.target() && row.commentFile != "" {
			bad.add("%s: an issue this run creates has no reader to notify, so it cannot carry a comment", row.key)
		}
		if row.parent != "" {
			bad.addAll(checkPublishRef(set.byKey, row.key+": parent", row.parent))
		}
		bodies, found := checkPublishBody(set.byKey, row)
		set.bodies[row.key] = bodies
		bad.addAll(found)
	}

	for _, b := range set.blocks {
		bad.addAll(checkPublishRef(set.byKey, "blocked_by: blocked", b.blocked))
		bad.addAll(checkPublishRef(set.byKey, "blocked_by: by", b.by))
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
// must not reach GitHub, and answers with what it made of them.
//
// Making them here is what holds the whole run to one judgement: the stages
// send these values, so a body cannot be judged at one moment and sent as it
// was at another.
func checkPublishBody(rows map[string]publishRow, row publishRow) (rowBodies, []string) {
	var found []string

	vs, err := Check(row.body, row.locale, row.kind, row.mapping)
	if err != nil {
		found = append(found, fmt.Sprintf("%s: %v", row.draft, err))
	}
	for _, v := range vs {
		found = append(found, fmt.Sprintf("%s: %s", row.draft, v.Message))
	}

	var bodies rowBodies
	bodies.body, found = judgePublishBody(rows, row.draft, row.body, found)
	if row.commentFile != "" {
		bodies.comment, found = judgePublishBody(rows, row.commentFile, row.comment, found)
	}
	return bodies, found
}

// judgePublishBody is one body: what the run will send, and what is wrong with
// it added to found.
func judgePublishBody(rows map[string]publishRow, name, text string, found []string) (ghapi.Body, []string) {
	// Judged as written, before any substitution: what the gh shim would have
	// refused had this gone out through it must be refused here too, from the
	// same judgement rather than a copy of it.
	body, err := ghapi.NewBody(text)
	if err != nil {
		found = append(found, fmt.Sprintf("%s: %v", name, err))
	}
	// A comment is substituted into and read back like a body, so a name no row
	// defines has to stop the run here as well. Left to the write stage it
	// would stop it after the edit that comment belongs to had already landed.
	for _, s := range placeholdersIn(text) {
		if rows[s.Name].key == "" {
			found = append(found, fmt.Sprintf("%s line %d: no row defines the placeholder %s",
				name, s.Line, s.Name))
		}
	}
	return body, found
}

// placeholdersIn is ghmd's reading of a body in the shape a plan publishes.
func placeholdersIn(body string) []PlannedSubstitution {
	return planned(ghmd.Placeholders(body))
}

// planned turns what ghmd found into what a plan reports.
func planned(found []ghmd.Placeholder) []PlannedSubstitution {
	var out []PlannedSubstitution
	for _, p := range found {
		out = append(out, PlannedSubstitution{Line: p.Line, Name: p.Name})
	}
	return out
}

// substitutionsIn is placeholdersIn with the row that holds them named.
func substitutionsIn(row publishRow) []PlannedSubstitution {
	out := placeholdersIn(row.body)
	for i := range out {
		out[i].In = row.key
	}
	return out
}

// readPublishTargets reads every issue the run has to know something about
// before it writes: what a target's updated_at is now, and what integer id an
// issue on either end of a link has.
//
// Serially, which is what GitHub's own REST guidance asks for over concurrent
// requests, and what the rest of this package does.
func readPublishTargets(ctx context.Context, c *ghapi.Client, set publishSet, record *publishRecord) (map[string]int64, error) {
	var bad violations
	ids := map[string]int64{}

	read := func(key string) (ghapi.Issue, bool) {
		n, err := strconv.Atoi(key)
		if err != nil {
			return ghapi.Issue{}, false
		}
		got, err := c.Issue(ctx, set.repo, n)
		if err != nil {
			bad.add("#%d could not be read: %v", n, err)
			return ghapi.Issue{}, false
		}
		ids[key] = got.ID
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
		if !record.fresh(row, got.UpdatedAt) {
			bad.add("#%d has changed since the draft was written (%s, now %s):"+
				" re-fetch it, carry the change into %s, and update updated_at",
				row.number, record.baseline(row), got.UpdatedAt, row.draft)
		}
	}
	// An issue named only as a parent or a blocker is read for its id alone.
	// Nothing is written to it, so nothing about it can be stale.
	for _, key := range referencedNumbers(set) {
		if _, known := ids[key]; !known {
			read(key)
		}
	}

	if err := bad.err(set.file); err != nil {
		return nil, err
	}
	return ids, nil
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
func PublishDryRun(ctx context.Context, c *ghapi.Client, m PublishManifest, file string) (PublishPlan, error) {
	p, err := plan(ctx, c, m, file)
	if err != nil {
		return PublishPlan{}, err
	}
	return p.render(), nil
}

// render projects the plan into what the command prints, from the same
// predicates the stages skip on: a step listed here is a step that will run.
func (p publishPlan) render() PublishPlan {
	out := PublishPlan{
		Create: []PlannedIssue{}, Link: []PlannedLink{}, Substitute: []PlannedSubstitution{},
		Edit: []PlannedIssue{}, Comment: []int{}, BlockedBy: []PlannedLink{},
	}
	for _, row := range p.set.rows {
		planned := PlannedIssue{Key: row.key, Number: p.numberOf(row.key), Title: row.title, Labels: row.labels}
		if p.needsCreate(row) {
			out.Create = append(out.Create, planned)
		}
		if p.needsLink(row) {
			out.Link = append(out.Link, PlannedLink{From: row.key, To: row.parent})
		}
		// A created issue reaches this too, where its body went out holding a
		// forward reference: the write that fills it in is the same request as
		// a target's edit, and leaving it out was the plan claiming a run
		// would do less than it does.
		if p.needsWrite(row) {
			out.Edit = append(out.Edit, planned)
		}
		if p.needsComment(row) {
			out.Comment = append(out.Comment, row.number)
		}
		if !p.needsCreate(row) && !p.needsWrite(row) {
			// Its body is where the manifest wants it, so nothing is left to
			// substitute into it.
			continue
		}
		for _, s := range substitutionsIn(row) {
			s.Number = p.numberOf(s.Name)
			out.Substitute = append(out.Substitute, s)
		}
	}
	for _, b := range p.set.blocks {
		if p.needsBlock(b) {
			out.BlockedBy = append(out.BlockedBy, PlannedLink{From: b.blocked, To: b.by})
		}
	}
	return out
}

// numberOf is the issue a key names: the number it always had, or the one this
// run has already given it, or 0 for one still to be created.
func (p publishPlan) numberOf(key string) int {
	// Only a numeric key can be a number, and the keys were classified when
	// the manifest was parsed; asking strconv about a placeholder allocates an
	// error to throw away, once per occurrence per scan.
	if numberKey.MatchString(key) {
		n, _ := strconv.Atoi(key)
		return n
	}
	return p.record.numbered[key].number
}
