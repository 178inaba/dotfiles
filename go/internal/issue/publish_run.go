package issue

import (
	"context"
	"encoding/json/v2"
	"fmt"
	"os"
	"slices"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// The order of the stages is forced by the bodies rather than chosen. A parent
// names subs that do not exist when it is created, so every issue is created
// first and the numbers filled in afterwards; a sub cannot be linked to a
// parent that has no number, and a dependency cannot be registered against an
// issue that has none either.

// Published is what one run did.
type Published struct {
	// The issues created, in creation order.
	Created []PublishedIssue `json:"created"`
	// The issues whose body, title and labels were written.
	Edited []PublishedIssue `json:"edited"`
	// The sub-issue links made, by manifest key.
	Linked []PlannedLink `json:"linked"`
	// The edit-notification comments posted.
	Commented []PublishedComment `json:"commented"`
	// The dependencies registered, by manifest key.
	BlockedBy []PlannedLink `json:"blocked_by"`
	// What GitHub stored less of than was asked for. A label or an assignee it
	// declines to apply is reported here rather than failing the run: the
	// issue itself is written, and its REST reference says it drops both for a
	// user without push access.
	Degraded []string `json:"degraded"`
	// What went wrong without changing what was written.
	Warnings []string `json:"warnings"`
}

// PublishedIssue is one issue a run wrote.
type PublishedIssue struct {
	Key    string `json:"key"`
	Number int    `json:"number"`
	URL    string `json:"url"`
}

// PublishedComment is one edit-notification comment a run posted.
type PublishedComment struct {
	Number int    `json:"number"`
	URL    string `json:"url"`
}

// Publish writes the manifest's issues, recording each write as it lands.
//
// Every check is made first, by the same function a dry run uses, so a run
// either publishes or is refused with nothing written.
func Publish(ctx context.Context, c *ghapi.Client, m PublishManifest, file string) (Published, error) {
	p, err := plan(ctx, c, m, file)
	if err != nil {
		return Published{}, err
	}

	r := &publishRun{plan: p, client: c}
	for _, stage := range []func(context.Context) error{r.create, r.link, r.write, r.block} {
		if err := stage(ctx); err != nil {
			// What landed comes back with the failure. The record is what a
			// re-run reads, but a degradation is not in it: a label GitHub
			// declined to apply is noticed once, at the create, and a re-run
			// skips that create and so never mentions it again.
			return r.out, err
		}
	}
	return r.out, nil
}

// publishRun is one run in progress.
type publishRun struct {
	plan   publishPlan
	client *ghapi.Client
	out    Published
}

// record is where the run writes down what has landed. The plan holds it, and
// the two must be the same one: the plan answers "what is left" from it while
// the stages append to it.
func (r *publishRun) record() *publishRecord { return r.plan.record }

// create makes the issues the manifest has no numbers for, in row order.
func (r *publishRun) create(ctx context.Context) error {
	for _, row := range r.plan.set.rows {
		if !r.plan.needsCreate(row) {
			continue
		}
		// Whatever is numbered by now, which is the references back to issues
		// created earlier in this same run.
		body, left := r.plan.substitute(r.plan.set.body(row.key))

		assignees := []string{r.plan.viewer}
		got, err := r.client.CreateIssue(ctx, r.plan.set.repo, ghapi.IssueChange{
			Title: &row.title, Body: &body, Labels: &row.labels, Assignees: &assignees,
		})
		if err != nil {
			return r.abort("create %s: %v", row.key, err)
		}
		if err := r.record().append(publishRecordLine{
			Step: stepCreate, Key: row.key, Number: got.Number, ID: got.ID, Title: row.title,
		}); err != nil {
			return err
		}
		r.out.Created = append(r.out.Created, PublishedIssue{Key: row.key, Number: got.Number, URL: got.URL})
		r.reportDrops(row, got)

		if err := r.readBack(row, row.draft, got.Body, false); err != nil {
			return err
		}
		if len(left) == 0 {
			// Its body went out finished, so the write stage owes it nothing.
			// Recorded rather than remembered: a run that stops here has to be
			// able to tell this issue from one still holding a forward
			// reference, and after a restart it cannot work out which it was —
			// by then every placeholder resolves either way.
			if err := r.record().append(publishRecordLine{Step: stepBodyFinal, Key: row.key}); err != nil {
				return err
			}
		}
	}
	return nil
}

// link makes every parent/sub link the manifest declares, whether the sub was
// created by this run or already existed.
func (r *publishRun) link(ctx context.Context) error {
	for _, row := range r.plan.set.rows {
		if !r.plan.needsLink(row) {
			continue
		}
		id, err := r.idOf(row.key)
		if err != nil {
			return r.abort("link %s to %s: %v", row.key, row.parent, err)
		}
		parent, err := r.client.AddSubIssue(ctx, r.plan.set.repo, r.plan.numberOf(row.parent), id)
		if err != nil {
			return r.abort("link %s to %s: %v", row.key, row.parent, err)
		}
		if err := r.record().append(publishRecordLine{
			Step: stepLink, Key: row.key, Other: row.parent,
		}); err != nil {
			return err
		}
		r.out.Linked = append(r.out.Linked, PlannedLink{From: row.key, To: row.parent})
		// Both ends of a link move, so both are recorded rather than the one
		// this code reasoned about: the parent arrives in the response, and
		// the sub is read back. Refreshing an issue no row edits costs
		// nothing, since only a target is ever compared.
		if err := r.recordFreshness(row.parent, parent.UpdatedAt); err != nil {
			return err
		}
		if err := r.refresh(ctx, row.key); err != nil {
			return err
		}
	}
	return nil
}

// write finishes the bodies and puts them where they belong: a forward
// reference is a number by now, and a target gets its title and labels in the
// same request.
func (r *publishRun) write(ctx context.Context) error {
	for _, row := range r.plan.set.rows {
		if err := r.patch(ctx, row); err != nil {
			return err
		}
		if err := r.comment(ctx, row); err != nil {
			return err
		}
	}
	return nil
}

func (r *publishRun) patch(ctx context.Context, row publishRow) error {
	if !r.plan.needsWrite(row) {
		return nil
	}
	// From the draft rather than from what the create sent: substitution only
	// ever fills a placeholder in, so starting over reaches the same text, and
	// a resumed run has nothing else to start from anyway.
	body, left := r.plan.substitute(r.plan.set.body(row.key))
	if err := r.unfilled(row.draft, left); err != nil {
		return err
	}

	number := r.plan.numberOf(row.key)
	ch := ghapi.IssueChange{Body: &body}
	if row.target() {
		// The title and the whole label set, because the endpoint replaces
		// rather than adds: a row that adds a label names the ones already
		// there. An issue this run created needs neither — it was opened with
		// them, and this write is only finishing its body.
		ch.Title, ch.Labels = &row.title, &row.labels
	}
	got, err := r.client.EditIssue(ctx, r.plan.set.repo, number, ch)
	if err != nil {
		return r.abort("edit #%d: %v", number, err)
	}
	if err := r.record().append(publishRecordLine{Step: stepBodyFinal, Key: row.key}); err != nil {
		return err
	}
	if err := r.recordFreshness(row.key, got.UpdatedAt); err != nil {
		return err
	}
	if row.target() {
		r.out.Edited = append(r.out.Edited, PublishedIssue{Key: row.key, Number: number, URL: got.URL})
		r.reportDrops(row, got)
	}
	return r.readBack(row, row.draft, got.Body, true)
}

func (r *publishRun) comment(ctx context.Context, row publishRow) error {
	if !r.plan.needsComment(row) {
		return nil
	}
	body, left := r.plan.substitute(r.plan.set.comment(row.key))
	if err := r.unfilled(row.commentFile, left); err != nil {
		return err
	}

	number := r.plan.numberOf(row.key)
	url, err := r.client.CreateIssueComment(ctx, r.plan.set.repo, number, body)
	if err != nil {
		return r.abort("comment on #%d: %v", number, err)
	}
	if err := r.record().append(publishRecordLine{Step: stepComment, Key: row.key, URL: url}); err != nil {
		return err
	}
	r.out.Commented = append(r.out.Commented, PublishedComment{Number: number, URL: url})
	// A comment moves the issue, and the response is the comment rather than
	// the issue, so where it left it has to be read.
	return r.refresh(ctx, row.key)
}

// block registers the dependencies, last, because one cannot name an issue
// that does not exist yet.
func (r *publishRun) block(ctx context.Context) error {
	for _, b := range r.plan.set.blocks {
		if !r.plan.needsBlock(b) {
			continue
		}
		id, err := r.idOf(b.by)
		if err != nil {
			return r.abort("register %s as blocked by %s: %v", b.blocked, b.by, err)
		}
		blocked, err := r.client.AddBlockedBy(ctx, r.plan.set.repo, r.plan.numberOf(b.blocked), id)
		if err != nil {
			return r.abort("register %s as blocked by %s: %v", b.blocked, b.by, err)
		}
		if err := r.record().append(publishRecordLine{
			Step: stepBlockedBy, Key: b.blocked, Other: b.by,
		}); err != nil {
			return err
		}
		r.out.BlockedBy = append(r.out.BlockedBy, PlannedLink{From: b.blocked, To: b.by})
		if err := r.recordFreshness(b.blocked, blocked.UpdatedAt); err != nil {
			return err
		}
		if err := r.refresh(ctx, b.by); err != nil {
			return err
		}
	}
	return nil
}

// readBack checks the body GitHub says it stored.
//
// A placeholder whose issue is already numbered is a failure wherever it is
// found: the substitution was there to be made and was not, which is the
// accident this command exists to end. One whose issue has no number yet is a
// forward reference, which is expected until every issue exists and a failure
// afterwards.
func (r *publishRun) readBack(row publishRow, from, stored string, everyIssueExists bool) error {
	var missed, forward []PlannedSubstitution
	for _, s := range placeholdersIn(stored) {
		s.In = row.key
		if r.plan.numberOf(s.Name) != 0 {
			missed = append(missed, s)
		} else {
			forward = append(forward, s)
		}
	}
	where := fmt.Sprintf("#%d, written from %s", r.plan.numberOf(row.key), from)
	if len(missed) > 0 {
		return r.abort("%s, holds %s whose issue is already numbered: %s",
			where, plural(len(missed), "placeholder"), namesOf(missed))
	}
	if everyIssueExists {
		return r.unfilled(where, forward)
	}
	return nil
}

// unfilled is how every placeholder that should have been replaced and was not
// is reported, so that the name of the body it is in cannot be left out of one
// of them.
func (r *publishRun) unfilled(where string, left []PlannedSubstitution) error {
	if len(left) == 0 {
		return nil
	}
	return r.abort("%s still holds %s though every issue in the run now exists: %s",
		where, plural(len(left), "placeholder"), namesOf(left))
}

// refresh re-reads a target this run has just moved and records where it left
// it, so that a later run of the same manifest is not refused over its own
// work. Only a target is ever compared, so only a target is worth reading.
func (r *publishRun) refresh(ctx context.Context, key string) error {
	if !r.plan.isTarget(key) {
		return nil
	}
	got, err := r.client.Issue(ctx, r.plan.set.repo, r.plan.numberOf(key))
	if err != nil {
		r.out.Warnings = append(r.out.Warnings, fmt.Sprintf(
			"#%s could not be re-read after being written, so a re-run of this manifest will ask for a fresh draft of it: %v",
			key, err))
		return nil
	}
	return r.recordFreshness(key, got.UpdatedAt)
}

func (r *publishRun) recordFreshness(key, updatedAt string) error {
	if !r.plan.isTarget(key) || updatedAt == "" {
		return nil
	}
	return r.record().append(publishRecordLine{Step: stepFreshness, Key: key, UpdatedAt: updatedAt})
}

// reportDrops names what GitHub stored less of than was asked for.
func (r *publishRun) reportDrops(row publishRow, got ghapi.Issue) {
	for _, want := range row.labels {
		if !slices.Contains(got.Labels, want) {
			r.out.Degraded = append(r.out.Degraded,
				fmt.Sprintf("#%d did not receive the label %q", got.Number, want))
		}
	}
	// Only a create asks for an assignee; an edit leaves whoever is on the
	// issue alone, so it has nothing to have dropped.
	if !row.target() && !slices.Contains(got.Assignees, r.plan.viewer) {
		r.out.Degraded = append(r.out.Degraded,
			fmt.Sprintf("#%d was not assigned to %s", got.Number, r.plan.viewer))
	}
}

// idOf is the integer GitHub addresses an issue by, which the link endpoints
// take in place of the number.
func (r *publishRun) idOf(key string) (int64, error) {
	if id, ok := r.plan.ids[key]; ok {
		return id, nil
	}
	if n, ok := r.record().numbered[key]; ok && n.id != 0 {
		return n.id, nil
	}
	return 0, fmt.Errorf("the integer id of %s is not known", key)
}

// abort ends the run, saying how to carry on.
//
// The record is the instruction rather than the message: running the same
// manifest again picks up at the first step that is not in it.
func (r *publishRun) abort(format string, args ...any) error {
	return fmt.Errorf("%s\nwhat had already been written is recorded in %s;"+
		" fix the cause and run the same manifest again to carry on from there",
		fmt.Sprintf(format, args...), r.record().file)
}

func namesOf(left []PlannedSubstitution) string {
	out := make([]string, 0, len(left))
	for _, s := range left {
		out = append(out, fmt.Sprintf("#{%s} on line %d", s.Name, s.Line))
	}
	return strings.Join(out, ", ")
}

func plural(n int, word string) string {
	if n == 1 {
		return "a " + word
	}
	return fmt.Sprintf("%d %ss", n, word)
}

// substitute replaces the placeholders whose issues are numbered, leaving the
// rest — and everything inside code — alone.
//
// It returns the names it could not replace, so that a caller can tell "not
// yet" from "never": one before every issue exists is a forward reference, and
// one after is a body that would go out broken.
func (p publishPlan) substitute(body ghapi.Body) (ghapi.Body, []PlannedSubstitution) {
	out, left := body.Substitute(p.numbers())
	return out, planned(left)
}

// numbers is every key this run can put a number in place of, right now.
//
// Built at each call rather than once, because the create stage numbers the
// rows as it goes: what a body created halfway through the run may refer to is
// what exists by then. A key with no number yet is left out, which is what
// makes it one ghmd reports back as unfilled.
func (p publishPlan) numbers() map[string]int {
	out := make(map[string]int, len(p.set.byKey))
	for key, row := range p.set.byKey {
		// The row already knows whether its key was a number, so this asks it
		// rather than putting numberOf's regexp in the loop.
		n := row.number
		if n == 0 {
			n = p.record.numbered[key].number
		}
		if n != 0 {
			out[key] = n
		}
	}
	return out
}

// append writes one completed step, before the run moves on to the next.
//
// A run that dies between the request GitHub accepted and this line does that
// step again when the manifest is re-run. The window is one append wide, and
// `ccx pr reply-threads` lives with the same one; probing GitHub instead would
// refuse a manifest that deliberately creates two issues with the same title.
func (r *publishRecord) append(l publishRecordLine) error {
	b, err := json.Marshal(l)
	if err != nil {
		return fmt.Errorf("record the %s of %s: %w", l.Step, l.Key, err)
	}
	f, err := os.OpenFile(r.file, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("record the %s of %s: %w", l.Step, l.Key, err)
	}
	defer f.Close()
	if _, err := fmt.Fprintf(f, "%s\n", b); err != nil {
		return fmt.Errorf("record the %s of %s: %w", l.Step, l.Key, err)
	}
	r.remember(l)
	return nil
}
