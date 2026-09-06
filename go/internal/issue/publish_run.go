package issue

import (
	"context"
	"encoding/json/v2"
	"fmt"
	"os"
	"slices"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghmd"
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
func Publish(ctx context.Context, c *ghapi.Client, m PublishManifest, dir, file string) (Published, error) {
	p, err := plan(ctx, c, m, dir, file)
	if err != nil {
		return Published{}, err
	}

	r := &publishRun{plan: p, client: c, record: p.record, bodies: map[string]string{}}
	// The bodies are the run's working copy: a create substitutes into them
	// what is numbered by then, and the write stage finishes the rest.
	for _, row := range p.set.rows {
		r.bodies[row.key] = row.body
	}

	for _, stage := range []func(context.Context) error{r.create, r.link, r.write, r.block} {
		if err := stage(ctx); err != nil {
			return r.out, err
		}
	}
	return r.out, nil
}

// publishRun is one run in progress.
type publishRun struct {
	plan   publishPlan
	client *ghapi.Client
	record publishRecord
	bodies map[string]string
	out    Published
}

// create makes the issues the manifest has no numbers for, in row order.
func (r *publishRun) create(ctx context.Context) error {
	for _, row := range r.plan.set.rows {
		if row.target() || r.record.numbered[row.key].number != 0 {
			continue
		}
		// Whatever is numbered by now, which is the references back to issues
		// created earlier in this same run.
		body, left := r.plan.substitute(r.bodies[row.key])
		r.bodies[row.key] = body

		assignees := []string{r.plan.viewer}
		got, err := r.client.CreateIssue(ctx, r.plan.set.repo, ghapi.IssueChange{
			Title: &row.title, Body: &body, Labels: &row.labels, Assignees: &assignees,
		})
		if err != nil {
			return r.abort("create %s: %v", row.key, err)
		}
		if err := r.record.append(publishRecordLine{
			Step: stepCreate, Key: row.key, Number: got.Number, ID: got.ID,
		}); err != nil {
			return err
		}
		r.out.Created = append(r.out.Created, PublishedIssue{Key: row.key, Number: got.Number, URL: got.URL})
		r.reportDrops(row, got)

		if err := r.readBack(row, got.Body, false); err != nil {
			return err
		}
		if len(left) == 0 {
			// Its body went out finished, so the write stage owes it nothing.
			// Recorded rather than remembered, because a run that stops here
			// has to be able to tell this issue from one still holding a
			// forward reference.
			if err := r.record.append(publishRecordLine{Step: stepPatch, Key: row.key}); err != nil {
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
		if row.parent == "" || r.record.linked[row.key] {
			continue
		}
		id, err := r.idOf(row.key)
		if err != nil {
			return r.abort("link %s to %s: %v", row.key, row.parent, err)
		}
		path := fmt.Sprintf("repos/%s/issues/%d/sub_issues", r.plan.set.repo, r.plan.numberOf(row.parent))
		if err := r.client.Post(ctx, path, map[string]any{"sub_issue_id": id}, nil); err != nil {
			return r.abort("link %s to %s: %v", row.key, row.parent, err)
		}
		if err := r.record.append(publishRecordLine{Step: stepLink, Key: row.key, Parent: row.parent}); err != nil {
			return err
		}
		r.out.Linked = append(r.out.Linked, PlannedLink{From: row.key, To: row.parent})
		// A link moves the parent, so a parent that is also a target is
		// re-read before a later run compares it against the manifest.
		if err := r.refresh(ctx, row.parent); err != nil {
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
	if r.record.patched[row.key] {
		return nil
	}
	body, left := r.plan.substitute(r.bodies[row.key])
	r.bodies[row.key] = body
	if len(left) > 0 {
		return r.abort("%s still holds %s though every issue in the run now exists: %s",
			row.draft, plural(len(left), "placeholder"), namesOf(left))
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
	if err := r.record.append(publishRecordLine{Step: stepPatch, Key: row.key}); err != nil {
		return err
	}
	if err := r.recordFreshness(row.key, got.UpdatedAt); err != nil {
		return err
	}
	if row.target() {
		r.out.Edited = append(r.out.Edited, PublishedIssue{Key: row.key, Number: number, URL: got.URL})
		r.reportDrops(row, got)
	}
	return r.readBack(row, got.Body, true)
}

func (r *publishRun) comment(ctx context.Context, row publishRow) error {
	if row.commentFile == "" || r.record.commented[row.key] {
		return nil
	}
	body, left := r.plan.substitute(row.comment)
	if len(left) > 0 {
		return r.abort("%s still holds %s though every issue in the run now exists: %s",
			row.commentFile, plural(len(left), "placeholder"), namesOf(left))
	}

	number := r.plan.numberOf(row.key)
	var got struct {
		HTMLURL string `json:"html_url"`
	}
	path := fmt.Sprintf("repos/%s/issues/%d/comments", r.plan.set.repo, number)
	if err := r.client.Post(ctx, path, map[string]any{"body": body}, &got); err != nil {
		return r.abort("comment on #%d: %v", number, err)
	}
	if err := r.record.append(publishRecordLine{Step: stepComment, Key: row.key, URL: got.HTMLURL}); err != nil {
		return err
	}
	r.out.Commented = append(r.out.Commented, PublishedComment{Number: number, URL: got.HTMLURL})
	// A comment moves the issue, and the response is the comment rather than
	// the issue, so where it left it has to be read.
	return r.refresh(ctx, row.key)
}

// block registers the dependencies, last, because one cannot name an issue
// that does not exist yet.
func (r *publishRun) block(ctx context.Context) error {
	for _, b := range r.plan.set.blocks {
		if r.record.blocked[b] {
			continue
		}
		id, err := r.idOf(b.by)
		if err != nil {
			return r.abort("register %s as blocked by %s: %v", b.blocked, b.by, err)
		}
		path := fmt.Sprintf("repos/%s/issues/%d/dependencies/blocked_by",
			r.plan.set.repo, r.plan.numberOf(b.blocked))
		if err := r.client.Post(ctx, path, map[string]any{"issue_id": id}, nil); err != nil {
			return r.abort("register %s as blocked by %s: %v", b.blocked, b.by, err)
		}
		if err := r.record.append(publishRecordLine{Step: stepBlockedBy, Key: b.blocked, By: b.by}); err != nil {
			return err
		}
		r.out.BlockedBy = append(r.out.BlockedBy, PlannedLink{From: b.blocked, To: b.by})
		if err := r.refresh(ctx, b.blocked); err != nil {
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
func (r *publishRun) readBack(row publishRow, stored string, everyIssueExists bool) error {
	var missed, forward []PlannedSubstitution
	for _, s := range placeholdersIn(stored) {
		if r.plan.numberOf(s.Name) != 0 {
			missed = append(missed, s)
		} else {
			forward = append(forward, s)
		}
	}
	number := r.plan.numberOf(row.key)
	if len(missed) > 0 {
		return r.abort("#%d was written holding %s whose issue is already numbered: %s",
			number, plural(len(missed), "placeholder"), namesOf(missed))
	}
	if everyIssueExists && len(forward) > 0 {
		return r.abort("#%d was written holding %s though every issue in the run now exists: %s",
			number, plural(len(forward), "placeholder"), namesOf(forward))
	}
	return nil
}

// refresh re-reads a target this run has just moved and records where it left
// it, so that a later run of the same manifest is not refused over its own
// work. Only a target is ever compared, so only a target is worth reading.
func (r *publishRun) refresh(ctx context.Context, key string) error {
	if !r.plan.targets[key] {
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
	if !r.plan.targets[key] || updatedAt == "" {
		return nil
	}
	return r.record.append(publishRecordLine{Step: stepFreshness, Key: key, UpdatedAt: updatedAt})
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
	if got, ok := r.plan.live[key]; ok {
		return got.ID, nil
	}
	if n, ok := r.record.numbered[key]; ok && n.id != 0 {
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
		fmt.Sprintf(format, args...), PublishedLog(r.plan.set.file))
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
func (p publishPlan) substitute(body string) (string, []PlannedSubstitution) {
	var b strings.Builder
	var left []PlannedSubstitution
	at := 0
	for s := range ghmd.Segments(body) {
		if s.Kind != ghmd.Prose {
			continue
		}
		text := body[s.Start:s.End]
		for _, m := range placeholderRef.FindAllStringSubmatchIndex(text, -1) {
			name := text[m[2]:m[3]]
			number := p.numberOf(name)
			if number == 0 {
				left = append(left, PlannedSubstitution{Line: s.Line, Name: name})
				continue
			}
			b.WriteString(body[at : s.Start+m[0]])
			fmt.Fprintf(&b, "#%d", number)
			at = s.Start + m[1]
		}
	}
	b.WriteString(body[at:])
	return b.String(), left
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
