package issue

import (
	"encoding/json/v2"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strings"
)

// A run writes to GitHub, and GitHub has no undo. What has already landed is
// therefore written down as it lands, so that a run interrupted anywhere can
// be started again from the same manifest and pick up where it stopped rather
// than creating a second copy of everything before it.

// PublishedLog is where what a manifest has already written is recorded.
//
// Beside the manifest, and named after it, so that the record is bound to the
// run it describes and is found again without being declared.
func PublishedLog(manifestFile string) string { return manifestFile + ".published" }

// publishStep is which of a run's writes a record line describes.
type publishStep string

const (
	stepCreate  publishStep = "create"
	stepLink    publishStep = "link"
	stepComment publishStep = "comment"
	// stepBodyFinal says the body on GitHub is the one the draft asks for and
	// needs no further write. Not "a patch happened": an issue whose body went
	// out complete at creation reaches this without a second request, and the
	// run has to be able to tell that from one created holding a forward
	// reference — which it cannot recompute after a restart, since by then
	// every placeholder resolves either way.
	stepBodyFinal publishStep = "body_final"
	stepBlockedBy publishStep = "blocked_by"
	// stepFreshness records what an issue's updated_at became after this run
	// touched it. Not a write of its own: it is what a re-run compares
	// against, in place of the snapshot the manifest was written from.
	stepFreshness publishStep = "freshness"
)

// publishRecordLine is one completed step.
type publishRecordLine struct {
	Step   publishStep `json:"step"`
	Key    string      `json:"key,omitzero"`
	Number int         `json:"number,omitzero"`
	ID     int64       `json:"id,omitzero"`
	// Title is what a created issue was called, which is how a record left by
	// a different run is recognised; see describes.
	Title string `json:"title,omitzero"`
	// Other is the key at the far end of a link or a dependency: the parent,
	// or the issue waited for. Recorded rather than implied, so that a
	// manifest whose parent was changed after a partial run is linked again
	// rather than read as done.
	Other     string `json:"other,omitzero"`
	URL       string `json:"url,omitzero"`
	UpdatedAt string `json:"updated_at,omitzero"`
}

// publishRecord is what a manifest has already written.
//
// Held by pointer wherever a run carries it: the plan answers "what is left"
// from these maps, and the stages append to them as they go, so a copy would
// let the two disagree part-way through a run.
type publishRecord struct {
	file string
	// numbered is the number and id each created placeholder received.
	numbered map[string]publishNumber
	// linked and blocked are keyed by both ends, so that changing one end in
	// the manifest is not read as already done.
	linked    map[publishBlock]bool
	blocked   map[publishBlock]bool
	final     map[string]bool
	commented map[string]bool
	// wroteAt is the updated_at each touched issue has now, which is what a
	// re-run checks a target against.
	wroteAt map[string]string
	// titles is what each created placeholder was called, which is how a
	// record left behind by a different run is told from this one's; see
	// describes.
	titles map[string]string
}

// publishNumber is an issue a run created.
type publishNumber struct {
	number int
	id     int64
}

// readPublishRecord reads the record, treating its absence as nothing written.
//
// A line that does not decode is dropped rather than reported: the only way to
// produce one is to be interrupted mid-append, and the step it half-describes
// is exactly the one the run should do again.
func readPublishRecord(file string) (*publishRecord, error) {
	r := &publishRecord{
		file: file, numbered: map[string]publishNumber{},
		linked: map[publishBlock]bool{}, blocked: map[publishBlock]bool{},
		final: map[string]bool{}, commented: map[string]bool{},
		wroteAt: map[string]string{}, titles: map[string]string{},
	}
	b, err := os.ReadFile(file)
	if errors.Is(err, fs.ErrNotExist) {
		// Nothing has been written yet, which is the ordinary first run.
		return r, nil
	}
	if err != nil {
		// Anything else — a permission, a directory — is not "nothing has been
		// written". Reading it as that would drop the one guarantee the record
		// exists to give, that a re-run does not create everything twice.
		return nil, fmt.Errorf("read what this manifest has already written: %w", err)
	}
	for line := range strings.Lines(string(b)) {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var l publishRecordLine
		if json.Unmarshal([]byte(line), &l) != nil {
			continue
		}
		r.remember(l)
	}
	return r, nil
}

// describes reports whether the record was left by a run of this manifest.
//
// The record is found by the manifest's name, and the skill keeps the manifest
// under one fixed name in the scratchpad, so a record can outlive the set it
// describes and be read as this one's progress — under which a second run of
// issue-draft writes nothing at all, or fills the previous run's issues with
// this one's bodies. The title an issue was created under is what tells them
// apart: this run never changes a created issue's title, so a row that says
// something else is a row about a different issue.
func (r publishRecord) describes(set publishSet) error {
	for key, was := range r.titles {
		row, ok := set.byKey[key]
		if !ok || row.title == was {
			continue
		}
		return fmt.Errorf(
			"%s records %s as an issue created with the title %q, and the manifest calls it %q:"+
				" this record was left by a different run.\nIf this is a new set of issues, delete %s and run again;"+
				" if it is the same one, put the title back",
			r.file, key, was, row.title, r.file)
	}
	return nil
}

// remember applies one step to what the record holds, so that reading the file
// back and appending to it during a run agree by construction.
func (r *publishRecord) remember(l publishRecordLine) {
	switch l.Step {
	case stepCreate:
		r.numbered[l.Key] = publishNumber{number: l.Number, id: l.ID}
		r.titles[l.Key] = l.Title
	case stepLink:
		r.linked[publishBlock{blocked: l.Key, by: l.Other}] = true
	case stepBodyFinal:
		r.final[l.Key] = true
	case stepComment:
		r.commented[l.Key] = true
	case stepBlockedBy:
		r.blocked[publishBlock{blocked: l.Key, by: l.Other}] = true
	case stepFreshness:
		r.wroteAt[l.Key] = l.UpdatedAt
	}
}

// fresh reports whether a target still looks like the issue its draft was
// written against.
//
// Two values pass, and both have to. What this run last left the issue at is
// one: a run that links a sub to an existing parent moves that parent, and
// without it a re-run would refuse over a move it made itself. The snapshot in
// the manifest is the other, and leaving it out is what made the documented
// way out of a refusal — re-fetch the issue, carry the change into the draft,
// update updated_at — change nothing, because the comparison never looked at
// the field the instruction says to edit.
//
// A target whose body is already written is not checked at all. Freshness
// guards against overwriting somebody's change, and there is no write left to
// overwrite it with.
func (r publishRecord) fresh(row publishRow, live string) bool {
	if r.final[row.key] {
		return true
	}
	if at, ok := r.wroteAt[row.key]; ok && live == at {
		return true
	}
	return live == row.updatedAt
}

// baseline is what a refusal reports the issue was expected to be at.
func (r publishRecord) baseline(row publishRow) string {
	if at, ok := r.wroteAt[row.key]; ok {
		return at
	}
	return row.updatedAt
}
