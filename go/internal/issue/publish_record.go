package issue

import (
	"encoding/json/v2"
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
	// fresh is the updated_at each touched issue has now, which is what a
	// re-run checks a target against.
	fresh map[string]string
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
func readPublishRecord(file string) *publishRecord {
	r := &publishRecord{
		file: file, numbered: map[string]publishNumber{},
		linked: map[publishBlock]bool{}, blocked: map[publishBlock]bool{},
		final: map[string]bool{}, commented: map[string]bool{}, fresh: map[string]string{},
	}
	b, err := os.ReadFile(file)
	if err != nil {
		return r
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
	return r
}

// remember applies one step to what the record holds, so that reading the file
// back and appending to it during a run agree by construction.
func (r *publishRecord) remember(l publishRecordLine) {
	switch l.Step {
	case stepCreate:
		r.numbered[l.Key] = publishNumber{number: l.Number, id: l.ID}
	case stepLink:
		r.linked[publishBlock{blocked: l.Key, by: l.Other}] = true
	case stepBodyFinal:
		r.final[l.Key] = true
	case stepComment:
		r.commented[l.Key] = true
	case stepBlockedBy:
		r.blocked[publishBlock{blocked: l.Key, by: l.Other}] = true
	case stepFreshness:
		r.fresh[l.Key] = l.UpdatedAt
	}
}

// baseline is what a target's live updated_at is compared against: what this
// run last left it at, or the snapshot the draft was written from.
//
// The distinction is the whole reason the timestamp is recorded. A run that
// links a sub to an existing parent moves that parent's updated_at; without
// this, re-running the same manifest would refuse as "the issue has moved"
// over a move it made itself.
func (r publishRecord) baseline(row publishRow) string {
	if at, ok := r.fresh[row.key]; ok {
		return at
	}
	return row.updatedAt
}
