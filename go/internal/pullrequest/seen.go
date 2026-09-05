package pullrequest

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"time"

	"github.com/178inaba/dotfiles/go/internal/contract"
	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// Where a run records that it judged a pull request.
//
// Local rather than on the pull request itself: this is the skill's own
// bookkeeping — "where I got to" — and a person keeps that in a note rather
// than in a comment every collaborator has to scroll past. It follows that a
// fresh machine has no record, which costs one reading and loses nothing.

// Seen is the record one pull request's state file holds.
type Seen struct {
	// The fetched_at of the document the run judged from. The
	// count on the next run measures everything against it, so it is the
	// document's own instant rather than the moment the record was written:
	// anything arriving during the run is dated after it and counts next time.
	SeenAt string `json:"seen_at" contract:"required,nonempty"`
}

// SeenRecord is where the record went and what it now says.
type SeenRecord struct {
	// The absolute path of the state file.
	Path   string `json:"path"`
	SeenAt string `json:"seen_at"`
}

// SeenStore writes one record to the path it is given.
//
// Supplied by the caller, for the reason Store is: turning a value into the
// bytes of a contract document belongs to the command layer, while deciding
// whether the record may move at all belongs here.
type SeenStore func(path string, s Seen) error

// SeenPath is one pull request's state file under stateHome.
//
// The repository is laid out as directories rather than flattened into one
// name, for the reason the cache does the same: the filesystem bounds each
// component, and two repositories differing only in where a separator fell
// cannot collide.
func SeenPath(stateHome string, repo ghapi.Repo, number int) string {
	return filepath.Join(stateHome, "ccx", "seen", repo.Owner, repo.Name, strconv.Itoa(number)+".json")
}

// ReadSeen is the instant a run last judged this pull request from, or nil
// where there is nothing usable to read.
//
// Every way it can be unusable — no file, no directory, bytes that will not
// parse, a record that breaks its own declaration, a value that is not a date
// — reads as nothing recorded. That counts everything on the next run, which
// costs one reading; the other direction would measure a judgment against a
// value nobody wrote.
func ReadSeen(stateHome string, repo ghapi.Repo, number int) *string {
	if stateHome == "" {
		return nil
	}
	path := SeenPath(stateHome, repo, number)
	b, err := os.ReadFile(path)
	if err != nil {
		return nil
	}
	var s Seen
	if err := contract.Unmarshal(b, &s, path); err != nil {
		return nil
	}
	if _, err := time.Parse(time.RFC3339, s.SeenAt); err != nil {
		return nil
	}
	return &s.SeenAt
}

// WriteSeen records that a run reached a judgment on the document that carries
// seenAt.
//
// A seenAt older than what is already recorded is refused: writing it would
// move the mark backwards and resurface every remark judged in between. An
// equal one is the same run recorded twice and overwrites. What is there but
// unusable is overwritten too, since there is nothing to compare against.
func WriteSeen(stateHome string, repo ghapi.Repo, number int, seenAt string, store SeenStore) (SeenRecord, error) {
	if stateHome == "" {
		return SeenRecord{}, fmt.Errorf("no state directory to record in: set XDG_STATE_HOME or a home directory")
	}
	at, err := time.Parse(time.RFC3339, seenAt)
	if err != nil {
		return SeenRecord{}, fmt.Errorf("the document's fetched_at is not a date: %s", seenAt)
	}
	path := SeenPath(stateHome, repo, number)

	if recorded := ReadSeen(stateHome, repo, number); recorded != nil {
		// Parsed by ReadSeen already, so this cannot fail on anything that
		// reached here.
		was, _ := time.Parse(time.RFC3339, *recorded)
		if at.Before(was) {
			return SeenRecord{}, fmt.Errorf(
				"the document was fetched at %s, before the %s already recorded in %s; "+
					"fetch the pull request again before recording", seenAt, *recorded, path)
		}
	}

	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return SeenRecord{}, fmt.Errorf("failed to create the state dir: %s", dir)
	}
	if err := store(path, Seen{SeenAt: seenAt}); err != nil {
		return SeenRecord{}, err
	}
	return SeenRecord{Path: path, SeenAt: seenAt}, nil
}
