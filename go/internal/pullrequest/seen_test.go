package pullrequest_test

import (
	"encoding/json/v2"
	"os"
	"path/filepath"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/pullrequest"
)

// storeSeen stands in for the command layer's writer, which owns turning a
// record into the bytes of its contract.
func storeSeen(path string, s pullrequest.Seen) error {
	b, err := json.Marshal(s)
	if err != nil {
		return err
	}
	return os.WriteFile(path, b, 0o644)
}

func TestSeenPath(t *testing.T) {
	t.Parallel()

	got := pullrequest.SeenPath("/state", repo, 5)
	want := filepath.Join("/state", "ccx", "seen", "owner", "repo", "5.json")
	if got != want {
		t.Errorf("SeenPath = %q, want %q", got, want)
	}
}

func TestWriteSeenRecordsAndOverwrites(t *testing.T) {
	t.Parallel()

	state := t.TempDir()

	first, err := pullrequest.WriteSeen(state, repo, 5, "2026-01-10T00:00:00Z", storeSeen)
	if err != nil {
		t.Fatalf("WriteSeen: %v", err)
	}
	if first.Path != pullrequest.SeenPath(state, repo, 5) || first.SeenAt != "2026-01-10T00:00:00Z" {
		t.Errorf("WriteSeen = %+v, want the path and the value it recorded", first)
	}
	if at := pullrequest.ReadSeen(state, repo, 5); at == nil || *at != "2026-01-10T00:00:00Z" {
		t.Fatalf("ReadSeen = %v, want what was just written", at)
	}

	// A later run of the same skill on the same pull request moves the mark
	// forward; an equal one is the same run recorded twice and overwrites too.
	for _, at := range []string{"2026-01-10T00:00:00Z", "2026-01-11T00:00:00Z"} {
		if _, err := pullrequest.WriteSeen(state, repo, 5, at, storeSeen); err != nil {
			t.Fatalf("WriteSeen(%s): %v", at, err)
		}
		if got := pullrequest.ReadSeen(state, repo, 5); got == nil || *got != at {
			t.Errorf("ReadSeen = %v, want %q", got, at)
		}
	}
}

// Recording an older document would resurface remarks a later run has already
// judged, so it is refused rather than written.
func TestWriteSeenRefusesAnOlderDocument(t *testing.T) {
	t.Parallel()

	state := t.TempDir()
	if _, err := pullrequest.WriteSeen(state, repo, 5, "2026-01-11T00:00:00Z", storeSeen); err != nil {
		t.Fatalf("WriteSeen: %v", err)
	}

	_, err := pullrequest.WriteSeen(state, repo, 5, "2026-01-10T00:00:00Z", storeSeen)
	if err == nil {
		t.Fatal("WriteSeen with an older document = nil, want a refusal")
	}
	if got := pullrequest.ReadSeen(state, repo, 5); got == nil || *got != "2026-01-11T00:00:00Z" {
		t.Errorf("ReadSeen = %v, want the refusal to have left the record alone", got)
	}
}

// Nowhere to write is refused rather than written relative to whatever the
// working directory happens to be.
func TestWriteSeenRefusesAnEmptyStateHome(t *testing.T) {
	t.Parallel()

	if _, err := pullrequest.WriteSeen("", repo, 5, "2026-01-10T00:00:00Z", storeSeen); err == nil {
		t.Error("WriteSeen with no state directory = nil, want a refusal")
	}
}

// Every way the record can be unusable reads as "nothing recorded", which
// costs one reading and never a judgment made against a value nobody wrote.
func TestReadSeenTreatsWhatItCannotUseAsAbsent(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name    string
		content string
	}{
		{name: "no file at all"},
		{name: "not json", content: "{"},
		{name: "no seen_at", content: `{}`},
		{name: "a seen_at that is not a date", content: `{"seen_at":"whenever"}`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			state := t.TempDir()
			if tc.content != "" {
				path := pullrequest.SeenPath(state, repo, 5)
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatalf("MkdirAll: %v", err)
				}
				if err := os.WriteFile(path, []byte(tc.content), 0o644); err != nil {
					t.Fatalf("WriteFile: %v", err)
				}
			}

			if got := pullrequest.ReadSeen(state, repo, 5); got != nil {
				t.Errorf("ReadSeen = %v, want it read as absent", *got)
			}
			// And the next run overwrites it rather than refusing against a
			// value it could not read.
			if _, err := pullrequest.WriteSeen(state, repo, 5, "2026-01-10T00:00:00Z", storeSeen); err != nil {
				t.Errorf("WriteSeen over an unusable record: %v", err)
			}
		})
	}
}
