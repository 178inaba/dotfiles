package issue_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/issue"
)

func TestPublishedLogSitsBesideTheManifest(t *testing.T) {
	t.Parallel()

	const manifest = "/tmp/run/publish-manifest.json"
	if want := manifest + ".published"; issue.PublishedLog(manifest) != want {
		t.Errorf("PublishedLog(%q) = %q, want %q", manifest, issue.PublishedLog(manifest), want)
	}
}

// TestPublishResumesFromTheRecord covers what the record is for: the steps it
// already holds are gone from the plan, and the ones it does not are still
// there.
func TestPublishResumesFromTheRecord(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("PARENT", "parent.md"),
			row("SUB_A", "sub-a.md", withParent("PARENT")),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"parent.md": leafDraft + "\nComposed of #{SUB_A}.\n",
		"sub-a.md":  leafDraft,
	})
	writeRecord(t, file, `{"step":"create","key":"PARENT","number":100,"id":900,"title":"A title"}`)

	got, err := issue.PublishDryRun(t.Context(), reading(t, nil), m, file)
	if err != nil {
		t.Fatalf("PublishDryRun: %v", err)
	}

	if len(got.Create) != 1 || got.Create[0].Key != "SUB_A" {
		t.Errorf("would create %+v, want SUB_A alone — PARENT is already recorded", got.Create)
	}
	// The number the parent received is what the plan now shows for it, which
	// is how a resumed run knows a forward reference is no longer forward.
	if len(got.Substitute) != 1 || got.Substitute[0].Number != 0 {
		t.Errorf("would substitute %+v, want SUB_A still unnumbered", got.Substitute)
	}
}

// TestPublishIgnoresATornRecordLine is the reason a bad line is dropped rather
// than reported: the only way to write one is to be interrupted mid-append,
// and the step it half-describes is the one to do again.
func TestPublishIgnoresATornRecordLine(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("A", "a.md")},
	}
	file := writeManifest(t, m, map[string]string{"a.md": leafDraft})
	writeRecord(t, file, `{"step":"create","key":"A","numb`)

	got, err := issue.PublishDryRun(t.Context(), reading(t, nil), m, file)
	if err != nil {
		t.Fatalf("PublishDryRun: %v", err)
	}
	if len(got.Create) != 1 {
		t.Errorf("would create %+v, want A — the torn line records nothing", got.Create)
	}
}

// TestPublishChecksFreshnessAgainstTheRecord is the case a naive freshness
// check gets wrong: linking a sub to an existing parent moves that parent's
// updated_at, so a re-run would refuse over a move it made itself.
func TestPublishChecksFreshnessAgainstTheRecord(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo:   ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{row("42", "42.md", withUpdatedAt("2026-01-01T00:00:00Z"))},
	}
	file := writeManifest(t, m, map[string]string{"42.md": leafDraft})
	writeRecord(t, file, `{"step":"freshness","key":"42","updated_at":"2026-02-02T00:00:00Z"}`)

	c := reading(t, map[string]string{
		"/repos/owner/repo/issues/42": liveIssue(42, 900, "2026-02-02T00:00:00Z"),
	})
	if _, err := issue.PublishDryRun(t.Context(), c, m, file); err != nil {
		t.Errorf("PublishDryRun refused a move this run made itself: %v", err)
	}
}

// TestPublishRefusesARecordFromAnotherRun is the hazard the fixed manifest
// name creates: the record outlives the set it describes, and a second run of
// issue-draft in the same scratchpad would otherwise read it as its own
// progress — writing nothing at all, or filling the previous run's issues with
// this run's bodies.
func TestPublishRefusesARecordFromAnotherRun(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("PARENT", "parent.md")},
	}
	file := writeManifest(t, m, map[string]string{"parent.md": leafDraft})
	writeRecord(t, file,
		`{"step":"create","key":"PARENT","number":100,"id":900,"title":"An issue from the run before"}`)

	_, err := issue.PublishDryRun(t.Context(), refusing(t), m, file)
	if err == nil {
		t.Fatal("PublishDryRun accepted a record left by another run, want a refusal")
	}
	for _, want := range []string{"different run", "An issue from the run before", "delete"} {
		if !strings.Contains(err.Error(), want) {
			t.Errorf("the refusal does not mention %q:\n%s", want, err)
		}
	}
}

// TestPublishTakesTheDraftsWordAfterSomebodyElseMoved is the way out of a
// freshness refusal, which the refusal itself and --help both describe:
// re-fetch the issue, carry the change into the draft, update updated_at. It
// only works if the check looks at the field the instruction says to edit.
func TestPublishTakesTheDraftsWordAfterSomebodyElseMoved(t *testing.T) {
	t.Parallel()

	const moved = "2026-03-03T00:00:00Z"
	m := issue.PublishManifest{
		Repo:   ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{row("42", "42.md", withUpdatedAt(moved))},
	}
	file := writeManifest(t, m, map[string]string{"42.md": leafDraft})
	// This run linked something to #42 and stopped; somebody else has since
	// edited it, and the draft has been rewritten against what they left.
	writeRecord(t, file, `{"step":"freshness","key":"42","updated_at":"2026-02-02T00:00:00Z"}`)

	c := reading(t, map[string]string{"/repos/owner/repo/issues/42": liveIssue(42, 900, moved)})
	if _, err := issue.PublishDryRun(t.Context(), c, m, file); err != nil {
		t.Errorf("PublishDryRun refused a draft written against the issue as it now stands: %v", err)
	}
}

// TestPublishSkipsFreshnessForABodyAlreadyWritten is the other half: freshness
// guards against overwriting somebody's change, and once the write has landed
// there is nothing left to overwrite it with.
func TestPublishSkipsFreshnessForABodyAlreadyWritten(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("42", "42.md", withUpdatedAt(fresh), withComment("42-comment.md")),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"42.md":         leafDraft,
		"42-comment.md": "The body was rewritten.\n",
	})
	writeRecord(t, file, `{"step":"body_final","key":"42"}`)

	c := reading(t, map[string]string{
		"/repos/owner/repo/issues/42": liveIssue(42, 900, "2026-09-09T00:00:00Z"),
	})
	got, err := issue.PublishDryRun(t.Context(), c, m, file)
	if err != nil {
		t.Fatalf("PublishDryRun refused an issue it has nothing left to write to: %v", err)
	}
	// The comment it still owes is what the re-run is for.
	if diff := cmp.Diff([]int{42}, got.Comment); diff != "" {
		t.Errorf("PublishDryRun comment (-want +got):\n%s", diff)
	}
}

// TestPublishRefusesAnUnreadableRecord is why absence and failure are told
// apart: reading a permission error as "nothing has been written" would drop
// the guarantee that a re-run does not create everything twice.
func TestPublishRefusesAnUnreadableRecord(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("A", "a.md")},
	}
	file := writeManifest(t, m, map[string]string{"a.md": leafDraft})
	// A directory where the record belongs: unreadable as a file, and nothing
	// a test has to change a mode to arrange.
	if err := os.Mkdir(issue.PublishedLog(file), 0o700); err != nil {
		t.Fatalf("stand a directory in for an unreadable record: %v", err)
	}

	if _, err := issue.PublishDryRun(t.Context(), refusing(t), m, file); err == nil {
		t.Error("PublishDryRun read an unreadable record as nothing written, want a refusal")
	}
}

func writeRecord(t *testing.T, manifestFile string, lines ...string) {
	t.Helper()

	var b []byte
	for _, l := range lines {
		b = append(append(b, l...), '\n')
	}
	if err := os.WriteFile(issue.PublishedLog(manifestFile), b, 0o600); err != nil {
		t.Fatalf("write %s: %v", filepath.Base(issue.PublishedLog(manifestFile)), err)
	}
}
