package issue_test

import (
	"os"
	"path/filepath"
	"testing"

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
			func() issue.PublishManifestIssue {
				r := row("SUB_A", "sub-a.md")
				r.Parent = ptr("PARENT")
				return r
			}(),
		},
	}
	dir, file := manifestDir(t, m, map[string]string{
		"parent.md": leafDraft + "\nComposed of #{SUB_A}.\n",
		"sub-a.md":  leafDraft,
	})
	writeRecord(t, file, `{"step":"create","key":"PARENT","number":100,"id":900}`)

	got, err := issue.PublishDryRun(t.Context(), reading(t, nil), m, dir, file)
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
	dir, file := manifestDir(t, m, map[string]string{"a.md": leafDraft})
	writeRecord(t, file, `{"step":"create","key":"A","numb`)

	got, err := issue.PublishDryRun(t.Context(), reading(t, nil), m, dir, file)
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
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{func() issue.PublishManifestIssue {
			r := row("42", "42.md")
			r.UpdatedAt = ptr("2026-01-01T00:00:00Z")
			return r
		}()},
	}
	dir, file := manifestDir(t, m, map[string]string{"42.md": leafDraft})
	writeRecord(t, file, `{"step":"freshness","key":"42","updated_at":"2026-02-02T00:00:00Z"}`)

	c := reading(t, map[string]string{
		"/repos/owner/repo/issues/42": liveIssue(42, 900, "2026-02-02T00:00:00Z"),
	})
	if _, err := issue.PublishDryRun(t.Context(), c, m, dir, file); err != nil {
		t.Errorf("PublishDryRun refused a move this run made itself: %v", err)
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
