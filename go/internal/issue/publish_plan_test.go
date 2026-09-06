package issue_test

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/issue"
)

// A leaf body that satisfies the section check, so that a case about anything
// else does not also have to be about headings.
const leafDraft = `## Background / Purpose

Why.

## Requirements

What.

## Acceptance criteria

- [ ] Done.

## Affected code

- somewhere.go

## Out of scope

- Everything else.
`

// writeManifest writes a manifest and its files into a temporary directory and
// answers with the manifest's path, which is all a caller passes on.
func writeManifest(t *testing.T, m issue.PublishManifest, files map[string]string) string {
	t.Helper()

	dir := t.TempDir()
	for name, body := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
	}
	b, err := json.Marshal(m)
	if err != nil {
		t.Fatalf("marshal the manifest: %v", err)
	}
	file := filepath.Join(dir, "publish-manifest.json")
	if err := os.WriteFile(file, b, 0o600); err != nil {
		t.Fatalf("write the manifest: %v", err)
	}
	return file
}

func ptr[T any](v T) *T { return &v }

// row is the shortest spelling of a manifest row a case can start from, with
// whatever else the case is about applied to it.
func row(key, draft string, mods ...func(*issue.PublishManifestIssue)) issue.PublishManifestIssue {
	r := issue.PublishManifestIssue{
		Key: ptr(key), Draft: ptr(draft), Title: ptr("A title"),
		Labels: []string{"enhancement"}, Locale: ptr("en"), Kind: ptr("leaf"),
	}
	for _, mod := range mods {
		mod(&r)
	}
	return r
}

// The modifications the cases reach for, named so that a row reads as what is
// different about it.
func withParent(key string) func(*issue.PublishManifestIssue) {
	return func(r *issue.PublishManifestIssue) { r.Parent = ptr(key) }
}

func withUpdatedAt(at string) func(*issue.PublishManifestIssue) {
	return func(r *issue.PublishManifestIssue) { r.UpdatedAt = ptr(at) }
}

func withComment(file string) func(*issue.PublishManifestIssue) {
	return func(r *issue.PublishManifestIssue) { r.Comment = ptr(file) }
}

func withLocale(l string) func(*issue.PublishManifestIssue) {
	return func(r *issue.PublishManifestIssue) { r.Locale = ptr(l) }
}

func withKind(k string) func(*issue.PublishManifestIssue) {
	return func(r *issue.PublishManifestIssue) { r.Kind = ptr(k) }
}

// fresh is the snapshot most target rows carry, since the case is usually
// about something else.
const fresh = "2026-01-01T00:00:00Z"

// refusing is a client that fails the test if it is reached at all, which is
// how "nothing is written before every check has passed" is held to
// structurally rather than by reading the code.
func refusing(t *testing.T) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(_ http.ResponseWriter, r *http.Request) {
		t.Errorf("a %s of %s was sent despite the refusal", r.Method, r.URL.Path)
	}))
}

// reading answers the issue endpoint from a fixed set and fails the test on
// any other request, so that a case can reach the live read without being able
// to reach a write.
func reading(t *testing.T, issues map[string]string) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodGet {
			t.Errorf("a %s of %s was sent despite the refusal", r.Method, r.URL.Path)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		if r.URL.Path == "/user" {
			http.Error(w, `{"login":"178inaba"}`, http.StatusOK)
			return
		}
		body, ok := issues[r.URL.Path]
		if !ok {
			w.WriteHeader(http.StatusNotFound)
			body = `{"message":"Not Found"}`
		}
		if _, err := w.Write([]byte(body)); err != nil {
			t.Errorf("write the response: %v", err)
		}
	}))
}

// liveIssue is a GitHub issue object with the fields a plan reads.
func liveIssue(number int, id int64, updatedAt string) string {
	return fmt.Sprintf(`{"number":%d,"id":%d,"title":"t","body":"b","state":"open",
		"updated_at":%q,
		"html_url":"https://github.com/owner/repo/issues/%d",
		"repository_url":"https://api.github.com/repos/owner/repo"}`,
		number, id, updatedAt, number)
}

func TestPublishRefuses(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		manifest issue.PublishManifest
		files    map[string]string
		// live is the issue endpoint's answers; nil means no request may be
		// sent at all.
		live map[string]string
		want string
	}{
		{
			name: "the repository is not owner/name",
			manifest: issue.PublishManifest{
				Repo: ptr("not a repo"), Issues: []issue.PublishManifestIssue{row("A", "a.md")},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "repo:",
		},
		{
			name: "a key is neither a number nor a placeholder",
			manifest: issue.PublishManifest{
				Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("sub-a", "a.md")},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "neither an issue number nor a placeholder",
		},
		{
			name: "a key appears twice",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("A", "a.md"), row("A", "a.md")},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "key A appears more than once",
		},
		{
			name: "a target carries no snapshot",
			manifest: issue.PublishManifest{
				Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("42", "a.md")},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "needs updated_at",
		},
		{
			name: "a placeholder carries a snapshot",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("A", "a.md", withUpdatedAt("2026-01-01T00:00:00Z"))},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "cannot carry updated_at",
		},
		{
			name: "a placeholder carries a comment",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("A", "a.md", withComment("c.md"))},
			},
			files: map[string]string{"a.md": leafDraft, "c.md": "changed"},
			want:  "cannot carry a comment",
		},
		{
			name: "a draft is not there",
			manifest: issue.PublishManifest{
				Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("A", "missing.md")},
			},
			want: "draft not found beside the manifest",
		},
		{
			name: "a comment file is not there",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("42", "a.md", withUpdatedAt("2026-01-01T00:00:00Z"), withComment("missing.md"))},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "comment not found beside the manifest",
		},
		{
			name: "a draft fails the section check",
			manifest: issue.PublishManifest{
				Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("A", "a.md")},
			},
			files: map[string]string{"a.md": "## Background / Purpose\n\nOnly this.\n"},
			want:  "a.md:",
		},
		{
			name: "an unsupported locale",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("A", "a.md", withLocale("fr"))},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "unsupported locale",
		},
		{
			name: "an unsupported kind",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("A", "a.md", withKind("epic"))},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "unsupported kind",
		},
		{
			name: "a body numbers its items with bare #N",
			manifest: issue.PublishManifest{
				Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("A", "a.md")},
			},
			files: map[string]string{"a.md": leafDraft + "\n#1 first #2 second #3 third\n"},
			want:  "3 distinct bare #N in #1 to #9 number the items",
		},
		{
			name: "a comment numbers its items with bare #N",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("42", "a.md", withUpdatedAt("2026-01-01T00:00:00Z"), withComment("c.md"))},
			},
			files: map[string]string{"a.md": leafDraft, "c.md": "#1 one #2 two #3 three\n"},
			want:  "3 distinct bare #N in #1 to #9 number the items",
		},
		{
			name: "a body names a placeholder no row defines",
			manifest: issue.PublishManifest{
				Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("A", "a.md")},
			},
			files: map[string]string{"a.md": leafDraft + "\nWaits for #{SUB_B}.\n"},
			want:  "no row defines the placeholder SUB_B",
		},
		{
			// Refused here rather than at the write stage, which is reached
			// only after the edit this comment belongs to has landed.
			name: "an edit-notification comment names a placeholder no row defines",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("42", "a.md", withUpdatedAt(fresh), withComment("c.md"))},
			},
			files: map[string]string{"a.md": leafDraft, "c.md": "See #{NOPE}.\n"},
			want:  "c.md line 1: no row defines the placeholder NOPE",
		},
		{
			name: "a parent names a placeholder no row defines",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("A", "a.md", withParent("PARENT"))},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "parent: no row defines the placeholder PARENT",
		},
		{
			name: "a dependency names a placeholder no row defines",
			manifest: issue.PublishManifest{
				Repo:      ptr("owner/repo"),
				Issues:    []issue.PublishManifestIssue{row("A", "a.md")},
				BlockedBy: []issue.PublishManifestBlockedBy{{Blocked: ptr("A"), By: ptr("SUB_B")}},
			},
			files: map[string]string{"a.md": leafDraft},
			want:  "blocked_by: by: no row defines the placeholder SUB_B",
		},
		{
			name: "a target has moved since the draft was written",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("42", "a.md", withUpdatedAt("2026-01-01T00:00:00Z"))},
			},
			files: map[string]string{"a.md": leafDraft},
			live:  map[string]string{"/repos/owner/repo/issues/42": liveIssue(42, 900, "2026-02-02T00:00:00Z")},
			want:  "#42 has changed since the draft was written",
		},
		{
			name: "an issue a link names is not there",
			manifest: issue.PublishManifest{
				Repo:   ptr("owner/repo"),
				Issues: []issue.PublishManifestIssue{row("A", "a.md", withParent("77"))},
			},
			files: map[string]string{"a.md": leafDraft},
			live:  map[string]string{},
			want:  "#77 could not be read",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			file := writeManifest(t, tt.manifest, tt.files)
			c := refusing(t)
			if tt.live != nil {
				c = reading(t, tt.live)
			}

			// Both entry points refuse identically: a dry run that accepted
			// what a run refuses would be showing a plan that cannot happen.
			if _, err := issue.PublishDryRun(t.Context(), c, tt.manifest, file); err == nil {
				t.Fatal("PublishDryRun succeeded, want a refusal")
			} else if !strings.Contains(err.Error(), tt.want) {
				t.Errorf("PublishDryRun refused with %q, want it to mention %q", err, tt.want)
			}

			if _, err := os.Stat(issue.PublishedLog(file)); !os.IsNotExist(err) {
				t.Error("a refusal left a record behind, want none")
			}
		})
	}
}

// TestPublishRefusalNamesEveryViolation is why a refusal collects rather than
// returning the first thing it finds: fixing a manifest one refusal at a time
// would re-read every live issue on every attempt.
func TestPublishRefusalNamesEveryViolation(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("A", "a.md"),
			row("B", "b.md", withParent("PARENT")),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"a.md": leafDraft + "\nWaits for #{SUB_C}.\n",
		"b.md": leafDraft,
	})

	_, err := issue.PublishDryRun(t.Context(), refusing(t), m, file)
	if err == nil {
		t.Fatal("PublishDryRun succeeded, want a refusal")
	}
	for _, want := range []string{"no row defines the placeholder SUB_C", "no row defines the placeholder PARENT"} {
		if !strings.Contains(err.Error(), want) {
			t.Errorf("the refusal does not mention %q:\n%s", want, err)
		}
	}
}

func TestPublishDryRunPlansTheWholeSet(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("PARENT", "parent.md"),
			row("SUB_A", "sub-a.md", withParent("PARENT")),
			row("42", "42.md", withParent("PARENT"), withUpdatedAt(fresh), withComment("42-comment.md")),
		},
		BlockedBy: []issue.PublishManifestBlockedBy{{Blocked: ptr("42"), By: ptr("SUB_A")}},
	}
	file := writeManifest(t, m, map[string]string{
		"parent.md":     leafDraft + "\nComposed of #{SUB_A}.\n",
		"sub-a.md":      leafDraft,
		"42.md":         leafDraft + "\nWaits for #{SUB_A}.\n",
		"42-comment.md": "The body was rewritten.\n",
	})
	c := reading(t, map[string]string{
		"/repos/owner/repo/issues/42": liveIssue(42, 900, "2026-01-01T00:00:00Z"),
	})

	got, err := issue.PublishDryRun(t.Context(), c, m, file)
	if err != nil {
		t.Fatalf("PublishDryRun: %v", err)
	}

	want := issue.PublishPlan{
		Create: []issue.PlannedIssue{
			{Key: "PARENT", Title: "A title", Labels: []string{"enhancement"}},
			{Key: "SUB_A", Title: "A title", Labels: []string{"enhancement"}},
		},
		Link: []issue.PlannedLink{{From: "SUB_A", To: "PARENT"}, {From: "42", To: "PARENT"}},
		// The line the placeholder is on, which is what a failure to replace
		// it later has to name.
		Substitute: []issue.PlannedSubstitution{
			{In: "PARENT", Line: 21, Name: "SUB_A"},
			{In: "42", Line: 21, Name: "SUB_A"},
		},
		// The parent is edited as well as created: its body names a sub that
		// does not exist when it goes out, so a second write fills the number
		// in. SUB_A's body is finished at creation and is not here.
		Edit: []issue.PlannedIssue{
			{Key: "PARENT", Title: "A title", Labels: []string{"enhancement"}},
			{Key: "42", Number: 42, Title: "A title", Labels: []string{"enhancement"}},
		},
		Comment:   []int{42},
		BlockedBy: []issue.PlannedLink{{From: "42", To: "SUB_A"}},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("PublishDryRun (-want +got):\n%s", diff)
	}

	if _, err := os.Stat(issue.PublishedLog(file)); !os.IsNotExist(err) {
		t.Error("a dry run left a record behind, want none")
	}
}

// TestPublishDryRunLeavesQuotedPlaceholdersAlone is why the scan skips code:
// #{NAME} is string interpolation in Ruby and Elixir, so a draft that quotes
// some is not naming an issue and would stop a run that cannot help it.
func TestPublishDryRunLeavesQuotedPlaceholdersAlone(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("A", "a.md")},
	}
	file := writeManifest(t, m, map[string]string{
		// The last block is the one a tilde line used to cut short: the
		// placeholder goes on the line after that marker rather than on it,
		// because a marker line is Fence either way and a placeholder written
		// on one would be skipped for the wrong reason.
		"a.md": leafDraft + "\nInline `#{NAME}` and fenced:\n\n```ruby\nputs \"#{NAME}\"\n```\n\n```\n~~~\n#{NAME}\n```\n",
	})

	got, err := issue.PublishDryRun(t.Context(), reading(t, nil), m, file)
	if err != nil {
		t.Fatalf("PublishDryRun: %v", err)
	}
	if len(got.Substitute) != 0 {
		t.Errorf("planned substitutions %+v, want none", got.Substitute)
	}
}
