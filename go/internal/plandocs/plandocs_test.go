package plandocs

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"
)

// writeTree lays out a fixture repository under dir, creating the parent of
// every path as it goes.
func writeTree(t *testing.T, dir string, files map[string]string) {
	t.Helper()
	for name, body := range files {
		full := filepath.Join(dir, filepath.FromSlash(name))
		if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(full, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
}

// abs turns the fixture-relative names a case expects into the absolute paths
// the collection carries.
func abs(dir string, names ...string) []string {
	if len(names) == 0 {
		return nil
	}
	out := make([]string, 0, len(names))
	for _, name := range names {
		out = append(out, filepath.Join(dir, filepath.FromSlash(name)))
	}
	return out
}

// The walk of one tree holding every rule at once, so that the cases below
// assert against a single fixture rather than each building its own variant of
// the same thing.
func TestCollectWalksTheWholeTree(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"CLAUDE.md": "@imported.md\n" +
			"[a](docs/a.md)\n" +
			"a mention of `[skipped](docs/skipped.md)` stays a mention\n" +
			"```\n[fenced](docs/fenced.md)\n```\n" +
			"[missing](docs/missing.md)\n" +
			"[fragment](docs/a.md#section)\n" +
			"[site](https://example.com/x.md) [mail](mailto:a@example.com) [here](#anchor)\n",
		"imported.md":        "[b](docs/b.md)\n",
		"docs/a.md":          "@c.md\n[rule](../.claude/rules/scoped.md)\n",
		"docs/b.md":          "[deep](deep1.md)\n",
		"docs/c.md":          "[too deep](toodeep.md)\n",
		"docs/deep1.md":      "[too deep](toodeep2.md)\n",
		"docs/toodeep.md":    "",
		"docs/toodeep2.md":   "",
		"docs/skipped.md":    "",
		"docs/fenced.md":     "",
		"docs/fromrule.md":   "",
		"docs/fromimport.md": "",
		// The harness does not expand an import written in a rule, so both
		// forms reach a planner the same way — as depth-1 links.
		".claude/rules/unscoped.md": "[r](../../docs/fromrule.md)\n@../../docs/fromimport.md\n",
		".claude/rules/scoped.md":   "---\npaths:\n  - \"**/*.go\"\n---\n",
	})

	got, err := Collect(dir, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		// The @ closure and the unscoped rule are what the harness already
		// has; the scoped rule is not, which is why it turns up below.
		Loaded: abs(dir, "CLAUDE.md", "imported.md", ".claude/rules/unscoped.md"),
		Documents: abs(dir,
			"docs/a.md", "docs/b.md", "docs/fromrule.md", "docs/fromimport.md",
			"docs/c.md", ".claude/rules/scoped.md", "docs/deep1.md",
		),
		Warnings: []Warning{{Target: "docs/missing.md", Source: filepath.Join(dir, "CLAUDE.md")}},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// An import past the harness's four-hop limit is a link, because the harness
// stopped before loading the file it is written in.
func TestCollectFollowsAnImportPastTheHopLimit(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"CLAUDE.md": "@h1.md\n",
		"h1.md":     "@h2.md\n",
		"h2.md":     "@h3.md\n",
		"h3.md":     "@h4.md\n",
		"h4.md":     "@h5.md\n",
		"h5.md":     "",
	})

	got, err := Collect(dir, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded:    abs(dir, "CLAUDE.md", "h1.md", "h2.md", "h3.md", "h4.md"),
		Documents: abs(dir, "h5.md"),
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// The @ closure follows every import regardless of extension, as the harness
// does, while only .md targets are ever listed to read.
func TestCollectClosesOverImportsOfAnyExtension(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"CLAUDE.md":    "@package.json and @notes.md\n",
		"package.json": "{}\n",
		"notes.md":     "[x](x.md)\n",
		"x.md":         "",
	})

	got, err := Collect(dir, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded:    abs(dir, "CLAUDE.md", "package.json", "notes.md"),
		Documents: abs(dir, "x.md"),
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// @~/ resolves against the home directory, which is why Collect is given one
// rather than reading the environment.
func TestCollectResolvesHomeImports(t *testing.T) {
	dir, home := t.TempDir(), t.TempDir()
	writeTree(t, dir, map[string]string{"CLAUDE.md": "@~/imported.md\n@~/gone.md\n"})
	writeTree(t, home, map[string]string{"imported.md": "[deeper](deeper.md)\n", "deeper.md": ""})

	got, err := Collect(dir, home)
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded:    append(abs(dir, "CLAUDE.md"), abs(home, "imported.md")...),
		Documents: abs(home, "deeper.md"),
		Warnings:  []Warning{{Target: "~/gone.md", Source: filepath.Join(dir, "CLAUDE.md")}},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// All three project instruction paths are roots, and a rule loads without any
// of them.
func TestCollectReadsEveryRoot(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"CLAUDE.md":                 "[one](one.md)\n",
		".claude/CLAUDE.md":         "[two](../two.md)\n",
		"CLAUDE.local.md":           "[three](three.md)\n",
		".claude/rules/unscoped.md": "[four](../../four.md)\n",
		"one.md":                    "",
		"two.md":                    "",
		"three.md":                  "",
		"four.md":                   "",
	})

	got, err := Collect(dir, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded: abs(dir, "CLAUDE.md", ".claude/CLAUDE.md", "CLAUDE.local.md", ".claude/rules/unscoped.md"),
		// Depth 1 in the order the depth-0 files were read.
		Documents: abs(dir, "one.md", "two.md", "three.md", "four.md"),
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// A rule saved with CRLF declares its paths field like any other, and a rule
// directory shared between projects by a symlink is still read.
func TestCollectReadsRulesWrittenTheOtherWays(t *testing.T) {
	dir, shared := t.TempDir(), t.TempDir()
	writeTree(t, shared, map[string]string{
		"unscoped.md": "[r](../../r.md)\n",
		"scoped.md":   "---\r\npaths:\r\n  - \"**/*.go\"\r\n---\r\n",
	})
	writeTree(t, dir, map[string]string{"CLAUDE.md": "[s](.claude/rules/scoped.md)\n"})
	if err := os.MkdirAll(filepath.Join(dir, ".claude"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(shared, filepath.Join(dir, ".claude", "rules")); err != nil {
		t.Fatal(err)
	}
	writeTree(t, dir, map[string]string{"r.md": ""})

	got, err := Collect(dir, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		// Named under .claude/rules, the spelling the project uses, rather
		// than under the directory the link points at.
		Loaded:    abs(dir, "CLAUDE.md", ".claude/rules/unscoped.md"),
		Documents: abs(dir, ".claude/rules/scoped.md", "r.md"),
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

func TestCollectOnRepositoriesWithNothingToWalk(t *testing.T) {
	tests := map[string]struct {
		files map[string]string
		// The only thing these cases vary: documents and warnings are empty
		// in all of them, which is the point.
		loaded []string
	}{
		// The 「対象なし」 signal: no project instruction file of any kind.
		"no instruction file at all": {files: map[string]string{"README.md": "[x](x.md)\n"}},
		// An empty documents list says nothing about the roots, which is why
		// loaded is what the skills branch on.
		"instructions with no links": {
			files:  map[string]string{"CLAUDE.md": "a mention of `docs/a.md` and nothing else\n"},
			loaded: []string{"CLAUDE.md"},
		},
		// A scoped rule is not loaded at launch, and nothing links it here.
		"only a scoped rule": {
			files: map[string]string{".claude/rules/scoped.md": "---\npaths:\n  - \"**/*.go\"\n---\n"},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			dir := t.TempDir()
			writeTree(t, dir, tt.files)

			got, err := Collect(dir, t.TempDir())
			if err != nil {
				t.Fatal(err)
			}

			want := Collection{Loaded: abs(dir, tt.loaded...)}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}
