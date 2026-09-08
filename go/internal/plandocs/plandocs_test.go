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

// Run from a subdirectory, the answer is the repository's, because that is
// what the session running there has in context. Its own instruction files
// are read last, after the ones above them.
func TestCollectFromASubdirectory(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		".git/HEAD":                        "ref: refs/heads/main\n",
		"CLAUDE.md":                        "[top](docs/top.md)\n",
		".claude/rules/unscoped.md":        "[r](../../docs/rule.md)\n",
		"go/CLAUDE.md":                     "[near](near.md)\n",
		"go/.claude/rules/unscoped.md":     "[gr](../../rule.md)\n",
		"docs/top.md":                      "",
		"docs/rule.md":                     "",
		"go/near.md":                       "",
		"go/rule.md":                       "",
		"outside-the-repository/CLAUDE.md": "[never](never.md)\n",
		"outside-the-repository/never.md":  "",
	})

	got, err := Collect(filepath.Join(dir, "go"), t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded: abs(dir,
			"CLAUDE.md", "go/CLAUDE.md",
			".claude/rules/unscoped.md", "go/.claude/rules/unscoped.md",
		),
		Documents: abs(dir, "docs/top.md", "go/near.md", "docs/rule.md", "go/rule.md"),
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
		// Both lists empty is the 「対象なし」 signal, and no project
		// instruction file of any kind is one way to reach it.
		"no instruction file at all": {files: map[string]string{"README.md": "[x](x.md)\n"}},
		// An empty documents list says nothing about the roots, which is why
		// neither list is the signal on its own.
		"instructions with no links": {
			files:  map[string]string{"CLAUDE.md": "a mention of `docs/a.md` and nothing else\n"},
			loaded: []string{"CLAUDE.md"},
		},
		// A scoped rule is not loaded at launch, nothing links it here, and no
		// path was given for it to match.
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

// scopedTree is the fixture the path cases below run against: a repository
// whose rules nobody links, so that the only way any of them reaches documents
// is through a given path.
func scopedTree(t *testing.T) (dir, home string) {
	t.Helper()
	dir, home = t.TempDir(), t.TempDir()
	writeTree(t, dir, map[string]string{
		".git/HEAD":                 "ref: refs/heads/main\n",
		"CLAUDE.md":                 "[a](docs/a.md)\n",
		"docs/a.md":                 "",
		".claude/rules/unscoped.md": "",
		".claude/rules/scoped.md":   "---\npaths:\n  - \"**/go/**\"\n---\n",
	})
	writeTree(t, home, map[string]string{
		// The user's own rules. The scoped one is matched against a given
		// path; the unscoped one is nobody's business here, since it belongs
		// to the user rather than to the project, and must not reach loaded.
		".claude/rules/user-scoped.md":   "---\npaths:\n  - \"**/go/**\"\n---\n",
		".claude/rules/user-unscoped.md": "",
	})
	return dir, home
}

// A given path is what makes a scoped rule a document: the project's and the
// user's alike, and only when the path is one their patterns match.
func TestCollectMatchesScopedRulesAgainstTheGivenPaths(t *testing.T) {
	dir, home := scopedTree(t)
	// The two trees are separate, so the want values are assembled from both.
	userRule := filepath.Join(home, ".claude", "rules", "user-scoped.md")

	tests := map[string]struct {
		paths     []string
		documents []string
	}{
		// The leading ** matches zero segments, which is what every rule in
		// the repository this serves relies on.
		"a path the patterns match": {
			paths:     []string{"go/internal/x.go"},
			documents: []string{filepath.Join(dir, ".claude", "rules", "scoped.md"), userRule},
		},
		"the same path spelled absolutely": {
			paths:     []string{filepath.Join(dir, "go", "internal", "x.go")},
			documents: []string{filepath.Join(dir, ".claude", "rules", "scoped.md"), userRule},
		},
		// A relative path is read from the top of the repository, not from the
		// working directory, and it need not exist to be matched.
		"a path the patterns do not match": {paths: []string{"docs/README.md"}},
		"a path outside the repository":    {paths: []string{"/somewhere/else/go/x.go"}},
		"no paths at all":                  {},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got, err := Collect(dir, home, tt.paths...)
			if err != nil {
				t.Fatal(err)
			}

			want := Collection{
				Loaded:    abs(dir, "CLAUDE.md", ".claude/rules/unscoped.md"),
				Documents: append(abs(dir, "docs/a.md"), tt.documents...),
			}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// The two corners of the documented pattern syntax a naive matcher gets wrong:
// a brace group is expanded, and a pattern that is not a glob at all matches
// nothing rather than failing the run.
func TestCollectMatchesTheDocumentedPatternSyntax(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		".git/HEAD":               "ref: refs/heads/main\n",
		".claude/rules/braces.md": "---\npaths:\n  - \"src/*.{ts,tsx}\"\n---\n",
		".claude/rules/broken.md": "---\npaths:\n  - \"photos [2024/**\"\n---\n",
	})

	tests := map[string]struct {
		path      string
		documents []string
	}{
		"a brace alternative":             {path: "src/a.tsx", documents: []string{".claude/rules/braces.md"}},
		"outside every alternative":       {path: "src/a.js"},
		"what the invalid pattern spells": {path: "photos [2024/x.png"},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got, err := Collect(dir, t.TempDir(), tt.path)
			if err != nil {
				t.Fatal(err)
			}

			want := Collection{Documents: abs(dir, tt.documents...)}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// One file reached under two spellings is one entry. This repository's own
// rules are the case: .claude/rules is a symlink into the checkout, so a rule
// is both a project rule and the user's, and a document may link it besides.
func TestCollectListsOneFileFoundTwiceOnce(t *testing.T) {
	dir, home := t.TempDir(), t.TempDir()
	writeTree(t, dir, map[string]string{
		".git/HEAD": "ref: refs/heads/main\n",
		// The link reaches the rule by its place in the checkout; the match
		// below reaches the same file through the user's symlinked directory.
		"CLAUDE.md":        "[l](shared/linked.md)\n",
		"shared/linked.md": "---\npaths:\n  - \"**/go/**\"\n---\n",
		"shared/other.md":  "---\npaths:\n  - \"**/go/**\"\n---\n",
	})
	if err := os.MkdirAll(filepath.Join(home, ".claude"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(filepath.Join(dir, "shared"), filepath.Join(home, ".claude", "rules")); err != nil {
		t.Fatal(err)
	}

	// An import puts a file in the loaded set whatever its frontmatter says,
	// so a scoped rule reached that way is already in context and must not be
	// listed to read.
	writeTree(t, dir, map[string]string{
		"CLAUDE.md":          "[l](shared/linked.md)\n@shared/imported.md\n",
		"shared/imported.md": "---\npaths:\n  - \"**/go/**\"\n---\n",
	})

	got, err := Collect(dir, home, "go/x.go")
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded: append(abs(dir, "CLAUDE.md"), filepath.Join(dir, "shared", "imported.md")),
		Documents: []string{
			// Listed by the link walk, and not a second time by the match
			// that reached the same file as ~/.claude/rules/linked.md.
			filepath.Join(dir, "shared", "linked.md"),
			// Named under the spelling it was found by, which says which
			// entry matched, rather than under the link's target.
			filepath.Join(home, ".claude", "rules", "other.md"),
		},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// A project rule is matched against the path made relative to the directory
// holding its own .claude/, which below the top is not the path the top's own
// rules are matched against.
func TestCollectMatchesEachProjectRuleAgainstItsOwnDirectory(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		".git/HEAD":                     "ref: refs/heads/main\n",
		".claude/rules/top.md":          "---\npaths:\n  - \"go/internal/**\"\n---\n",
		"go/.claude/rules/near.md":      "---\npaths:\n  - \"internal/**\"\n---\n",
		"go/.claude/rules/as-if-top.md": "---\npaths:\n  - \"go/internal/**\"\n---\n",
	})

	got, err := Collect(filepath.Join(dir, "go"), t.TempDir(), "go/internal/x.go")
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{Documents: abs(dir, ".claude/rules/top.md", "go/.claude/rules/near.md")}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}
