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

// A Markdown link is no reference, wherever it is written: in an instruction
// file, in a file one imports, in a document it links, or in a rule. Nothing
// it reaches is listed and a link to a file that is not there warns about
// nothing, while the imports beside the links are still expanded.
func TestCollectFollowsNoLink(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"CLAUDE.md": "@imported.md\n" +
			"[a](a.md)\n" +
			"[rule](.claude/rules/scoped.md)\n" +
			"[missing](missing.md)\n",
		"imported.md":               "[c](c.md)\n",
		"a.md":                      "[b](b.md)\n",
		"b.md":                      "",
		"c.md":                      "",
		".claude/rules/unscoped.md": "@../../x.md\n[y](../../y.md)\n",
		".claude/rules/scoped.md":   "---\npaths:\n  - \"**/*.go\"\n---\n",
		"x.md":                      "",
		"y.md":                      "",
	})

	got, err := Collect(dir, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	// x.md is absent too: the harness does not expand an import written in a
	// rule, so it is in context nowhere.
	want := Collection{Loaded: abs(dir, "CLAUDE.md", "imported.md", ".claude/rules/unscoped.md")}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// An import written in a file at the harness's four-hop limit is listed
// nowhere, because the harness stopped before expanding it.
func TestCollectStopsAtTheHopLimit(t *testing.T) {
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
		Loaded: abs(dir, "CLAUDE.md", "h1.md", "h2.md", "h3.md", "h4.md"),
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// The @ closure follows every import regardless of extension, as the harness
// does.
func TestCollectClosesOverImportsOfAnyExtension(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"CLAUDE.md":    "@package.json and @notes.md\n",
		"package.json": "{}\n",
		"notes.md":     "",
	})

	got, err := Collect(dir, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded: abs(dir, "CLAUDE.md", "package.json", "notes.md"),
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

// @~/ resolves against the home directory, which is why Collect is given one
// rather than reading the environment. A missing one written twice in a file
// is one warning.
func TestCollectResolvesHomeImports(t *testing.T) {
	dir, home := t.TempDir(), t.TempDir()
	writeTree(t, dir, map[string]string{"CLAUDE.md": "@~/imported.md\n@~/gone.md\n@~/gone.md\n"})
	writeTree(t, home, map[string]string{"imported.md": ""})

	got, err := Collect(dir, home)
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded:   append(abs(dir, "CLAUDE.md"), abs(home, "imported.md")...),
		Warnings: []Warning{{Target: "~/gone.md", Source: filepath.Join(dir, "CLAUDE.md")}},
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
		"CLAUDE.md":                 "",
		".claude/CLAUDE.md":         "",
		"CLAUDE.local.md":           "",
		".claude/rules/unscoped.md": "",
	})

	got, err := Collect(dir, t.TempDir())
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		Loaded: abs(dir, "CLAUDE.md", ".claude/CLAUDE.md", "CLAUDE.local.md", ".claude/rules/unscoped.md"),
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
		"unscoped.md": "",
		"scoped.md":   "---\r\npaths:\r\n  - \"**/*.go\"\r\n---\r\n",
	})
	writeTree(t, dir, map[string]string{".git/HEAD": "ref: refs/heads/main\n", "CLAUDE.md": ""})
	if err := os.MkdirAll(filepath.Join(dir, ".claude"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(shared, filepath.Join(dir, ".claude", "rules")); err != nil {
		t.Fatal(err)
	}

	got, err := Collect(dir, t.TempDir(), "x.go")
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		// Named under .claude/rules, the spelling the project uses, rather
		// than under the directory the link points at.
		Loaded: abs(dir, "CLAUDE.md", ".claude/rules/unscoped.md"),
		Rules:  abs(dir, ".claude/rules/scoped.md"),
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
		"CLAUDE.md":                        "",
		".claude/rules/unscoped.md":        "",
		"go/CLAUDE.md":                     "",
		"go/.claude/rules/unscoped.md":     "",
		"outside-the-repository/CLAUDE.md": "",
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
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}

func TestCollectOnRepositoriesWithNothingToList(t *testing.T) {
	tests := map[string]struct {
		files map[string]string
		// The only thing these cases vary: rules and warnings are empty
		// in all of them, which is the point.
		loaded []string
	}{
		// Both lists empty is the 「対象なし」 signal, and no project
		// instruction file of any kind is one way to reach it.
		"no instruction file at all": {files: map[string]string{"README.md": "[x](x.md)\n"}},
		// An empty rules list says nothing about the roots, which is why
		// neither list is the signal on its own.
		"instructions and no path": {
			files:  map[string]string{"CLAUDE.md": "see [a](docs/a.md) and `docs/b.md`\n"},
			loaded: []string{"CLAUDE.md"},
		},
		// A scoped rule is not loaded at launch, and no path was given for it
		// to match.
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

// scopedTree is the fixture the path cases below run against. Its CLAUDE.md
// links a document, which no case lists: a given path is the only way anything
// reaches rules.
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

// A given path is what makes a scoped rule one to read: the project's and the
// user's alike, and only when the path is one their patterns match.
func TestCollectMatchesScopedRulesAgainstTheGivenPaths(t *testing.T) {
	dir, home := scopedTree(t)
	// The two trees are separate, so the want values are assembled from both.
	userRule := filepath.Join(home, ".claude", "rules", "user-scoped.md")

	tests := map[string]struct {
		paths []string
		rules []string
	}{
		// The leading ** matches zero segments, which is what every rule in
		// the repository this serves relies on.
		"a path the patterns match": {
			paths: []string{"go/internal/x.go"},
			rules: []string{filepath.Join(dir, ".claude", "rules", "scoped.md"), userRule},
		},
		"the same path spelled absolutely": {
			paths: []string{filepath.Join(dir, "go", "internal", "x.go")},
			rules: []string{filepath.Join(dir, ".claude", "rules", "scoped.md"), userRule},
		},
		// One match is enough, and it is looked for in every path given
		// rather than only in the first.
		"several paths of which the last one matches": {
			paths: []string{"README.md", "docs/x.md", "go/internal/x.go"},
			rules: []string{filepath.Join(dir, ".claude", "rules", "scoped.md"), userRule},
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
				Loaded: abs(dir, "CLAUDE.md", ".claude/rules/unscoped.md"),
				Rules:  tt.rules,
			}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// The corners of the documented field a naive reader gets wrong: a brace group
// is expanded, and neither a pattern that is not a glob nor a value that is
// neither a string nor a list fails the run.
func TestCollectMatchesTheDocumentedPatternSyntax(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		".git/HEAD":               "ref: refs/heads/main\n",
		".claude/rules/braces.md": "---\npaths:\n  - \"src/*.{ts,tsx}\"\n---\n",
		".claude/rules/broken.md": "---\npaths:\n  - \"photos [2024/**\"\n---\n",
		// A paths key holding nothing usable is still a scoped rule, so it
		// stays out of loaded, and it has no pattern to match anything with.
		".claude/rules/empty.md":  "---\npaths: []\n---\n",
		".claude/rules/number.md": "---\npaths: 42\n---\n",
	})

	tests := map[string]struct {
		path  string
		rules []string
	}{
		"a brace alternative":             {path: "src/a.tsx", rules: []string{".claude/rules/braces.md"}},
		"outside every alternative":       {path: "src/a.js"},
		"what the invalid pattern spells": {path: "photos [2024/x.png"},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got, err := Collect(dir, t.TempDir(), tt.path)
			if err != nil {
				t.Fatal(err)
			}

			want := Collection{Rules: abs(dir, tt.rules...)}
			if diff := cmp.Diff(want, got); diff != "" {
				t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// Every row of the documentation's pattern table, each with a path it matches
// and one it does not. A leading slash anchors a pattern to the basis, and a
// pattern with no slash matches at any depth below it.
func TestCollectMatchesTheDocumentationTable(t *testing.T) {
	tests := map[string]struct {
		matches []string
		misses  []string
	}{
		"**/*.ts":              {matches: []string{"sub/a.ts"}, misses: []string{"sub/a.js"}},
		"src/**/*":             {matches: []string{"src/x/a.go"}, misses: []string{"lib/a.go"}},
		"*.md":                 {matches: []string{"README.md", "sub/c.md"}, misses: []string{"sub/c.txt"}},
		"/*.md":                {matches: []string{"README.md"}, misses: []string{"sub/c.md"}},
		"src/components/*.tsx": {matches: []string{"src/components/a.tsx"}, misses: []string{"src/components/x/a.tsx"}},
	}
	for pattern, tt := range tests {
		t.Run(pattern, func(t *testing.T) {
			dir := t.TempDir()
			writeTree(t, dir, map[string]string{
				".git/HEAD":          "ref: refs/heads/main\n",
				".claude/rules/r.md": "---\npaths:\n  - \"" + pattern + "\"\n---\n",
			})
			assertRuleMatches(t, dir, tt.matches, tt.misses)
		})
	}
}

// A string value is a comma-separated list of patterns, and so is each entry of
// a list. The split is at a comma outside a brace group, since the comma inside
// one separates the alternatives of a single pattern.
func TestCollectReadsTheCommaSeparatedForm(t *testing.T) {
	tests := map[string]struct {
		paths   string
		matches []string
		misses  []string
	}{
		"a string of one pattern": {
			paths:   `"src/**"`,
			matches: []string{"src/a.ts"},
			misses:  []string{"lib/b.ts"},
		},
		"a string of several patterns": {
			paths:   `"src/**, lib/**"`,
			matches: []string{"src/a.ts", "lib/b.ts"},
			misses:  []string{"docs/c.md"},
		},
		"no space after the comma": {
			paths:   `"src/**,lib/**"`,
			matches: []string{"src/a.ts", "lib/b.ts"},
			misses:  []string{"docs/c.md"},
		},
		"a brace group beside a comma": {
			paths:   `"src/*.{ts,tsx}, lib/**"`,
			matches: []string{"src/a.tsx", "lib/b.ts"},
			misses:  []string{"src/a.js"},
		},
		"a comma inside a list entry": {
			paths:   "\n  - \"src/**, lib/**\"",
			matches: []string{"src/a.ts", "lib/b.ts"},
			misses:  []string{"docs/c.md"},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			dir := t.TempDir()
			writeTree(t, dir, map[string]string{
				".git/HEAD":          "ref: refs/heads/main\n",
				".claude/rules/r.md": "---\npaths: " + tt.paths + "\n---\n",
			})
			assertRuleMatches(t, dir, tt.matches, tt.misses)
		})
	}
}

// assertRuleMatches checks that the fixture's one rule, .claude/rules/r.md, is
// listed for every path in matches and for none in misses.
func assertRuleMatches(t *testing.T, dir string, matches, misses []string) {
	t.Helper()
	for _, c := range []struct {
		paths []string
		want  Collection
	}{
		{matches, Collection{Rules: abs(dir, ".claude/rules/r.md")}},
		{misses, Collection{}},
	} {
		for _, path := range c.paths {
			got, err := Collect(dir, t.TempDir(), path)
			if err != nil {
				t.Fatal(err)
			}
			if diff := cmp.Diff(c.want, got); diff != "" {
				t.Errorf("Collect(%q) mismatch (-want +got):\n%s", path, diff)
			}
		}
	}
}

// One file reached under two spellings is one entry. This repository's own
// rules are the case: .claude/rules is a symlink into the checkout, so a rule
// is both a project rule and the user's, and an instruction file may import it
// besides.
func TestCollectListsOneFileFoundTwiceOnce(t *testing.T) {
	dir, home := t.TempDir(), t.TempDir()
	writeTree(t, dir, map[string]string{
		".git/HEAD": "ref: refs/heads/main\n",
		// CLAUDE.md reaches two rules by their place in the checkout, where
		// the match below reaches the same files through the user's symlinked
		// directory. The import puts its rule in the loaded set whatever its
		// frontmatter says; the link puts its rule nowhere.
		"CLAUDE.md":          "[l](shared/linked.md)\n@shared/imported.md\n",
		"shared/linked.md":   "---\npaths:\n  - \"**/go/**\"\n---\n",
		"shared/imported.md": "---\npaths:\n  - \"**/go/**\"\n---\n",
		"shared/other.md":    "---\npaths:\n  - \"**/go/**\"\n---\n",
	})
	if err := os.MkdirAll(filepath.Join(home, ".claude"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(filepath.Join(dir, "shared"), filepath.Join(home, ".claude", "rules")); err != nil {
		t.Fatal(err)
	}

	got, err := Collect(dir, home, "go/x.go")
	if err != nil {
		t.Fatal(err)
	}

	want := Collection{
		// The imported rule is already in context, so the match that reached
		// it as ~/.claude/rules/imported.md lists nothing.
		Loaded: abs(dir, "CLAUDE.md", "shared/imported.md"),
		// Named under the spelling the match found them by, which says which
		// entry matched: the link naming linked.md is no earlier spelling of
		// it.
		Rules: []string{
			filepath.Join(home, ".claude", "rules", "linked.md"),
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

	want := Collection{Rules: abs(dir, ".claude/rules/top.md", "go/.claude/rules/near.md")}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Collect() mismatch (-want +got):\n%s", diff)
	}
}
