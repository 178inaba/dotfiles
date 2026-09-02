package ghshim

import (
	"flag"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

var update = flag.Bool("update", false, "update .golden files")

// The cases that do not belong to the decision are in resolve_test.go (finding
// the real gh) and ghshim_test.go (the hand-off and the fail-closed net).
//
// -update generates the goldens from this package, and the guidance is compared
// in full, so changing a word of it takes a deliberate regeneration. That is
// what they are for.

const (
	// The values the goldens hold placeholders for. Decide takes them as
	// functions, so a test can pin them.
	fixtureDir    = "/fixture/repo"
	fixtureRemote = "git@github.com:owner/repo.git"
)

// multiline is the two-line body the second rule fires on. The shell built it
// with $(printf 'line1\nline2'), whose trailing newline the substitution ate.
const multiline = "line1\nline2"

// bodyFixtures writes the body files the shell suite kept in $BODY_DIR and
// returns the directory. The unreadable one is left for unreadableBody, which
// the test skips as root.
func bodyFixtures(t *testing.T) string {
	t.Helper()

	return writeFixtures(t, map[string]string{
		"hash-numbering.md":  "- #1 foo\n- #2 bar\n- #3 baz\n",
		"ordered-list.md":    "1. foo\n2. bar\n3. baz\n",
		"two-distinct.md":    "see #1 and #2 and #2\n",
		"backtick-refs.md":   "- `#1` foo\n- `#2` bar\n- `#3` baz\n",
		"fenced-refs.md":     "before\n```\n#1 #2 #3\n```\nafter\n",
		"cross-repo-refs.md": "- foo/bar#1 x\n- foo/bar#2 y\n- foo/bar#3 z\n",
		"multi-digit.md":     "refs #123 #456 #789\n",
		"alnum-suffix.md":    "colors #1a2b3c and #2f4f4f, place #3rd\n",

		"quoted-closes.md":             "Related\n\n`Closes #656`\n",
		"fenced-closes.md":             "before\n```\ncloses #656\n```\nafter\n",
		"quoted-cross-repo-closes.md":  "see `Resolves foo/bar#12` here\n",
		"raw-closes.md":                "Closes #656\n",
		"quoted-placeholder-closes.md": "docs update: `Closes #N` placeholder\n",
		"quoted-closes-no-ref.md":      "call `closes the stream` explicitly\n",
		"quoted-discloses.md":          "word `discloses #656` here\n",
	})
}

// writeFixtures lays out one temporary directory of files a test names on a
// command line.
func writeFixtures(t *testing.T, files map[string]string) string {
	t.Helper()

	dir := t.TempDir()
	for name, content := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(content), 0o644); err != nil {
			t.Fatalf("WriteFile(%q): %v", name, err)
		}
	}
	return dir
}

// unreadableBody writes a body file nothing can read. Root can read it anyway,
// so the caller skips there, as the shell suite did.
func unreadableBody(t *testing.T, dir string) string {
	t.Helper()

	path := filepath.Join(dir, "unreadable.md")
	if err := os.WriteFile(path, []byte("body\n"), 0o000); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	return path
}

// testEnv is the environment the goldens are generated under.
func testEnv() Env {
	return Env{
		ClaudeCode:   "1",
		Dir:          func() string { return fixtureDir },
		OriginRemote: func() string { return fixtureRemote },
	}
}

type decideCase struct {
	name string
	argv []string
	// ghRepo is GH_REPO; noClaudeCode unsets CLAUDECODE.
	ghRepo       string
	noClaudeCode bool
	// block is what the decision must be.
	block bool
	// golden names a file under testdata whose content the whole message must
	// equal, with the placeholders resolved. Only meaningful when block.
	golden string
}

func runDecideCases(t *testing.T, bodies string, cases []decideCase) {
	t.Helper()

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			env := testEnv()
			env.GHRepo = tc.ghRepo
			if tc.noClaudeCode {
				env.ClaudeCode = ""
			}

			got := Decide(tc.argv, env)
			if tc.block != (got != nil) {
				t.Fatalf("Decide = %v, want blocked: %v", got, tc.block)
			}
			if tc.golden == "" {
				return
			}

			want := wantGolden(t, tc.golden, bodies, got.Message)
			if diff := cmp.Diff(want, got.Message); diff != "" {
				t.Errorf("message differs from %s.golden (re-run with -update) (-want +got):\n%s", tc.golden, diff)
			}
		})
	}
}

// wantGolden is the expected message, with the three values that vary per run
// put back. The file holds placeholders for them so that it is the same on
// every machine.
//
// Every golden comparison goes through here, so -update reaches all of them
// from this one place.
func wantGolden(t *testing.T, name, bodies, got string) string {
	t.Helper()

	fromGolden, toGolden := replacers(bodies)

	path := filepath.Join("testdata", name+".golden")
	if *update {
		if err := os.WriteFile(path, []byte(toGolden.Replace(got)), 0o644); err != nil {
			t.Fatalf("WriteFile(%q): %v", path, err)
		}
	}

	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	return fromGolden.Replace(string(b))
}

// replacers derives both directions of the placeholder table from one list, so
// that a fourth value cannot be added to one direction alone — which would
// write a golden that only matches on the machine that wrote it.
//
// bodies is empty for the messages that name no body file. An empty pattern
// would have the replacer insert at every position, so that pair is left out
// rather than passed through.
func replacers(bodies string) (fromGolden, toGolden *strings.Replacer) {
	pairs := [][2]string{
		{"{{CWD}}", fixtureDir},
		{"{{REMOTE}}", fixtureRemote},
	}
	if bodies != "" {
		pairs = append(pairs, [2]string{"{{BODY_DIR}}", bodies})
	}

	var forward, reverse []string
	for _, p := range pairs {
		forward = append(forward, p[0], p[1])
		reverse = append(reverse, p[1], p[0])
	}
	return strings.NewReplacer(forward...), strings.NewReplacer(reverse...)
}

func TestDecideReadSubcommands(t *testing.T) {
	t.Parallel()

	runDecideCases(t, "", []decideCase{
		{name: "read: pr view", argv: []string{"pr", "view", "1"}},
		{name: "read: issue list", argv: []string{"issue", "list"}},
		{name: "read: repo clone", argv: []string{"repo", "clone", "foo/bar"}},
		{name: "read: pr view without CLAUDECODE", argv: []string{"pr", "view", "1"}, noClaudeCode: true},
		{name: "excluded: repo create", argv: []string{"repo", "create", "foo/bar", "--public"}},
		{name: "excluded: repo fork", argv: []string{"repo", "fork", "foo/bar"}},

		// Too few arguments to name a noun and a verb: gh gets them as they are.
		{name: "no arguments", argv: nil},
		{name: "noun only", argv: []string{"issue"}},
		{name: "version flag", argv: []string{"--version"}},
	})
}

func TestDecideRepositoryExplicitness(t *testing.T) {
	t.Parallel()

	runDecideCases(t, "", []decideCase{
		// Blocked: the repository is not on the command line.
		{name: "issue create without -R", argv: []string{"issue", "create", "--title", "x", "--body", "y"}, block: true, golden: "rule1-issue-create"},
		{name: "pr create without -R", argv: []string{"pr", "create", "--title", "x", "--body", "y"}, block: true},
		{name: "issue comment with bare number", argv: []string{"issue", "comment", "1", "--body", "x"}, block: true, golden: "rule1-issue-selector"},
		{name: "pr comment with bare number", argv: []string{"pr", "comment", "55", "--body", "x"}, block: true, golden: "rule1-pr-selector"},
		{name: "pr edit with branch selector", argv: []string{"pr", "edit", "feature/54-add-eli5-mode", "--body", "x"}, block: true},
		{name: "pr merge without -R", argv: []string{"pr", "merge", "5", "--squash"}, block: true},
		{name: "release create without -R", argv: []string{"release", "create", "v1", "--title", "v1"}, block: true, golden: "rule1-release"},
		{name: "label create without -R", argv: []string{"label", "create", "bug", "--color", "FF0000"}, block: true, golden: "rule1-label"},
		{name: "repo edit without a positional", argv: []string{"repo", "edit", "--description", "x"}, block: true, golden: "rule1-repo-positional"},
		{name: "repo edit with a bare name", argv: []string{"repo", "edit", "dotfiles", "--description", "x"}, block: true},
		{name: "repo rename without -R", argv: []string{"repo", "rename", "new-name"}, block: true, golden: "rule1-repo-rename"},
		{name: "repo rename with OWNER/REPO as the new name", argv: []string{"repo", "rename", "178inaba/dotfiles"}, block: true},

		// Allowed: the repository is named.
		{name: "issue create with -R", argv: []string{"issue", "create", "-R", "foo/bar", "--title", "x", "--body", "y"}},
		{name: "pr create with --repo", argv: []string{"pr", "create", "--repo", "foo/bar", "--title", "x"}},
		{name: "pr comment with --repo=", argv: []string{"pr", "comment", "--repo=foo/bar", "1", "--body", "x"}},
		{name: "issue comment with -R attached", argv: []string{"issue", "comment", "-Rfoo/bar", "1", "--body", "x"}},
		{name: "repo edit with OWNER/REPO", argv: []string{"repo", "edit", "178inaba/dotfiles", "--description", "x"}},
		{name: "repo edit with HOST/OWNER/REPO", argv: []string{"repo", "edit", "github.com/178inaba/dotfiles", "--description", "x"}},
		{name: "repo edit with a repository URL", argv: []string{"repo", "edit", "https://github.com/178inaba/dotfiles", "--description", "x"}},
		{name: "repo rename with -R", argv: []string{"repo", "rename", "new-name", "-R", "178inaba/dotfiles"}},
		{name: "issue close with an issue URL", argv: []string{"issue", "close", "https://github.com/178inaba/dotfiles/issues/59"}},
		{name: "pr comment with a PR URL", argv: []string{"pr", "comment", "https://github.com/178inaba/dotfiles/pull/55", "--body", "x"}},
		{name: "release create with -R", argv: []string{"release", "create", "v1", "-R", "foo/bar", "--title", "v1"}},

		// A positional may follow the flags.
		{name: "repo delete with a flag before the positional", argv: []string{"repo", "delete", "--yes", "178inaba/dotfiles"}},
		{name: "repo delete with a flag before a bare name", argv: []string{"repo", "delete", "--yes", "dotfiles"}, block: true},
		{name: "issue close with a value flag before the URL", argv: []string{"issue", "close", "-c", "done", "https://github.com/178inaba/dotfiles/issues/59"}},

		// A value must not be mistaken for the positional.
		{name: "repo sync does not read --source as the target", argv: []string{"repo", "sync", "-s", "178inaba/dotfiles", "dotfiles"}, block: true},
		{name: "repo sync with an explicit target", argv: []string{"repo", "sync", "-s", "178inaba/upstream", "178inaba/dotfiles"}},
		{name: "repo edit does not read --homepage as the target", argv: []string{"repo", "edit", "--homepage", "https://github.com/178inaba/dotfiles", "--description", "x"}, block: true},

		// A long option with no name is gh's "bad flag syntax", and it names no
		// repository — reading its value as the positional would let one
		// through on an argv that cannot run.
		{name: "an empty long name is not the positional", argv: []string{"repo", "edit", "--=178inaba/dotfiles"}, block: true},

		// -- ends the flags.
		{name: "positional after --", argv: []string{"repo", "edit", "--", "178inaba/dotfiles"}},
		{name: "-R after -- is a positional, not explicitness", argv: []string{"issue", "create", "--", "-R", "foo/bar"}, block: true},

		// Help right after the verb is not a write.
		{name: "repo edit --help", argv: []string{"repo", "edit", "--help"}},
		{name: "pr create -h", argv: []string{"pr", "create", "-h"}},
		{name: "repo edit -h is --homepage, not help", argv: []string{"repo", "edit", "-h", "https://example.com", "--description", "x"}, block: true},

		// GH_REPO reaches gh as an environment variable, never in the argv.
		{name: "GH_REPO covers issue create", argv: []string{"issue", "create", "--title", "x", "--body", "y"}, ghRepo: "foo/bar"},
		{name: "GH_REPO covers repo rename", argv: []string{"repo", "rename", "new-name"}, ghRepo: "foo/bar"},
		{name: "GH_REPO does not cover repo edit", argv: []string{"repo", "edit", "--description", "x"}, ghRepo: "foo/bar", block: true},
		{name: "empty GH_REPO is not explicitness", argv: []string{"issue", "create", "--title", "x"}, block: true},

		// Without CLAUDECODE nothing is judged.
		{name: "no CLAUDECODE: issue create", argv: []string{"issue", "create", "--title", "x", "--body", "y"}, noClaudeCode: true},
		{name: "no CLAUDECODE: repo rename", argv: []string{"repo", "rename", "new-name"}, noClaudeCode: true},
		{name: "no CLAUDECODE: repo delete with a bare name", argv: []string{"repo", "delete", "--yes", "dotfiles"}, noClaudeCode: true},
	})
}

func TestDecideInlineBody(t *testing.T) {
	t.Parallel()

	runDecideCases(t, "", []decideCase{
		{name: "multiline --body=", argv: []string{"pr", "edit", "-R", "foo/bar", "1", "--body=" + multiline}, block: true},
		{name: "multiline -b", argv: []string{"pr", "create", "-R", "foo/bar", "--title", "x", "-b", multiline}, block: true},
		{name: "multiline -b attached", argv: []string{"issue", "comment", "-R", "foo/bar", "1", "-b" + multiline}, block: true},
		// pflag accepts an attached = on a short flag too.
		{name: "multiline -b=", argv: []string{"issue", "comment", "-R", "foo/bar", "1", "-b=" + multiline}, block: true},
		// Both rules are broken; the first one answers. The order is behaviour.
		{name: "rule 1 wins over rule 2", argv: []string{"pr", "edit", "1", "--body", multiline}, block: true, golden: "rule1-pr-selector-multiline"},
		{name: "single-line --body", argv: []string{"pr", "edit", "-R", "foo/bar", "1", "--body", "line1"}},

		// A multi-line value that is not a body is not a body.
		{name: "multiline value in --title", argv: []string{"pr", "create", "-R", "foo/bar", "--title", multiline, "--body", "x"}},
		{name: "issue develop: -b is --base, not --body", argv: []string{"issue", "develop", "-R", "foo/bar", "1", "-b", multiline}},

		// close and reopen carry their body in -c, which has no file form.
		{name: "issue close: multiline -c", argv: []string{"issue", "close", "-R", "foo/bar", "1", "-c", multiline}, block: true, golden: "rule2-issue-close"},
		{name: "pr close: multiline -c", argv: []string{"pr", "close", "-R", "foo/bar", "1", "-c", multiline}, block: true, golden: "rule2-pr-close"},

		{name: "message: rule 2 points at --body-file", argv: []string{"pr", "edit", "-R", "foo/bar", "1", "--body", multiline}, block: true, golden: "rule2-body-file"},
		{name: "message: rule 2 points release at --notes-file", argv: []string{"release", "create", "v1", "-R", "foo/bar", "--title", "v1", "--notes", multiline}, block: true, golden: "rule2-notes-file"},
		{name: "release: multiline -n", argv: []string{"release", "create", "v1", "-R", "foo/bar", "--title", "v1", "-n", multiline}, block: true},
		// The echo of the command quotes its arguments the way bash's printf %q
		// did, so an argument with a space stays re-runnable.
		{name: "message: the echoed command is quoted", argv: []string{"pr", "edit", "-R", "foo/bar", "--title", "a b", "--body", multiline}, block: true, golden: "quoting"},

		// Without CLAUDECODE nothing is judged.
		{name: "no CLAUDECODE: multiline --body", argv: []string{"pr", "edit", "-R", "foo/bar", "1", "--body", multiline}, noClaudeCode: true},
		{name: "no CLAUDECODE: multiline -c", argv: []string{"issue", "close", "-R", "foo/bar", "1", "-c", multiline}, noClaudeCode: true},
	})
}

func TestDecideBodyFile(t *testing.T) {
	t.Parallel()

	bodies := bodyFixtures(t)
	at := func(name string) string { return filepath.Join(bodies, name) }

	cases := []decideCase{
		// Every spelling of the flag has to reach the same file.
		{name: "body-file: -F short flag, bare #N", argv: []string{"issue", "create", "-R", "foo/bar", "--title", "x", "-F", at("hash-numbering.md")}, block: true},
		{name: "body-file: --body-file= form", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file=" + at("hash-numbering.md")}, block: true},
		{name: "body-file: -F= form", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "-F=" + at("hash-numbering.md")}, block: true},
		{name: "body-file: -F attached form", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "-F" + at("hash-numbering.md")}, block: true},

		// An unreadable named path is a refusal, and the reason is named.
		{name: "body-file: nonexistent path", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("missing.md")}, block: true, golden: "bodyfile-missing"},
		{name: "body-file: a directory is not a regular file", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", bodies}, block: true, golden: "bodyfile-not-regular"},
		{name: "body-file: notes-file names its own flag", argv: []string{"release", "create", "v1", "-R", "foo/bar", "--title", "v1", "--notes-file", at("missing.md")}, block: true, golden: "bodyfile-notes-missing"},
		// The one carve-out: the shim cannot read stdin without eating what gh
		// is about to read, so it gives up on the scan instead.
		{name: "body-file: the stdin spelling is fail open", argv: []string{"issue", "comment", "-R", "foo/bar", "1", "--body-file", "-"}},

		// A flag written last with nothing left to take carries no value, so it
		// cannot erase the body the same flag named earlier. gh rejects the
		// argv either way; what this pins is that the scan does not fall open
		// on the way there.
		{name: "body-file: a trailing -F does not erase the file already named", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("hash-numbering.md"), "-F"}, block: true},

		{name: "no CLAUDECODE: unreadable body file", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("missing.md")}, noClaudeCode: true},
	}
	runDecideCases(t, bodies, cases)

	if os.Getuid() == 0 {
		t.Log("running as root, which can read a file with no permission bits")
		return
	}
	unreadable := unreadableBody(t, bodies)
	runDecideCases(t, bodies, []decideCase{
		{name: "body-file: no read permission", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", unreadable}, block: true, golden: "bodyfile-unreadable"},
	})
}

func TestDecideBareHashRefs(t *testing.T) {
	t.Parallel()

	bodies := bodyFixtures(t)
	at := func(name string) string { return filepath.Join(bodies, name) }

	runDecideCases(t, bodies, []decideCase{
		{name: "message: rule 3 reports the distinct count", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("hash-numbering.md")}, block: true, golden: "rule3-body-file"},
		{name: "inline --body: bare #N", argv: []string{"issue", "comment", "-R", "foo/bar", "1", "--body", "fix #1, #2, #3"}, block: true, golden: "rule3-inline"},

		// Forms GitHub does not link, and forms that look like real references.
		{name: "body-file: ordered list numbering", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("ordered-list.md")}},
		{name: "body-file: only 2 distinct #N", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("two-distinct.md")}},
		{name: "body-file: #N in backticks", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("backtick-refs.md")}},
		{name: "body-file: #N in a fenced code block", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("fenced-refs.md")}},
		{name: "body-file: OWNER/REPO#N form", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("cross-repo-refs.md")}},
		{name: "body-file: multi-digit #N only", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("multi-digit.md")}},
		{name: "body-file: hex color / ordinal #N", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("alnum-suffix.md")}},

		// Release notes render as markdown too, and -F means --notes-file there.
		{name: "release: -F is --notes-file, and is scanned", argv: []string{"release", "create", "v1", "-R", "foo/bar", "--title", "v1", "-F", at("hash-numbering.md")}, block: true},
		{name: "release: bare #N in --notes", argv: []string{"release", "create", "v1", "-R", "foo/bar", "--title", "v1", "--notes", "fix #1, #2, #3"}, block: true},
		{name: "release: ordered list numbering in --notes-file", argv: []string{"release", "create", "v1", "-R", "foo/bar", "--title", "v1", "--notes-file", at("ordered-list.md")}},
		{name: "issue reopen: bare #N in --comment", argv: []string{"issue", "reopen", "-R", "foo/bar", "1", "--comment", "fix #1, #2, #3"}, block: true},

		// Plain text renders no autolinks, so those flags are not bodies.
		{name: "label create: --description is not a rendered body", argv: []string{"label", "create", "bug", "-R", "foo/bar", "--color", "FF0000", "--description", "fix #1, #2, #3"}},
		{name: "repo edit: --description is not a rendered body", argv: []string{"repo", "edit", "foo/bar", "--description", "fix #1, #2, #3"}},

		{name: "no CLAUDECODE: bare #N numbering", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("hash-numbering.md")}, noClaudeCode: true},
		{name: "no CLAUDECODE: bare #N in --notes", argv: []string{"release", "create", "v1", "-R", "foo/bar", "--title", "v1", "--notes", "fix #1, #2, #3"}, noClaudeCode: true},
	})
}

func TestDecideQuotedClosingKeyword(t *testing.T) {
	t.Parallel()

	bodies := bodyFixtures(t)
	at := func(name string) string { return filepath.Join(bodies, name) }

	runDecideCases(t, bodies, []decideCase{
		{name: "pr create: quoted Closes #N", argv: []string{"pr", "create", "-R", "foo/bar", "--title", "x", "--body-file", at("quoted-closes.md")}, block: true, golden: "rule4-body-file"},
		{name: "pr edit: fenced closes #N", argv: []string{"pr", "edit", "-R", "foo/bar", "1", "--body-file", at("fenced-closes.md")}, block: true},
		{name: "pr create: quoted cross-repo Resolves", argv: []string{"pr", "create", "-R", "foo/bar", "--title", "x", "--body-file", at("quoted-cross-repo-closes.md")}, block: true},
		{name: "pr edit inline --body: quoted Fixes #N", argv: []string{"pr", "edit", "-R", "foo/bar", "1", "--body", "see `Fixes #12` here"}, block: true, golden: "rule4-inline"},

		{name: "pr create: raw Closes #N", argv: []string{"pr", "create", "-R", "foo/bar", "--title", "x", "--body-file", at("raw-closes.md")}},
		{name: "pr create: quoted placeholder Closes #N", argv: []string{"pr", "create", "-R", "foo/bar", "--title", "x", "--body-file", at("quoted-placeholder-closes.md")}},
		{name: "pr edit: quoted closes without a #ref", argv: []string{"pr", "edit", "-R", "foo/bar", "1", "--body-file", at("quoted-closes-no-ref.md")}},
		{name: "pr edit: quoted discloses (word boundary)", argv: []string{"pr", "edit", "-R", "foo/bar", "1", "--body-file", at("quoted-discloses.md")}},

		// The keyword only works in a pull request body.
		{name: "issue create: quoted Closes #N is out of scope", argv: []string{"issue", "create", "-R", "foo/bar", "--title", "x", "--body-file", at("quoted-closes.md")}},
		{name: "pr comment: quoted Closes #N is out of scope", argv: []string{"pr", "comment", "-R", "foo/bar", "1", "--body-file", at("quoted-closes.md")}},

		{name: "no CLAUDECODE: quoted Closes #N", argv: []string{"pr", "create", "-R", "foo/bar", "--title", "x", "--body-file", at("quoted-closes.md")}, noClaudeCode: true},
	})
}

// TestDecideEveryBodyFlagIsScanned is the drift check the shell suite ran as
// assert_body_is_scanned. Registering a body flag is not enough on its own: the
// argv walk only records a value whose spelling is also registered as taking
// one, so a half-updated pair would skip that verb's body scan without a word.
// Every registered noun and verb goes through both rules once.
func TestDecideEveryBodyFlagIsScanned(t *testing.T) {
	t.Parallel()

	bodies := bodyFixtures(t)
	numbering := filepath.Join(bodies, "hash-numbering.md")

	// The spellings differ per verb, and some verbs have only one of the two.
	for _, tt := range []struct {
		name       string
		prefix     []string
		inlineFlag string
		fileFlag   string
	}{
		{name: "issue create", prefix: []string{"issue", "create", "-R", "foo/bar", "--title", "x"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "issue comment", prefix: []string{"issue", "comment", "-R", "foo/bar", "1"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "issue edit", prefix: []string{"issue", "edit", "-R", "foo/bar", "1"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "issue close", prefix: []string{"issue", "close", "-R", "foo/bar", "1"}, inlineFlag: "--comment"},
		{name: "issue reopen", prefix: []string{"issue", "reopen", "-R", "foo/bar", "1"}, inlineFlag: "--comment"},
		{name: "pr create", prefix: []string{"pr", "create", "-R", "foo/bar", "--title", "x"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "pr comment", prefix: []string{"pr", "comment", "-R", "foo/bar", "1"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "pr edit", prefix: []string{"pr", "edit", "-R", "foo/bar", "1"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "pr close", prefix: []string{"pr", "close", "-R", "foo/bar", "1"}, inlineFlag: "--comment"},
		{name: "pr reopen", prefix: []string{"pr", "reopen", "-R", "foo/bar", "1"}, inlineFlag: "--comment"},
		{name: "pr merge", prefix: []string{"pr", "merge", "-R", "foo/bar", "1"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "pr review", prefix: []string{"pr", "review", "-R", "foo/bar", "1"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "pr revert", prefix: []string{"pr", "revert", "-R", "foo/bar", "1"}, inlineFlag: "--body", fileFlag: "--body-file"},
		{name: "release create", prefix: []string{"release", "create", "v1", "-R", "foo/bar"}, inlineFlag: "--notes", fileFlag: "--notes-file"},
		{name: "release edit", prefix: []string{"release", "edit", "v1", "-R", "foo/bar"}, inlineFlag: "--notes", fileFlag: "--notes-file"},
	} {
		var cases []decideCase
		if tt.inlineFlag != "" {
			cases = append(cases, decideCase{
				name:  "body reaches rule 2: " + tt.name,
				argv:  append(append([]string(nil), tt.prefix...), tt.inlineFlag, multiline),
				block: true,
			})
		}
		if tt.fileFlag != "" {
			cases = append(cases, decideCase{
				name:  "body reaches rule 3: " + tt.name,
				argv:  append(append([]string(nil), tt.prefix...), tt.fileFlag, numbering),
				block: true,
			})
		}
		runDecideCases(t, bodies, cases)
	}
}

// The two mutations the fifth rule refuses, written as a query would spell
// them. unresolveMutation is the near miss: the parent issue leaves it out of
// scope, so it is what holds the identifier match to whole words.
const (
	resolveMutation   = `mutation { resolveReviewThread(input:{threadId:"x"}) { thread { isResolved } } }`
	replyMutation     = `mutation { addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:"x", body:"y"}) { comment { id } } }`
	unresolveMutation = `mutation { unresolveReviewThread(input:{threadId:"x"}) { thread { isResolved } } }`
)

// apiFixtures writes the query files the fifth rule reads and returns the
// directory. They are separate from bodyFixtures because no golden names one:
// every message the api cases compare in full is written with an argv that
// holds no temporary path.
func apiFixtures(t *testing.T) string {
	t.Helper()

	return writeFixtures(t, map[string]string{
		"reply.graphql": replyMutation + "\n",
		"request.json":  `{"query": ` + strconv.Quote(resolveMutation) + `}`,
		// JSON that parses and holds no query, and a query that holds a
		// mutation and is not JSON: neither contributes any query text.
		"variables.json": `{"variables": {"threadId": "x"}}`,
		"raw.graphql":    resolveMutation + "\n",
	})
}

func TestDecideAPIGraphQLThreadMutations(t *testing.T) {
	t.Parallel()

	queries := apiFixtures(t)
	at := func(name string) string { return filepath.Join(queries, name) }

	runDecideCases(t, "", []decideCase{
		// Blocked: the query holds one of the two mutations.
		{name: "graphql: inline -f query", argv: []string{"api", "graphql", "-f", "query=" + resolveMutation}, block: true, golden: "rule5-graphql"},
		{name: "graphql: --raw-field spelling", argv: []string{"api", "graphql", "--raw-field", "query=" + replyMutation}, block: true},
		{name: "graphql: -F reads the file the @ names", argv: []string{"api", "graphql", "-F", "query=@" + at("reply.graphql")}, block: true},
		{name: "graphql: --field spelling", argv: []string{"api", "graphql", "--field", "query=@" + at("reply.graphql")}, block: true},
		{name: "graphql: -F= attached form", argv: []string{"api", "graphql", "-F=query=@" + at("reply.graphql")}, block: true},
		{name: "graphql: --input JSON", argv: []string{"api", "graphql", "--input", at("request.json")}, block: true},
		{name: "graphql: --input= attached form", argv: []string{"api", "graphql", "--input=" + at("request.json")}, block: true},

		// The same endpoint under its other spellings.
		{name: "graphql: a leading slash", argv: []string{"api", "/graphql", "-f", "query=" + resolveMutation}, block: true},
		{name: "graphql: the full URL", argv: []string{"api", "https://api.github.com/graphql", "-f", "query=" + resolveMutation}, block: true},
		{name: "graphql: an enterprise URL", argv: []string{"api", "https://ghe.example.com/api/graphql", "-f", "query=" + resolveMutation}, block: true},

		// An input that cannot be read is refused rather than passed.
		{name: "graphql: --input names no file", argv: []string{"api", "graphql", "--input", "missing.json"}, block: true, golden: "rule5-query-file"},
		{name: "graphql: -F query=@ names no file", argv: []string{"api", "graphql", "-F", "query=@missing.graphql"}, block: true},
		{name: "graphql: --input names a directory", argv: []string{"api", "graphql", "--input", queries}, block: true},

		// Allowed: a read, another mutation, and the one next door.
		{name: "graphql: a query reaches gh", argv: []string{"api", "graphql", "-f", "query={ viewer { login } }"}},
		{name: "graphql: another mutation reaches gh", argv: []string{"api", "graphql", "-f", `query=mutation { addPullRequestReview(input:{pullRequestId:"x"}) { clientMutationId } }`}},
		{name: "graphql: unresolveReviewThread is out of scope", argv: []string{"api", "graphql", "-f", "query=" + unresolveMutation}},

		// -f is the static spelling, so its @ is part of the string.
		{name: "graphql: -f does not read the file the @ names", argv: []string{"api", "graphql", "-f", "query=@" + at("reply.graphql")}},
		// Only the field named query carries one.
		{name: "graphql: another field is not the query", argv: []string{"api", "graphql", "-F", "body=@" + at("reply.graphql"), "-f", "query={ viewer { login } }"}},

		// The stdin spellings are the carve-out, as --body-file - already is.
		{name: "graphql: --input - is fail open", argv: []string{"api", "graphql", "--input", "-"}},
		{name: "graphql: -F query=@- is fail open", argv: []string{"api", "graphql", "-F", "query=@-"}},

		// An input gh will read and this rule cannot: left to gh.
		{name: "graphql: --input JSON without a query", argv: []string{"api", "graphql", "--input", at("variables.json")}},
		{name: "graphql: --input that is not JSON", argv: []string{"api", "graphql", "--input", at("raw.graphql")}},

		// Help is not a request.
		{name: "api --help", argv: []string{"api", "--help"}},
		{name: "api -h", argv: []string{"api", "-h"}},

		// Without CLAUDECODE nothing is judged.
		{name: "no CLAUDECODE: graphql thread mutation", argv: []string{"api", "graphql", "-f", "query=" + resolveMutation}, noClaudeCode: true},
		{name: "no CLAUDECODE: graphql unreadable input", argv: []string{"api", "graphql", "--input", "missing.json"}, noClaudeCode: true},
	})

	if os.Getuid() == 0 {
		t.Log("running as root, which can read a file with no permission bits")
		return
	}
	unreadable := unreadableBody(t, queries)
	runDecideCases(t, "", []decideCase{
		{name: "graphql: -F query=@ names a file with no read permission", argv: []string{"api", "graphql", "-F", "query=@" + unreadable}, block: true},
	})
}

func TestDecideAPIRESTReplies(t *testing.T) {
	t.Parallel()

	const replies = "repos/o/r/pulls/1/comments/2/replies"

	runDecideCases(t, "", []decideCase{
		// Blocked: a POST to the replies endpoint, however the method arrives.
		{name: "rest: fields make it a POST", argv: []string{"api", replies, "-f", "body=hi"}, block: true, golden: "rule5-rest"},
		{name: "rest: --input makes it a POST", argv: []string{"api", replies, "--input", "body.json"}, block: true},
		{name: "rest: -X POST apart from its value", argv: []string{"api", "-X", "POST", replies, "--input", "body.json"}, block: true},
		{name: "rest: -XPOST attached", argv: []string{"api", "-XPOST", "repos/{owner}/{repo}/pulls/1/comments/2/replies", "-f", "body=hi"}, block: true},
		{name: "rest: --method is case-insensitive", argv: []string{"api", "--method", "post", replies}, block: true},

		// The same endpoint under its other spellings.
		{name: "rest: a leading slash", argv: []string{"api", "/" + replies, "-f", "body=hi"}, block: true},
		{name: "rest: the full URL", argv: []string{"api", "https://api.github.com/" + replies, "-f", "body=hi"}, block: true},
		{name: "rest: an enterprise URL", argv: []string{"api", "https://ghe.example.com/api/v3/" + replies, "-f", "body=hi"}, block: true},
		{name: "rest: a trailing slash", argv: []string{"api", replies + "/", "-f", "body=hi"}, block: true},
		// A query string or a fragment hangs off the end of a path both
		// patterns anchor, so normalisation drops them.
		{name: "rest: a query string", argv: []string{"api", replies + "?per_page=1", "-f", "body=hi"}, block: true},
		{name: "rest: a fragment", argv: []string{"api", replies + "#x", "-f", "body=hi"}, block: true},
		// The stdin carve-out belongs to the query scan, which this half does
		// not run: nothing is read here, so nothing is given up either.
		{name: "rest: --input - still makes it a POST", argv: []string{"api", replies, "--input", "-"}, block: true},

		// Allowed: everything that is not a POST there.
		{name: "rest: no method and no field is a GET", argv: []string{"api", replies}},
		{name: "rest: an explicit GET beats the implicit POST", argv: []string{"api", "-X", "GET", replies, "-f", "body=x"}},
		{name: "rest: a GET reaches gh", argv: []string{"api", "repos/foo/bar"}},
		{name: "rest: another endpoint reaches gh", argv: []string{"api", "repos/o/r/pulls/1/comments", "-f", "body=x"}},
		{name: "rest: editing a review comment is out of scope", argv: []string{"api", "-X", "PATCH", "repos/o/r/pulls/comments/2", "-f", "body=x"}},

		// Without CLAUDECODE nothing is judged.
		{name: "no CLAUDECODE: rest reply", argv: []string{"api", replies, "-f", "body=hi"}, noClaudeCode: true},
	})
}

// TestDecideAPIEveryValueFlagIsScanned is the drift check for apiValueFlags,
// in the shape TestDecideRepositoryExplicitness runs for gh repo sync
// --source. A flag missing a row is walked as a boolean, its value is taken
// for the endpoint the rule matches on, and the reply that follows goes
// through without a word — the failure the table's own comment names.
//
// The flags are listed here rather than read out of the table, because the
// table is what is under test: iterating it would drop the case for the row
// that went missing along with the row. The count is asserted both ways, so a
// row added to the table has to be added here too.
func TestDecideAPIEveryValueFlagIsScanned(t *testing.T) {
	t.Parallel()

	// Every value-taking flag of gh api, copied from gh api --help as the
	// table was, with a value gh would accept. None may be read as the
	// endpoint. --method is the one that also decides the rule, so it carries
	// the method the rule is looking for.
	long := map[string]string{
		"cache":     "60s",
		"field":     "a=b",
		"header":    "Accept: application/json",
		"hostname":  "github.com",
		"input":     "body.json",
		"jq":        ".",
		"method":    "POST",
		"preview":   "nebula",
		"raw-field": "a=b",
		"template":  "{{.}}",
	}
	short := map[byte]string{
		'F': "a=b",
		'f': "a=b",
		'H': "Accept: application/json",
		'p': "nebula",
		'q': ".",
		't': "{{.}}",
		'X': "POST",
	}

	if len(apiValueFlags.long) != len(long) {
		t.Errorf("the table lists %d long flags and this test %d; they are copied from the same help", len(apiValueFlags.long), len(long))
	}
	if len(apiValueFlags.short) != len(short) {
		t.Errorf("the table lists %d short flags and this test %d; they are copied from the same help", len(apiValueFlags.short), len(short))
	}

	const replies = "repos/o/r/pulls/1/comments/2/replies"
	var cases []decideCase

	for name, value := range long {
		cases = append(cases, decideCase{
			name:  "--" + name + " does not read as the endpoint",
			argv:  []string{"api", "--" + name, value, replies, "-f", "body=hi"},
			block: true,
		})
	}
	for flag, value := range short {
		cases = append(cases, decideCase{
			name:  "-" + string(flag) + " does not read as the endpoint",
			argv:  []string{"api", "-" + string(flag), value, replies, "-f", "body=hi"},
			block: true,
		})
	}
	runDecideCases(t, "", cases)
}
