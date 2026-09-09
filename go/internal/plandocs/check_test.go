package plandocs

import (
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"
)

// checkFixture is the one tree every case below is read against, so that a
// case is a plan and an expectation rather than a tree of its own. It answers
// with the repository and the home the check is given.
func checkFixture(t *testing.T) (dir, home string) {
	t.Helper()

	dir, home = t.TempDir(), t.TempDir()
	writeTree(t, dir, map[string]string{
		".git/HEAD":                          "ref: refs/heads/main\n",
		".editorconfig":                      "root = true\n",
		"go/internal/pullrequest/pending.go": "package pullrequest\n\nfunc Pending() {}\n",
		"docs/guide.md":                      "# Guide\n",
		// A linked worktree marks itself with a .git file rather than a
		// directory, and what is checked out in it belongs to another branch.
		"nested/.git":      "gitdir: /elsewhere\n",
		"nested/inside.go": "package inside\n\nfunc OnlyInNested() {}\n",
		"bin/blob":         "\x00\x01not text\n",
	})
	writeTree(t, home, map[string]string{"notes/reference.md": "# Notes\n"})
	return dir, home
}

// check runs one plan against the fixture, writing it outside the repository
// where a plan file actually lives.
func check(t *testing.T, dir, home, plan string) Checked {
	t.Helper()

	file := filepath.Join(t.TempDir(), "plan.md")
	writeTree(t, filepath.Dir(file), map[string]string{"plan.md": plan})
	got, err := Check(file, dir, home)
	if err != nil {
		t.Fatalf("Check: %v", err)
	}
	return got
}

// What a plan says about paths, read against a tree that has some of them.
func TestCheckReadsThePathsAPlanCites(t *testing.T) {
	dir, home := checkFixture(t)

	tests := map[string]struct {
		plan string
		want []Finding
	}{
		"a path from the top of the repository": {
			plan: "Edit `go/internal/pullrequest/pending.go` first.\n",
		},
		"a path under the home directory": {
			plan: "See `~/notes/reference.md`.\n",
		},
		// The spelling a plan actually uses once it has named a file in full.
		"a bare basename found anywhere in the tree": {
			plan: "Then `pending.go` gets the change.\n",
		},
		"a partial path found anywhere in the tree": {
			plan: "Then `pullrequest/pending.go` gets the change.\n",
		},
		"a dotfile at the top": {
			plan: "Follow `.editorconfig`.\n",
		},
		"a path with a glob": {
			plan: "Run over `go/**/*.go` and `testdata/*.txt`.\n",
		},
		"a path with a placeholder": {
			plan: "Write `.claude/worktrees/<name>/` and `go/{a,b}.go`.\n",
		},
		"a reference carrying line numbers": {
			plan: "See `go/internal/pullrequest/pending.go:12` and `docs/guide.md:1-4,9`.\n",
		},
		"a slash command": {
			plan: "Run `/deep-review` and `/simplify`.\n",
		},
		"a branch name": {
			plan: "Branch `feature/293-count-bodied-approvals` holds it.\n",
		},
		// The branch excuse is the tree's layout's to confirm nowhere: a
		// repository with a directory named after a branch type would
		// otherwise have every branch of that type reported.
		"a branch name whose type is also a directory at the top": {
			plan: "Branch `docs/293-count-bodied-approvals` holds it.\n",
		},
		"a planned artifact annotated in English": {
			plan: "Add `go/internal/plandocs/check.go` (new) beside it.\n",
		},
		"the home directory itself": {
			plan: "A `~/` path is read from the home directory.\n",
		},
		// Not a path into this checkout: a module path, a directory in another
		// project, a branch. The check has no answer for any of them.
		"a path whose first segment is not at the top": {
			plan: "It decodes with `encoding/json/v2`.\n",
		},
		// The walk skips the repository's own directory, so nothing under it
		// can be found here however plainly it is there.
		"a path under the repository's own directory": {
			plan: "The fixture writes `.git/HEAD`.\n",
		},
		// A filename means a file wherever the plan puts it, so the rule above
		// may not swallow one.
		"a missing file under a first segment that is not at the top": {
			plan: "Edit `elsewhere/nope.go`.\n",
			want: []Finding{{Line: 1, Ref: "elsewhere/nope.go"}},
		},
		"a missing directory under a real one": {
			plan: "Add it under `go/internal/nosuchpackage/`.\n",
			want: []Finding{{Line: 1, Ref: "go/internal/nosuchpackage/"}},
		},
		"a path that is nowhere in the tree": {
			plan: "Edit `go/internal/missing/nope.go` first.\n",
			want: []Finding{{Line: 1, Ref: "go/internal/missing/nope.go"}},
		},
		// The branch-name rule may not swallow a real file under a directory
		// that happens to be spelled like a branch type.
		"a missing path whose first segment is a branch type": {
			plan: "Edit `docs/missing.md` first.\n",
			want: []Finding{{Line: 1, Ref: "docs/missing.md"}},
		},
		"a missing path carrying line numbers": {
			plan: "See `go/internal/missing/nope.go:12`.\n",
			want: []Finding{{Line: 1, Ref: "go/internal/missing/nope.go:12"}},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			if diff := cmp.Diff(tt.want, check(t, dir, home, tt.plan).MissingPaths); diff != "" {
				t.Errorf("missing paths mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// What a plan says about names, read against the files that hold them.
func TestCheckReadsTheSymbolsAPlanCites(t *testing.T) {
	dir, home := checkFixture(t)

	tests := map[string]struct {
		plan string
		want []Finding
	}{
		"a name a file holds": {
			plan: "The `Pending` function decides it.\n",
		},
		"a qualified name whose last segment a file holds": {
			plan: "The `pullrequest.Pending` function decides it.\n",
		},
		"a flag": {
			plan: "Pass `--no-exit`, or `-R` for another repository.\n",
		},
		"a span with no letter in it": {
			plan: "Submitted `2026-01-05`, edited `2026-01-10`.\n",
		},
		"a worktree name": {
			plan: "In `feature-293-count-bodied-approvals` it is done.\n",
		},
		"a span carrying whitespace": {
			plan: "Run `go -C go test ./...` afterwards.\n",
		},
		// The annotation is read before a span is classified, so the two
		// spellings of it are asserted once each across the two tables rather
		// than twice each.
		"a planned artifact annotated in Japanese": {
			plan: "`noSuchFixture` のケースを足す（新規）。\n",
		},
		"a name nothing holds": {
			plan: "The `noSuchFixture` case decides it.\n",
			want: []Finding{{Line: 1, Ref: "noSuchFixture"}},
		},
		"a qualified name whose last segment nothing holds": {
			plan: "The `pullrequest.NoSuchThing` function decides it.\n",
			want: []Finding{{Line: 1, Ref: "pullrequest.NoSuchThing"}},
		},
		// A linked worktree under the repository is another branch's checkout,
		// so what only it holds is not in this one.
		"a name only a nested checkout holds": {
			plan: "The `OnlyInNested` function decides it.\n",
			want: []Finding{{Line: 1, Ref: "OnlyInNested"}},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			if diff := cmp.Diff(tt.want, check(t, dir, home, tt.plan).UnresolvedSymbols); diff != "" {
				t.Errorf("unresolved symbols mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// A plan is not its own evidence: a name it invents is unresolved even when
// the plan itself is kept inside the repository being walked.
func TestCheckDoesNotReadThePlanAsEvidence(t *testing.T) {
	dir, home := checkFixture(t)
	file := filepath.Join(dir, "docs", "plan.md")
	writeTree(t, dir, map[string]string{"docs/plan.md": "The `noSuchFixture` case decides it.\n"})

	got, err := Check(file, dir, home)
	if err != nil {
		t.Fatalf("Check: %v", err)
	}
	want := []Finding{{Line: 1, Ref: "noSuchFixture"}}
	if diff := cmp.Diff(want, got.UnresolvedSymbols); diff != "" {
		t.Errorf("unresolved symbols mismatch (-want +got):\n%s", diff)
	}
}

// What a plan says it ran, read off the fenced blocks that hold the commands.
func TestCheckReadsTheCommandsAPlanLists(t *testing.T) {
	dir, home := checkFixture(t)

	tests := map[string]struct {
		plan string
		want []Finding
	}{
		"a command recorded with an exit status": {
			plan: "```bash\ngo test ./...\n# => exit 0: all packages pass\n```\n",
		},
		"a command recorded as not run": {
			plan: "```bash\ngit push\n# => not run: plan mode is read-only\n```\n",
		},
		"a run of lines closed by one record": {
			plan: "```sh\ncd go\n\n# build it\ngo build ./...\n# => exit 0: builds\n```\n",
		},
		"a block in another language": {
			plan: "```go\nfunc main() {}\n```\n",
		},
		"a block with no language at all": {
			plan: "```\njust text\n```\n",
		},
		"a command with no record": {
			plan: "```bash\ngo test ./...\n```\n",
			want: []Finding{{Line: 2, Ref: "go test ./..."}},
		},
		"a record that says neither thing": {
			plan: "```bash\ngo test ./...\n# => probably fine\n```\n",
			want: []Finding{{Line: 2, Ref: "go test ./..."}},
		},
		// The record closes the run before it, so the lines after it are a
		// second command and want a record of their own.
		"a second run left unrecorded": {
			plan: "```zsh\ngo vet ./...\n# => exit 0: clean\ngolangci-lint run\n```\n",
			want: []Finding{{Line: 4, Ref: "golangci-lint run"}},
		},
		"two blocks, one of them unrecorded": {
			plan: "```console\nls\n# => exit 0: lists\n```\n\ntext\n\n```shell\npwd\n```\n",
			want: []Finding{{Line: 9, Ref: "pwd"}},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			if diff := cmp.Diff(tt.want, check(t, dir, home, tt.plan).UnrecordedCommands); diff != "" {
				t.Errorf("unrecorded commands mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// Findings come out in the order a reader meets them, since the symbols are
// answered by a walk whose order says nothing about the plan.
func TestCheckOrdersFindingsByLine(t *testing.T) {
	dir, home := checkFixture(t)
	got := check(t, dir, home, "The `zzzMissing` case.\nThe `aaaMissing` case, and `mmmMissing` too.\n")

	want := []Finding{
		{Line: 1, Ref: "zzzMissing"},
		{Line: 2, Ref: "aaaMissing"},
		{Line: 2, Ref: "mmmMissing"},
	}
	if diff := cmp.Diff(want, got.UnresolvedSymbols); diff != "" {
		t.Errorf("unresolved symbols mismatch (-want +got):\n%s", diff)
	}
}

// The report names the plan it is about, as the plan was named: a caller
// running this over several plans has nothing else to tell them apart by.
// That the lists reach the wire as arrays rather than as absences is the
// command's promise and is asserted where the command is.
func TestCheckNamesThePlan(t *testing.T) {
	dir, home := checkFixture(t)
	file := filepath.Join(t.TempDir(), "plan.md")
	writeTree(t, filepath.Dir(file), map[string]string{"plan.md": "Nothing to check here.\n"})

	got, err := Check(file, dir, home)
	if err != nil {
		t.Fatalf("Check: %v", err)
	}
	if got.Plan != file {
		t.Errorf("plan = %q, want %q", got.Plan, file)
	}
}

// A plan that is not there is a failure rather than a clean report, since a
// caller told to fix its findings would read one as nothing to fix.
func TestCheckFailsOnAPlanThatIsNotThere(t *testing.T) {
	dir, home := checkFixture(t)
	if _, err := Check(filepath.Join(dir, "no-such-plan.md"), dir, home); err == nil {
		t.Error("Check on a missing plan = nil, want an error")
	}
}
