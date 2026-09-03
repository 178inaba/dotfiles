package contract_test

import (
	"flag"
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/contract"
	"github.com/178inaba/dotfiles/go/internal/contract/gen"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/worktree"
	"github.com/google/go-cmp/cmp"
)

var update = flag.Bool("update", false, "rewrite what is generated from the sources")

// outputFile carries the half of the table that reflection cannot see. It is
// committed because nothing regenerates it on the way to a build.
const outputFile = "docs_gen.go"

// TestGenerated is what makes the contract a rendering rather than a copy.
//
// A test rather than a step in CI, so the failure arrives where the change was
// made, with the command to fix it. Both files it covers are projections of
// the same doc comments and json tags, so one reading of the sources answers
// for both and -update writes them together.
func TestGenerated(t *testing.T) {
	// internal/contract is two levels below the module root.
	d, err := gen.Extract(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("extract: %v", err)
	}

	t.Run(outputFile, func(t *testing.T) {
		want, err := gen.Source(d)
		if err != nil {
			t.Fatalf("render: %v", err)
		}
		checkFile(t, outputFile, want)
	})

	// What ccx renders against is assembled in table.go, and swapping two of
	// the generated variables there would still compile and still be a table.
	//
	// Skipped under -update, where std is the table compiled in rather than
	// the one just written; an ordinary run makes the same check.
	t.Run("std", func(t *testing.T) {
		if *update {
			t.Skip("std is the table this run replaced")
		}
		wired := gen.Docs{
			Fields:   contract.StdTable.Fields,
			Types:    contract.StdTable.Types,
			Enums:    contract.StdTable.Enums,
			EnumDocs: contract.StdTable.EnumDocs,
			Packages: contract.StdTable.Packages,
		}
		if diff := cmp.Diff(wired, d); diff != "" {
			t.Errorf("std does not carry the generated table:\n%s", diff)
		}
	})

	// The table is read from the sources rather than taken from the compiled
	// docs_gen.go, so that a stale generated file cannot decide what these say.
	fresh := contract.Table{
		Fields:     d.Fields,
		Types:      d.Types,
		Enums:      d.Enums,
		EnumDocs:   d.EnumDocs,
		Packages:   d.Packages,
		Marshalers: contract.StdMarshalers,
	}

	// Five real contracts. Three output ones, for what the walk has to get
	// right about a document a command prints: pointers, a field reaching into
	// another package, and enums with an omitzero. Two input ones, which are
	// the first pinned here, for the exclusive groups: a review carries one at
	// the top and another nested under its comments, and a threads entry
	// carries the optional form of it a level down.
	//
	// These characterise rather than specify — render_test.go pins the format.
	// What they catch is a doc comment or a json tag changing the published
	// contract without anyone noticing.
	goldens := []struct {
		name string
		typ  reflect.Type
		mode contract.Mode
	}{
		{"detection", reflect.TypeFor[worktree.Detection](), contract.Output},
		{"preparation", reflect.TypeFor[pullrequest.Preparation](), contract.Output},
		{"skipped", reflect.TypeFor[worktree.Skipped](), contract.Output},
		{"review_file", reflect.TypeFor[pullrequest.ReviewFile](), contract.Input},
		{"threads_file", reflect.TypeFor[pullrequest.ThreadsFile](), contract.Input},
	}

	for _, tc := range goldens {
		t.Run(tc.name, func(t *testing.T) {
			got, err := fresh.Render(tc.typ, tc.mode)
			if err != nil {
				t.Fatalf("Render: %v", err)
			}
			checkFile(t, filepath.Join("testdata", tc.name+".txt"), []byte(got))
		})
	}
}

// checkFile compares a committed projection of the sources against what the
// sources now say, or rewrites it under -update.
func checkFile(t *testing.T, path string, want []byte) {
	t.Helper()

	if *update {
		if err := os.WriteFile(path, want, 0o644); err != nil {
			t.Fatalf("write %s: %v", path, err)
		}
		return
	}

	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	if diff := cmp.Diff(string(got), string(want)); diff != "" {
		t.Errorf("%s is out of date; to regenerate, run `go generate ./internal/contract/...`\n%s", path, diff)
	}
}
