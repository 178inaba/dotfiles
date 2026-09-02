package contract

import (
	"flag"
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

var update = flag.Bool("update", false, "rewrite the golden files")

// TestGolden renders three real contracts, for the three things the walk has
// to get right: pointers, a field reaching into another package, and enums
// with an omitzero.
//
// These characterise rather than specify — render_test.go pins the format.
// What they catch is a doc comment or a json tag changing the published
// contract without anyone noticing.
func TestGolden(t *testing.T) {
	tests := []struct {
		name string
		typ  reflect.Type
		mode Mode
	}{
		{"detection", reflect.TypeFor[worktree.Detection](), Output},
		{"preparation", reflect.TypeFor[pullrequest.Preparation](), Output},
		{"skipped", reflect.TypeFor[worktree.Skipped](), Output},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := Render(tc.typ, tc.mode)
			if err != nil {
				t.Fatalf("Render: %v", err)
			}

			golden := filepath.Join("testdata", tc.name+".txt")
			if *update {
				if err := os.WriteFile(golden, []byte(got), 0o600); err != nil {
					t.Fatalf("write %s: %v", golden, err)
				}
				return
			}
			want, err := os.ReadFile(golden)
			if err != nil {
				t.Fatalf("read %s: %v", golden, err)
			}
			if got != string(want) {
				t.Errorf("Render mismatch for %s; got:\n%s\nwant:\n%s", tc.name, got, want)
			}
		})
	}
}
