package cmd

import (
	"os"
	"path/filepath"
	"slices"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/skill"
)

// TestPublishedNamesTheContract keeps the set from quietly emptying, which
// would turn `ccx skill refs` into a check that passes on anything.
func TestPublishedNamesTheContract(t *testing.T) {
	got := published()

	if len(got.Commands) != len(contracts) {
		t.Errorf("published names %d commands, want %d", len(got.Commands), len(contracts))
	}
	// One from each place an identifier comes from.
	for _, want := range []string{"head_oid", "in_use_by_process", "release_manual_steps", "missing_section"} {
		if !slices.Contains(got.Identifiers, want) {
			t.Errorf("published does not name %q", want)
		}
	}
}

// TestSkillRefsOnThisRepository is the case a fixture cannot make: what this
// repository's skills actually say holds together. Here rather than beside
// CheckRefs because the contract it checks against is assembled here.
func TestSkillRefsOnThisRepository(t *testing.T) {
	skills := filepath.Join("..", "..", "..", "claude", ".claude", "skills")
	if _, err := os.Stat(skills); err != nil {
		t.Skipf("the repository's skills are not there: %v", err)
	}

	got, err := skill.CheckRefs(skills, published())
	if err != nil {
		t.Fatalf("CheckRefs: %v", err)
	}
	for _, v := range got.Violations {
		t.Errorf("%s:%d %s %s", v.File, v.Line, v.Type, v.Ref)
	}
}
