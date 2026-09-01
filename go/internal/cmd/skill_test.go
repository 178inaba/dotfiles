package cmd

import "testing"

// TestPublishedNamesTheContract keeps the set the skills are checked against
// from quietly emptying: a change that made it so would turn `ccx skill refs`
// into a check that passes on anything.
func TestPublishedNamesTheContract(t *testing.T) {
	got := published()

	if len(got.Commands) != len(contracts) {
		t.Errorf("published names %d commands, want %d", len(got.Commands), len(contracts))
	}
	// One from each of the places an identifier comes from: a field, a value
	// of a set, a section key, and an exit status's symbol.
	for _, want := range []string{"head_oid", "in_use_by_process", "release_manual_steps", "missing_section"} {
		if !contains(got.Identifiers, want) {
			t.Errorf("published does not name %q", want)
		}
	}
}

func contains(all []string, want string) bool {
	for _, s := range all {
		if s == want {
			return true
		}
	}
	return false
}
