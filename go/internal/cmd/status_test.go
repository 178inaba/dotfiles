package cmd

import (
	"testing"

	"github.com/178inaba/dotfiles/go/internal/issue"
)

// TestSectionsCheckStatusesCoverEveryClass keeps the published numbers and the
// classes that produce them in step. A class with no status would exit 1 as an
// ordinary failure, which is the one thing this command's contract promises it
// does not do.
func TestSectionsCheckStatusesCoverEveryClass(t *testing.T) {
	want := map[issue.Class]int{
		issue.MissingSection:        2,
		issue.UnknownHeading:        3,
		issue.MappedMachineKey:      4,
		issue.HeadingLocaleMismatch: 5,
	}
	for class, code := range want {
		got, ok := sectionsCheckStatus(class)
		if !ok {
			t.Errorf("class %d has no status", class)
			continue
		}
		if got != code {
			t.Errorf("class %d = %d, want %d", class, got, code)
		}
	}
	if len(sectionsCheckStatuses) != len(want) {
		t.Errorf("sectionsCheckStatuses has %d entries, want %d", len(sectionsCheckStatuses), len(want))
	}
}

// TestStatusesRender pins the block a --help prints, so that the numbers a
// reader is given are the ones the process returns.
func TestStatusesRender(t *testing.T) {
	got := statuses{
		{code: 0, meaning: "the answer is on standard output"},
		{code: 6, symbol: "section_not_found", meaning: "the body does not carry the section"},
	}.render()
	want := `  0  the answer is on standard output
  6  section_not_found — the body does not carry the section
`
	if got != want {
		t.Errorf("render() =\n%q\nwant\n%q", got, want)
	}
}
