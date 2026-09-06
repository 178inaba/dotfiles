package ghapi_test

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

func TestNewBody(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		text    string
		refused bool
	}{
		{name: "empty"},
		{name: "prose", text: "a body with no numbering in it\n"},
		{name: "two references are a mention", text: "see #1 and #2\n"},
		{name: "three are item numbering", text: "#1 first #2 second #3 third\n", refused: true},
		{name: "code is not numbering", text: "`#1` `#2` `#3`\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			body, err := ghapi.NewBody(tt.text)
			switch {
			case tt.refused && err == nil:
				t.Fatalf("NewBody(%q) = %q, want a refusal", tt.text, body)
			case tt.refused:
				// The gh shim's words for the same body, from the same place.
				if want := ghmd.RefuseBareHashRefs(tt.text); err.Error() != want {
					t.Errorf("NewBody(%q) error = %q, want %q", tt.text, err, want)
				}
			case err != nil:
				t.Fatalf("NewBody(%q): %v", tt.text, err)
			case body.String() != tt.text:
				t.Errorf("NewBody(%q).String() = %q", tt.text, body.String())
			}
		})
	}
}

func TestBodySubstitute(t *testing.T) {
	t.Parallel()

	body, err := ghapi.NewBody("subs: #{A} #{B} #{C}, and #{LATER}\n")
	if err != nil {
		t.Fatalf("NewBody: %v", err)
	}

	// A run against a new repository numbers its first issues 1, 2 and 3.
	// Judging the result would refuse exactly that, which is why substitution
	// makes no second judgement.
	got, left := body.Substitute(map[string]int{"A": 1, "B": 2, "C": 3})
	if want := "subs: #1 #2 #3, and #{LATER}\n"; got.String() != want {
		t.Errorf("Substitute() = %q, want %q", got.String(), want)
	}
	if diff := cmp.Diff([]ghmd.Placeholder{{Name: "LATER", Line: 1}}, left); diff != "" {
		t.Errorf("Substitute() left (-want +got):\n%s", diff)
	}

	// The same text, offered to the constructor, is what a second judgement
	// would have amounted to.
	if _, err := ghapi.NewBody(got.String()); err == nil ||
		!strings.Contains(err.Error(), "3 distinct") {
		t.Errorf("NewBody(%q) = %v, want the refusal a second judgement would make", got, err)
	}
}
