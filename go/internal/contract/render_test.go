package contract

import (
	"reflect"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

// The types below stand in for the shapes the real contracts take, so that the
// format is pinned by something written out by hand rather than by whatever
// the renderer happened to produce. The golden tests over the real types are
// in golden_test.go and characterise; this one specifies.

type sampleKind string

const (
	sampleAlpha sampleKind = "alpha"
	sampleBeta  sampleKind = "beta"
)

type sampleRef struct {
	Number int    `json:"number"`
	URL    string `json:"url"`
}

type sampleOut struct {
	Found  bool        `json:"found"`
	Kind   sampleKind  `json:"kind"`
	Parent *sampleRef  `json:"parent"`
	Refs   []sampleRef `json:"refs"`
	Note   string      `json:"note,omitzero"`
}

type sampleIn struct {
	ID      *string `json:"id" contract:"required"`
	Body    *string `json:"body"`
	Entries []int   `json:"entries" contract:"required"`
}

func sampleTable() Table {
	p := reflect.TypeFor[sampleOut]().PkgPath()
	return Table{
		Fields: map[string]string{
			p + ".sampleOut.Found":  "Found is whether anything was there at all.",
			p + ".sampleOut.Kind":   "",
			p + ".sampleOut.Parent": "Parent is null when there is none, and also when it could not be read — the warning tells those apart.",
			p + ".sampleOut.Refs":   "",
			p + ".sampleOut.Note":   "Note is absent unless the flag that fetches it was given.",
			p + ".sampleRef.Number": "",
			p + ".sampleRef.URL":    "",
			p + ".sampleIn.ID":      "",
			p + ".sampleIn.Body":    "Body is omitted to resolve without replying.",
			p + ".sampleIn.Entries": "",
		},
		Enums: map[string][]string{p + ".sampleKind": {string(sampleAlpha), string(sampleBeta)}},
	}
}

func TestRenderOutput(t *testing.T) {
	got, err := sampleTable().Render(reflect.TypeFor[sampleOut](), Output)
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	want := `  found     boolean
            Found is whether anything was there at all.
  kind      string, one of: alpha, beta
  parent    object (may be null)
            Parent is null when there is none, and also when it could not be read — the
            warning tells those apart.
    number  integer
    url     string
  refs      array of object
    number  integer
    url     string
  note      string (absent when empty)
            Note is absent unless the flag that fetches it was given.
`
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Render (-want +got):\n%s", diff)
	}
}

// TestRenderInput covers the one place the two modes differ: a pointer means
// "may be null" in an output and "may be omitted" in an input, where
// required-ness is carried by the tag instead.
func TestRenderInput(t *testing.T) {
	got, err := sampleTable().Render(reflect.TypeFor[sampleIn](), Input)
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	want := `  id        string (required)
  body      string (optional)
            Body is omitted to resolve without replying.
  entries   array of integer (required)
`
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Render (-want +got):\n%s", diff)
	}
}

type sampleMarshaler struct {
	Hidden bool
}

// MarshalJSON makes sampleMarshaler serialise as something the field walk
// cannot see, which is the case the guard below exists for.
func (sampleMarshaler) MarshalJSON() ([]byte, error) { return []byte("null"), nil }

type sampleWithMarshaler struct {
	Odd sampleMarshaler `json:"odd"`
}

// TestRenderRefusesUnlistedMarshaler is the guard that keeps a custom
// marshaler from being documented as its Go fields. Rendering the struct's
// fields would describe a shape that never reaches the wire.
func TestRenderRefusesUnlistedMarshaler(t *testing.T) {
	_, err := Table{Fields: map[string]string{}}.Render(reflect.TypeFor[sampleWithMarshaler](), Output)
	if err == nil {
		t.Fatal("Render succeeded on a type with an unlisted custom marshaler")
	}
	if want := "sampleMarshaler"; !strings.Contains(err.Error(), want) {
		t.Errorf("error %q does not name the offending type %q", err, want)
	}
}

// TestRenderUsesMarshalerOverride shows the way out of the guard: a type that
// serialises for itself says what it serialises as.
func TestRenderUsesMarshalerOverride(t *testing.T) {
	tbl := Table{
		Fields:     map[string]string{},
		Marshalers: map[reflect.Type]Marshaled{reflect.TypeFor[sampleMarshaler](): {Kind: "null, or an array of numbers"}},
	}
	got, err := tbl.Render(reflect.TypeFor[sampleWithMarshaler](), Output)
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	if want := "  odd       null, or an array of numbers\n"; got != want {
		t.Errorf("Render = %q, want %q", got, want)
	}
}
