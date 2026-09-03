package contract

import (
	"reflect"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

// The types below stand in for the shapes the real contracts take, so the
// format is pinned by hand rather than by whatever the renderer produced.
// golden_test.go characterises the real types; this one specifies.

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
	Total  int         `json:"total" contract:"required"`
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
			p + ".sampleOut.Total":  "",
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
  total     integer (required)
`
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Render (-want +got):\n%s", diff)
	}
}

// TestRenderInput covers the one place the modes differ: a pointer reads as
// null on the way out and as omitted on the way in.
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

// sampleGroup is a plain embedded struct: json inlines its fields, so the
// rendering puts them where they are on the wire.
type sampleGroup struct {
	Left  string `json:"left"`
	Right string `json:"right"`
}

// sampleChoice is a group exactly one of whose members is supplied.
type sampleChoice struct {
	Text *string `json:"text"`
	File *string `json:"file"`
}

// sampleMaybe is the same with the member optional, which is what a group of
// two ways to say a thing looks like where saying it at all is optional.
type sampleMaybe struct {
	Note *string `json:"note"`
	Ref  *string `json:"ref"`
}

type sampleEmbedded struct {
	Head string `json:"head"`
	sampleGroup
	sampleChoice `contract:"exclusive,required"`
	sampleMaybe  `contract:"exclusive"`
}

func embeddedTable() Table {
	p := reflect.TypeFor[sampleEmbedded]().PkgPath()
	return Table{Fields: map[string]string{p + ".sampleChoice.Text": "The thing itself, written inline."}}
}

// TestRenderEmbeddedGroups pins the two shapes an embedded struct takes: no
// tag and it is the parent's own fields, exclusive and it is a heading its
// members sit under. A member keeps its own qualifier either way — the heading
// says how many of the group may appear, the qualifier whether that one key
// may be left out, and both are true.
func TestRenderEmbeddedGroups(t *testing.T) {
	got, err := embeddedTable().Render(reflect.TypeFor[sampleEmbedded](), Input)
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	want := `  head             string (optional)
  left             string (optional)
  right            string (optional)
  exactly one of:
    text           string (optional)
                   The thing itself, written inline.
    file           string (optional)
  at most one of:
    note           string (optional)
    ref            string (optional)
`
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Render (-want +got):\n%s", diff)
	}
}

// TestIdentifiersOmitsGroupHeadings keeps a heading out of the set a SKILL.md
// is checked against: it is a sentence about the fields under it rather than a
// name anything can refer to.
func TestIdentifiersOmitsGroupHeadings(t *testing.T) {
	got, err := embeddedTable().Identifiers(reflect.TypeFor[sampleEmbedded]())
	if err != nil {
		t.Fatalf("Identifiers: %v", err)
	}
	want := []string{"head", "left", "right", "text", "file", "note", "ref"}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Identifiers (-want +got):\n%s", diff)
	}
}

type sampleBadTag struct {
	Thing string `json:"thing" contract:"requried"`
}

// TestRenderRefusesUnknownContractValue is the guard on a stringly typed
// declaration: a misspelt constraint would otherwise render as an ordinary
// optional field and be enforced by nothing.
func TestRenderRefusesUnknownContractValue(t *testing.T) {
	_, err := Table{Fields: map[string]string{}}.Render(reflect.TypeFor[sampleBadTag](), Input)
	if err == nil {
		t.Fatal("Render succeeded on a contract tag holding an unknown value")
	}
	if want := "requried"; !strings.Contains(err.Error(), want) {
		t.Errorf("error %q does not name the offending value %q", err, want)
	}
}

type sampleMarshaler struct {
	Hidden bool
}

// MarshalJSON serialises as something the field walk cannot see.
func (sampleMarshaler) MarshalJSON() ([]byte, error) { return []byte("null"), nil }

type sampleWithMarshaler struct {
	Odd sampleMarshaler `json:"odd"`
}

// TestRenderRefusesUnlistedMarshaler keeps a custom marshaler from being
// documented as its Go fields, which never reach the wire.
func TestRenderRefusesUnlistedMarshaler(t *testing.T) {
	_, err := Table{Fields: map[string]string{}}.Render(reflect.TypeFor[sampleWithMarshaler](), Output)
	if err == nil {
		t.Fatal("Render succeeded on a type with an unlisted custom marshaler")
	}
	if want := "sampleMarshaler"; !strings.Contains(err.Error(), want) {
		t.Errorf("error %q does not name the offending type %q", err, want)
	}
}

// TestRenderUsesMarshalerOverride is the way out of the guard.
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
