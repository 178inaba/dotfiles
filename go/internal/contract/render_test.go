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

type samplePartial struct {
	Kind sampleKind `json:"kind"`
}

// TestRenderPartlyDocumentedEnum pins the row a value with no explanation of
// its own still gets. Its second column is empty, which used to take the whole
// row out of the help while Identifiers went on publishing the name.
func TestRenderPartlyDocumentedEnum(t *testing.T) {
	p := reflect.TypeFor[samplePartial]().PkgPath()
	tbl := Table{
		Fields:   map[string]string{},
		Enums:    map[string][]string{p + ".sampleKind": {string(sampleAlpha), string(sampleBeta)}},
		EnumDocs: map[string]string{p + ".sampleKind." + string(sampleAlpha): "The first."},
	}
	got, err := tbl.Render(reflect.TypeFor[samplePartial](), Output)
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	want := `  kind      string, one of:
    alpha   The first.
    beta
`
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Render (-want +got):\n%s", diff)
	}
}

// sampleValues declares its value constraints in the opposite order to the one
// a row states them in, which is the point: the tag is a set.
type sampleValues struct {
	File   *string `json:"file" contract:"nonempty,barefilename"`
	Name   string  `json:"name" contract:"required,nonempty"`
	Number int     `json:"number" contract:"required,positive"`
	Plain  *string `json:"plain"`
}

// TestRenderStatesValueConstraints pins how a constraint on what a field holds
// joins the one on whether it is there: in the same brackets, after it.
func TestRenderStatesValueConstraints(t *testing.T) {
	got, err := Table{Fields: map[string]string{}}.Render(reflect.TypeFor[sampleValues](), Input)
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	want := `  file      string (optional, a bare file name, not empty)
  name      string (required, not empty)
  number    integer (required, positive)
  plain     string (optional)
`
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("Render (-want +got):\n%s", diff)
	}
}

type sampleMisplacedExclusive struct {
	Thing string `json:"thing" contract:"exclusive"`
}

type sampleMisplacedRequired struct {
	sampleGroup `contract:"required"`
}

type sampleUnwiredGroup struct {
	Thing sampleGroup `contract:"exclusive"`
}

type sampleHiddenGroup struct {
	sampleGroup `json:"-" contract:"exclusive,required"`
}

type sampleValueOnAnInteger struct {
	Count *int `json:"count" contract:"nonempty"`
}

type samplePositiveOnAString struct {
	Name string `json:"name" contract:"positive"`
}

// TestRenderRefusesAMisplacedContractValue is the other half of the guard: a
// value written where nothing reads it is dropped on the floor exactly as a
// misspelt one would be.
func TestRenderRefusesAMisplacedContractValue(t *testing.T) {
	for _, tc := range []struct {
		name string
		typ  reflect.Type
		want []string
	}{
		{"exclusive on a named field", reflect.TypeFor[sampleMisplacedExclusive](), []string{"exclusive"}},
		{"required on a plain group", reflect.TypeFor[sampleMisplacedRequired](), []string{"required"}},
		{"exclusive on a field json does not inline", reflect.TypeFor[sampleUnwiredGroup](), []string{"exclusive"}},
		{"exclusive on a group json is told to skip", reflect.TypeFor[sampleHiddenGroup](), []string{"exclusive"}},
		// A rule about values binds a kind rather than a place, so what it has
		// nothing to bind to is a field of another kind. The message names the
		// kind as well, since the value alone does not say what is wrong.
		{"a value constraint off its kind", reflect.TypeFor[sampleValueOnAnInteger](), []string{"nonempty", "string"}},
		// The same refusal from the other side, naming the kinds a rule about
		// numbers binds the way a rendered row names them.
		{"a rule about numbers on a string", reflect.TypeFor[samplePositiveOnAString](), []string{"positive", "an integer"}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			_, err := Table{Fields: map[string]string{}}.Render(tc.typ, Input)
			if err == nil {
				t.Fatal("Render succeeded on a contract value with nothing to bind to")
			}
			for _, want := range tc.want {
				if !strings.Contains(err.Error(), want) {
					t.Errorf("error %q does not carry %q", err, want)
				}
			}
		})
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

type sampleEmbeddedMarshaler struct {
	sampleMarshaler
}

// TestRenderRefusesAnEmbeddedMarshaler covers the guard on the other route
// into a struct: a group is nothing but its fields, so a type putting
// something else on the wire has none to inline.
func TestRenderRefusesAnEmbeddedMarshaler(t *testing.T) {
	_, err := Table{Fields: map[string]string{}}.Render(reflect.TypeFor[sampleEmbeddedMarshaler](), Output)
	if err == nil {
		t.Fatal("Render succeeded on an embedded type that serialises itself")
	}
	if want := "sampleMarshaler"; !strings.Contains(err.Error(), want) {
		t.Errorf("error %q does not name the offending type %q", err, want)
	}
}

// TestRenderRefusesAListedEmbeddedMarshaler is the same refusal for a type the
// table does describe: the override says what the type puts out on its own,
// which is not a set of keys to inline into the document around it.
func TestRenderRefusesAListedEmbeddedMarshaler(t *testing.T) {
	tbl := Table{
		Fields:     map[string]string{},
		Marshalers: map[reflect.Type]Marshaled{reflect.TypeFor[sampleMarshaler](): {Kind: "null, or an array of numbers"}},
	}
	_, err := tbl.Render(reflect.TypeFor[sampleEmbeddedMarshaler](), Output)
	if err == nil {
		t.Fatal("Render succeeded on an embedded type that serialises itself")
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
