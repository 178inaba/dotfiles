package contract

import (
	"strings"
	"testing"
)

// decodeKinds carries one field of each kind a message has a word for, so that
// the vocabulary is specified by documents rather than by a table read back to
// itself.
type decodeKinds struct {
	Flag   *bool           `json:"flag"`
	Count  *int            `json:"count"`
	Name   *string         `json:"name"`
	List   []string        `json:"list"`
	Nested *decodeNested   `json:"nested"`
	Refs   []decodeNested  `json:"refs"`
	Extra  map[string]bool `json:"extra"`
	Self   *validSelf      `json:"self"`
	decodeGroup
}

type decodeNested struct {
	Number *int `json:"number"`
}

// decodeGroup is the embedded form a mutually exclusive group takes: its
// member reaches the wire as a key of the object its parent was read from.
type decodeGroup struct {
	Body *string `json:"body"`
}

func assertUnmarshal(t *testing.T, in, want string) {
	t.Helper()

	err := unmarshalKinds(in)
	if err == nil {
		t.Fatalf("Unmarshal(%s) accepted a document, want %q", in, want)
	}
	if err.Error() != want {
		t.Errorf("Unmarshal(%s) = %q, want %q", in, err, want)
	}
}

// assertNotJSON checks only the half of the fallback that is this package's:
// the decoder's own sentence follows it, and pinning that would pin the
// standard library's wording rather than ours.
func assertNotJSON(t *testing.T, in string) {
	t.Helper()

	err := unmarshalKinds(in)
	if err == nil {
		t.Fatalf("Unmarshal(%s) accepted a document, want it refused", in)
	}
	if !strings.HasPrefix(err.Error(), "invalid JSON in doc.json (") || !strings.HasSuffix(err.Error(), ")") {
		t.Errorf("Unmarshal(%s) = %q, want the document named and the decoder quoted after it", in, err)
	}
}

func unmarshalKinds(in string) error {
	var got decodeKinds
	return Unmarshal([]byte(in), &got, "doc.json")
}

// TestUnmarshalNamesTheFieldTheDecoderRefused is the half of "error messages
// name the field" a decode failure used to miss: each parser restated a whole
// entry's shape, which is the shape --help already publishes.
func TestUnmarshalNamesTheFieldTheDecoderRefused(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   string
		want string
	}{
		{
			name: "a boolean",
			in:   `{"flag":"yes"}`,
			want: "flag must be a boolean in doc.json",
		},
		{
			name: "a number",
			in:   `{"count":"7"}`,
			want: "count must be a number in doc.json",
		},
		{
			name: "a string",
			in:   `{"name":3}`,
			want: "name must be a string in doc.json",
		},
		{
			name: "an array",
			in:   `{"list":{}}`,
			want: "list must be an array in doc.json",
		},
		{
			name: "an object",
			in:   `{"nested":"x"}`,
			want: "nested must be an object in doc.json",
		},
		{
			// The walk ends on an index rather than a key: the element itself
			// is the wrong kind, not something inside it.
			name: "a list element, by index",
			in:   `{"refs":[5]}`,
			want: "refs[0] must be an object in doc.json",
		},
		{
			// The path is written the way a reader of the document reads it,
			// not as the RFC 6901 pointer the decoder supplies.
			name: "inside a list, by index",
			in:   `{"refs":[{"number":1},{"number":"x"}]}`,
			want: "refs[1].number must be a number in doc.json",
		},
		{
			name: "a member of an embedded group",
			in:   `{"body":3}`,
			want: "body must be a string in doc.json",
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertUnmarshal(t, tc.in, tc.want)
		})
	}
}

// TestUnmarshalNamesTheDocumentWhereNoFieldIsWrong is the branch that has no
// field to name and is a SemanticError all the same, so branching on the error
// type alone would send it down the field-naming path and produce a message
// with no field in it.
func TestUnmarshalNamesTheDocumentWhereNoFieldIsWrong(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   string
		want string
	}{
		{
			name: "the root is the wrong kind",
			in:   `[]`,
			want: "doc.json must be an object",
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertUnmarshal(t, tc.in, tc.want)
		})
	}
}

// TestUnmarshalSaysNothingItCannotBeSureOf is the validator's posture applied
// to the translation: where the declaration does not answer what a pointer
// means, the refusal says only that the bytes did not decode, rather than
// naming a field it worked out lexically.
func TestUnmarshalSaysNothingItCannotBeSureOf(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   string
	}{
		{"not valid JSON at all", `not json`},
		// validSelf's Go fields are not what it puts on the wire, so neither
		// the key inside it nor its own kind is something to name.
		{"inside a type that serialises itself", `{"self":{"need":5}}`},
		{"a type that serialises itself", `{"self":"x"}`},
		// A map is a kind the renderer refuses to describe, so no --help
		// published a shape for extra and there is none to hold it to.
		{"a kind no declaration can publish", `{"extra":"x"}`},
		{"inside a kind no declaration can publish", `{"extra":{"a":"yes"}}`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertNotJSON(t, tc.in)
		})
	}
}
