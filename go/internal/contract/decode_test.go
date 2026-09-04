package contract

import "testing"

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
	decodeGroup
}

type decodeNested struct {
	Number *int `json:"number"`
}

// decodeGroup is the embedded form a mutually exclusive group takes, whose
// members json inlines into the object its parent was read from — so the
// decoder's pointer at one of them is flat, and the walk has to look inside a
// field that has no key of its own.
type decodeGroup struct {
	Body *string `json:"body"`
}

func assertUnmarshal(t *testing.T, in, want string) {
	t.Helper()

	var got decodeKinds
	err := Unmarshal([]byte(in), &got, "doc.json")
	if err == nil {
		t.Fatalf("Unmarshal(%s) accepted a document, want %q", in, want)
	}
	if err.Error() != want {
		t.Errorf("Unmarshal(%s) = %q, want %q", in, err, want)
	}
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

// TestUnmarshalNamesTheDocumentWhereNoFieldIsWrong covers the two branches that
// have no field to name. A root mismatch is a SemanticError too, so branching
// on the error type alone would send it down the field-naming path and produce
// a message with no field in it.
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
		{
			name: "not valid JSON at all",
			in:   `not json`,
			want: `invalid JSON in doc.json (jsontext: invalid character 'o' in literal null (expecting 'u') after offset 1)`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertUnmarshal(t, tc.in, tc.want)
		})
	}
}

// TestUnmarshalSaysNothingItCannotBeSureOf is the validator's posture applied
// to the translation: a pointer that does not resolve against the declaration
// says only that the document did not decode, rather than naming a field it
// worked out lexically.
func TestUnmarshalSaysNothingItCannotBeSureOf(t *testing.T) {
	assertUnmarshal(t, `{"extra":{"a":"yes"}}`,
		`invalid JSON in doc.json (json: cannot unmarshal JSON string into Go bool within "/extra/a")`)
}
