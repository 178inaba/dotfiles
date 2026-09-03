package contract

import (
	"errors"
	"reflect"
	"testing"
)

// The types below are the shapes a declaration can take, kept apart from
// render_test.go's: what the renderer needs to describe and what the validator
// needs to refuse are different sets, and one type serving both would grow
// fields neither test reads.

type validRef struct {
	Number *int    `json:"number" contract:"required"`
	Note   *string `json:"note"`
}

type validChoice struct {
	Body     *string `json:"body"`
	BodyFile *string `json:"body_file"`
}

type validMaybe struct {
	Draft *string `json:"draft"`
	Final *string `json:"final"`
}

type validDoc struct {
	Name        *string    `json:"name" contract:"required"`
	Ref         *validRef  `json:"ref"`
	Refs        []validRef `json:"refs"`
	validChoice `contract:"exclusive,required"`
	validMaybe  `contract:"exclusive"`
}

type validUntagged struct {
	Anything *string `json:"anything"`
}

// TestValidateReadsPresenceFromTheDocument is the rule the whole release rests
// on: a key is supplied when it is there and not null. All three of the tagged
// types are pointers today, so a validator walking the decoded value would
// pass every one of these — the distinction only appears against the bytes.
func TestValidateReadsPresenceFromTheDocument(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   string
		want string
	}{
		{
			name: "every key supplied",
			in:   `{"name":"n","body":"b","draft":"d"}`,
		},
		{
			name: "a required key left out",
			in:   `{"body":"b"}`,
			want: "doc.json is missing name",
		},
		{
			name: "a required key written as null",
			in:   `{"name":null,"body":"b"}`,
			want: "doc.json is missing name",
		},
		{
			name: "an optional key left out",
			in:   `{"name":"n","body":"b"}`,
		},
		{
			name: "an optional key written as null",
			in:   `{"name":"n","note":null,"body":"b"}`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertValidate(t, tc.in, reflect.TypeFor[validDoc](), tc.want)
		})
	}
}

// TestValidateChecksAnExclusiveGroup covers both cardinalities against a null
// member, which is the case the two halves of the presence rule decide
// together: a null beside a value is one member, not two.
func TestValidateChecksAnExclusiveGroup(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   string
		want string
	}{
		{
			name: "exactly one, satisfied",
			in:   `{"name":"n","body_file":"b.md"}`,
		},
		{
			name: "exactly one, both members set",
			in:   `{"name":"n","body":"b","body_file":"b.md"}`,
			want: "doc.json sets both body and body_file",
		},
		{
			name: "exactly one, neither member set",
			in:   `{"name":"n"}`,
			want: "doc.json sets neither body nor body_file",
		},
		{
			name: "exactly one, a null beside a value",
			in:   `{"name":"n","body":null,"body_file":"b.md"}`,
		},
		{
			name: "exactly one, both members null",
			in:   `{"name":"n","body":null,"body_file":null}`,
			want: "doc.json sets neither body nor body_file",
		},
		{
			// at most one: the group without required, which is what a
			// resolve-only thread entry is.
			name: "at most one, neither member set",
			in:   `{"name":"n","body":"b"}`,
		},
		{
			name: "at most one, both members set",
			in:   `{"name":"n","body":"b","draft":"d","final":"f"}`,
			want: "doc.json sets both draft and final",
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertValidate(t, tc.in, reflect.TypeFor[validDoc](), tc.want)
		})
	}
}

// TestValidateNamesWhereTheViolationIs is the point of the exercise: the
// message the parsers used to give restated a whole entry's shape, and a
// reader had to work out which key it meant.
func TestValidateNamesWhereTheViolationIs(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   string
		want string
	}{
		{
			name: "inside a nested object",
			in:   `{"name":"n","body":"b","ref":{"note":"x"}}`,
			want: "ref is missing number in doc.json",
		},
		{
			name: "inside a list, by index",
			in:   `{"name":"n","body":"b","refs":[{"number":1},{"note":"x"}]}`,
			want: "refs[1] is missing number in doc.json",
		},
		{
			// A null element is an element: it decodes into a zero struct
			// whose required keys are all absent, which is what the parsers
			// refused before this moved.
			name: "a null list element",
			in:   `{"name":"n","body":"b","refs":[null]}`,
			want: "refs[0] is missing number in doc.json",
		},
		{
			// Nothing is under a null object, so there is nothing to name.
			name: "a null nested object",
			in:   `{"name":"n","body":"b","ref":null}`,
		},
		{
			name: "an empty list",
			in:   `{"name":"n","body":"b","refs":[]}`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertValidate(t, tc.in, reflect.TypeFor[validDoc](), tc.want)
		})
	}
}

// TestValidateHasNothingToSayWithoutADeclaration keeps the output side quiet.
// render passes every document the module prints, most of whose types carry no
// tag and some of which are not documents at all.
func TestValidateHasNothingToSayWithoutADeclaration(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   string
		typ  reflect.Type
	}{
		{"an untagged struct", `{"anything":"x"}`, reflect.TypeFor[validUntagged]()},
		{"an untagged struct with nothing in it", `{}`, reflect.TypeFor[validUntagged]()},
		{"a type that is not a struct", `["a","b"]`, reflect.TypeFor[[]string]()},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertValidate(t, tc.in, tc.typ, "")
		})
	}
}

// validSelf serialises itself, so what it puts on the wire is not the field
// below and a declaration on that field describes a key nobody sends.
type validSelf struct {
	Need *string `json:"need" contract:"required"`
}

func (validSelf) MarshalJSON() ([]byte, error) { return []byte(`"self"`), nil }

type validSelfDoc struct {
	One  *validSelf  `json:"one"`
	Many []validSelf `json:"many"`
}

// TestValidateStopsAtATypeThatSerialisesItself is the case a walk keyed off the
// field's own type gets wrong: deref sees past a pointer but not into a list,
// so many would be descended into where one is not, and the two would disagree
// about the same type. Both are silence here — a required key under a type
// whose fields never reach the wire is not one a document could have supplied.
func TestValidateStopsAtATypeThatSerialisesItself(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   string
	}{
		{"reached through a pointer", `{"one":{}}`},
		{"reached through a list", `{"many":[{}]}`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assertValidate(t, tc.in, reflect.TypeFor[validSelfDoc](), "")
		})
	}
}

// TestUnmarshalRefusesBeforeItAnswers is the guarantee the entry point exists
// for: a caller cannot hold a decoded value the declaration was not applied to.
func TestUnmarshalRefusesBeforeItAnswers(t *testing.T) {
	var got validDoc
	err := Unmarshal([]byte(`{"body":"b"}`), &got, "doc.json")
	if err == nil {
		t.Fatal("Unmarshal accepted a document with no name")
	}
	var ve *ViolationError
	if !errors.As(err, &ve) {
		t.Errorf("error %[1]T (%[1]v) is not a *ViolationError, so a parser cannot tell it from a decode failure", err)
	}

	var ok validDoc
	if err := Unmarshal([]byte(`{"name":"n","body":"b"}`), &ok, "doc.json"); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if ok.Name == nil || *ok.Name != "n" {
		t.Errorf("Unmarshal did not decode name, got %+v", ok)
	}
}

// TestUnmarshalLeavesADecodeFailureAlone is the other half of requirement 4:
// each parser still maps the decoder's own complaint until #159 folds those in,
// so a mismatched type must not arrive dressed as a violation.
func TestUnmarshalLeavesADecodeFailureAlone(t *testing.T) {
	var got validDoc
	err := Unmarshal([]byte(`{"name":5}`), &got, "doc.json")
	if err == nil {
		t.Fatal("Unmarshal accepted a name that is not a string")
	}
	var ve *ViolationError
	if errors.As(err, &ve) {
		t.Errorf("a decode failure arrived as a *ViolationError: %v", err)
	}
}

// assertValidate runs Validate and reports on the message, since what these specify is
// the wording as much as the refusal.
func assertValidate(t *testing.T, in string, typ reflect.Type, want string) {
	t.Helper()

	err := Validate([]byte(in), typ, "doc.json")
	if want == "" {
		if err != nil {
			t.Errorf("Validate(%s) = %v, want it accepted", in, err)
		}
		return
	}
	if err == nil {
		t.Fatalf("Validate(%s) accepted a document, want %q", in, want)
	}
	if err.Error() != want {
		t.Errorf("Validate(%s) = %q, want %q", in, err, want)
	}
}
