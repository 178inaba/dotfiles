package contract

import (
	"encoding/json/jsontext"
	json "encoding/json/v2"
	"errors"
	"fmt"
	"reflect"
	"strconv"
)

// decodeError turns the decoder's complaint into the vocabulary a violation
// already speaks, so that one document does not refuse in two styles.
//
// Built from the pointer and the declaring type rather than from a table each
// parser keeps of its own: the shape such a table restates is the shape
// --help publishes.
//
// The split is three ways rather than two. A root mismatch is a SemanticError
// too, with an empty pointer, so branching on the error type alone would send
// it down the field-naming path and produce a message with no field in it.
// Which of the two a decode failure is, violation decides from the same empty
// path a missing key at the top level arrives with.
func (tb Table) decodeError(err error, t reflect.Type, document string) error {
	var se *json.SemanticError
	if !errors.As(err, &se) {
		return notJSON(document, err)
	}
	path, at, ok := tb.resolvePointer(t, se.JSONPointer)
	if !ok {
		return notJSON(document, err)
	}
	word, ok := jsonWord(at)
	if !ok {
		return notJSON(document, err)
	}
	return violation(path, document, "must be "+word)
}

// notJSON is what is left to say where nothing narrower can be: the bytes were
// not usable, and the decoder's own words are the only diagnosis.
//
// Naming a field worked out lexically would be worse than saying less, which
// is the posture the validator takes towards a type it cannot describe.
func notJSON(document string, err error) error {
	return fmt.Errorf("invalid JSON in %s (%v)", document, err)
}

// resolvePointer walks the declaration alongside the decoder's RFC 6901
// pointer, and reports the path in the notation a reader of the document reads
// (refs[1].number) together with the type the document was held to there.
//
// Walking rather than reading the tokens off is what tells an index from a
// key: a pointer says only that the step was "1", and which of the two it is
// belongs to the type. The path is spelt through the same join and index a
// violation goes through, so both halves of a refusal name a place the same
// way.
//
// The type comes back from the walk rather than from the decoder's own GoType,
// which carries the same answer: what a message says a field should have been
// is then what the declaration published, not what the decoder made of it.
func (tb Table) resolvePointer(t reflect.Type, p jsontext.Pointer) (string, reflect.Type, bool) {
	var path string
	for tok := range p.Tokens() {
		t = deref(t)
		if tb.opaque(t) {
			return "", nil, false
		}

		switch t.Kind() {
		case reflect.Slice, reflect.Array:
			if _, err := strconv.Atoi(tok); err != nil {
				return "", nil, false
			}
			path = index(path, tok)
			t = t.Elem()
		case reflect.Struct:
			f, ok := jsonField(t, tok)
			if !ok {
				return "", nil, false
			}
			path = join(path, tok)
			t = f
		default:
			return "", nil, false
		}
	}

	// The last type is asked the same question as every one before it: a leaf
	// that serialises itself would otherwise be described by its Go kind, and
	// a struct that marshals as an array would be told to be an object.
	t = deref(t)
	if tb.opaque(t) {
		return "", nil, false
	}
	return path, t, true
}

// opaque reports whether a type puts something other than its Go fields on the
// wire, which is where the walk stops: a pointer into such a type is not about
// the fields the declaration published, and the table's own word for its shape
// is a whole phrase rather than something a "must be" can take.
func (tb Table) opaque(t reflect.Type) bool { return tb.marshals(t) || marshaler(t) != nil }

// jsonField is the field a key reaches, looking inside an embedded group the
// way json inlines one: a group's members reach the wire as keys of the object
// its parent was read from, so the decoder's pointer at one of them is flat.
func jsonField(t reflect.Type, name string) (reflect.Type, bool) {
	for i := range t.NumField() {
		f := t.Field(i)
		if n, _, named := jsonName(f); named {
			if n == name {
				return f.Type, true
			}
			continue
		}
		if inner := groupType(f); inner != nil {
			if ft, ok := jsonField(inner, name); ok {
				return ft, true
			}
		}
	}
	return nil, false
}

// jsonWord is the JSON kind a Go type is read from, with the article a
// sentence about it needs.
//
// Not kindOf, whose vocabulary describes the declaration to the reader of a
// --help — integer, array of object, string, one of: — where this names the
// kind the decoder wanted and JSON has no integer.
//
// Its subject is the kind and not the value: a number too large for an int
// arrives here as a number that could not be handled, and is told it must be
// one. Value rules are a declaration of their own (178inaba/dotfiles#160,
// 178inaba/dotfiles#161) rather than something to guess at from a kind.
func jsonWord(t reflect.Type) (string, bool) {
	switch t.Kind() {
	case reflect.Bool:
		return "a boolean", true
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Float32, reflect.Float64:
		return "a number", true
	case reflect.String:
		return "a string", true
	case reflect.Slice, reflect.Array:
		return "an array", true
	case reflect.Struct, reflect.Map:
		return "an object", true
	}
	return "", false
}
