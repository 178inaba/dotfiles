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
	if path == "" {
		return fmt.Errorf("%s must be %s", document, word)
	}
	return fmt.Errorf("%s must be %s in %s", path, word, document)
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
// belongs to the type. The path is spelt with the same join and the same
// brackets a violation uses, so both halves of a refusal name a place the same
// way.
func (tb Table) resolvePointer(t reflect.Type, p jsontext.Pointer) (string, reflect.Type, bool) {
	var path string
	for tok := range p.Tokens() {
		t = deref(t)
		// A type that serialises itself puts something other than its fields
		// on the wire, so the rest of the pointer is not about them.
		if _, over := tb.Marshalers[t]; over || marshaler(t) != nil {
			return "", nil, false
		}

		switch t.Kind() {
		case reflect.Slice, reflect.Array:
			if _, err := strconv.Atoi(tok); err != nil {
				return "", nil, false
			}
			path += "[" + tok + "]"
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
	return path, deref(t), true
}

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
