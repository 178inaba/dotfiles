package contract

import (
	json "encoding/json/v2"
	"fmt"
	"reflect"
	"slices"
	"strconv"
	"strings"
)

// ViolationError is a document that decoded but does not match the declaration
// on the type it decoded into.
//
// A type of its own so that a caller can tell it from a decode failure: each
// parser still wraps the decoder's complaint in a message of its own, and one
// of those closing over a field-level refusal would lose it.
type ViolationError struct{ msg string }

func (e *ViolationError) Error() string { return e.msg }

// Unmarshal decodes b into v and checks it against v's declaration, so that a
// caller cannot obtain a decoded value the declaration was not applied to.
//
// document is what a refusal names, which is the file a parser was given, or
// whatever a command reading standard input calls that input.
func Unmarshal(b []byte, v any, document string) error {
	return std.Unmarshal(b, v, document)
}

// Validate checks bytes against the declaration on t. The same call serves the
// way out, where the bytes are what render produced rather than what a caller
// supplied.
func Validate(b []byte, t reflect.Type, document string) error {
	return std.Validate(b, t, document)
}

// Unmarshal decodes b into v and checks it against v's declaration.
//
// The decode comes first so that a document that is malformed as well as
// incomplete reports the malformation: the parsers still map the decoder's own
// errors, and 178inaba/dotfiles#159 is where those join the same translation.
func (tb Table) Unmarshal(b []byte, v any, document string) error {
	if err := json.Unmarshal(b, v); err != nil {
		return err
	}
	return tb.Validate(b, reflect.TypeOf(v), document)
}

// Validate checks bytes against the declaration on t.
//
// Against the bytes rather than a decoded value, because that is the only
// place the two halves of presence are both visible: absent and null are the
// same nil once a document has been decoded into a pointer, and the same zero
// once it has been decoded into a string.
func (tb Table) Validate(b []byte, t reflect.Type, document string) error {
	if t == nil {
		return nil
	}
	var doc any
	if err := json.Unmarshal(b, &doc); err != nil {
		return err
	}
	return tb.check(t, doc, "", document)
}

// check reads one object against the declaration on the struct it decoded into.
//
// Driven by the JSON value rather than by the Go one, which is what lets a
// null be told from an absent key, and what keeps a field this walks past from
// mattering: a type is descended into only where the document actually holds
// an object or a list there.
func (tb Table) check(t reflect.Type, value any, path, document string) error {
	t = deref(t)
	if t.Kind() != reflect.Struct {
		return nil
	}
	// A value that is not an object — a null list element is the one that
	// arrives here — supplies no key at all, which is what its required
	// fields are then missing.
	obj, _ := value.(map[string]any)

	for i := range t.NumField() {
		f := t.Field(i)
		name, _, named := jsonName(f)
		values, err := contractValues(t, f, named)
		if err != nil {
			return err
		}

		if !named {
			if err := tb.checkGroup(f, values, obj, value, path, document); err != nil {
				return err
			}
			continue
		}

		if slices.Contains(values, "required") && !supplied(obj, name) {
			return violation(path, document, "is missing "+name)
		}
		inner, ok := tb.contained(f.Type)
		if !ok || obj[name] == nil {
			continue
		}
		if err := tb.walkValue(inner, obj[name], join(path, name), document); err != nil {
			return err
		}
	}
	return nil
}

// checkGroup reads the declaration on an embedded struct, whose fields json
// inlines into the very object its parent was read from.
func (tb Table) checkGroup(f reflect.StructField, values []string, obj map[string]any, value any, path, document string) error {
	inner := groupType(f)
	if inner == nil || tb.marshals(inner) || tb.checkMarshaler(inner) != nil {
		return nil
	}
	if slices.Contains(values, "exclusive") {
		if err := cardinality(inner, obj, slices.Contains(values, "required"), path, document); err != nil {
			return err
		}
	}
	return tb.check(inner, value, path, document)
}

// cardinality reports how many of a group's keys a document may supply, and
// which it did.
//
// Derived from the tag the same way the rendered heading is, so "exactly one
// of" in a --help and this refusal cannot come to disagree.
func cardinality(inner reflect.Type, obj map[string]any, exactlyOne bool, path, document string) error {
	var members, set []string
	for i := range inner.NumField() {
		name, _, named := jsonName(inner.Field(i))
		if !named {
			continue
		}
		members = append(members, name)
		if supplied(obj, name) {
			set = append(set, name)
		}
	}
	switch {
	case len(set) > 1:
		return violation(path, document, "sets "+tooMany(set))
	case exactlyOne && len(set) == 0:
		return violation(path, document, "sets "+none(members))
	}
	return nil
}

// tooMany and none spell a group's members the way a sentence about two of
// them reads, since every group has exactly two today and "more than one of a,
// b" would be a stilted way to say it.
func tooMany(set []string) string {
	if len(set) == 2 {
		return "both " + set[0] + " and " + set[1]
	}
	return "more than one of " + strings.Join(set, ", ")
}

func none(members []string) string {
	if len(members) == 2 {
		return "neither " + members[0] + " nor " + members[1]
	}
	return "none of " + strings.Join(members, ", ")
}

// walkValue descends into whatever the document holds under a key: an object,
// or a list of them at any depth.
func (tb Table) walkValue(inner reflect.Type, value any, path, document string) error {
	list, ok := value.([]any)
	if !ok {
		return tb.check(inner, value, path, document)
	}
	for i, e := range list {
		if err := tb.walkValue(inner, e, path+"["+strconv.Itoa(i)+"]", document); err != nil {
			return err
		}
	}
	return nil
}

// contained is the struct a value's contents are made of, and whether its Go
// fields are what reach the wire.
//
// Unlike the renderer's nested, a type that serialises itself and has not said
// what it serialises as is skipped rather than refused: render validates every
// document the module prints, including types the doc table was never read
// from, and a walk that stops early there says nothing rather than the wrong
// thing.
func (tb Table) contained(t reflect.Type) (reflect.Type, bool) {
	if over, ok := tb.Marshalers[deref(t)]; ok {
		return over.Elem, over.Elem != nil
	}
	if tb.checkMarshaler(deref(t)) != nil {
		return nil, false
	}
	inner := enumOf(t)
	return inner, inner.Kind() == reflect.Struct
}

// supplied reports whether a document gave a key a value. An explicit null is
// the writer saying "not this one", which is what leaving the key out says.
func supplied(obj map[string]any, name string) bool {
	v, ok := obj[name]
	return ok && v != nil
}

// violation names the object the offending key sits in, which at the top level
// is the document itself.
func violation(path, document, what string) error {
	if path == "" {
		return &ViolationError{msg: document + " " + what}
	}
	return &ViolationError{msg: fmt.Sprintf("%s %s in %s", path, what, document)}
}

func join(path, name string) string {
	if path == "" {
		return name
	}
	return path + "." + name
}
