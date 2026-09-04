package contract

import (
	json "encoding/json/v2"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strconv"
	"strings"
)

// Unmarshal decodes b into v and holds it to v's declaration.
func Unmarshal(b []byte, v any, document string) error {
	return std.Unmarshal(b, v, document)
}

// Validate holds bytes to the declaration on t.
func Validate(b []byte, t reflect.Type, document string) error {
	return std.Validate(b, t, document)
}

// Unmarshal decodes b into v and checks it against v's declaration, so that a
// caller cannot obtain a decoded value the declaration was not applied to.
//
// The decode comes first, so a document that is malformed as well as
// incomplete reports the malformation. Both halves refuse in the same words:
// the decoder's own complaint is translated here rather than in each parser,
// so that neither a missing field nor a mistyped one restates a whole entry.
//
// document is what a refusal names — the file a parser was given, or whatever
// a command reading standard input calls that input.
func (tb Table) Unmarshal(b []byte, v any, document string) error {
	if err := json.Unmarshal(b, v); err != nil {
		return tb.decodeError(err, reflect.TypeOf(v), document)
	}
	return tb.Validate(b, reflect.TypeOf(v), document)
}

// Validate checks bytes against the declaration on t.
//
// Against the bytes rather than a decoded value, because that is the only
// place the two halves of presence are both visible: absent and null are the
// same nil once a document has been decoded into a pointer, and the same zero
// once it has been decoded into a string.
//
// The same call serves the way out, where the bytes are what render produced.
func (tb Table) Validate(b []byte, t reflect.Type, document string) error {
	var doc any
	if err := json.Unmarshal(b, &doc); err != nil {
		return err
	}
	obj, _ := doc.(map[string]any)
	return tb.check(t, obj, "", document)
}

// check reads one object against the declaration on the struct it decoded into.
//
// A nil obj is a document that supplied no key at all, which is what its
// required fields are then missing.
func (tb Table) check(t reflect.Type, obj map[string]any, path, document string) error {
	t = deref(t)
	if t.Kind() != reflect.Struct {
		return nil
	}

	for i := range t.NumField() {
		f := t.Field(i)
		name, _, named := jsonName(f)
		values, err := contractValues(t, f, named)
		if err != nil {
			return err
		}

		if !named {
			if err := tb.checkGroup(f, values, obj, path, document); err != nil {
				return err
			}
			continue
		}

		if slices.Contains(values, "required") && !supplied(obj, name) {
			return violation(path, document, "is missing "+name)
		}
		if err := checkValue(values, obj, name, path, document); err != nil {
			return err
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

// checkValue holds what a document put under a key to the rules its field
// declares about values.
//
// Read from the document rather than from the decoded field, which is what
// binds a rule to the kind it names: an empty body_file is the same empty
// string whether the field behind it is a string or a *string.
//
// Only a supplied key has a value to be held to a rule, read through the same
// predicate the presence check above uses: saying a value may not be empty is
// not saying a key has to be there.
func checkValue(values []string, obj map[string]any, name, path, document string) error {
	if !supplied(obj, name) {
		return nil
	}
	for _, c := range valueConstraints {
		if slices.Contains(values, c.value) && c.refuses(obj[name]) {
			return violation(path, document, "sets "+name+" "+c.refusal)
		}
	}
	return nil
}

// checkGroup reads the declaration on an embedded struct, whose fields json
// inlines into the very object its parent was read from.
//
// The three conditions below are the ones group renders against, asked through
// the same three helpers: not a group at all, or a group whose fields are not
// what reaches the wire. It refuses where this passes over, because render
// checks every document the module prints — types the doc table was never read
// from included — and saying nothing there beats saying the wrong thing.
func (tb Table) checkGroup(f reflect.StructField, values []string, obj map[string]any, path, document string) error {
	inner := groupType(f)
	if inner == nil || tb.marshals(inner) || marshaler(inner) != nil {
		return nil
	}
	if slices.Contains(values, "exclusive") {
		if err := cardinality(inner, obj, slices.Contains(values, "required"), path, document); err != nil {
			return err
		}
	}
	return tb.check(inner, obj, path, document)
}

// cardinality reports how many of a group's keys a document may supply, and
// which it did. Read off the tag the way group reads its heading off it, so
// that what a --help promises and what a refusal says cannot drift apart.
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
// them reads. Every group has exactly two today; the general form is there so
// that declaring a third changes the wording rather than making it wrong.
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

// walkValue descends into whatever the document holds under a key.
//
// A value that is neither a list nor an object — a null list element is the
// one that arrives here — reaches check as a nil map, which is every key
// absent rather than a document nobody looked at.
func (tb Table) walkValue(inner reflect.Type, value any, path, document string) error {
	if list, ok := value.([]any); ok {
		for i, e := range list {
			if err := tb.walkValue(inner, e, index(path, strconv.Itoa(i)), document); err != nil {
				return err
			}
		}
		return nil
	}
	obj, _ := value.(map[string]any)
	return tb.check(inner, obj, path, document)
}

// contained is the struct a value's contents are made of, and whether its Go
// fields are what reach the wire.
//
// Shaped like kindOf rather than like nested: a list's element may itself be
// either of the two cases below, and asking the list is the shallower
// question. nested can be shallow because walk reaches it only once describe
// has put the element through kindOf, which is where the renderer refuses.
func (tb Table) contained(t reflect.Type) (reflect.Type, bool) {
	t = deref(t)
	if over, ok := tb.Marshalers[t]; ok {
		return over.Elem, over.Elem != nil
	}
	if marshaler(t) != nil {
		return nil, false
	}
	if t.Kind() == reflect.Slice || t.Kind() == reflect.Array {
		return tb.contained(t.Elem())
	}
	return t, t.Kind() == reflect.Struct
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
		return errors.New(document + " " + what)
	}
	return fmt.Errorf("%s %s in %s", path, what, document)
}

// join and index spell the two steps a path can take. Shared with the walk
// that reads a decoder's pointer, so that a missing key and a mistyped one
// name the same place in the same notation.
func index(path, i string) string { return path + "[" + i + "]" }

func join(path, name string) string {
	if path == "" {
		return name
	}
	return path + "." + name
}
