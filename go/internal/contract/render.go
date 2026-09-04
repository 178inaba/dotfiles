// Package contract declares what a ccx command prints and accepts, renders it
// and enforces it, all from the types that define it.
//
// Written into each command's help by hand it would be a transcription of a
// struct's json tags kept in another file, which is the arrangement
// 178inaba/dotfiles#131 exists to end: nothing fails when the two disagree.
// Everything a contract consists of is already declared, so it is rendered.
//
// The same declaration is what refuses a document, at both boundaries one
// crosses: Unmarshal on the way in and render on the way out. A hand-written
// check beside the tag would be the same arrangement again, one layer down.
//
// Reflection cannot see doc comments or the constants of a named type. Those
// come from Table, which gen/ extracts from source into docs_gen.go.
package contract

import (
	encodingjson "encoding/json"
	json "encoding/json/v2"
	"fmt"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"unicode/utf8"
)

// marshalerInterfaces are the two ways a type takes its own serialisation over.
//
// Both versions, not just the one this module writes in: jsontext.Value
// implements only the v1 one, and a v2-only guard would walk into it and
// describe raw JSON as an array of numbers.
var marshalerInterfaces = []reflect.Type{
	reflect.TypeFor[json.MarshalerTo](),
	reflect.TypeFor[encodingjson.Marshaler](),
}

// Mode is which side of a command a type sits on, which is what decides how a
// pointer reads: null on the way out, omitted on the way in, where a
// contract:"required" tag carries the requirement instead.
type Mode int

const (
	// Output is a document the command prints.
	Output Mode = iota
	// Input is a document the command reads.
	Input
)

// Table is what the types cannot say about themselves at run time.
type Table struct {
	// The four tables are keyed "<import path>.<Type>" and, for a field or a
	// value, that with ".<name>" after it.
	Fields map[string]string
	Types  map[string]string
	// In declaration order.
	Enums    map[string][]string
	EnumDocs map[string]string
	// Every import path the table was read from. A type from anywhere else
	// stops the render rather than describing all of its fields as unexplained.
	Packages []string
	// A type with a custom marshaler and no entry here stops the render:
	// walking its Go fields would describe a shape that never reaches the wire,
	// which is worse than saying nothing.
	Marshalers map[reflect.Type]Marshaled
}

// Marshaled is what a type with a custom marshaler puts on the wire.
type Marshaled struct {
	Kind string
	// The type the value is made of, where the wire form wraps a list. Without
	// it a reader is told "array of object" and has to go looking.
	Elem reflect.Type
}

const (
	minNameColumn = 12
	// LineWidth is what a rendered contract is laid out to. Exported so that a
	// caller checking its own text against the same column does not restate it.
	LineWidth = 88
)

// Render describes t as the plain text a --help prints.
func (tb Table) Render(t reflect.Type, mode Mode) (string, error) {
	rows, err := tb.walk(t, mode, 0, map[reflect.Type]bool{})
	if err != nil {
		return "", err
	}

	// In runes: three of the assessment values are Japanese, and a column
	// measured in bytes puts their rows out of line with the rest.
	width := minNameColumn
	for _, r := range rows {
		if end := len(r.indent()) + utf8.RuneCountInString(r.name) + 2; end > width {
			width = end
		}
	}

	var b strings.Builder
	for _, line := range wrap(tb.Types[typeKey(deref(t))], LineWidth-2) {
		b.WriteString("  " + line + "\n")
	}
	if b.Len() > 0 {
		b.WriteString("\n")
	}

	pad := strings.Repeat(" ", width)
	for _, r := range rows {
		head := r.indent() + r.name
		// A row with nothing in its second column — a group's heading, or a
		// value nobody explained — still has a name to print, and the loop
		// below runs once per line of that column.
		if r.kind == "" {
			b.WriteString(head + "\n")
		}
		for i, line := range wrap(r.kind, LineWidth-width) {
			if i == 0 {
				b.WriteString(head + pad[:width-utf8.RuneCountInString(head)] + line + "\n")
				continue
			}
			b.WriteString(pad + line + "\n")
		}
		for _, line := range wrap(r.doc, LineWidth-width) {
			b.WriteString(pad + line + "\n")
		}
	}
	return b.String(), nil
}

// Identifiers is every name a contract publishes: the JSON keys, the members
// of every value set, and the keys of any nested document.
//
// A skill may name one of these where it acts on it, and anything else that
// looks like one is a reference to something gone. Rendered from the same walk
// as the help, so the two cannot disagree about what the contract contains.
func (tb Table) Identifiers(t reflect.Type) ([]string, error) {
	rows, err := tb.walk(t, Output, 0, map[reflect.Type]bool{})
	if err != nil {
		return nil, err
	}
	out := make([]string, 0, len(rows))
	for _, r := range rows {
		// A group's heading is a sentence about the fields under it rather
		// than a name a skill could refer to.
		if r.heading {
			continue
		}
		out = append(out, r.name)
		out = append(out, r.enum...)
	}
	return out, nil
}

type row struct {
	depth int
	name  string
	kind  string
	doc   string
	// The field's value set, explained members or not: the rendering shows it
	// only where they were, and Identifiers wants it either way.
	enum []string
	// heading marks the row that states an exclusive group's cardinality. It
	// carries no kind and no doc, and is the one row that names nothing.
	heading bool
}

func (r row) indent() string { return strings.Repeat("  ", r.depth+1) }

// walk turns a struct into rows. seen breaks a cycle rather than reporting one: no contract type is
// self-referential today, and recursing for ever is a worse answer to one
// appearing than a name printed without its fields.
func (tb Table) walk(t reflect.Type, mode Mode, depth int, seen map[reflect.Type]bool) ([]row, error) {
	t = deref(t)
	if t.Kind() != reflect.Struct {
		return nil, fmt.Errorf("contract: %s is not a struct", t)
	}
	if len(tb.Packages) > 0 && !slices.Contains(tb.Packages, t.PkgPath()) {
		return nil, fmt.Errorf("contract: %s is in %s, which the doc table was not read from", t, t.PkgPath())
	}
	if seen[t] {
		return nil, nil
	}
	seen[t] = true
	defer delete(seen, t)

	var rows []row
	for i := range t.NumField() {
		f := t.Field(i)
		name, opts, named := jsonName(f)
		// Whether or not the field has a json name, so that a declaration on
		// an embedded group — which never has one — is checked like any other.
		values, err := contractValues(t, f, named)
		if err != nil {
			return nil, err
		}

		if !named {
			group, err := tb.group(t, f, values, mode, depth, seen)
			if err != nil {
				return nil, err
			}
			rows = append(rows, group...)
			continue
		}

		kind, err := tb.describe(f.Type, mode, values, opts)
		if err != nil {
			return nil, err
		}
		rows = append(rows, row{
			depth: depth, name: name, kind: kind,
			doc:  tb.Fields[key(t, f.Name)],
			enum: tb.Enums[typeKey(enumOf(f.Type))],
		})

		rows = append(rows, tb.values(f.Type, depth+1)...)

		if inner, ok := tb.nested(f.Type); ok {
			nested, err := tb.walk(inner, mode, depth+1, seen)
			if err != nil {
				return nil, err
			}
			rows = append(rows, nested...)
		}
	}
	return rows, nil
}

// group renders an embedded struct, whose fields json inlines into the
// document the parent describes.
//
// Without exclusive they are the parent's own fields and are rendered as such.
// With it the group has a cardinality of its own, which is a statement about
// the fields together rather than about any one of them, so it goes on a
// heading they sit under.
func (tb Table) group(t reflect.Type, f reflect.StructField, values []string, mode Mode, depth int, seen map[reflect.Type]bool) ([]row, error) {
	// A field the wire never sees, embedded or not, has nothing to describe.
	// json:"-" reaches here as an unnamed field too, and is not inlined.
	inner := groupType(f)
	if inner == nil {
		// Nothing of the field reaches the document, so a declaration on it is
		// read by nobody — the same silent no-op contractValues refuses where
		// a value has the wrong sort of field to bind to.
		if len(values) > 0 {
			return nil, fmt.Errorf("contract: %s.%s declares %s, but nothing of the field reaches the document",
				t, f.Name, strings.Join(values, ", "))
		}
		return nil, nil
	}
	// A group is nothing but its fields, so a type that puts something else on
	// the wire has none to inline. Refused for the reason checkMarshaler
	// refuses: describing fields nobody sends is worse than saying nothing.
	if tb.marshals(inner) {
		return nil, fmt.Errorf("contract: %s is embedded in %s and serialises itself, so its fields are not what would be inlined", inner, t)
	}
	if err := tb.checkMarshaler(inner); err != nil {
		return nil, err
	}

	var rows []row
	if slices.Contains(values, "exclusive") {
		heading := "at most one of:"
		if slices.Contains(values, "required") {
			heading = "exactly one of:"
		}
		rows = append(rows, row{depth: depth, name: heading, heading: true})
		depth++
	}
	members, err := tb.walk(inner, mode, depth, seen)
	if err != nil {
		return nil, err
	}
	return append(rows, members...), nil
}

// groupType is the struct an embedded field inlines into the document its
// parent describes, or nil where the field is not one.
//
// Shared with the validator rather than restated there, so that what a --help
// renders as a group and what a refusal reads as one cannot come apart.
// json:"-" arrives here as an unnamed field like any other and is not inlined.
func groupType(f reflect.StructField) reflect.Type {
	inner := deref(f.Type)
	if !f.Anonymous || f.Tag.Get("json") == "-" || inner.Kind() != reflect.Struct {
		return nil
	}
	return inner
}

// valueConstraint is a rule about what a key holds, rather than about whether
// it is there.
type valueConstraint struct {
	// value is the word a contract tag declares the rule with.
	value string
	// kinds is what the rule constrains. A rule about a string is enforced by
	// nothing on a field that is not one, so declaring it there is refused for
	// the reason a misspelt value is.
	//
	// A set rather than one kind, because JSON has one number and Go has five
	// integers: what a rule about an integer's value can be said of is the
	// width-independent thing the document holds, not the width the field
	// happens to have.
	kinds []reflect.Kind
	// noun is how a refusal names that set, in the words the decoder's own
	// messages use — the set is what it describes, so spelling out the Go
	// kinds would answer a question the reader did not ask.
	noun string
	// text is how a rendered row states the rule, and refusal how a violation
	// does, after "sets <key> ".
	text    string
	refusal string
	// refuses reports whether the value a document supplied breaks the rule.
	// What it receives is what the decoder made of the document rather than
	// the field's Go type, so a rule about numbers would be handed a float64.
	refuses func(any) bool
}

// valueConstraints is the whole vocabulary of rules about values: how each one
// renders and what each one refuses, in one place.
//
// Not a rendering list beside a checking switch, because a rule that rendered
// without being enforced is the published-but-unchecked rule this package
// exists to end, one layer up.
//
// The order is the order a row states them in, so that the same declaration
// reads the same way however the tag was written.
var valueConstraints = []valueConstraint{
	{
		value: "barefilename", kinds: stringKinds, noun: "a string",
		text: "a bare file name", refusal: "to a path, not a bare file name",
		refuses: func(v any) bool {
			s, ok := v.(string)
			return ok && strings.ContainsRune(s, filepath.Separator)
		},
	},
	{
		value: "nonempty", kinds: stringKinds, noun: "a string",
		text: "not empty", refusal: "to an empty string",
		refuses: func(v any) bool {
			s, ok := v.(string)
			return ok && s == ""
		},
	},
	{
		value: "positive", kinds: integerKinds, noun: "an integer",
		text: "positive", refusal: "to a number that is not positive",
		refuses: func(v any) bool {
			// A float64 because that is what a JSON number decodes into
			// through an any, whatever the width of the field behind it.
			n, ok := v.(float64)
			return ok && n <= 0
		},
	},
}

// integerKinds and stringKinds are the Go kinds behind one JSON kind, which is
// the granularity a rule about a value binds at.
//
// integerKinds is read by kindOf and jsonWord too, so what the renderer calls
// an integer, what a decode failure asks for, and what a rule about an
// integer's value may be declared on are one list rather than three.
var (
	integerKinds = []reflect.Kind{reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64}
	stringKinds  = []reflect.Kind{reflect.String}
)

// knownValues is every constraint this renderer can state: the two about where
// a key is, and the vocabulary above about what it holds.
//
// An unrecognised one stops the render rather than being ignored: a struct tag
// is stringly typed, and a misspelt constraint that rendered as an ordinary
// optional field would be enforced by nothing and look like a decision.
var knownValues = func() []string {
	names := []string{"required", "exclusive"}
	for _, c := range valueConstraints {
		names = append(names, c.value)
	}
	return names
}()

// contractValues reads a field's declared constraints. Several are written
// comma-separated in the one tag, since a field carries as many as are true of
// it.
func contractValues(t reflect.Type, f reflect.StructField, named bool) ([]string, error) {
	tag, ok := f.Tag.Lookup("contract")
	if !ok {
		return nil, nil
	}
	values := strings.Split(tag, ",")
	for _, v := range values {
		if !slices.Contains(knownValues, v) {
			return nil, fmt.Errorf("contract: %s.%s declares %q, which is not one of %s",
				t, f.Name, v, strings.Join(knownValues, ", "))
		}
	}

	// Where a value has nothing to bind to it is read by nobody, which is the
	// same silent no-op a misspelling would be. exclusive marks a group, and a
	// field with a key of its own is not one; required is that key's presence,
	// or, beside exclusive, the group's cardinality.
	exclusive := slices.Contains(values, "exclusive")
	switch {
	case exclusive && named:
		return nil, fmt.Errorf("contract: %s.%s declares exclusive, which marks an embedded group rather than a field with a key of its own", t, f.Name)
	case slices.Contains(values, "required") && !named && !exclusive:
		return nil, fmt.Errorf("contract: %s.%s declares required with no key of its own and no exclusive group to be the cardinality of", t, f.Name)
	}

	// A rule about a value binds the kind it names, so one written on a field
	// of another kind is read by nobody — the same silent no-op again.
	kind := deref(f.Type).Kind()
	for _, c := range valueConstraints {
		if slices.Contains(values, c.value) && !slices.Contains(c.kinds, kind) {
			return nil, fmt.Errorf("contract: %s.%s declares %s, which constrains %s, on a field of kind %s",
				t, f.Name, c.value, c.noun, kind)
		}
	}
	return values, nil
}

func (tb Table) describe(t reflect.Type, mode Mode, values []string, opts string) (string, error) {
	base, err := tb.kindOf(t)
	if err != nil {
		return "", err
	}

	var qs []string
	switch {
	// Read on either side: "you must supply this" on the way in and "this is
	// always present" on the way out are the same statement about the wire.
	case slices.Contains(values, "required"):
		qs = append(qs, "required")
	case mode == Input:
		qs = append(qs, "optional")
	case strings.Contains(opts, "omitzero"):
		// The key is left out rather than written as null, so saying both
		// would describe two shapes only one of which appears.
		if t.Kind() == reflect.Pointer {
			qs = append(qs, "may be absent")
		} else {
			qs = append(qs, "absent when empty")
		}
	case t.Kind() == reflect.Pointer && !tb.marshals(t):
		// A type that serialises itself has already said whether it can be
		// null; the pointer is Go's business rather than the wire's.
		qs = append(qs, "may be null")
	}

	// After the one above rather than beside it: whether the key has to be
	// there is the first thing a reader asks, and what it may hold the second.
	for _, c := range valueConstraints {
		if slices.Contains(values, c.value) {
			qs = append(qs, c.text)
		}
	}
	return qualify(base, qs...), nil
}

// qualify attaches how a field may be absent, and what it may hold, to what it
// is.
//
// In brackets and before the list: a value set is itself comma-separated, so
// "one of: a, b, required" reads as three values and a qualifier past the
// colon reads as one more member.
func qualify(base string, qs ...string) string {
	if len(qs) == 0 {
		return base
	}
	q := strings.Join(qs, ", ")
	if kind, values, found := strings.Cut(base, ", one of"); found {
		return kind + " (" + q + "), one of" + values
	}
	return base + " (" + q + ")"
}

// enumOf is the named type behind a field, past any pointer or list: where a
// value set is declared, and where a nested document's fields are.
func enumOf(t reflect.Type) reflect.Type {
	t = deref(t)
	for t.Kind() == reflect.Slice || t.Kind() == reflect.Array {
		t = deref(t.Elem())
	}
	return t
}

func (tb Table) values(t reflect.Type, depth int) []row {
	t = enumOf(t)
	if t.Kind() != reflect.String || !tb.documented(t) {
		return nil
	}
	rows := make([]row, 0, len(tb.Enums[typeKey(t)]))
	for _, v := range tb.Enums[typeKey(t)] {
		rows = append(rows, row{depth: depth, name: v, kind: tb.EnumDocs[typeKey(t)+"."+v]})
	}
	return rows
}

func (tb Table) documented(t reflect.Type) bool {
	for _, v := range tb.Enums[typeKey(t)] {
		if tb.EnumDocs[typeKey(t)+"."+v] != "" {
			return true
		}
	}
	return false
}

func (tb Table) marshals(t reflect.Type) bool {
	_, ok := tb.Marshalers[deref(t)]
	return ok
}

func (tb Table) kindOf(t reflect.Type) (string, error) {
	// The override is looked up through a pointer, since a marshaler declared
	// on the value is reached by the pointer too and the wire form is the same.
	if over, ok := tb.Marshalers[deref(t)]; ok {
		return over.Kind, nil
	}
	if err := tb.checkMarshaler(t); err != nil {
		return "", err
	}

	if slices.Contains(integerKinds, t.Kind()) {
		return "integer", nil
	}

	switch t.Kind() {
	case reflect.Pointer:
		return tb.kindOf(t.Elem())
	case reflect.Slice, reflect.Array:
		of, err := tb.kindOf(t.Elem())
		if err != nil {
			return "", err
		}
		return "array of " + of, nil
	case reflect.Struct:
		return "object", nil
	case reflect.Bool:
		return "boolean", nil
	case reflect.String:
		if values := tb.Enums[typeKey(t)]; len(values) > 0 {
			if tb.documented(t) {
				return "string, one of:", nil
			}
			return "string, one of: " + strings.Join(values, ", "), nil
		}
		return "string", nil
	}
	return "", fmt.Errorf("contract: no rendering for %s (kind %s)", t, t.Kind())
}

// checkMarshaler is the guard: a type that serialises itself and has not said
// what it serialises as would be described by its Go fields, which is a
// confident lie rather than a gap.
func (tb Table) checkMarshaler(t reflect.Type) error {
	if iface := marshaler(t); iface != nil {
		return fmt.Errorf("contract: %s implements %s and is not in the marshaler table, so its JSON shape is not its fields", t, iface)
	}
	return nil
}

// marshaler is the interface a type takes its own serialisation over, or nil
// where its Go fields are what reach the wire.
//
// Separate from checkMarshaler so that the validator, which passes over such a
// type rather than refusing it, can ask without building a message it throws
// away.
func marshaler(t reflect.Type) reflect.Type {
	for _, iface := range marshalerInterfaces {
		if t.Implements(iface) || reflect.PointerTo(t).Implements(iface) {
			return iface
		}
	}
	return nil
}

func jsonName(f reflect.StructField) (name, opts string, ok bool) {
	tag, ok := f.Tag.Lookup("json")
	if !ok || tag == "-" {
		return "", "", false
	}
	name, opts, _ = strings.Cut(tag, ",")
	return name, opts, name != ""
}

// nested is the struct whose fields belong under a field. A type that
// serialises itself says which one it is, since its own fields are not what
// reaches the wire.
func (tb Table) nested(t reflect.Type) (reflect.Type, bool) {
	if over, ok := tb.Marshalers[deref(t)]; ok {
		return over.Elem, over.Elem != nil
	}
	inner := enumOf(t)
	return inner, inner.Kind() == reflect.Struct
}

func deref(t reflect.Type) reflect.Type {
	for t.Kind() == reflect.Pointer {
		t = t.Elem()
	}
	return t
}

func key(t reflect.Type, field string) string { return typeKey(t) + "." + field }

func typeKey(t reflect.Type) string { return t.PkgPath() + "." + t.Name() }

// Wrap breaks text to the width a help is laid out at.
//
// Exported because an intro that is built rather than typed — the schema's key
// list, say — has the same column to stay inside.
func Wrap(text string) string { return strings.Join(wrap(text, LineWidth), "\n") }

// wrap breaks text to width in runes: a doc comment may hold an em dash, and
// a column is measured in what a terminal shows.
func wrap(text string, width int) []string {
	if text == "" {
		return nil
	}
	var lines []string
	line, n := "", 0
	for _, word := range strings.Fields(text) {
		w := utf8.RuneCountInString(word)
		switch {
		case line == "":
			line, n = word, w
		case n+1+w <= width:
			line, n = line+" "+word, n+1+w
		default:
			lines = append(lines, line)
			line, n = word, w
		}
	}
	return append(lines, line)
}
