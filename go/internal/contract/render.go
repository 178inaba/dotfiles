// Package contract renders what a ccx command prints and accepts, from the
// types that define it.
//
// Written into each command's help by hand it would be a transcription of a
// struct's json tags kept in another file, which is the arrangement
// 178inaba/dotfiles#131 exists to end: nothing fails when the two disagree.
// Everything a contract consists of is already declared, so it is rendered.
//
// Reflection cannot see doc comments or the constants of a named type. Those
// come from Table, which gen/ extracts from source into docs_gen.go.
package contract

import (
	encodingjson "encoding/json"
	json "encoding/json/v2"
	"fmt"
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
		name, opts, ok := jsonName(f)
		if !ok {
			continue
		}

		kind, err := tb.describe(f.Type, mode, f, opts)
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

func (tb Table) describe(t reflect.Type, mode Mode, f reflect.StructField, opts string) (string, error) {
	base, err := tb.kindOf(t)
	if err != nil {
		return "", err
	}

	switch {
	case mode == Input && f.Tag.Get("contract") == "required":
		return qualify(base, "required"), nil
	case mode == Input:
		return qualify(base, "optional"), nil
	case strings.Contains(opts, "omitzero"):
		// The key is left out rather than written as null, so saying both
		// would describe two shapes only one of which appears.
		if t.Kind() == reflect.Pointer {
			return qualify(base, "may be absent"), nil
		}
		return qualify(base, "absent when empty"), nil
	case t.Kind() == reflect.Pointer && !tb.marshals(t):
		// A type that serialises itself has already said whether it can be
		// null; the pointer is Go's business rather than the wire's.
		return qualify(base, "may be null"), nil
	}
	return base, nil
}

// qualify attaches how a field may be absent to what it is.
//
// In brackets and before the list: a value set is itself comma-separated, so
// "one of: a, b, required" reads as three values and a qualifier past the
// colon reads as one more member.
func qualify(base, q string) string {
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
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return "integer", nil
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
	for _, iface := range marshalerInterfaces {
		if t.Implements(iface) || reflect.PointerTo(t).Implements(iface) {
			return fmt.Errorf("contract: %s implements %s and is not in the marshaler table, so its JSON shape is not its fields", t, iface)
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
