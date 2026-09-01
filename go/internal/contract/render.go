// Package contract renders what a ccx command prints and accepts, from the
// types that define it.
//
// A skill's only relationship with ccx is that it runs a command, so the
// contract has to be obtainable from the command itself. Writing it into each
// command's help by hand would put a transcription of a struct's json tags in
// a different file from the struct, which is the arrangement 178inaba/dotfiles#131
// exists to end: nothing fails when the two disagree. Everything a contract
// consists of is already declared — the tags give the names and their order,
// pointer-ness gives nullability, omitzero gives absence, a named string type
// with constants gives a value set, and the field's doc comment gives the
// meaning — so the contract is rendered from the type rather than written
// about it.
//
// Reflection cannot see doc comments or the constants of a named type. Those
// come from Table, which gen/ extracts from source into docs_gen.go.
package contract

import (
	encodingjson "encoding/json"
	json "encoding/json/v2"
	"fmt"
	"reflect"
	"strings"
)

// marshalerInterfaces are the two ways a type takes its own serialisation over.
//
// Both versions, not just the one this module writes in: jsontext.Value is a
// defined []byte that implements only the v1 interface, so a v2-only guard
// would walk into it and describe raw JSON as an array of numbers.
var marshalerInterfaces = []reflect.Type{
	reflect.TypeFor[json.MarshalerTo](),
	reflect.TypeFor[encodingjson.Marshaler](),
}

// Mode is which side of a command a type sits on, which decides how a pointer
// reads. On the way out a pointer means the value may be null; on the way in
// it means the field may be left out, and whether it has to be there is said
// by a contract:"required" tag instead.
type Mode int

const (
	// Output is a document the command prints.
	Output Mode = iota
	// Input is a document the command reads.
	Input
)

// Table is what the types cannot say about themselves at run time.
type Table struct {
	// Fields is keyed "<import path>.<Type>.<Field>" and holds the field's doc
	// comment, flattened to one line.
	Fields map[string]string
	// Types is keyed "<import path>.<Type>" and holds a type's own doc
	// comment, which is where a sentence about the document as a whole lives.
	Types map[string]string
	// Enums is keyed "<import path>.<Type>" and holds a named string type's
	// declared constants, in declaration order.
	Enums map[string][]string
	// EnumDocs is keyed "<import path>.<Type>.<value>" and holds what one
	// constant means, which is as much a part of a contract as the value.
	EnumDocs map[string]string
	// Marshalers is how a type that serialises itself says what it serialises
	// as. A type with a custom marshaler that is not in here stops the render:
	// walking its Go fields would describe a shape that never reaches the wire,
	// which is worse than saying nothing.
	Marshalers map[reflect.Type]Marshaled
}

// Marshaled is what a type with a custom marshaler puts on the wire.
type Marshaled struct {
	// Kind is the one-line description, standing in for what the Go fields
	// would have said.
	Kind string
	// Elem is the type the value is made of, where the wire form is a list or
	// a wrapper around one. Without it a reader is told "array of object" and
	// has to go looking for what is in the object.
	Elem reflect.Type
}

// The layout. Names sit in a column wide enough for the longest of them, and
// anything that does not fit beside a name wraps under the description column.
const (
	minNameColumn = 12
	lineWidth     = 88
)

// Render describes t as the plain text a --help prints.
func (tb Table) Render(t reflect.Type, mode Mode) (string, error) {
	rows, err := tb.walk(t, mode, 0, map[reflect.Type]bool{})
	if err != nil {
		return "", err
	}

	width := minNameColumn
	for _, r := range rows {
		if end := len(r.indent()) + len(r.name) + 2; end > width {
			width = end
		}
	}

	var b strings.Builder
	// The type's own doc comment first: a sentence like "most of the fields
	// are null on a stopping status" is about the document rather than about
	// any one field, and there is nowhere else for it to go.
	for _, line := range wrap(tb.Types[typeKey(deref(t))], lineWidth-2) {
		b.WriteString("  " + line + "\n")
	}
	if b.Len() > 0 {
		b.WriteString("\n")
	}

	pad := strings.Repeat(" ", width)
	for _, r := range rows {
		head := r.indent() + r.name
		// The kind wraps like the doc does: a value set with six members runs
		// well past a terminal, and a name column is no reason to let it.
		for i, line := range wrap(r.kind, lineWidth-width) {
			if i == 0 {
				b.WriteString(head + pad[len(head):] + line + "\n")
				continue
			}
			b.WriteString(pad + line + "\n")
		}
		for _, line := range wrap(r.doc, lineWidth-width) {
			b.WriteString(pad + line + "\n")
		}
	}
	return b.String(), nil
}

// row is one field, flattened out of the nesting so that every name in the
// block shares one column.
type row struct {
	depth int
	name  string
	kind  string
	doc   string
}

func (r row) indent() string { return strings.Repeat("  ", r.depth+1) }

// walk turns a struct into rows, descending into the structs its fields hold.
//
// seen breaks a cycle rather than reporting one: no contract type is
// self-referential today, and a renderer that recurses for ever is a worse
// answer to one appearing than a name printed without its fields.
func (tb Table) walk(t reflect.Type, mode Mode, depth int, seen map[reflect.Type]bool) ([]row, error) {
	t = deref(t)
	if t.Kind() != reflect.Struct {
		return nil, fmt.Errorf("contract: %s is not a struct", t)
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
		rows = append(rows, row{depth: depth, name: name, kind: kind, doc: tb.Fields[key(t, f.Name)]})

		// A value set whose members are explained is listed one to a line
		// rather than run together, since the meaning is the half a caller
		// branching on the value actually needs.
		rows = append(rows, tb.values(f.Type, depth+1)...)

		// The fields of a struct the command nests are part of the same
		// contract, and a reader given "object" and nothing else has to go
		// looking for what is in it.
		if inner := tb.nested(f.Type); inner != nil {
			nested, err := tb.walk(*inner, mode, depth+1, seen)
			if err != nil {
				return nil, err
			}
			rows = append(rows, nested...)
		}
	}
	return rows, nil
}

// describe is the one-line type of a field: what it is, then how it may be
// absent.
func (tb Table) describe(t reflect.Type, mode Mode, f reflect.StructField, opts string) (string, error) {
	base, err := tb.kindOf(t)
	if err != nil {
		return "", err
	}

	// The qualifier goes in brackets rather than after a comma, because a
	// value set is itself a comma-separated list and "one of: a, b, required"
	// reads as four values.
	switch {
	case mode == Input && f.Tag.Get("contract") == "required":
		return base + " (required)", nil
	case mode == Input:
		return base + " (optional)", nil
	case strings.Contains(opts, "omitzero"):
		// The key is left out rather than written as null, so saying both
		// would describe two shapes only one of which appears.
		if t.Kind() == reflect.Pointer {
			return base + " (may be absent)", nil
		}
		return base + " (absent when empty)", nil
	case t.Kind() == reflect.Pointer && !tb.marshals(t):
		// A type that serialises itself has already said whether it can be
		// null, and the pointer here is Go's business rather than the wire's.
		return base + " (may be null)", nil
	}
	return base, nil
}

// values lists an enum's members with what each of them means, for the value
// sets that say. One whose members carry no explanation stays as the inline
// list in the kind.
func (tb Table) values(t reflect.Type, depth int) []row {
	t = deref(t)
	for t.Kind() == reflect.Slice || t.Kind() == reflect.Array {
		t = deref(t.Elem())
	}
	if t.Kind() != reflect.String || !tb.documented(t) {
		return nil
	}
	rows := make([]row, 0, len(tb.Enums[typeKey(t)]))
	for _, v := range tb.Enums[typeKey(t)] {
		rows = append(rows, row{depth: depth, name: v, kind: tb.EnumDocs[typeKey(t)+"."+v]})
	}
	return rows
}

// documented is whether any of a value set's members carries a meaning.
func (tb Table) documented(t reflect.Type) bool {
	for _, v := range tb.Enums[typeKey(t)] {
		if tb.EnumDocs[typeKey(t)+"."+v] != "" {
			return true
		}
	}
	return false
}

// marshals is whether the type behind t takes its own serialisation over.
func (tb Table) marshals(t reflect.Type) bool {
	_, ok := tb.Marshalers[deref(t)]
	return ok
}

// kindOf names a type the way a JSON document would.
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

// checkMarshaler is the guard. A type that serialises itself and has not said
// what it serialises as would otherwise be described by its Go fields, which
// is a confident lie rather than a gap.
func (tb Table) checkMarshaler(t reflect.Type) error {
	for _, iface := range marshalerInterfaces {
		if t.Implements(iface) || reflect.PointerTo(t).Implements(iface) {
			return fmt.Errorf("contract: %s implements %s and is not in the marshaler table, so its JSON shape is not its fields", t, iface)
		}
	}
	return nil
}

// jsonName reads a field's tag: the wire name and the options after it.
func jsonName(f reflect.StructField) (name, opts string, ok bool) {
	tag, ok := f.Tag.Lookup("json")
	if !ok || tag == "-" {
		return "", "", false
	}
	name, opts, _ = strings.Cut(tag, ",")
	return name, opts, name != ""
}

// nested is the struct whose fields belong under a field, or nil where there
// is none. A type that serialises itself says which one it is, since its own
// fields are not what reaches the wire.
func (tb Table) nested(t reflect.Type) *reflect.Type {
	if over, ok := tb.Marshalers[deref(t)]; ok {
		if over.Elem == nil {
			return nil
		}
		return &over.Elem
	}
	return elem(t)
}

// elem is the struct a field ultimately holds, or nil where it holds none.
func elem(t reflect.Type) *reflect.Type {
	t = deref(t)
	for t.Kind() == reflect.Slice || t.Kind() == reflect.Array {
		t = deref(t.Elem())
	}
	if t.Kind() != reflect.Struct {
		return nil
	}
	return &t
}

func deref(t reflect.Type) reflect.Type {
	for t.Kind() == reflect.Pointer {
		t = t.Elem()
	}
	return t
}

func key(t reflect.Type, field string) string { return typeKey(t) + "." + field }

func typeKey(t reflect.Type) string { return t.PkgPath() + "." + t.Name() }

// wrap breaks text to width, counting runes, since a doc comment may hold an
// em dash and a column is measured in what a terminal shows.
func wrap(text string, width int) []string {
	if text == "" {
		return nil
	}
	var lines []string
	line := ""
	for _, word := range strings.Fields(text) {
		switch {
		case line == "":
			line = word
		case len([]rune(line))+1+len([]rune(word)) <= width:
			line += " " + word
		default:
			lines = append(lines, line)
			line = word
		}
	}
	return append(lines, line)
}
