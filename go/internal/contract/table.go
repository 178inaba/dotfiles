package contract

import (
	"reflect"
)

//go:generate go test . -run=^TestGenerated$ -update

// marshalers is the exception list the renderer's guard is checked against: a
// type that serialises itself says here what it puts on the wire, since its Go
// fields are no longer the wire form. No type in this module does at present,
// and the guard refuses to render one that is missing from here.
var marshalers = map[reflect.Type]Marshaled{}

// std is the table this module's types are rendered against.
var std = Table{
	Fields:     genFields,
	Types:      genTypes,
	Enums:      genEnums,
	EnumDocs:   genEnumDocs,
	Packages:   genPackages,
	Marshalers: marshalers,
}

// Render describes t as the plain text a --help prints.
func Render(t reflect.Type, mode Mode) (string, error) { return std.Render(t, mode) }

// Identifiers is every name t's contract publishes.
func Identifiers(t reflect.Type) ([]string, error) { return std.Identifiers(t) }

// Paths is every path t's contract publishes.
func Paths(t reflect.Type) ([]string, error) { return std.Paths(t) }
