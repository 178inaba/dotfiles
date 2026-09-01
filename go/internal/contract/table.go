package contract

import (
	"reflect"

	"github.com/178inaba/dotfiles/go/internal/issue"
)

//go:generate go run ./gen

// marshalers is the exception list the renderer's guard is checked against.
//
// Only two types serialise themselves, and both do it for the same reason: a
// list GitHub declined to supply is null rather than empty, because "nothing
// is blocking this" and "what is blocking this could not be read" are
// different answers. Their Go fields are the wrapper rather than the wire
// form, so each says what it puts out and which type the elements are.
var marshalers = map[reflect.Type]Marshaled{
	reflect.TypeFor[issue.RefList](): {
		Kind: "array of object, or null when the list could not be read",
		Elem: reflect.TypeFor[issue.Ref](),
	},
	reflect.TypeFor[issue.PRList](): {
		Kind: "array of object, or null when the list could not be read",
		Elem: reflect.TypeFor[issue.PR](),
	},
}

// std is the table this module's types are rendered against.
var std = Table{Fields: genFields, Types: genTypes, Enums: genEnums, EnumDocs: genEnumDocs, Marshalers: marshalers}

// Render describes t as the plain text a --help prints.
func Render(t reflect.Type, mode Mode) (string, error) { return std.Render(t, mode) }

// Identifiers is every name t's contract publishes.
func Identifiers(t reflect.Type) ([]string, error) { return std.Identifiers(t) }
