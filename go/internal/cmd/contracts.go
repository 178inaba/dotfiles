package cmd

import (
	"reflect"

	"github.com/178inaba/dotfiles/go/internal/issue"
)

// contracts is the one place a command is bound to the types it reads and
// writes.
//
// It is one table rather than a field on each command because two things read
// it: the help each command prints, and the set of identifiers `ccx skill refs`
// checks a SKILL.md against. A second command-to-type mapping for the latter
// would be the very inventory this arrangement exists to delete.
//
// A command missing from here prints its Short and its flags and nothing else,
// which is visible, rather than a stale contract, which is not.
var contracts = map[string]help{
	"issue tree": {
		intro: `Resolve where an issue sits among its parent, its children and its blockers.

GitHub answers each of these separately — the parent through an endpoint whose
404 means "no parent", the children and the blockers through paginated ones —
so assembling them at each point of use makes the handling of a 404, a page
join and the issue's own exclusion drift. This does it once.

Runs against the repository the working directory belongs to unless -R names
another one. The two annotations are flags because each costs a round trip per
sub-issue, and this runs at the start of every skill that reads a leaf issue.`,
		output:   reflect.TypeFor[issue.Hierarchy](),
		statuses: with(),
	},

	"issue sections schema": {
		intro: `Print one section's row of the schema an issue body is written against.

Takes no locale, because its callers are consumers: they accept a heading in
either language, since they do not know which one the issue they are reading
was written in.`,
		output:   reflect.TypeFor[issue.Section](),
		statuses: with(),
	},

	"issue sections list": {
		intro: `Print every section of the schema for one locale and one issue kind.

This is what the drafting side renders a body from, so the heading is already
chosen and the requirement already decided. Both flags are required: neither
has a defensible default, and a wrong guess would report every heading as
being in the wrong language.`,
		output:   reflect.TypeFor[issue.Listing](),
		statuses: with(),
	},

	"issue sections check": {
		intro: `Check a draft's "## " headings against the schema.

This is the one subcommand that answers with a status rather than with JSON.
Its caller needs only pass or fail, while the reasons are for a person and a
model to read, so they go to standard error one per line and the status names
the class of the worst of them.

--mapping takes a file of "<key> <heading>" lines, one per line, naming the
headings a repository's own issue template uses in place of the schema's.`,
		statuses: checkStatuses(),
	},

	"issue sections find": {
		intro: `Print one section of an issue body.

Both canonical headings are accepted, so a caller does not have to know which
language the body was written in. Pass the body as a file: reading it from an
argument would put a whole issue on a command line.`,
		output:   reflect.TypeFor[issue.Found](),
		statuses: with(sectionNotFound),
	},
}

// longFor is a command's help text, or empty where none is registered.
func longFor(path string) string {
	h, ok := contracts[path]
	if !ok {
		return ""
	}
	return h.String()
}
