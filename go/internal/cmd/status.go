package cmd

import (
	"fmt"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/issue"
)

// status is one exit status a command publishes.
//
// The number, the name a caller branches on and the sentence a reader needs
// are one declaration, because the help and the process have to agree: the
// meanings used to live only in comments, where nothing kept them in step with
// the numbers beside them.
type status struct {
	code   int
	symbol string
	// meaning is what reaching this status says, not what to do about it —
	// that belongs to whoever is calling.
	meaning string
}

// statuses is a command's whole exit-status contract.
type statuses []status

// render is the block a --help prints.
func (ss statuses) render() string {
	var b strings.Builder
	for _, s := range ss {
		if s.symbol == "" {
			fmt.Fprintf(&b, "  %d  %s\n", s.code, s.meaning)
			continue
		}
		fmt.Fprintf(&b, "  %d  %s — %s\n", s.code, s.symbol, s.meaning)
	}
	return b.String()
}

// commonStatuses are what every command in this tree does, so that a reader of
// one help does not have to guess whether the two ordinary outcomes apply.
var commonStatuses = statuses{
	{code: 0, meaning: "the answer is on standard output"},
	{code: 1, meaning: "the command could not answer; the reason is on standard error"},
}

// with returns the common statuses followed by a command's own.
func with(own ...status) statuses {
	return append(append(statuses{}, commonStatuses...), own...)
}

// classStatus binds a violation class to the status it exits with.
//
// The numbers are the contract `ccx issue sections check` publishes and
// internal/issue deliberately does not know them: a library function returning
// 4 would be carrying a process boundary around with it. Keeping the pair
// together is what stops the published number and the returned one drifting.
type classStatus struct {
	class  issue.Class
	status status
}

// sectionsCheckStatuses is the contract of `ccx issue sections check`, whose
// answer is a status rather than JSON: the reasons are for a person and a model
// to read, so they go to standard error one per line and the status names the
// class.
var sectionsCheckStatuses = []classStatus{
	{issue.MissingSection, status{code: 2, symbol: "missing_section", meaning: "a section the kind requires and the draft lacks"}},
	{issue.UnknownHeading, status{code: 3, symbol: "unknown_heading", meaning: "a heading in neither the schema nor the template mapping"}},
	{issue.MappedMachineKey, status{code: 4, symbol: "mapped_machine_key", meaning: "a template renaming a heading other skills find by its text"}},
	{issue.HeadingLocaleMismatch, status{code: 5, symbol: "heading_locale_mismatch", meaning: "a canonical heading written in the other language"}},
}

// sectionsCheckStatus is the status a violation class exits with.
func sectionsCheckStatus(class issue.Class) (int, bool) {
	for _, cs := range sectionsCheckStatuses {
		if cs.class == class {
			return cs.status.code, true
		}
	}
	return 0, false
}

// checkStatuses is the whole list as a --help renders it.
//
// The two ordinary outcomes are spelled out rather than taken from
// commonStatuses, because this command prints nothing when it passes: "the
// answer is on standard output" would describe an empty stream.
func checkStatuses() statuses {
	out := statuses{
		{code: 0, meaning: "the draft's headings match the schema"},
		{code: 1, meaning: "the check could not run; the reason is on standard error"},
	}
	for _, cs := range sectionsCheckStatuses {
		out = append(out, cs.status)
	}
	return out
}

// sectionNotFound is `ccx issue sections find`'s own status. It is separate
// from the table above because a body that does not carry a section is not a
// violation of the schema; it is an answer, and a caller branches on it
// without reading the message.
var sectionNotFound = status{code: 6, symbol: "section_not_found", meaning: "the body does not carry the requested section"}
