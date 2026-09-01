package skill

import (
	_ "embed"
	"regexp"
	"strings"
	"sync"
)

// Contract is what the ccx commands publish, as this check needs to see it.
//
// It is passed in rather than read from the packages that define it: internal/
// cmd already imports this one, so importing the contract here would be a
// cycle, and injecting it keeps this check ignorant of which command renders
// which type.
type Contract struct {
	// Commands are the "<group> <name>" paths a SKILL.md may invoke, which is
	// what decides whether it is checked at all.
	Commands []string
	// Identifiers is every field name, value and key any command publishes.
	//
	// The set is global rather than per-command on purpose: a skill may name a
	// field of a command it does not run itself, because it delegates the
	// running to another skill's procedure, and those references are correct.
	//
	// What that costs is that a rename is caught only when the name leaves
	// every registered type. head_oid is declared seven times, so renaming one
	// of them leaves the token in the set and nothing here fails — which is
	// also why testing this check by renaming one is no test at all.
	Identifiers []string
}

// set is the identifiers as a lookup, since the scan asks about every
// snake_case word in every backticked span of every file.
func (c Contract) set() map[string]bool {
	out := make(map[string]bool, len(c.Identifiers))
	for _, id := range c.Identifiers {
		out[id] = true
	}
	return out
}

// contractIdentifier is what a reference to a contract looks like in prose:
// lower snake_case with at least one underscore.
//
// One-word names are out of reach. A bare `path` or `body` is indistinguishable
// from ordinary prose, and an allowlist entry for every one of them would be a
// hundred lines of noise for the tail of the coverage. The contract's
// distinctive names are the multi-word ones.
var contractIdentifier = regexp.MustCompile(`^[a-z][a-z0-9]*(_[a-z0-9]+)+$`)

// notIdentifier splits a backticked span into the words it holds.
//
// The whole span is rarely the identifier: `blocked_by: null` and
// `type: "worktree"` are how these are actually written, and matching the span
// as one would let almost every real reference through unchecked.
var notIdentifier = regexp.MustCompile(`[^a-z0-9_]+`)

//go:embed contract_allow.txt
var allowFile string

// allowed is the tokens that look like contract identifiers and are not.
//
// Each line is a token, then whitespace, then why it is not one. A line here
// is a claim that nothing in any command publishes the name, so it is worth
// the sentence.
var allowed = sync.OnceValue(func() map[string]bool {
	out := map[string]bool{}
	for line := range strings.Lines(allowFile) {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		token, _, _ := strings.Cut(line, " ")
		out[token] = true
	}
	return out
})

// invokesCommand reports whether a file runs any ccx command.
//
// A skill that runs none is exempt: its snake_case words are about something
// else entirely, which is what db-schema-design's valid_from is.
func invokesCommand(content string, commands []string) bool {
	for _, cmd := range commands {
		if strings.Contains(content, "ccx "+cmd) {
			return true
		}
	}
	return false
}

// contractFindings are the references to contract identifiers that no command
// publishes any more.
func contractFindings(file, content string, c Contract) []RefFinding {
	if len(c.Identifiers) == 0 || !invokesCommand(content, c.Commands) {
		return nil
	}
	known, published := allowed(), c.set()

	var out []RefFinding
	for i, line := range strings.Split(content, "\n") {
		for _, span := range backticked(line) {
			for _, token := range notIdentifier.Split(span, -1) {
				if !contractIdentifier.MatchString(token) || known[token] || published[token] {
					continue
				}
				out = append(out, RefFinding{Type: UnknownContractField, File: file, Line: i + 1, Ref: token})
			}
		}
	}
	return out
}

// backticked is the spans between single backticks on one line.
//
// Fenced blocks are left in rather than skipped: a fence in a SKILL.md holds a
// command line or an example, and a field named in one is as much a reference
// as a field named in a sentence.
func backticked(line string) []string {
	parts := strings.Split(line, "`")
	var out []string
	for i := 1; i < len(parts); i += 2 {
		out = append(out, parts[i])
	}
	return out
}
