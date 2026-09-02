package skill

import (
	_ "embed"
	"regexp"
	"strings"
	"sync"
)

// Contract is what the ccx commands publish, as this check needs to see it.
//
// Passed in rather than read from the packages that define it: internal/cmd is
// the only place that knows which command renders which type, and this check
// stays ignorant of it.
type Contract struct {
	// The "<group> <name>" paths a SKILL.md may invoke, which is what decides
	// whether it is checked at all.
	Commands []string
	// Every field name, value and key any command publishes.
	//
	// Global rather than per-command on purpose: a skill may name a field of a
	// command it does not run, because it delegates the running to another
	// skill's procedure. The price is that a rename is caught only once the
	// name has left every registered type — head_oid is declared seven times,
	// so renaming one of them fails nothing here.
	Identifiers []string
}

func (c Contract) set() map[string]bool {
	out := make(map[string]bool, len(c.Identifiers))
	for _, id := range c.Identifiers {
		out[id] = true
	}
	return out
}

// contractIdentifier is what a reference looks like in prose. One-word names
// are out of reach: a bare `path` cannot be told from ordinary prose.
var contractIdentifier = regexp.MustCompile(`^[a-z][a-z0-9]*(_[a-z0-9]+)+$`)

// notIdentifier splits a span into words, since `blocked_by: null` is how
// these are written and matching one whole would let almost every one pass.
var notIdentifier = regexp.MustCompile(`[^a-z0-9_]+`)

//go:embed contract_allow.txt
var allowFile string

// allowed is the tokens that look like identifiers and are not. Each line
// carries its reason, being a claim that no command publishes the name.
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

// invokesCommand is the gate: a skill that runs none is exempt, its
// snake_case words being about something else entirely.
func invokesCommand(content string, commands []string) bool {
	for _, cmd := range commands {
		if strings.Contains(content, "ccx "+cmd) {
			return true
		}
	}
	return false
}

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

// backticked keeps fenced blocks in: a field named in an example is as much a
// reference as one named in a sentence.
func backticked(line string) []string {
	parts := strings.Split(line, "`")
	var out []string
	for i := 1; i < len(parts); i += 2 {
		out = append(out, parts[i])
	}
	return out
}
