package skill

import (
	_ "embed"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
)

// A SKILL.md names the fields of the ccx commands it runs, at the point where
// it acts on them. A rename that leaves the old name behind in a skill is
// silent: the command answers with the new name and the skill goes on
// instructing the model to read one that is not there. This check makes that
// decidable.

// ContractViolation is one thing wrong with the contract identifiers a skill
// names.
type ContractViolation string

const (
	// UnknownContractField is a name that reads like a field or a value of a
	// ccx command's contract and is not one any more. A skill naming a field
	// where it acts on it is the arrangement; a rename that leaves the old
	// name behind in a skill is what this catches.
	UnknownContractField ContractViolation = "unknown_contract_field"
)

// ContractFinding is one violation. Every path is relative to the skills
// directory.
type ContractFinding struct {
	Type ContractViolation `json:"type"`
	File string            `json:"file"`
	Line int               `json:"line"`
	Ref  string            `json:"ref"`
}

// Contract is the outcome of one check.
type Contract struct {
	SkillsDir  string            `json:"skills_dir"`
	Violations []ContractFinding `json:"violations"`
	// Warnings is always empty, and is here so that the shape matches the
	// other checks a caller reads.
	Warnings []string `json:"warnings"`
}

// Published is what the ccx commands publish, as this check needs to see it.
//
// Passed in rather than read from the packages that define it: internal/cmd is
// the only place that knows which command renders which type, and this check
// stays ignorant of it.
type Published struct {
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

func (p Published) set() map[string]bool {
	out := make(map[string]bool, len(p.Identifiers))
	for _, id := range p.Identifiers {
		out[id] = true
	}
	return out
}

// CheckContract inspects the contract identifiers the skills under skillsDir
// name.
func CheckContract(skillsDir string, published Published) (Contract, error) {
	info, err := os.Stat(skillsDir)
	if err != nil || !info.IsDir() {
		return Contract{}, fmt.Errorf("skills directory not found: %s", skillsDir)
	}
	root, err := filepath.Abs(skillsDir)
	if err != nil {
		return Contract{}, err
	}

	found, _, err := skillFiles(root)
	if err != nil {
		return Contract{}, fmt.Errorf("skills directory not found: %s", skillsDir)
	}
	if len(found) == 0 {
		return Contract{}, fmt.Errorf("no */SKILL.md found under %s", root)
	}

	violations := []ContractFinding{}
	for _, rel := range found {
		content, err := os.ReadFile(filepath.Join(root, rel))
		if err != nil {
			continue
		}
		violations = append(violations, contractFindings(rel, string(content), published)...)
	}
	sortFindings(violations, func(f ContractFinding) (string, int, string) {
		return f.File, f.Line, string(f.Type)
	})
	return Contract{SkillsDir: root, Violations: violations, Warnings: []string{}}, nil
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

func contractFindings(file, content string, p Published) []ContractFinding {
	if len(p.Identifiers) == 0 || !invokesCommand(content, p.Commands) {
		return nil
	}
	known, published := allowed(), p.set()

	var out []ContractFinding
	for i, line := range strings.Split(content, "\n") {
		for _, span := range backticked(line) {
			for _, token := range notIdentifier.Split(span, -1) {
				if !contractIdentifier.MatchString(token) || known[token] || published[token] {
					continue
				}
				out = append(out, ContractFinding{Type: UnknownContractField, File: file, Line: i + 1, Ref: token})
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
