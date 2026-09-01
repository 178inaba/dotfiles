// Package skillcheck inspects a SKILL.md as it is saved.
//
// What it prevents: frontmatter that is broken as YAML reaching main unnoticed.
// Claude Code's own parser is forgiving enough to load the skill anyway, so the
// only ways anyone found out were noticing it by eye or remembering to run the
// checker — and a broken argument-hint sat in two files until somebody did.
//
// PostToolUse rather than PreToolUse because there is no documented way to get
// the edited content from a PreToolUse payload; reconstructing it from
// old_string and new_string would be reimplementing the harness. The price is
// that the write cannot be undone: exiting 2 does not roll it back, it only
// puts the problem in front of the model in the same turn.
//
// The detection is internal/skill's, called directly. It used to be a shell
// script this started, which is why the seams for running one are gone: two
// implementations of one contract drift, and the state where the hook passes
// and the checker fails is exactly what nobody would notice.
package skillcheck

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/skill"
)

// name is the only file this hook is about.
const name = "SKILL.md"

// Hook checks the file that was just written.
type Hook struct{}

// New returns the hook.
func New() Hook { return Hook{} }

// Run implements the hook contract.
//
// Everything before the check — the wrong tool, a file that is not a SKILL.md,
// a payload it cannot read — fails open, since none of those is a check that
// failed. A check that could not be made does report itself, because a hook
// that says nothing is indistinguishable from one that found nothing wrong.
func (h Hook) Run(_ context.Context, in hooks.Payload) hooks.Result {
	if !hooks.IsEditTool(in.ToolName) {
		return hooks.Result{}
	}
	target := in.FilePath
	if target == "" || filepath.Base(target) != name {
		return hooks.Result{}
	}
	// The edit tools promise an absolute path, but a relative one left as it
	// is would be resolved from wherever the hook was started, and the check
	// would report a file that is not there.
	if !filepath.IsAbs(target) {
		if in.Dir == "" {
			return hooks.Result{}
		}
		target = filepath.Join(in.Dir, target)
	}

	checked, err := skill.CheckFrontmatter(target)
	if err != nil {
		return blocked(fmt.Sprintf("The frontmatter of %s was not checked.\n\n%v\n", target, err) + recheck(target))
	}
	if len(checked.Violations) == 0 {
		return hooks.Result{}
	}

	var b strings.Builder
	b.WriteString("This SKILL.md has invalid frontmatter:\n\n")
	for _, v := range checked.Violations {
		fmt.Fprintf(&b, "  %s: %s\n", target, describe(v))
	}
	return blocked(b.String() + recheck(target))
}

func blocked(message string) hooks.Result {
	return hooks.Result{Decision: hooks.Block, Message: message}
}

// describe renders one finding. A kind this does not know is named rather than
// dropped: the check gained a violation, and saying so beats silence.
func describe(v skill.Violation) string {
	switch v.Type {
	case skill.InvalidYAML:
		return string(v.Type) + " — " + v.Message
	case skill.MissingField:
		return fmt.Sprintf("%s — `%s` is missing or empty", v.Type, v.Field)
	case skill.NameMismatch:
		return fmt.Sprintf("%s — expected `%s`, actual `%s`", v.Type, v.Expected, v.Actual)
	case skill.UnquotedFlow:
		return fmt.Sprintf("%s — line %d: the value of `%s` starts with an unquoted `[` or `{`, "+
			"so YAML reads it as a sequence or mapping instead of a string — quote it", v.Type, v.Line, v.Key)
	default:
		return fmt.Sprintf("%s — %+v", v.Type, v)
	}
}

// recheck is the command that runs the same check again.
func recheck(target string) string {
	return fmt.Sprintf("\nRe-check with:\n  ccx skill frontmatter %s\n", shellQuote(target))
}

// shellQuote makes a path safe to paste back into a shell, and leaves an
// ordinary one alone so the guidance stays readable.
func shellQuote(s string) string {
	if s != "" && !strings.ContainsFunc(s, needsQuote) {
		return s
	}
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// needsQuote reports whether a character is one a shell would not leave alone.
func needsQuote(r rune) bool {
	switch {
	case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9':
		return false
	}
	return !strings.ContainsRune(`_@%+=:,./-`, r)
}
