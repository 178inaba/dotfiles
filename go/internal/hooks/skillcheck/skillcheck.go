// Package skillcheck inspects a SKILL.md as it is saved.
//
// Two checks, in order, and the second runs only once the first finds
// nothing: frontmatter that is broken as YAML reaching main unnoticed, and a
// body over the size guide growing there unnoticed too. Claude Code's own
// frontmatter parser is forgiving enough to load the skill anyway, so the
// only ways anyone found out were noticing it by eye or remembering to run
// the checker — and a broken argument-hint sat in two files until somebody
// did. A Japanese body is the size check's own version of that: it can clear
// 500 lines' worth of tokens while staying under 500 lines, so line count
// alone never flags it, and nothing else here would either.
//
// PostToolUse rather than PreToolUse because there is no documented way to get
// the edited content from a PreToolUse payload; reconstructing it from
// old_string and new_string would be reimplementing the harness. The price is
// that the write cannot be undone: exiting 2 does not roll it back, it only
// puts the problem in front of the model in the same turn.
//
// The frontmatter violation blocks; the size report does not. A guide is
// something to split against rather than a hard limit, and refusing the save
// would stop a body mid-split, so it is reported through the directive's
// additionalContext on an ordinary exit 0 instead — read by the model in the
// same turn, same as a block's message, but without undoing anything. Only
// one report ever goes out: a frontmatter violation is worth blocking on its
// own, and a size report beside it would never reach anyone, since Claude
// Code reads a directive only from a hook that exited 0.
//
// The token estimate has no language gate: its weights are per character
// class, so one estimate holds for a body in any language.
//
// The detection is internal/skill's, called directly for both checks: two
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
		return blocked(fmt.Sprintf("The frontmatter of %s was not checked.\n\n%v\n", target, err) + recheck("frontmatter", target))
	}
	if len(checked.Violations) != 0 {
		var b strings.Builder
		b.WriteString("This SKILL.md has invalid frontmatter:\n\n")
		for _, v := range checked.Violations {
			fmt.Fprintf(&b, "  %s: %s\n", target, describe(v))
		}
		return blocked(b.String() + recheck("frontmatter", target))
	}

	measured, err := skill.MeasureSize(target)
	if err != nil {
		return blocked(fmt.Sprintf("The size of %s was not measured.\n\n%v\n", target, err) + recheck("size", target))
	}
	sz := measured.Skills[0]
	if !sz.OverGuide() {
		return hooks.Result{}
	}
	return hooks.Result{
		Decision: hooks.Allow,
		Directive: hooks.Directive{HookSpecificOutput: hooks.HookSpecificOutput{
			HookEventName:     "PostToolUse",
			AdditionalContext: sizeReport(target, sz),
		}},
	}
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

// recheck is the ccx skill subcommand that runs the same check again.
func recheck(subcommand, target string) string {
	return fmt.Sprintf("\nRe-check with:\n  ccx skill %s %s\n", subcommand, shellQuote(target))
}

// sizeGuideline is the design principle a size report points a reader back
// to, so the report is not the only place the number is explained.
const sizeGuideline = "The guide is the design principle サイズ上限の目安 of the skill-authoring skill: " +
	"a body over it puts the rules every run needs before optional detail; split deterministic " +
	"plumbing into ccx and conditional detail into references/."

// sizeReport is the additionalContext for a body over either guide. Both
// measurements go out whichever guide is exceeded, so a split aimed at one
// does not walk into the other unseen.
func sizeReport(target string, sz skill.Measurement) string {
	return fmt.Sprintf("%s is over the size guide: %d lines (guide: %d), %d estimated tokens (guide: %d).\n\n%s\n",
		target, sz.Lines, skill.LineGuide, sz.EstimatedTokens, skill.TokenGuide, sizeGuideline) +
		recheck("size", target)
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
