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
// The detection lives in the shell checker rather than here. Two
// implementations of one contract drift, and the state where the hook passes
// and the script fails is exactly what nobody would notice.
package skillcheck

import (
	"context"
	"encoding/json/jsontext"
	"encoding/json/v2"
	"fmt"
	"io"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// name is the only file this hook is about.
const name = "SKILL.md"

// checker is the script, relative to the repository root.
var checker = filepath.Join("claude", ".claude", "skills", "skill-authoring", "scripts", "check-skill-frontmatter.sh")

// Deps are the seams. Use Default for the real ones.
type Deps struct {
	Runner runner.Runner
	// Script locates the checker. False means the repository could not be
	// found, which is fail-closed here: see Run.
	Script func() (string, bool)
}

// Default wires the real implementations.
func Default() Deps {
	return Deps{Runner: runner.Exec{}, Script: script}
}

// script derives the checker's path from the repository the configuration is
// stowed from.
//
// The shell version found it by walking up from its own file, which resolved to
// ~/.claude/… through the stow symlink and to the repository when a test ran it
// there. A Go binary has no file to walk up from, so it asks where the
// repository is; the consequence is that the "Re-check with:" guidance always
// names the repository copy, which is the one worth editing anyway.
func script() (string, bool) {
	repo, ok := selfbuild.Repo()
	if !ok {
		return "", false
	}
	return filepath.Join(repo, checker), true
}

// Hook checks the file that was just written.
type Hook struct{ deps Deps }

// New returns the hook.
func New(d Deps) Hook { return Hook{deps: d} }

// Run implements the hook contract.
//
// It fails closed: a check that could not run reports that rather than passing
// in silence, because a hook that says nothing is indistinguishable from one
// that found nothing wrong. Everything before the check — the wrong tool, a
// file that is not a SKILL.md, a payload it cannot read — fails open, since
// none of those is a check that failed.
func (h Hook) Run(ctx context.Context, in hooks.Payload, stderr io.Writer) hooks.Result {
	switch in.ToolName {
	case "Edit", "Write", "NotebookEdit":
	default:
		return hooks.Result{}
	}
	target := in.FilePath
	if target == "" || filepath.Base(target) != name {
		return hooks.Result{}
	}
	// The edit tools promise an absolute path, but a relative one left as it
	// is would be resolved from wherever the hook was started, and the checker
	// would report a file that is not there.
	if !filepath.IsAbs(target) {
		if in.Dir == "" {
			return hooks.Result{}
		}
		target = filepath.Join(in.Dir, target)
	}

	path, ok := h.deps.Script()
	if !ok {
		fmt.Fprintf(stderr, "The frontmatter of %s was not checked.\n\n"+
			"This repository could not be located from ~/.claude/settings.json,\n"+
			"so there was nothing to run the check with.\n", target)
		return hooks.Result{Decision: hooks.Block}
	}

	// bash rather than the script directly: it carries no execute bit, and
	// every script in this repository is started this way.
	out, err := h.deps.Runner.Run(ctx, runner.Command{Name: "bash", Args: []string{path, target}})
	if err != nil {
		fmt.Fprintf(stderr, "The frontmatter of %s was not checked.\n\n"+
			"check-skill-frontmatter.sh failed before it could inspect the file:\n%s\n"+
			"Fix the reported prerequisite.\n", target, indent(runner.Stderr(err)))
		recheck(stderr, path, target)
		return hooks.Result{Decision: hooks.Block}
	}

	var result struct {
		Violations []jsontext.Value `json:"violations"`
	}
	if err := json.Unmarshal(out, &result); err != nil {
		fmt.Fprintf(stderr, "The frontmatter of %s was not checked.\n\n"+
			"check-skill-frontmatter.sh answered with something that is not its own output:\n  %v\n", target, err)
		recheck(stderr, path, target)
		return hooks.Result{Decision: hooks.Block}
	}
	if len(result.Violations) == 0 {
		return hooks.Result{}
	}

	fmt.Fprintf(stderr, "This SKILL.md has invalid frontmatter:\n\n")
	for _, raw := range result.Violations {
		fmt.Fprintf(stderr, "  %s: %s\n", target, describe(raw))
	}
	recheck(stderr, path, target)
	return hooks.Result{Decision: hooks.Block}
}

// violation is one finding. The checker's own header is the contract; the
// fields that are not part of the type in hand stay empty.
type violation struct {
	Type     string `json:"type"`
	Message  string `json:"message"`
	Field    string `json:"field"`
	Expected string `json:"expected"`
	Actual   string `json:"actual"`
	Key      string `json:"key"`
	Line     int    `json:"line"`
}

// describe renders one finding. A type this does not know is printed as it
// arrived: the checker gained a kind of violation, and saying so beats
// dropping it.
func describe(raw jsontext.Value) string {
	var v violation
	if err := json.Unmarshal(raw, &v); err != nil {
		return string(raw)
	}
	switch v.Type {
	case "invalid_yaml":
		return v.Type + " — " + v.Message
	case "missing_field":
		return fmt.Sprintf("%s — `%s` is missing or empty", v.Type, v.Field)
	case "name_mismatch":
		return fmt.Sprintf("%s — expected `%s`, actual `%s`", v.Type, v.Expected, v.Actual)
	case "unquoted_flow":
		return fmt.Sprintf("%s — line %d: the value of `%s` starts with an unquoted `[` or `{`, "+
			"so YAML reads it as a sequence or mapping instead of a string — quote it", v.Type, v.Line, v.Key)
	default:
		return v.Type + " — " + string(raw)
	}
}

// recheck prints the command that runs the same check again. It names the
// script that was actually invoked, so that editing the checker itself does not
// leave the guidance pointing at a stale stowed copy.
func recheck(w io.Writer, path, target string) {
	fmt.Fprintf(w, "\nRe-check with:\n  bash %s %s\n", shellQuote(path), shellQuote(target))
}

// bare is every character a shell leaves alone.
var bare = regexp.MustCompile(`^[A-Za-z0-9_@%+=:,./-]+$`)

// shellQuote makes a path safe to paste back into a shell, and leaves an
// ordinary one alone so the guidance stays readable.
func shellQuote(s string) string {
	if s != "" && bare.MatchString(s) {
		return s
	}
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// indent shifts the checker's own diagnostics under the line introducing them.
func indent(b []byte) string {
	var out strings.Builder
	for line := range strings.Lines(strings.TrimRight(string(b), "\n")) {
		fmt.Fprintf(&out, "  %s", line)
	}
	out.WriteString("\n")
	return out.String()
}
