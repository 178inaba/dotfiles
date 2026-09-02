package ghshim

import "fmt"

// The guidance the guard writes when it refuses.
//
// A string here is English because it is written for the model: the judgement
// runs only when CLAUDECODE is set, so nothing typed at an interactive shell
// can produce one of these. The three messages in ghshim.go that report a
// failure of the shim itself are written for a person instead — the way
// through they name is something a person does at a shell — and are Japanese.
// That is the question to ask of a new message: who is it addressed to. Not
// who can see it, which sorts nothing: selfbuild.Report goes to the same
// stderr in English, because its reader is whoever called the module.
//
// The shape is the one the repository's other guards use
// (internal/hooks/noopwait, internal/hooks/worktreeguard): Blocked: and a line
// saying what was refused, the facts as an indented label block, then Fix: and
// a way through that the verb actually has.

// repoFlagStep and repoFlagHint open and close the first rule's advice wherever
// -R is a way through, which is four of the five.
const (
	repoFlagStep = "Fix: name the repository with -R owner/repo and run again."
	repoFlagHint = `Even when the current directory's remote is certainly the right one, pass the
value from
  gh repo view --json nameWithOwner -q .nameWithOwner
to -R rather than leaving it implicit.`
)

// bodyFileStep opens the second rule's advice. Where the body goes next is the
// only part that differs per verb, so it is the only part bodyRecovery holds.
const bodyFileStep = "Fix: write the body to a file (a scratchpad, say)"

// notExplicitMessage is the first rule: the repository has to be named.
func notExplicitMessage(c command, argv []string, dir, remote string) string {
	return fmt.Sprintf(`Blocked: this gh write subcommand does not name the repository it targets.

  command:           %s
  unnamed target:    gh %s %s
  working directory: %s
  origin remote:     %s

Left as it is, gh resolves the target from the working directory, so the origin
above (or whatever remote gh settles on) is what gets written to. Having cd'd
into another repository to look at something is enough to send the issue or the
pull request there instead.

%s
`, attemptedCommand(argv), c.noun, c.verb, dir, remote, explicitnessRecovery(c))
}

// explicitnessRecovery is the way through, taken from the same classification
// the test uses: offering only -R would read as unsatisfiable under gh repo,
// which has none, and offering a URL to a verb with no selector would be a way
// through that is not one.
func explicitnessRecovery(c command) string {
	k := classify(c)

	// byPositional is the one with no -R to offer, so it is written out. The
	// other four share the skeleton repoFlagStep opens and repoFlagHint closes,
	// and differ only in the example and the note under it.
	if k == byPositional {
		return fmt.Sprintf(`Fix: name the repository in a positional argument and run again.
  gh repo %s owner/repo ...
The form is OWNER/REPO, HOST/OWNER/REPO or a repository URL. A bare REPO
names nothing, because gh completes it with the authenticated user.

gh repo %s has no -R/--repo and ignores the environment, so the
positional is the only way; it may come before or after the flags.`, c.verb, c.verb)
	}

	example := fmt.Sprintf("gh %s %s -R owner/repo ...", c.noun, c.verb)
	var note string
	switch k {
	case byFlagNotPositional:
		example = "gh repo rename new-name -R owner/repo"
		note = `The positional argument of gh repo rename is the new name, so it names no
repository.`

	case byFlagNoSelector:
		note = `create takes no selector, so there is no URL form that names the repository
here.`

	case byFlagOrURL:
		url := "https://github.com/owner/repo/issues/123"
		if c.noun == "pr" {
			url = "https://github.com/owner/repo/pull/123"
		}
		note = fmt.Sprintf(`Naming the target by its full URL works too — the URL holds the repository:
  gh %s %s %s ...
A number on its own, or a branch name, is resolved against the working
directory's remote and so names nothing.`, c.noun, c.verb, url)
	}

	if note != "" {
		note = "\n" + note
	}
	return fmt.Sprintf("%s\n  %s%s\n\n%s", repoFlagStep, example, note, repoFlagHint)
}

// multilineBodyMessage is the second rule: a body of more than one line has to
// go through a file.
func multilineBodyMessage(c command, bf bodyFlags, argv []string) string {
	return fmt.Sprintf(`Blocked: a multi-line body may not be passed inline with --%s/-%s.

  command: %s

Combining --%s "$(...)" with a heredoc stacks two layers of quoting, and a
mis-escape — a stray backslash before a backtick — ends up in the published
text.

%s
`, bf.inlineLong, bf.inlineShort, attemptedCommand(argv), bf.inlineLong, bodyRecovery(c, bf))
}

// bodyRecovery is the alternative to an inline body, taken from the table so
// that every verb is offered one it actually has.
func bodyRecovery(c command, bf bodyFlags) string {
	switch bf.recovery {
	case recoverByComment:
		return fmt.Sprintf(`%s, post it with
gh %s comment --body-file, and run %s without -%s:
  gh %s comment -R owner/repo 123 --body-file /path/to/body.md
  gh %s %s -R owner/repo 123`,
			bodyFileStep, c.noun, c.verb, bf.inlineShort, c.noun, c.noun, c.verb)
	default:
		return fmt.Sprintf(`%s and pass --%s
instead of --%s:
  gh %s %s -R owner/repo ... --%s /path/to/body.md`,
			bodyFileStep, bf.fileLong, bf.inlineLong, c.noun, c.verb, bf.fileLong)
	}
}

// unreadableBodyMessage is the refusal that comes before the two rules that
// read the body. The reason is spelled out because the fix differs: the file
// may not be written yet, the path may be wrong, or it may not be readable.
func unreadableBodyMessage(bf bodyFlags, argv []string, path, reason string) string {
	return fmt.Sprintf(`Blocked: the body file could not be read, so gh was not run.

  command:   %s
  body file: --%s %s
  reason:    %s

Without the body, the scan for bare #N numbering and for a quoted closing
keyword cannot run. gh itself exits before touching the API when it cannot read
the file, so blocking here loses no command that would otherwise have
succeeded.

Fix: write the body to the file if it is not written yet, or check the path
and run again — a relative one resolves against the working directory. To
pass the body on standard input use --%s -; the shim leaves that alone,
because reading stdin here would consume what gh is meant to read.
`, attemptedCommand(argv), bf.fileLong, path, reason, bf.fileLong)
}

// bareHashRefsMessage is the third rule: numbering an argument list with #N.
func bareHashRefsMessage(distinct int, source string) string {
	return fmt.Sprintf(`Blocked: the body numbers its items with bare #N.

  found:  %d distinct of #1 to #9
  source: %s

GitHub autolinks a bare #number, so using one to number a list of remarks
(#1, #2, ...) sends a reference notification to unrelated issues and pull
requests. A notification cannot be taken back.

Fix: if the numbering is the point, write it in a form without # — an
ordered list (1. 2. ...), say. If an issue or a pull request is really
being referenced, name it as OWNER/REPO#N:
  178inaba/dotfiles#3
That keeps the link and does not trip this guard.
`, distinct, source)
}

// quotedClosingKeywordMessage is the fourth rule. It is written as an
// interpreted string because it quotes a backtick, which a raw one cannot hold.
func quotedClosingKeywordMessage(source string) string {
	return fmt.Sprintf("Blocked: the pull request body holds a closing keyword inside backticks.\n"+
		"\n"+
		"  source: %s\n"+
		"\n"+
		"GitHub does not read Closes/Fixes/Resolves #N as a closing keyword inside a\n"+
		"code span or a code block, so merging the pull request leaves the issue open.\n"+
		"\n"+
		"Fix: if the issue is meant to close on the merge, write the keyword bare:\n"+
		"  Closes #656\n"+
		"To quote or document the keyword instead, replace the real number with a\n"+
		"placeholder (`Closes #N` — without a number it is not detected).\n",
		source)
}
