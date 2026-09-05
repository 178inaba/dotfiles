package cmd

import (
	"maps"
	"reflect"
	"slices"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/contract"

	"github.com/178inaba/dotfiles/go/internal/issue"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/reviewprs"
	"github.com/178inaba/dotfiles/go/internal/skill"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// contracts is the one place a command is bound to the types it reads and
// writes.
//
// One table rather than a field on each command because two things read it:
// the help, and the identifiers `ccx skill contract` checks a SKILL.md
// against. A second command-to-type mapping for the latter would be the very
// inventory this arrangement exists to delete.
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
		blocks:   []block{prints(reflect.TypeFor[issue.Hierarchy]())},
		statuses: with(),
	},

	"issue sections schema": {
		intro: `Print one section's row of the schema an issue body is written against.

Takes no locale, because its callers are consumers: they accept a heading in
either language, since they do not know which one the issue they are reading
was written in.

` + sectionKeys(),
		blocks:   []block{prints(reflect.TypeFor[issue.Section]())},
		statuses: with(),
	},

	"issue sections list": {
		intro: `Print every section of the schema for one locale and one issue kind.

This is what the drafting side renders a body from, so the heading is already
chosen and the requirement already decided. Both flags are required: neither
has a defensible default, and a wrong guess would report every heading as
being in the wrong language.

` + sectionKeys(),
		blocks:   []block{prints(reflect.TypeFor[issue.Listing]())},
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
		blocks:   []block{prints(reflect.TypeFor[issue.Found]())},
		statuses: with(sectionNotFound),
	},

	"pr context": {
		intro: `Fetch what a pull request is and what it changes into one file.

GitHub keeps the conversation apart from the submitted reviews and from the
threads on the diff, and asking for one of them is how a review misses what was
already said. All three are fetched at once, along with the issues the body
closes and their parents, the commit log and the whole diff, and normalised
into one document — so that a skill judging the work reconstructs it from the
document rather than running git and gh itself.

git runs here too: the base branch and ` + "`refs/pull/<n>/head`" + ` are fetched from
origin, and the diff is written beside the document. **Once the document has
been written, the commit it names as ` + "`pr.head_oid`" + ` is present in this
repository**, which is what lets the skills that follow read the surrounding
code. A pull request that moved while it was being read stops the command
instead, rather than leaving a document whose head and diff disagree.

<out-dir> has to exist. <pr-number> may be left out, in which case the pull
request is inferred from the branch checked out here. The work directory
paired with the context file is created too, since a caller that goes on to
reply to threads writes into it; it is the same directory
` + "`ccx pr prepare-review`" + ` hands out for the same pull request.

What is waiting on us is counted here rather than left to the reader:
` + "`pending`" + ` holds the threads it is our move on and the reviews and comments
that have arrived or been rewritten since the point ` + "`ccx pr seen`" + ` last
recorded for this pull request, with no body text in any of them. Reading a
remark to decide whether it counts is what that exists to prevent. The record
it measures against is local to this machine and private to the skill, so a
machine that has never run ` + "`ccx pr seen`" + ` counts everything — one reading, and
nothing lost.

` + limitSentence(),
		blocks: []block{
			prints(reflect.TypeFor[pullrequest.Stored]()),
			writes("The document written to that path", reflect.TypeFor[pullrequest.Context]()),
		},
		statuses: with(),
	},

	"pr freshness": {
		intro: `Compare the checkout here with the pull request's head.

Takes the path of a file written by ` + "`ccx pr context`" + `, and reads the pull
request's head, its two branches and whether it is ours out of it. Both
branches are fetched first, so no fetch is needed beforehand. A fast-forward
that is safe — behind only, with nothing uncommitted — is taken, and everything
else is reported rather than acted on.

Runs against the working directory, so the answer is about the checkout the
caller is standing in.`,
		blocks:   []block{prints(reflect.TypeFor[worktree.FreshnessReport]())},
		statuses: with(),
	},

	"pr prepare-review": {
		intro: `Settle what a review needs before it starts, in one call.

Flag validation, the pull request probe, the branch check, the context fetch,
the three-mode decision, the freshness check and the base branch, in one call:
each of them decides what the next one is allowed to assume, and a review that
runs them separately can proceed with one of them skipped.

An undefined flag stops the command with the defined ones listed, rather than
being ignored and letting the review run in a mode nobody asked for.
<scratchpad-dir> has to exist; the context file and the review work directory
are made under it.

The document is the one ` + "`ccx pr context`" + ` writes — its fields, ` + "`pending`" + ` among
them, are set out under ` + "`ccx pr context --help`" + ` and not repeated here — and git
runs the same way for it: **wherever context_path is set, the commit that
document names as ` + "`pr.head_oid`" + ` is present in this repository**. Where the
preparation stops before that — no pull request at all, or a checkout on
another branch — nothing was fetched and the promise does not apply.`,
		blocks:   []block{prints(reflect.TypeFor[pullrequest.Preparation]())},
		statuses: with(),
	},

	"pr post-review": {
		intro: `Post a review, after checking every comment still anchors to the diff.

Two checks run first, because both failures are silent otherwise: the local
HEAD is confirmed to be the pull request's head again, and every comment's
path and line are matched against the current diff, which is what turns a
line that has moved into a refusal here rather than a 422 from GitHub. The
review file has to sit in the work directory paired with the context file,
which is what keeps parallel reviews of different pull requests apart.

On a refusal the offending entries are listed on standard error. Put the line
numbers right and run it again; a remark whose line cannot be found belongs in
the body rather than dropped.`,
		blocks: []block{
			reads("Input (the review file named as the second argument)", reflect.TypeFor[pullrequest.ReviewFile]()),
			prints(reflect.TypeFor[pullrequest.Posted]()),
		},
		statuses: with(),
	},

	"pr reply-threads": {
		intro: `Reply to and resolve the review threads it is our move on.

A thread is named by its path and line rather than by its id, because a path
and a line are what the writer was reasoning about and an id is what gets
copied from the wrong thread. The selector is resolved against the threads the
context file marked ` + "`ball: \"mine\"`" + `; anything ambiguous, unmatched, or naming
an id that belongs elsewhere stops the run before a single reply is posted,
naming the threads it could have meant. Resolving a thread the context did not
mark ` + "`resolvable_by_me`" + ` is refused too: a person's remark is closed by that
person.

The local HEAD is confirmed to be the pull request's head, and then every
target thread is re-read live: one that has been resolved or answered since the
context was fetched stops the whole run, since the replies were written as one
judgement of one view.

Each reply that lands is recorded beside the threads file, so a re-run after a
partial failure cannot post the same reply twice — it refuses instead. On a
failure part-way through, the posted and the unprocessed threads are listed on
standard error; write a threads file holding only the unprocessed ones and run
it again.

--dry-run runs every one of those checks, the live re-read included, sends
nothing, records nothing, and prints the plan instead. A refusal in a dry run
is the same refusal with the same exit status.

The output is compact rather than indented, except when there is nothing to do.`,
		blocks: []block{
			reads("Input (the threads file named as the second argument)", reflect.TypeFor[pullrequest.ThreadsFile]()),
			prints(reflect.TypeFor[pullrequest.ThreadReplies]()),
			writes("Output with --dry-run", reflect.TypeFor[pullrequest.ReplyPlan]()),
		},
		statuses: with(),
	},

	"worktree detect": {
		intro: `Find the worktree an issue is already being worked on in.

Two namings match: the current one, <type>/<issue>-<slug>, and the one the
harness produced before these commands took over creating worktrees, since the
worktrees it made are still on disk.

Runs from anywhere inside the repository and looks at the main worktree's
linked ones, because the caller may be standing in a worktree already.`,
		blocks:   []block{prints(reflect.TypeFor[worktree.Detection]())},
		statuses: with(),
	},

	"worktree create": {
		intro: `Create a worktree for a new branch off a base branch.

The base is why this is a command rather than the harness's own worktree
primitive, which can only branch from one place. origin/<base-branch> is
preferred over the local branch of that name, and the status says which was
used.

An existing branch or an existing directory stops the command rather than
being reused or removed: either is likely to be the remains of earlier work,
and which it is is not something to guess at.

Files listed in .worktreeinclude are copied in, the same way the harness would
have.`,
		blocks:   []block{prints(reflect.TypeFor[worktree.Created]())},
		statuses: with(),
	},

	"worktree resolve": {
		intro: `Find the worktree for a pull request, or prepare to make one.

The first half of resolving a pull request's worktree; ` + "`ccx worktree checkout`" + `
is the second. Switching the session is the caller's, because no command can
see the session's state.

<pr-number> may be left out, in which case the pull request is inferred from
the branch checked out here. An existing worktree is brought up to date with
origin where that is a safe fast-forward; anything else is reported rather
than acted on, since neither an uncommitted change nor a commit that is not
pushed is this command's to discard. If the main worktree is on the pull
request's branch it is moved to the default branch first, so that the branch
is free to check out here.`,
		blocks:   []block{prints(reflect.TypeFor[worktree.Resolution]())},
		statuses: with(),
	},

	"worktree checkout": {
		intro: `Make a worktree at a pull request's head branch.

The second half of resolving a pull request's worktree, run with the name and
the head branch that ` + "`ccx worktree resolve`" + ` answered with.

Fork pull requests are out of scope: the head branch has to exist on origin.
The worktree this makes is not cleaned up when the session ends, because the
session did not create it — /cleanup-merged is what collects it.`,
		blocks:   []block{prints(reflect.TypeFor[worktree.CheckedOut]())},
		statuses: with(),
	},

	"worktree collect": {
		intro: `List the worktrees and branches whose work is finished.

Deletes nothing. The list goes to a person for approval, and
` + "`ccx worktree delete`" + ` takes back whatever survives that.

A branch with an open pull request is left out entirely rather than reported
as skipped: it is in flight, not finished. Everything judged finished but held
back for a reason appears under skipped, so that nothing disappears silently
between the two commands.

Never considered at all: main, master, develop, the remote's default branch,
and the main worktree, which cannot be removed anyway.`,
		blocks:   []block{prints(reflect.TypeFor[worktree.Collection]())},
		statuses: with(),
	},

	"worktree delete": {
		intro: `Delete the approved worktrees and branches read from standard input.

One deletion failing is recorded and the rest go on, because the list is a
batch a person approved and stopping at the first refusal would leave them to
work out which half happened. Only a broken premise — no repository, no lsof,
nothing approved — fails the command itself.

The processes holding a worktree as their working directory are checked again
here, immediately before the removal, so that somebody entering one between
the approval and the deletion does not lose it.

Deletion is git branch -d, so git's own merged check is a second safety net
under the judgement that put a branch on the list. Only a branch whose pull
request closed unmerged is deleted with -D, and only after its head is checked
against the one recorded at collection time. --force is never used.`,
		blocks: []block{
			reads("Input (JSON on standard input)", reflect.TypeFor[worktree.DeleteInput]()),
			prints(reflect.TypeFor[worktree.Deletion]()),
		},
		statuses: with(),
	},

	"review pending": {
		intro: `List the pull requests waiting for this user's review.

Somebody else's pull request, this user asked to review it, and this user has
not reviewed it yet. A pull request another reviewer has already been through
is left out, since the point of the loop this feeds is to get to the ones
nobody has looked at.`,
		blocks:   []block{prints(reflect.TypeFor[reviewprs.Pending]())},
		statuses: with(),
	},

	"review verify": {
		intro: `Check that this user's review reached each pull request.

The same judgement ` + "`ccx review pending`" + ` makes, asked from the other end: a
subagent that reported posting a review and did not would otherwise leave the
loop believing the work was done.

Each argument names one pull request as <owner>/<repo>#<number>.`,
		blocks:   []block{prints(reflect.TypeFor[reviewprs.Verification]())},
		statuses: with(),
	},

	"review clone": {
		intro: `Make a review clone of a repository available.

Reviews are done in a workspace of their own, at

    $XDG_DATA_HOME/` + reviewprs.Workspace + `/<owner>/<repo>

with $XDG_DATA_HOME defaulting to ~/.local/share. Away from wherever the user
keeps their own checkout, so that a worktree created for a review never turns
up in the repository they are working in. Nothing here is ever cleaned up
automatically; deleting the workspace is a person's to do.

An existing clone is brought up to date with git fetch --prune. A new one is
built in a hidden temporary directory beside its destination and moved into
place, so two subagents racing to clone the same repository never see a
half-finished one — the loser simply adopts the winner's. A crash that skips
the cleanup can leave one of those temporary .<repo>.XXXXXX directories in the
owner's directory; they are safe to remove.`,
		blocks:   []block{prints(reflect.TypeFor[reviewprs.Clone]())},
		statuses: with(),
	},

	"skill frontmatter": {
		intro: `Check the frontmatter of a skill directory or one SKILL.md.

Claude Code's own parser is forgiving enough to load a skill whose frontmatter
is not valid YAML, so nothing goes wrong loudly: an argument-hint holding two
flow sequences on one line sat broken in two files until somebody happened to
look.

<target> is a directory or a single SKILL.md; left out, it is the skills
directory of the repository this configuration is stowed from. Violations are
not a failure of the check — the caller reads them and decides. Only being
unable to check at all is.`,
		blocks:   []block{prints(reflect.TypeFor[skill.Frontmatter]())},
		statuses: with(),
	},

	"skill contract": {
		intro: `Check the contract identifiers skills name.

A skill names the fields of the commands it runs at the point where it acts on
them, which is the arrangement. What this catches is a rename that left the old
name behind in a skill: the command answers with the new name, the skill goes
on instructing the model to read one that is not there, and nothing says so.

Only names inside backticks are read, and only those with an underscore — a
bare word cannot be told from ordinary prose. A skill that runs none of these
commands is exempt, its snake_case words being about something else entirely.

<skills-dir> defaults to the skills directory of the repository this
configuration is stowed from.`,
		blocks:   []block{prints(reflect.TypeFor[skill.Contract]())},
		statuses: with(),
	},
}

// published is what `ccx skill contract` checks a SKILL.md against.
//
// Built from the same table the help is rendered from, so a field gone from
// the contract is gone from here in the same commit. Handed to the check
// rather than imported by it, because internal/cmd already imports
// internal/skill.
func published() skill.Published {
	out := skill.Published{
		Commands: slices.Sorted(maps.Keys(contracts)),
		// The section keys are contract too: `ccx issue sections schema` takes
		// one, and the skills that read an issue body name them.
		Identifiers: issue.Keys(),
	}
	for _, h := range contracts {
		for _, blk := range h.blocks {
			ids, err := contract.Identifiers(blk.typ)
			if err != nil {
				// The help tests report a type that cannot be walked; failing
				// here too would only turn that into a second, stranger
				// failure over in the skills.
				continue
			}
			out.Identifiers = append(out.Identifiers, ids...)
		}
		for _, s := range h.statuses {
			if s.symbol != "" {
				out.Identifiers = append(out.Identifiers, s.symbol)
			}
		}
	}
	slices.Sort(out.Identifiers)
	out.Identifiers = slices.Compact(out.Identifiers)
	return out
}

// limitSentence is read from the list the command itself uses, however long it
// has grown.
func limitSentence() string {
	return contract.Wrap(andList(limitVars[:]) +
		" raise the fetch limits; each takes a plain non-negative integer, and the " +
		"truncated flags below say when one of them was reached.")
}

// andList is how both sentences below name a list of things: A, B and C.
func andList(xs []string) string {
	last := len(xs) - 1
	return strings.Join(xs[:last], ", ") + " and " + xs[last]
}

// sectionKeys is the schema's key column, wrapped into a sentence.
//
// The keys are the one part of this contract that is data rather than a type,
// so they cannot come from the rendering. Read from the schema so that a key
// added to it appears here without anyone remembering to.
func sectionKeys() string {
	return contract.Wrap("The keys are " + andList(issue.Keys()) + ".")
}

// longFor is a command's help text, empty where none is registered.
func longFor(path string) string {
	h, ok := contracts[path]
	if !ok {
		return ""
	}
	return h.String()
}
