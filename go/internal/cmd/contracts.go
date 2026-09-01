package cmd

import (
	"reflect"

	"github.com/178inaba/dotfiles/go/internal/issue"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/worktree"
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
		blocks:   []block{prints(reflect.TypeFor[issue.Hierarchy]())},
		statuses: with(),
	},

	"issue sections schema": {
		intro: `Print one section's row of the schema an issue body is written against.

Takes no locale, because its callers are consumers: they accept a heading in
either language, since they do not know which one the issue they are reading
was written in.`,
		blocks:   []block{prints(reflect.TypeFor[issue.Section]())},
		statuses: with(),
	},

	"issue sections list": {
		intro: `Print every section of the schema for one locale and one issue kind.

This is what the drafting side renders a body from, so the heading is already
chosen and the requirement already decided. Both flags are required: neither
has a defensible default, and a wrong guess would report every heading as
being in the wrong language.`,
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
		intro: `Fetch a pull request's comments, reviews and threads into one file.

GitHub keeps the three apart — the conversation, the submitted reviews, and the
threads on the diff — and asking for one of them is how a review misses what
was already said. All three are fetched at once and normalised into one
document.

Standard output is the path and nothing else. On a pull request with a busy
conversation the document runs to hundreds of kilobytes, which is a size to
read with a tool that takes a path rather than to pass back through a
redirection. <out-dir> has to exist. <pr-number> may be left out, in which case
the pull request is inferred from the branch checked out here.

MAX_COMMENTS, MAX_THREADS and MAX_THREAD_COMMENTS raise the fetch limits; each
takes a plain non-negative integer, and the truncated flags below say when one
of them was reached.`,
		blocks: []block{
			prints(reflect.TypeFor[pullrequest.Stored]()),
			writes("The document written to that path", reflect.TypeFor[pullrequest.Context]()),
		},
		statuses: with(),
	},

	"pr freshness": {
		intro: `Compare the checkout here with the pull request's head.

Takes the path of a file written by ` + "`ccx pr context`" + `, and reads pr.head_oid,
pr.head_ref, pr.base_ref and is_own_pr out of it. Both branches are fetched
first, so no fetch is needed beforehand. A fast-forward that is safe — behind
only, with nothing uncommitted — is taken, and everything else is reported
rather than acted on.

Runs against the working directory, so the answer is about the checkout the
caller is standing in.`,
		blocks:   []block{prints(reflect.TypeFor[worktree.FreshnessReport]())},
		statuses: with(),
	},

	"pr prepare-review": {
		intro: `Settle everything a review needs before it starts.

Flag validation, the pull request probe, the branch check, the context fetch,
the three-mode decision, the freshness check and the base branch, in one call:
each of them decides what the next one is allowed to assume, and a review that
runs them separately can proceed with one of them skipped.

An undefined flag stops the command with the defined ones listed, rather than
being ignored and letting the review run in a mode nobody asked for.
<scratchpad-dir> has to exist; the context file and the review work directory
are made under it.`,
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
		intro: `Reply to and resolve the review threads awaiting our confirmation.

Only threads the context file flagged as awaiting our confirmation are
accepted; naming any other stops the run before a single reply is posted,
which is what keeps one run from settling somebody else's remark. The local
HEAD is confirmed to be the pull request's head first.

Each reply that lands is recorded beside the threads file, so a re-run after a
partial failure cannot post the same reply twice — it refuses instead. On a
failure part-way through, the posted and the unprocessed threads are listed on
standard error; write a threads file holding only the unprocessed ones and run
it again.

The output is compact rather than indented, except when there is nothing to do.`,
		blocks: []block{
			reads("Input (the threads file named as the second argument)", reflect.TypeFor[pullrequest.ThreadsFile]()),
			prints(reflect.TypeFor[pullrequest.ThreadReplies]()),
		},
		statuses: with(),
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
