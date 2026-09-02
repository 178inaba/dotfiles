package ghshim

// The three tables the judgement reads. The shell held each of them in a
// delimited string, because bash 3.2 has neither sets nor associative arrays;
// that is a property of the interpreter rather than of the guard, so the values
// are the same here and the shape is not.

// command is a noun and one of the verbs under it.
type command struct{ noun, verb string }

// writeVerbs are the verbs that write. Verbs where naming a repository would
// mean nothing are left out — gh repo create, fork, set-default and clone. The
// read fast path consults the same table, so "does this write" is defined once.
var writeVerbs = map[string]map[string]bool{
	"issue":   names("create", "comment", "edit", "close", "reopen", "delete", "develop", "lock", "unlock", "pin", "unpin", "transfer"),
	"pr":      names("create", "comment", "edit", "close", "reopen", "lock", "unlock", "merge", "ready", "revert", "review", "update-branch", "checkout"),
	"release": names("create", "edit", "delete", "upload", "delete-asset"),
	"repo":    names("edit", "delete", "archive", "unarchive", "rename", "sync"),
	"label":   names("create", "edit", "delete", "clone"),
}

// writes reports whether c is one of the guarded commands.
func writes(c command) bool { return writeVerbs[c.noun][c.verb] }

func names(ns ...string) map[string]bool {
	m := make(map[string]bool, len(ns))
	for _, n := range ns {
		m[n] = true
	}
	return m
}

// valueFlags are the flags of one command that take a value, copied from
// gh <noun> <verb> --help rather than guessed. Reading a value-taking flag as a
// boolean makes the walk treat its value as the positional argument, and a
// write with no repository named then goes through — gh repo sync --source
// owner/repo is the case that showed it.
//
// short is the option letters rather than a set of its own: each element is
// exactly one byte, so membership is an exact search.
type valueFlags struct {
	long  map[string]bool
	short string
}

// The rows hold what each verb declares for itself. -R/--repo is inherited
// from gh, which lists it apart from a command's own flags, so it is added
// once by withInheritedRepo rather than written into two dozen rows.
var valueFlagTable = withInheritedRepo(map[command]valueFlags{
	{"issue", "create"}: {names("assignee", "body", "body-file", "label", "milestone", "project", "recover", "template", "title"), "abFlmpTt"},

	{"issue", "comment"}: {names("body", "body-file"), "bF"},
	{"pr", "comment"}:    {names("body", "body-file"), "bF"},
	{"pr", "review"}:     {names("body", "body-file"), "bF"},

	{"issue", "edit"}:  {names("add-assignee", "add-label", "add-project", "body", "body-file", "milestone", "remove-assignee", "remove-label", "remove-project", "title"), "bFmt"},
	{"issue", "close"}: {names("comment", "reason"), "cr"},

	{"issue", "reopen"}: {names("comment"), "c"},
	{"pr", "close"}:     {names("comment"), "c"},
	{"pr", "reopen"}:    {names("comment"), "c"},

	{"issue", "develop"}: {names("base", "branch-repo", "name"), "bn"},

	{"issue", "lock"}: {names("reason"), "r"},
	{"pr", "lock"}:    {names("reason"), "r"},

	{"pr", "create"}:   {names("assignee", "base", "body", "body-file", "head", "label", "milestone", "project", "recover", "reviewer", "template", "title"), "aBbFHlmprTt"},
	{"pr", "edit"}:     {names("add-assignee", "add-label", "add-project", "add-reviewer", "base", "body", "body-file", "milestone", "remove-assignee", "remove-label", "remove-project", "remove-reviewer", "title"), "BbFmt"},
	{"pr", "merge"}:    {names("author-email", "body", "body-file", "match-head-commit", "subject"), "AbFt"},
	{"pr", "revert"}:   {names("body", "body-file", "title"), "bFt"},
	{"pr", "checkout"}: {names("branch"), "b"},

	{"release", "create"}: {names("discussion-category", "notes", "notes-file", "notes-start-tag", "target", "title"), "nFt"},
	{"release", "edit"}:   {names("discussion-category", "notes", "notes-file", "tag", "target", "title"), "nFt"},

	{"label", "create"}: {names("color", "description"), "cd"},
	{"label", "edit"}:   {names("color", "description", "name"), "cdn"},

	// gh repo does not inherit --repo, so these rows are complete as they are:
	// edit, delete, archive and unarchive have no way to name a repository but
	// the positional, and rename declares a --repo of its own.
	{"repo", "edit"}:   {names("add-topic", "default-branch", "description", "homepage", "remove-topic", "visibility"), "dh"},
	{"repo", "sync"}:   {names("branch", "source"), "bs"},
	{"repo", "rename"}: {names("repo"), "R"},
})

// inheritedRepo is gh's own -R/--repo, and the whole of what an unlisted
// command outside gh repo takes.
var inheritedRepo = valueFlags{names("repo"), "R"}

// withInheritedRepo adds -R/--repo to every command that inherits it, so that
// the table itself does not have to say so once per row.
func withInheritedRepo(table map[command]valueFlags) map[command]valueFlags {
	for c, f := range table {
		if c.noun == "repo" {
			continue
		}
		for n := range inheritedRepo.long {
			f.long[n] = true
		}
		f.short += inheritedRepo.short
		table[c] = f
	}
	return table
}

// valueFlagsFor answers for every command, listed or not.
//
// The rest of gh repo — delete, archive and unarchive — has only the boolean
// --yes, so an empty answer is the right one there.
func valueFlagsFor(c command) valueFlags {
	if f, ok := valueFlagTable[c]; ok {
		return f
	}
	if c.noun == "repo" {
		return valueFlags{}
	}
	return inheritedRepo
}

// recovery is how the second rule tells the caller to pass the body instead.
//
// It belongs to the table rather than being derived from whether the verb has a
// file form, because "no file form" does not imply that gh <noun> comment exists
// to take the body instead. TestEveryBodyFlagOffersARecoveryItHas holds the
// pairing.
type recovery int

const (
	// recoverByFile: the verb has a file form of the same body.
	recoverByFile recovery = iota
	// recoverByComment: close and reopen carry their body in the only flag they
	// have, so it moves to a separate gh <noun> comment.
	recoverByComment
)

// bodyFlags are the flags of one command that carry a body, and how to offer an
// alternative when one of them is refused. Membership follows the rule in the
// package comment: gh label create/edit --description and gh repo edit
// --description are plain text and stay out, and gh pr merge --subject is one
// line, so nothing can reach it either.
type bodyFlags struct {
	inlineLong, inlineShort string
	fileLong, fileShort     string
	recovery                recovery
}

// The three shapes a body flag comes in, named so that the table below reads as
// which verb takes which rather than as fifteen rows of literals.
var (
	body    = bodyFlags{"body", "b", "body-file", "F", recoverByFile}
	notes   = bodyFlags{"notes", "n", "notes-file", "F", recoverByFile}
	comment = bodyFlags{inlineLong: "comment", inlineShort: "c", recovery: recoverByComment}
)

var bodyFlagTable = map[command]bodyFlags{
	{"issue", "create"}:  body,
	{"issue", "comment"}: body,
	{"issue", "edit"}:    body,
	{"pr", "create"}:     body,
	{"pr", "comment"}:    body,
	{"pr", "edit"}:       body,
	{"pr", "merge"}:      body,
	{"pr", "review"}:     body,
	{"pr", "revert"}:     body,

	{"release", "create"}: notes,
	{"release", "edit"}:   notes,

	{"issue", "close"}:  comment,
	{"issue", "reopen"}: comment,
	{"pr", "close"}:     comment,
	{"pr", "reopen"}:    comment,
}

// bodyFlagsFor answers for every command; an unlisted one carries no body.
func bodyFlagsFor(c command) bodyFlags { return bodyFlagTable[c] }
