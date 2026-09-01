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

var valueFlagTable = map[command]valueFlags{
	{"issue", "create"}: {names("assignee", "body", "body-file", "label", "milestone", "project", "recover", "template", "title", "repo"), "abFlmpTtR"},

	{"issue", "comment"}: {names("body", "body-file", "repo"), "bFR"},
	{"pr", "comment"}:    {names("body", "body-file", "repo"), "bFR"},
	{"pr", "review"}:     {names("body", "body-file", "repo"), "bFR"},

	{"issue", "edit"}:  {names("add-assignee", "add-label", "add-project", "body", "body-file", "milestone", "remove-assignee", "remove-label", "remove-project", "title", "repo"), "bFmtR"},
	{"issue", "close"}: {names("comment", "reason", "repo"), "crR"},

	{"issue", "reopen"}: {names("comment", "repo"), "cR"},
	{"pr", "close"}:     {names("comment", "repo"), "cR"},
	{"pr", "reopen"}:    {names("comment", "repo"), "cR"},

	{"issue", "develop"}: {names("base", "branch-repo", "name", "repo"), "bnR"},

	{"issue", "lock"}: {names("reason", "repo"), "rR"},
	{"pr", "lock"}:    {names("reason", "repo"), "rR"},

	{"pr", "create"}:   {names("assignee", "base", "body", "body-file", "head", "label", "milestone", "project", "recover", "reviewer", "template", "title", "repo"), "aBbFHlmprTtR"},
	{"pr", "edit"}:     {names("add-assignee", "add-label", "add-project", "add-reviewer", "base", "body", "body-file", "milestone", "remove-assignee", "remove-label", "remove-project", "remove-reviewer", "title", "repo"), "BbFmtR"},
	{"pr", "merge"}:    {names("author-email", "body", "body-file", "match-head-commit", "subject", "repo"), "AbFtR"},
	{"pr", "revert"}:   {names("body", "body-file", "title", "repo"), "bFtR"},
	{"pr", "checkout"}: {names("branch", "repo"), "bR"},

	{"release", "create"}: {names("discussion-category", "notes", "notes-file", "notes-start-tag", "target", "title", "repo"), "nFtR"},
	{"release", "edit"}:   {names("discussion-category", "notes", "notes-file", "tag", "target", "title", "repo"), "nFtR"},

	{"label", "create"}: {names("color", "description", "repo"), "cdR"},
	{"label", "edit"}:   {names("color", "description", "name", "repo"), "cdnR"},

	{"repo", "edit"}:   {names("add-topic", "default-branch", "description", "homepage", "remove-topic", "visibility"), "dh"},
	{"repo", "sync"}:   {names("branch", "source"), "bs"},
	{"repo", "rename"}: {names("repo"), "R"},
}

// valueFlagsFor answers for every command, listed or not.
//
// issue, pr, release and label inherit -R/--repo from gh itself; the rest of
// gh repo — delete, archive and unarchive — has only the boolean --yes, and no
// --repo at all.
func valueFlagsFor(c command) valueFlags {
	if f, ok := valueFlagTable[c]; ok {
		return f
	}
	if c.noun == "repo" {
		return valueFlags{}
	}
	return valueFlags{names("repo"), "R"}
}

// recovery is how the second rule tells the caller to pass the body instead.
// It belongs to the table so that registering a body flag has to name one:
// deriving it from the flags afterwards left a branch that could not be reached.
type recovery int

const (
	// recoverByFile: the verb has a file form of the same body.
	recoverByFile recovery = iota
	// recoverByComment: close and reopen carry their body in the only flag they
	// have, so it moves to a separate gh <noun> comment.
	recoverByComment
)

// bodyFlags are the flags of one command that carry a body, and how to offer an
// alternative when one of them is refused.
//
// Membership is decided by whether GitHub renders the value as markdown, not by
// the spelling: the three body rules guard against mishaps — a mis-escape, an
// autolinked #N, a closing keyword that will not fire — that can only happen to
// rendered text. gh label create/edit --description and gh repo edit
// --description are plain text and stay out; gh pr merge --subject is one line,
// so nothing can reach it either.
//
// The spellings differ per verb, which is why they are recorded per verb: -b is
// --base under gh issue develop and --branch under gh pr checkout, and -F is
// --notes-file under gh release.
type bodyFlags struct {
	inlineLong, inlineShort string
	fileLong, fileShort     string
	recovery                recovery
}

var bodyFlagTable = map[command]bodyFlags{
	{"issue", "create"}:  {"body", "b", "body-file", "F", recoverByFile},
	{"issue", "comment"}: {"body", "b", "body-file", "F", recoverByFile},
	{"issue", "edit"}:    {"body", "b", "body-file", "F", recoverByFile},
	{"pr", "create"}:     {"body", "b", "body-file", "F", recoverByFile},
	{"pr", "comment"}:    {"body", "b", "body-file", "F", recoverByFile},
	{"pr", "edit"}:       {"body", "b", "body-file", "F", recoverByFile},
	{"pr", "merge"}:      {"body", "b", "body-file", "F", recoverByFile},
	{"pr", "review"}:     {"body", "b", "body-file", "F", recoverByFile},
	{"pr", "revert"}:     {"body", "b", "body-file", "F", recoverByFile},

	{"release", "create"}: {"notes", "n", "notes-file", "F", recoverByFile},
	{"release", "edit"}:   {"notes", "n", "notes-file", "F", recoverByFile},

	{"issue", "close"}:  {inlineLong: "comment", inlineShort: "c", recovery: recoverByComment},
	{"issue", "reopen"}: {inlineLong: "comment", inlineShort: "c", recovery: recoverByComment},
	{"pr", "close"}:     {inlineLong: "comment", inlineShort: "c", recovery: recoverByComment},
	{"pr", "reopen"}:    {inlineLong: "comment", inlineShort: "c", recovery: recoverByComment},
}

// bodyFlagsFor answers for every command; an unlisted one carries no body.
func bodyFlagsFor(c command) bodyFlags { return bodyFlagTable[c] }
