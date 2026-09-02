package ghshim

import (
	"fmt"
	"os"
	"strings"
)

// Env is what the decision reads from outside the argv.
//
// The environment is read once, by Execute, and handed down as values: nothing
// below this line calls os.Getenv, so the tests need neither t.Setenv nor a
// serial run.
type Env struct {
	// GHRepo is GH_REPO, which never appears in the argv because the shell
	// consumes an assignment before the command sees it. A value here means gh
	// resolves the repository from it rather than from the working directory.
	GHRepo string
	// ClaudeCode is CLAUDECODE. Empty means an interactive shell.
	ClaudeCode string

	// Dir and OriginRemote fill in the last two lines of the first rule's label
	// block, and are functions because that message is their only reader:
	// resolving the remote runs git, and doing it eagerly would put a
	// subprocess in front of every command the guard lets through.
	Dir          func() string
	OriginRemote func() string
}

// Block is a refusal. Message is the guidance written for the model, complete
// and ready for standard error.
type Block struct{ Message string }

// Decide reports whether argv may be handed to the real gh, returning nil when
// it may.
//
// The order the checks run in is part of the contract: a command that breaks
// both the first rule and one of the body rules is answered by the first, which
// is the one whose fix comes first.
func Decide(argv []string, env Env) *Block {
	if len(argv) < 2 {
		return nil
	}
	// gh api is judged apart, before the table: it has no verb — argv[1] is the
	// endpoint — so it can be no row of one, and the read fast path below would
	// let every one of them through.
	if argv[0] == "api" {
		return decideAPI(argv, env)
	}
	c := command{noun: argv[0], verb: argv[1]}
	if !writes(c) {
		return nil
	}

	vf := valueFlagsFor(c)
	bf := bodyFlagsFor(c)

	// -h is only help where the verb does not take a value for it — under
	// gh repo edit it is --homepage.
	if len(argv) > 2 {
		switch argv[2] {
		case "--help":
			return nil
		case "-h":
			if !strings.ContainsRune(vf.short, 'h') {
				return nil
			}
		}
	}

	if env.ClaudeCode == "" {
		return nil
	}

	s := scan(argv[2:], vf, bf, env.GHRepo)
	if !isExplicit(c, s) {
		return &Block{Message: notExplicitMessage(c, argv, env.Dir(), env.OriginRemote())}
	}
	return bodyBlock(c, bf, argv, s)
}

// decideAPI is the fifth rule: a review-thread reply or resolve may not be
// typed at gh api.
//
// The first rule is not applied here. What it asks for is that the repository
// not be resolved from the working directory, and a gh api endpoint names it
// outright — the {owner} and {repo} placeholders being the exception that gh
// itself fills in from there, which is a spelling of the endpoint rather than
// a way of leaving the target open.
//
// The order the checks run in is part of the contract, as it is in Decide.
// Help and the environment come first, as they do for the other four. Then a
// query that could be read and asks for a mutation answers before a file that
// could not be read: naming the mutation is the more useful of the two
// refusals, and the second is the one that fires when nothing is known.
func decideAPI(argv []string, env Env) *Block {
	// gh api declares no -h of its own, so both spellings are help.
	switch argv[1] {
	case "--help", "-h":
		return nil
	}
	if env.ClaudeCode == "" {
		return nil
	}

	s := scanAPI(argv[1:])
	endpoint := normaliseEndpoint(s.endpoint)

	if endpoint == "graphql" {
		query, source, reason := s.queryText()
		if name := threadMutationName(query); name != "" {
			return &Block{Message: apiThreadMutationMessage(argv, "mutation: "+name)}
		}
		if source != "" {
			return &Block{Message: apiQueryFileMessage(argv, source, reason)}
		}
		return nil
	}
	if replyEndpoint.MatchString(endpoint) && s.isPOST() {
		return &Block{Message: apiThreadMutationMessage(argv, "endpoint: POST "+endpoint)}
	}
	return nil
}

// bodyBlock applies the three rules that look at the body.
func bodyBlock(c command, bf bodyFlags, argv []string, s scanned) *Block {
	if strings.Contains(s.inlineBody, "\n") {
		return &Block{Message: multilineBodyMessage(c, bf, argv)}
	}

	var body, source string
	switch {
	case s.bodyFile != "":
		// The one carve-out from failing closed. Reading standard input here
		// would consume what gh is about to read, so the scan is given up
		// rather than the command; it reaches only the two rules below.
		if s.bodyFile == "-" {
			return nil
		}
		content, reason := readNamedFile(s.bodyFile)
		if reason != "" {
			return &Block{Message: unreadableBodyMessage(bf, argv, s.bodyFile, reason)}
		}
		body = content
		source = fmt.Sprintf("--%s %s", bf.fileLong, s.bodyFile)
	case s.inlineBody != "":
		body = s.inlineBody
		source = fmt.Sprintf("the --%s value", bf.inlineLong)
	default:
		return nil
	}

	if distinct := countBareHashRefs(body); distinct >= 3 {
		return &Block{Message: bareHashRefsMessage(distinct, source)}
	}
	if c.noun == "pr" && (c.verb == "create" || c.verb == "edit") && hasQuotedClosingKeyword(body) {
		return &Block{Message: quotedClosingKeywordMessage(source)}
	}
	return nil
}

// The three reasons a named body file cannot be read, told apart because the
// fix differs. They are guidance rather than a failure of this program, which
// is why they are returned as text and not as an error.
const (
	reasonMissing    = "the file does not exist (it may not be written out yet)"
	reasonNotRegular = "not a regular file (a directory, a process substitution, a pipe)"
	reasonUnreadable = "no read permission"
)

// readNamedFile reads a file a flag named, or says why it could not. Both the
// bodies of rules 2 to 4 and the GraphQL queries of rule 5 arrive this way.
//
// A file that gh can read but that is not a regular one — a process
// substitution, a piped /dev/stdin — is refused as well. That is a change from
// what gh alone would do, and it is kept because neither spelling appears in
// this repository's use while both would leave the content unscanned.
func readNamedFile(path string) (content, reason string) {
	// Anything that stops the stat — including a directory on the way that
	// cannot be searched — reads as absent, which is what the shell's -e said.
	info, err := os.Stat(path)
	switch {
	case err != nil:
		return "", reasonMissing
	case !info.Mode().IsRegular():
		return "", reasonNotRegular
	}
	// Permission is decided by opening the file rather than by reading the
	// mode bits, which would have to guess at the process's own identity.
	b, err := os.ReadFile(path)
	if err != nil {
		return "", reasonUnreadable
	}
	return string(b), ""
}
