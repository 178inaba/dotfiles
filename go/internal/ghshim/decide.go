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
	// GHRepo is GH_REPO. The shell could not see it in the argv either — an
	// assignment is consumed before the command sees it — so a value here
	// means gh will resolve the repository from it rather than from the
	// working directory, which is what the first rule asks for.
	GHRepo string
	// ClaudeCode is CLAUDECODE. Empty means an interactive shell, which the
	// guard leaves alone: the mishaps it exists for are an agent writing to a
	// repository it had only cd'd into, and a guard that costs something every
	// day gets switched off.
	ClaudeCode string

	// Dir and OriginRemote fill in the two lines the first rule's message ends
	// with. They are functions because that message is their only reader:
	// resolving the remote runs git, and the shell ran it only after the rule
	// had already failed. Resolving either eagerly would put a subprocess in
	// front of every command the guard lets through.
	Dir          func() string
	OriginRemote func() string
}

// Block is a refusal. Message is the guidance shown to the user, complete and
// ready for standard error.
type Block struct{ Message string }

// Decide reports whether argv may be handed to the real gh, returning nil when
// it may.
//
// The order the checks run in is part of the contract: a command that breaks
// both the first rule and one of the body rules is answered by the first, which
// is the one whose fix comes first.
func Decide(argv []string, env Env) *Block {
	// Too few arguments to name a command: gh gets them as they are and says
	// what it makes of them.
	if len(argv) < 2 {
		return nil
	}
	c := command{noun: argv[0], verb: argv[1]}
	if !writes(c) {
		return nil
	}

	vf := valueFlagsFor(c)
	bf := bodyFlagsFor(c)

	// Help right after the verb is a read. -h is only help where the verb does
	// not take a value for it — under gh repo edit it is --homepage.
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

// bodyBlock applies the three rules that look at the body: it must be one line,
// it must be readable, and what it says must be what GitHub will read.
func bodyBlock(c command, bf bodyFlags, argv []string, s scanned) *Block {
	// Whether the body is well formed is judged on the value itself, so a
	// multi-line value of some other flag is not touched.
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
		content, reason := readBody(s.bodyFile)
		if reason != "" {
			return &Block{Message: unreadableBodyMessage(bf, argv, s.bodyFile, reason)}
		}
		body = content
		source = fmt.Sprintf("--%s %s", bf.fileLong, s.bodyFile)
	case s.inlineBody != "":
		body = s.inlineBody
		source = fmt.Sprintf("--%s の本文", bf.inlineLong)
	default:
		return nil
	}

	if distinct := countBareHashRefs(body); distinct >= 3 {
		return &Block{Message: bareHashRefsMessage(distinct, source)}
	}
	// A closing keyword is only read as one in a pull request body; in an issue
	// body or a comment it is an ordinary link.
	if c.noun == "pr" && (c.verb == "create" || c.verb == "edit") && hasQuotedClosingKeyword(body) {
		return &Block{Message: quotedClosingKeywordMessage(source)}
	}
	return nil
}

// The three reasons a named body file cannot be read. They are told apart
// because the fix differs, and returned rather than wrapped in an error: each
// one is a line of the guidance, not a failure of this program.
const (
	reasonMissing    = "ファイルが存在しません（本文をまだ書き出していない可能性があります）"
	reasonNotRegular = "通常ファイルではありません（ディレクトリ・プロセス置換・パイプ等）"
	reasonUnreadable = "読み取り権限がありません"
)

// readBody reads a named body file, or says why it could not.
//
// A file that gh can read but that is not a regular one — a process
// substitution, a piped /dev/stdin — is refused as well. That is a change from
// what gh alone would do, and it is kept because neither spelling appears in
// this repository's use while both would leave the body unscanned.
func readBody(path string) (body, reason string) {
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
