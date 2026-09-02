// Package ghshim guards the writing subcommands of gh.
//
// It is installed as a program called gh, in front of the real one on PATH, and
// hands every invocation on to it unless one of four rules refuses.
//
// Rule 1: the repository has to be named on the command line. An agent that has
// cd'd into another repository to look at something would otherwise create the
// issue or the pull request there, because gh resolves the repository from the
// working directory's remote. What has to hold is that the repository is not
// resolved implicitly; -R is one sufficient condition for that and not the only
// one, and the forms gh accepts differ per noun, so the test differs per noun
// too — gh repo edit and its siblings take a positional and have no -R at all,
// and gh resolves GH_REPO only as a default for -R, so those cannot be covered
// by the environment either. Help right after the verb is exempt.
//
// Rule 2: a body of more than one line may not be passed inline. Combining
// --body "$(...)" with a heredoc stacks two layers of quoting, and a
// mis-escape — a stray backslash before a backtick — ends up in the published
// text. The judgement is on the value of the body flag itself, so a multi-line
// value of some other flag is not touched. It is a prohibition rather than a
// detection because the shim sees the string after the shell has finished with
// it, when a mis-escape can no longer be told from text that means to contain a
// backslash.
//
// Rule 3: a body may not number its items with a bare #N. GitHub autolinks
// those, so numbering a list of remarks sends a reference notification to
// unrelated issues, and a notification cannot be taken back. Three or more
// distinct #1 to #9 count as numbering. What GitHub does not link is excluded:
// code spans, fenced blocks, a digit followed by more alphanumerics, and the
// OWNER/REPO#N form.
//
// Rule 4: a pull request body may not hold a closing keyword inside backticks.
// GitHub does not read Closes #N as one there, so the issue stays open after
// the merge. Only gh pr create and gh pr edit are in scope, since that is where
// the keyword does anything.
//
// Which flags carry a body is decided by whether GitHub renders the value as
// markdown, not by the spelling — the same -b is --base under gh issue develop
// and --branch under gh pr checkout — so both are recorded per verb.
//
// This is a program rather than a PreToolUse hook because a hook is handed the
// command as a string, which does not say what will actually run; see
// claude/.claude/rules/hooks-design.md for the two failure modes that followed.
//
// The guard applies inside Claude Code sessions only, and reading subcommands
// go straight through. A refusal exits 78, which none of gh's own statuses use.
package ghshim

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"syscall"

	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// blockExit avoids every status gh documents (0, 1, 2, 4) and the shell's
// reserved 126 and 127, so that a caller can tell a refusal from gh's own
// failure.
const blockExit = 78

const noRealGHMessage = "gh shim: 実体の gh が見つかりません（GH_BIN と PATH を確認してください）。\n"

const internalErrorMessage = `gh shim: 判定中に内部エラーが発生したため、gh を実行しませんでした（%v）。

これは shim (~/.local/shims/gh) 側の不具合です。
緊急の回避策: PATH から ~/.local/shims を外すか、実体の gh を絶対パスで呼んでください。
`

const execFailedMessage = `gh shim: 実体の gh (%s) を起動できませんでした（%v）。

緊急の回避策: PATH から ~/.local/shims を外すか、実体の gh を絶対パスで呼んでください。
`

// deps are the seams run works through. Everything the process supplies is
// resolved once, in Execute, and passed as a value.
type deps struct {
	env      Env
	ghBin    string
	pathList string
	selfDir  string
	// exec returns only when the hand-off failed.
	exec    func(argv0 string, argv, env []string) error
	environ func() []string
	build   selfbuild.State
}

// Execute runs the guard and returns the process exit status.
//
// It takes no standard input and no standard output on purpose: reading
// standard input would consume what gh is about to read, and leaving the
// parameters out is what makes that checkable rather than promised.
func Execute(ctx context.Context, argv []string, stderr io.Writer) int {
	sb := selfbuild.NewDeps(argv)
	build := selfbuild.Run(ctx, sb)

	selfDir, err := filepath.EvalSymlinks(filepath.Dir(sb.Exe))
	if err != nil {
		selfDir = filepath.Dir(sb.Exe)
	}

	return run(argv, stderr, deps{
		env:      NewEnv(ctx, runner.Exec{}),
		ghBin:    os.Getenv("GH_BIN"),
		pathList: os.Getenv("PATH"),
		selfDir:  selfDir,
		exec:     syscall.Exec,
		environ:  os.Environ,
		build:    build,
	})
}

// NewEnv wires the inputs of the decision to the process.
func NewEnv(ctx context.Context, r runner.Runner) Env {
	return Env{
		GHRepo:     os.Getenv("GH_REPO"),
		ClaudeCode: os.Getenv("CLAUDECODE"),
		Dir: func() string {
			dir, err := os.Getwd()
			if err != nil {
				return ""
			}
			return dir
		},
		OriginRemote: func() string {
			out, err := runner.Git(ctx, r, "", "remote", "get-url", "origin")
			if err != nil || out == "" {
				return "(取得不可: git リポジトリ外、または origin が未設定)"
			}
			return out
		},
	}
}

// run is Execute with the process resolved away, so that a test can drive the
// hand-off without one.
//
// The deferred recover is the net the shell kept as a trap on EXIT; nothing
// here raises a panic on purpose. Without it a fault would still keep gh from
// running, since the hand-off is only on the path where every check passed —
// what it buys is the status, because a panic exits 2 and 78 was chosen not to
// collide with gh's own.
func run(argv []string, stderr io.Writer, d deps) (code int) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Fprintf(stderr, internalErrorMessage, r)
			code = blockExit
		}
	}()

	reportBuild(stderr, d.build)

	// Before the reading fast path, as in the shell: with no real gh there is
	// nothing to hand anything to, reads included.
	realGH, err := Real(d.ghBin, d.pathList, d.selfDir)
	if err != nil {
		fmt.Fprint(stderr, noRealGHMessage)
		return blockExit
	}

	if block := Decide(argv, d.env); block != nil {
		fmt.Fprint(stderr, block.Message)
		return blockExit
	}

	if err := d.exec(realGH, append([]string{realGH}, argv...), d.environ()); err != nil {
		fmt.Fprintf(stderr, execFailedMessage, realGH, err)
		return blockExit
	}
	// Not reached: a hand-off that worked replaced this process.
	return 0
}

// reportBuild follows the same contract as the script subcommands of ccx:
// standard error, and the status the guard arrived at is unchanged. A guard
// that started blocking everything because the toolchain is unhappy would be
// worse than a stale one.
func reportBuild(stderr io.Writer, build selfbuild.State) {
	if report := build.Report("gh shim"); report != "" {
		fmt.Fprintln(stderr, report)
	}
}
