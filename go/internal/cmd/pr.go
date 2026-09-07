package cmd

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// newPRCmd builds `ccx pr`, the commands that work from a pull request.
func newPRCmd(deps Deps) *cobra.Command {
	c := newParentCmd("pr", "Read and act on a pull request")
	c.AddCommand(prContextCmd(deps), prPrepareReviewCmd(deps), prFreshnessCmd(deps), prPostReviewCmd(deps),
		prReplyThreadsCmd(deps), prSeenCmd(deps), prCommentCmd(deps), prBodyAppendCmd(deps))
	return c
}

// prFreshnessCmd builds `ccx pr freshness`, the guard /deep-review and
// /review-response run before they read a diff or apply a fix.
func prFreshnessCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "freshness <pr-context.json>",
		Short: "Compare the checkout here with the pull request's head",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			content, err := readFile(args[0], "pr context file")
			if err != nil {
				return silent(err)
			}
			prContext, err := pullrequest.ParseContext([]byte(content), args[0])
			if err != nil {
				return silent(err)
			}

			report, err := worktree.CheckFreshness(c.Context(), runner.Exec{}, deps.Dir, prContext.Checkout())
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), report))
		},
	}
}

// prContextCmd builds `ccx pr context`, which /deep-review and
// /review-response both open with.
//
// Standard output is where things were put and nothing else. The context
// itself runs to hundreds of kilobytes on a large pull request, so it is
// written to a file here rather than passed back through a redirection the
// model composes — and the name is composed here too, because parallel
// subagents share one scratch directory and a fixed name has already caused
// one to read another repository's pull request. The work dir comes with it,
// since a caller that goes on to reply to threads writes into it.
func prContextCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "context <out-dir> [<pr-number>]",
		Short: "Fetch a pull request's comments, reviews and threads into a file",
		Args:  cobra.RangeArgs(1, 2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			outDir := args[0]
			if info, err := os.Stat(outDir); err != nil || !info.IsDir() {
				return silent(fmt.Errorf("output directory not found: %s", outDir))
			}
			number := 0
			if len(args) == 2 {
				var err error
				if number, err = issueNumber(args[1]); err != nil {
					return fmt.Errorf("invalid pr number: %s", args[1])
				}
			}
			limits, err := contextLimits()
			if err != nil {
				return silent(err)
			}

			client, err := deps.NewClient()
			if err != nil {
				return silent(err)
			}
			repo, err := currentRepo(c.Context(), client, deps.Dir)
			if err != nil {
				return silent(err)
			}
			meta, err := contextPR(c.Context(), client, repo, deps.Dir, number)
			if err != nil {
				return silent(err)
			}

			doc, err := pullrequest.OpenDocument(c.Context(), runner.Exec{}, deps.Dir, outDir, repo, meta)
			if err != nil {
				return silent(err)
			}
			fetched, err := pullrequest.Fetch(c.Context(), client, repo, meta, limits, doc.Change, stateHome())
			if err != nil {
				return silent(err)
			}
			if err := storeContext(doc.Path, fetched); err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), pullrequest.Stored{
				Path: doc.Path, WorkDir: doc.Work.Dir, ThreadsPath: doc.Work.ThreadsPath,
			}))
		},
	}
}

// stateHome is the directory the record of a judged pull request is kept
// under, empty where there is nowhere to derive one.
//
// Here rather than in the package, for the reason the clone workspace's
// equivalent is: t.Setenv changes the whole process and forbids a parallel
// test, so the package that keeps the record takes the directory as a
// parameter and only this thin reader touches the environment.
func stateHome() string { return xdgDir("XDG_STATE_HOME", "state") }

// xdgDir resolves one XDG base directory: the variable where it is set, and
// ~/.local/<fallback> where it is not.
//
// The empty string where there is no home directory to build on, which is
// nothing this package can recover from and which each caller answers for
// itself — a read of a record degrades to "nothing recorded", a write refuses.
//
// One implementation because there is one rule. Written twice, the second XDG
// root to be added would copy whichever spelling it happened to sit beside.
func xdgDir(variable, fallback string) string {
	if dir := os.Getenv(variable); dir != "" {
		return dir
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".local", fallback)
}

// currentRepo names the repository these commands work on.
//
// The wrapped failure is kept: "no git remote names a repository" is a
// different problem from being unauthenticated, and a message that hides which
// one it was sends the reader to debug the wrong thing.
func currentRepo(ctx context.Context, client *ghapi.Client, dir string) (ghapi.Repo, error) {
	repo, err := client.CurrentRepo(ctx, runner.Exec{}, dir)
	if err != nil {
		return ghapi.Repo{}, fmt.Errorf("failed to resolve the repository: %w", err)
	}
	return repo, nil
}

// contextPR resolves the pull request the context is about.
//
// The two ways it can fail need different answers, so they are reported apart:
// a number that names nothing is one thing, and a branch with no pull request
// is another, where naming a number is the way forward.
func contextPR(ctx context.Context, client *ghapi.Client, repo ghapi.Repo, dir string, number int) (ghapi.PullRequest, error) {
	if number == 0 {
		pr, err := client.PullRequestForCurrentBranch(ctx, runner.Exec{}, dir, repo)
		if err != nil {
			return ghapi.PullRequest{}, fmt.Errorf("could not infer PR from current branch; specify <pr-number> explicitly")
		}
		return pr, nil
	}
	pr, err := client.PullRequest(ctx, repo, number)
	if err != nil {
		return ghapi.PullRequest{}, fmt.Errorf("failed to fetch PR #%d: %v", number, err)
	}
	return pr, nil
}

// storeJSON writes a document to the path it is given, through a temporary
// file in the same directory, so that a run interrupted halfway leaves no
// partial document where a complete one is expected.
//
// One implementation for every document this package writes: the sequence is
// here precisely because getting it wrong leaves a torn file, and a second
// copy is one a later hardening would silently miss.
func storeJSON(path, tmpPrefix string, v any) error {
	dir := filepath.Dir(path)
	tmp, err := os.CreateTemp(dir, tmpPrefix)
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())

	if err := renderJSON(tmp, v); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(tmp.Name(), path)
}

// storeContext writes a fetched context to the path it is given.
func storeContext(path string, c pullrequest.Context) error {
	return storeJSON(path, ".pr-context.*", c)
}

// storeSeen writes one record of a judged pull request to the path it is
// given, atomically as the document is: a run interrupted halfway leaves
// either the previous record or the new one, and never a torn value the next
// run would read as nothing recorded.
func storeSeen(path string, s pullrequest.Seen) error {
	return storeJSON(path, ".seen.*", s)
}

// prSeenCmd builds `ccx pr seen`, which a skill runs at the end of a run that
// reached a judgment.
//
// The document rather than the pull request number, because what is recorded
// is the instant that document was read at: a number would leave the command
// to fetch one for itself, and the mark would then be later than the judgment
// it stands for, silently retiring whatever arrived in between.
func prSeenCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "seen <pr-context.json>",
		Short: "Record that a run judged this pull request",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			content, err := readFile(args[0], "pr context file")
			if err != nil {
				return silent(err)
			}
			prContext, err := pullrequest.ParseContext([]byte(content), args[0])
			if err != nil {
				return silent(err)
			}
			repo, err := ghapi.ParseRepo(prContext.Repo)
			if err != nil {
				return silent(fmt.Errorf("the document names no repository: %v", err))
			}

			record, err := pullrequest.WriteSeen(stateHome(), repo, prContext.PR.Number, prContext.FetchedAt, storeSeen)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), record))
		},
	}
}

// prPrepareReviewCmd builds `ccx pr prepare-review`, which /deep-review opens
// with: it settles which pull request, whether the checkout matches it, its
// context, its freshness and which mode the review runs in, in one call.
//
// Where there is no pull request it prepares a local review of the branch
// instead, which is the ordinary degradation Preparation.PRExists names rather
// than a second command: the main input is still a pull request number and
// what it hands out is still that pull request's context, so it belongs under
// pr even though this path reaches neither.
func prPrepareReviewCmd(deps Deps) *cobra.Command {
	var issue int
	var worktreeFlag, localOnly, noAutofix bool
	c := &cobra.Command{
		Use:   "prepare-review <scratchpad-dir> [<pr-number>]",
		Short: "Settle everything a review needs before it starts",
		Args:  cobra.RangeArgs(1, 2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			scratch := args[0]
			if info, err := os.Stat(scratch); err != nil || !info.IsDir() {
				return silent(fmt.Errorf("scratchpad directory not found: %s", scratch))
			}
			number := 0
			if len(args) == 2 {
				var err error
				if number, err = issueNumber(args[1]); err != nil {
					return fmt.Errorf("invalid argument: %s", args[1])
				}
			}

			client, err := deps.NewClient()
			if err != nil {
				return silent(err)
			}
			repo, err := currentRepo(c.Context(), client, deps.Dir)
			if err != nil {
				return silent(err)
			}

			options := pullrequest.Options{
				OutDir: scratch, Number: number, Issue: issue,
				Worktree: worktreeFlag, LocalOnly: localOnly, NoAutofix: noAutofix,
				StateHome: stateHome(),
			}
			prepared, err := pullrequest.Prepare(c.Context(), runner.Exec{}, client, repo, deps.Dir, options, storeContext)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), prepared))
		},
	}
	c.Flags().IntVar(&issue, "issue", 0, "issue the review is about, read with or without a pull request and instead of the ones the body names")
	c.Flags().BoolVar(&worktreeFlag, "worktree", false, "the checkout is a worktree already resolved for this pull request")
	c.Flags().BoolVar(&localOnly, "local-only", false, "do not post the findings as a review")
	c.Flags().BoolVar(&noAutofix, "no-autofix", false, "do not act on the findings")
	return c
}

// fetchLimit is one cap on what a fetch reads: the variable that raises it, the
// cap it raises, the flag the document sets where it was reached, and what it
// counts against where that is one item rather than the whole document.
//
// The four are one declaration because two readers need the same pairing: the
// command, which raises the cap the variable names, and the help, which
// publishes it so a caller answering a truncation is not left to guess which
// variable goes with which flag. Kept apart, the help could list a variable the
// command does not read.
//
// They stay environment variables rather than becoming flags: the only time
// anybody sets one is to run the same command again with more room, and the
// command line belongs to the skill.
type fetchLimit struct {
	variable string
	// A pointer into the limits handed in, since the command raises a copy of
	// the defaults rather than the defaults themselves.
	cap  func(*pullrequest.Limits) *int
	flag string
	// What the cap counts against, empty where it is the whole document.
	per string
}

// fetchLimits are the caps a caller raises when a pull request was cut short,
// in the order they are read.
var fetchLimits = [...]fetchLimit{
	{"MAX_COMMENTS", func(l *pullrequest.Limits) *int { return &l.Comments }, "comments_truncated", ""},
	{"MAX_REVIEWS", func(l *pullrequest.Limits) *int { return &l.Reviews }, "reviews_truncated", ""},
	{"MAX_THREADS", func(l *pullrequest.Limits) *int { return &l.Threads }, "threads_truncated", ""},
	{"MAX_THREAD_COMMENTS", func(l *pullrequest.Limits) *int { return &l.ThreadComments }, "review_threads[].comments_truncated", "thread"},
	{"MAX_ISSUE_COMMENTS", func(l *pullrequest.Limits) *int { return &l.IssueComments }, "linked_issues[].comments_truncated", "issue"},
}

func contextLimits() (pullrequest.Limits, error) {
	limits := pullrequest.DefaultLimits
	for _, l := range fetchLimits {
		value := os.Getenv(l.variable)
		if value == "" {
			continue
		}
		n, err := strconv.Atoi(value)
		if err != nil || strings.ContainsFunc(value, func(r rune) bool { return r < '0' || r > '9' }) {
			return pullrequest.Limits{}, fmt.Errorf("invalid %s: %s", l.variable, value)
		}
		*l.cap(&limits) = n
	}
	return limits, nil
}

func prPostReviewCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "post-review <pr-context.json> <review-file>",
		Short: "Post a review, after checking every comment still anchors to the diff",
		Args:  cobra.ExactArgs(2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			contextFile, reviewFile := args[0], args[1]
			context, err := readFile(contextFile, "pr context file")
			if err != nil {
				return silent(err)
			}
			review, err := readFile(reviewFile, "review file")
			if err != nil {
				return silent(err)
			}
			prContext, err := pullrequest.ParseContext([]byte(context), contextFile)
			if err != nil {
				return silent(err)
			}
			// The directory check comes before the contents: a comment
			// anchored to the wrong pull request is caught by the line check
			// only when there are comments, and where the file sits is what
			// stops it structurally.
			if err := pullrequest.RequireInWorkDir(reviewFile, "review_path", contextFile); err != nil {
				return silent(err)
			}

			submission, err := pullrequest.ParseSubmission([]byte(review), filepath.Dir(reviewFile), reviewFile)
			if err != nil {
				return silent(err)
			}
			client, err := deps.NewClient()
			if err != nil {
				return silent(err)
			}
			posted, err := pullrequest.Post(c.Context(), runner.Exec{}, client, deps.Dir, prContext.Target(), submission)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), posted))
		},
	}
}

// prCommentCmd builds `ccx pr comment`, the pull-request-level post a skill
// makes when there is something to say that belongs to no thread.
//
// The body comes from a file rather than from the command line: a report
// written as a shell argument loses its markdown to one missed escape. The
// file has to sit in the work dir paired with the document, which is what
// keeps parallel runs on different pull requests out of each other's files.
func prCommentCmd(deps Deps) *cobra.Command {
	var mark, bodyFile string
	cmd := &cobra.Command{
		Use:   "comment <pr-context.json> --mark <name> --body-file <name>",
		Short: "Post a comment on the pull request, marked as ours",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			contextFile := args[0]
			content, err := readFile(contextFile, "pr context file")
			if err != nil {
				return silent(err)
			}
			prContext, err := pullrequest.ParseContext([]byte(content), contextFile)
			if err != nil {
				return silent(err)
			}
			// Before the body is looked for: a run whose mark is wrong would
			// otherwise be told about a missing file it does not have.
			parsedMark, err := pullrequest.ParseMark(mark)
			if err != nil {
				return silent(err)
			}
			body, err := pullrequest.ParseCommentBody(pullrequest.WorkDir(contextFile), bodyFile)
			if err != nil {
				return silent(err)
			}

			client, err := deps.NewClient()
			if err != nil {
				return silent(err)
			}
			posted, err := pullrequest.PostComment(c.Context(), runner.Exec{}, client, deps.Dir,
				prContext.Target(), parsedMark, body)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), posted))
		},
	}
	cmd.Flags().StringVar(&mark, "mark", "", "the marker to write at the front of the comment (review-response)")
	cmd.Flags().StringVar(&bodyFile, "body-file", "", "the name of a markdown file in the work dir holding the body")
	// Discarded as the other required flags in this package are: the only way
	// these fail is on a flag this function did not declare.
	_ = cmd.MarkFlagRequired("mark")
	_ = cmd.MarkFlagRequired("body-file")
	return cmd
}

// prBodyAppendCmd builds `ccx pr body-append`, the write-down a skill makes
// when a decision nothing records has to be recorded before it can be cited.
//
// The body file holds the new section alone. What it is appended to is read
// from GitHub at the moment of the write, so a document fetched an hour ago
// cannot undo an edit made since — and a file holding only the section cannot
// wipe the rest of the body, which is what made the `gh pr edit` this replaces
// a footgun.
func prBodyAppendCmd(deps Deps) *cobra.Command {
	var bodyFile string
	cmd := &cobra.Command{
		Use:   "body-append <pr-context.json> --body-file <name>",
		Short: "Append one section to the pull request's body",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			contextFile := args[0]
			content, err := readFile(contextFile, "pr context file")
			if err != nil {
				return silent(err)
			}
			prContext, err := pullrequest.ParseContext([]byte(content), contextFile)
			if err != nil {
				return silent(err)
			}
			target := prContext.Target()
			// Before the body is looked for, as the mark is for `ccx pr
			// comment`: a run on somebody else's pull request would otherwise
			// be told about a missing file, which is not its fault.
			if err := target.RequireOwn(); err != nil {
				return silent(err)
			}
			section, err := pullrequest.ParseCommentBody(pullrequest.WorkDir(contextFile), bodyFile)
			if err != nil {
				return silent(err)
			}

			client, err := deps.NewClient()
			if err != nil {
				return silent(err)
			}
			appended, err := pullrequest.AppendBody(c.Context(), client, target, section)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), appended))
		},
	}
	cmd.Flags().StringVar(&bodyFile, "body-file", "", "the name of a markdown file in the work dir holding the section")
	// Discarded as the other required flags in this package are: the only way
	// this fails is on a flag this function did not declare.
	_ = cmd.MarkFlagRequired("body-file")
	return cmd
}

func prReplyThreadsCmd(deps Deps) *cobra.Command {
	var dryRun bool
	cmd := &cobra.Command{
		Use:   "reply-threads <pr-context.json> <threads-file>",
		Short: "Reply to and resolve the review threads it is our move on",
		Args:  cobra.ExactArgs(2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			contextFile, threadsFile := args[0], args[1]
			context, err := readFile(contextFile, "pr context file")
			if err != nil {
				return silent(err)
			}
			threads, err := readFile(threadsFile, "threads file")
			if err != nil {
				return silent(err)
			}

			prContext, err := pullrequest.ParseContext([]byte(context), contextFile)
			if err != nil {
				return silent(err)
			}
			if err := pullrequest.RequireInWorkDir(threadsFile, "threads_path", contextFile); err != nil {
				return silent(err)
			}
			// The check keeps its place ahead of parsing the threads file, so a
			// run with nothing to post still checks: that costs one round trip
			// on a rare path and leaves nothing to reason about.
			client, err := deps.NewClient()
			if err != nil {
				return silent(err)
			}
			if err := pullrequest.RequirePushedHead(c.Context(), runner.Exec{}, client, deps.Dir,
				prContext.Target(), "replying or resolving"); err != nil {
				return silent(err)
			}

			// The work dir is where a body_file is looked for, exactly as
			// post-review looks for a review body beside the review file.
			actions, err := pullrequest.ParseThreadActions([]byte(threads), filepath.Dir(threadsFile), threadsFile)
			if err != nil {
				return silent(err)
			}
			// Nothing to do is an ordinary answer, and the one exit of this
			// command that renders indented rather than compact.
			if len(actions) == 0 {
				if dryRun {
					return silent(renderJSON(c.OutOrStdout(), pullrequest.ReplyPlan{Plan: []pullrequest.PlannedThread{}}))
				}
				return silent(renderJSON(c.OutOrStdout(), pullrequest.ThreadReplies{
					Replied: []pullrequest.RepliedThread{}, Resolved: []string{},
					ResolveFailed: []pullrequest.FailedResolve{}, Warnings: []string{},
				}))
			}

			req := pullrequest.ReplyRequest{
				Actions: actions, Threads: prContext.KnownThreads(), ContextFile: contextFile, ThreadsFile: threadsFile,
			}
			if dryRun {
				planned, err := pullrequest.DryRun(c.Context(), client, req)
				if err != nil {
					return silent(err)
				}
				return silent(renderCompactJSON(c.OutOrStdout(), planned))
			}
			replies, err := pullrequest.Reply(c.Context(), client, req)
			if err != nil {
				return silent(err)
			}
			return silent(renderCompactJSON(c.OutOrStdout(), replies))
		},
	}
	cmd.Flags().BoolVar(&dryRun, "dry-run", false, "Run every check and print the plan without posting anything")
	return cmd
}
