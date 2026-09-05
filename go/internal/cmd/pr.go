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
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// newPRCmd builds `ccx pr`, the commands that work from a pull request.
func newPRCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("pr", "Read and act on a pull request")
	c.AddCommand(prContextCmd(build), prPrepareReviewCmd(build), prFreshnessCmd(build), prPostReviewCmd(build),
		prReplyThreadsCmd(build), prSeenCmd(build))
	return c
}

// prFreshnessCmd builds `ccx pr freshness`, the guard /deep-review and
// /review-response run before they read a diff or apply a fix.
func prFreshnessCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "freshness <pr-context.json>",
		Short: "Compare the checkout here with the pull request's head",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			content, err := readFile(args[0], "pr context file")
			if err != nil {
				return silent(err)
			}
			prContext, err := pullrequest.ParseContext([]byte(content), args[0])
			if err != nil {
				return silent(err)
			}

			// The working directory, which is the checkout the caller means:
			// these run inside the worktree being reviewed in.
			report, err := worktree.CheckFreshness(c.Context(), runner.Exec{}, ".", prContext.Checkout())
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
func prContextCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "context <out-dir> [<pr-number>]",
		Short: "Fetch a pull request's comments, reviews and threads into a file",
		Args:  cobra.RangeArgs(1, 2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
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

			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			repo, err := currentRepo(c.Context(), client)
			if err != nil {
				return silent(err)
			}
			meta, err := contextPR(c.Context(), client, repo, number)
			if err != nil {
				return silent(err)
			}

			// The working directory is the checkout the caller means, as the
			// freshness check reads it.
			doc, err := pullrequest.OpenDocument(c.Context(), runner.Exec{}, ".", outDir, repo, meta)
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
// Here rather than in the package, for the reason cloneOptions is: t.Setenv
// changes the whole process and forbids a parallel test, so the package takes
// the directory as a parameter and only this thin reader touches the
// environment. The rule is the XDG default — the variable, else ~/.local/state
// — which is the shape cloneOptions already follows for the data directory.
func stateHome() string {
	if home := os.Getenv("XDG_STATE_HOME"); home != "" {
		return home
	}
	home, err := os.UserHomeDir()
	if err != nil {
		// Nothing to build a path out of. Reported by whoever tries to write;
		// a read of the record degrades to "nothing recorded", which counts
		// everything and loses nothing.
		return ""
	}
	return filepath.Join(home, ".local", "state")
}

// currentRepo names the repository these commands work on.
//
// The wrapped failure is kept: "no git remote names a repository" is a
// different problem from being unauthenticated, and a message that hides which
// one it was sends the reader to debug the wrong thing.
func currentRepo(ctx context.Context, client *ghapi.Client) (ghapi.Repo, error) {
	repo, err := client.CurrentRepo(ctx, runner.Exec{}, ".")
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
func contextPR(ctx context.Context, client *ghapi.Client, repo ghapi.Repo, number int) (ghapi.PullRequest, error) {
	if number == 0 {
		pr, err := client.PullRequestForCurrentBranch(ctx, runner.Exec{}, ".", repo)
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

// storeContext writes a fetched context to the path it is given.
//
// Through a temporary file in the same directory, so that a run interrupted
// halfway leaves no partial document where a complete one is expected.
func storeContext(path string, c pullrequest.Context) error {
	dir := filepath.Dir(path)
	tmp, err := os.CreateTemp(dir, ".pr-context.*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())

	if err := renderJSON(tmp, c); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(tmp.Name(), path)
}

// storeSeen writes one record of a judged pull request to the path it is
// given, the way storeContext writes the document: whole to a temporary file
// beside it and renamed into place, so that a run interrupted halfway leaves
// either the previous record or the new one and never a torn value the next
// run would read as nothing recorded.
func storeSeen(path string, s pullrequest.Seen) error {
	dir := filepath.Dir(path)
	tmp, err := os.CreateTemp(dir, ".seen.*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())

	if err := renderJSON(tmp, s); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(tmp.Name(), path)
}

// prSeenCmd builds `ccx pr seen`, which a skill runs at the end of a run that
// reached a judgment.
//
// The document rather than the pull request number, because what is recorded
// is the instant that document was read at: a number would leave the command
// to fetch one for itself, and the mark would then be later than the judgment
// it stands for, silently retiring whatever arrived in between.
func prSeenCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "seen <pr-context.json>",
		Short: "Record that a run judged this pull request",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
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
func prPrepareReviewCmd(build selfbuild.State) *cobra.Command {
	var issue int
	var worktreeFlag, localOnly, noAutofix bool
	c := &cobra.Command{
		Use:   "prepare-review <scratchpad-dir> [<pr-number>]",
		Short: "Settle everything a review needs before it starts",
		Args:  cobra.RangeArgs(1, 2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
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

			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			repo, err := currentRepo(c.Context(), client)
			if err != nil {
				return silent(err)
			}

			options := pullrequest.Options{
				OutDir: scratch, Number: number, Issue: issue,
				Worktree: worktreeFlag, LocalOnly: localOnly, NoAutofix: noAutofix,
				StateHome: stateHome(),
			}
			prepared, err := pullrequest.Prepare(c.Context(), runner.Exec{}, client, repo, ".", options, storeContext)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), prepared))
		},
	}
	c.Flags().IntVar(&issue, "issue", 0, "issue the review is about, instead of the ones the body names")
	c.Flags().BoolVar(&worktreeFlag, "worktree", false, "the checkout is a worktree already resolved for this pull request")
	c.Flags().BoolVar(&localOnly, "local-only", false, "do not post the findings as a review")
	c.Flags().BoolVar(&noAutofix, "no-autofix", false, "do not act on the findings")
	return c
}

// contextLimits reads the caps a caller raises when a pull request was cut
// short.
//
// limitVars are the environment variables that raise the fetch limits, in the
// order the limits are read.
//
// Named here so that the help lists the same ones rather than a copy of them.
// They stay environment variables rather than becoming flags: the only time
// anybody sets one is to run the same command again with more room, and the
// command line belongs to the skill.
var limitVars = [4]string{"MAX_COMMENTS", "MAX_THREADS", "MAX_THREAD_COMMENTS", "MAX_ISSUE_COMMENTS"}

func contextLimits() (pullrequest.Limits, error) {
	limits := pullrequest.DefaultLimits
	for _, l := range []struct {
		name string
		out  *int
	}{
		{limitVars[0], &limits.Comments},
		{limitVars[1], &limits.Threads},
		{limitVars[2], &limits.ThreadComments},
		{limitVars[3], &limits.IssueComments},
	} {
		value := os.Getenv(l.name)
		if value == "" {
			continue
		}
		n, err := strconv.Atoi(value)
		if err != nil || strings.ContainsFunc(value, func(r rune) bool { return r < '0' || r > '9' }) {
			return pullrequest.Limits{}, fmt.Errorf("invalid %s: %s", l.name, value)
		}
		*l.out = n
	}
	return limits, nil
}

func prPostReviewCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "post-review <pr-context.json> <review-file>",
		Short: "Post a review, after checking every comment still anchors to the diff",
		Args:  cobra.ExactArgs(2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
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
			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			posted, err := pullrequest.Post(c.Context(), runner.Exec{}, client, ".", prContext.Target(), submission)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), posted))
		},
	}
}

func prReplyThreadsCmd(build selfbuild.State) *cobra.Command {
	var dryRun bool
	cmd := &cobra.Command{
		Use:   "reply-threads <pr-context.json> <threads-file>",
		Short: "Reply to and resolve the review threads it is our move on",
		Args:  cobra.ExactArgs(2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
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
			if err := pullrequest.RequireHead(c.Context(), runner.Exec{}, ".", prContext.PR.HeadOID, "replying or resolving"); err != nil {
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

			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
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
