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
	c.AddCommand(prContextCmd(build), prPrepareReviewCmd(build), prFreshnessCmd(build), prPostReviewCmd(build), prReplyThreadsCmd(build))
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
			pr, err := pullrequest.ParseCheckout([]byte(content))
			if err != nil {
				// The package reports the field without naming the file,
				// because the path is the caller's; a message that says which
				// file is missing a field is the useful one.
				return silent(fmt.Errorf("%v in %s", err, args[0]))
			}

			// The working directory, which is the checkout the caller means:
			// these run inside the worktree being reviewed in.
			report, err := worktree.CheckFreshness(c.Context(), runner.Exec{}, ".", pr)
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

			// The work dir and the change before the fetch, and the fetch
			// before the write: a head that moved has to stop the run while
			// there is still no document to disagree with it.
			store := contextStore{outDir: outDir, repo: repo}
			work, err := pullrequest.EnsureWorkFiles(store.Path(meta.Number))
			if err != nil {
				return silent(err)
			}
			// The working directory, which is the checkout the caller means,
			// as the freshness check reads it.
			change, err := pullrequest.ReadChange(c.Context(), runner.Exec{}, ".", meta, work.DiffPath)
			if err != nil {
				return silent(err)
			}

			fetched, err := pullrequest.Fetch(c.Context(), client, repo, meta, limits, change)
			if err != nil {
				return silent(err)
			}
			path, err := store.Write(fetched)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), pullrequest.Stored{
				Path: path, WorkDir: work.Dir, ThreadsPath: work.ThreadsPath,
			}))
		},
	}
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

// contextStore writes a fetched context into outDir.
//
// What the file is called is pullrequest's, since that is what takes the name
// apart again to find the review's work directory; where the directory is, is
// this layer's, which is why the two halves meet here.
type contextStore struct {
	outDir string
	repo   ghapi.Repo
}

// Path implements pullrequest.Store. Answered before the document exists,
// because the document carries the path of the diff file beside it.
func (s contextStore) Path(number int) string {
	return filepath.Join(s.outDir, pullrequest.ContextFileName(s.repo, number))
}

// Write implements pullrequest.Store.
//
// Through a temporary file in the same directory, so that a run interrupted
// halfway leaves no partial document where a complete one is expected.
func (s contextStore) Write(c pullrequest.Context) (string, error) {
	tmp, err := os.CreateTemp(s.outDir, ".pr-context.*")
	if err != nil {
		return "", err
	}
	defer os.Remove(tmp.Name())

	if err := renderJSON(tmp, c); err != nil {
		tmp.Close()
		return "", err
	}
	if err := tmp.Close(); err != nil {
		return "", err
	}
	path := s.Path(c.PR.Number)
	if err := os.Rename(tmp.Name(), path); err != nil {
		return "", err
	}
	return path, nil
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
				Number: number, Issue: issue,
				Worktree: worktreeFlag, LocalOnly: localOnly, NoAutofix: noAutofix,
			}
			store := contextStore{outDir: scratch, repo: repo}
			prepared, err := pullrequest.Prepare(c.Context(), runner.Exec{}, client, repo, ".", options, store)
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

// contextLimits reads the three caps a caller raises when a pull request was
// cut short.
//
// limitVars are the environment variables that raise the fetch limits, in the
// order the limits are read.
//
// Named here so that the help lists the same three rather than a copy of them.
// They stay environment variables rather than becoming flags: the only time
// anybody sets one is to run the same command again with more room, and the
// command line belongs to the skill.
var limitVars = [3]string{"MAX_COMMENTS", "MAX_THREADS", "MAX_THREAD_COMMENTS"}

func contextLimits() (pullrequest.Limits, error) {
	limits := pullrequest.DefaultLimits
	for _, l := range []struct {
		name string
		out  *int
	}{
		{limitVars[0], &limits.Comments},
		{limitVars[1], &limits.Threads},
		{limitVars[2], &limits.ThreadComments},
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
			target, err := pullrequest.ParseTarget([]byte(context))
			if err != nil {
				return silent(fmt.Errorf("%v in %s", err, contextFile))
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
			posted, err := pullrequest.Post(c.Context(), runner.Exec{}, client, ".", target, submission)
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

			known, headOID, err := pullrequest.ParseThreads([]byte(context))
			if err != nil {
				return silent(fmt.Errorf("%v in %s", err, contextFile))
			}
			if err := pullrequest.RequireInWorkDir(threadsFile, "threads_path", contextFile); err != nil {
				return silent(err)
			}
			if err := pullrequest.RequireHead(c.Context(), runner.Exec{}, ".", headOID, "replying or resolving"); err != nil {
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
				Actions: actions, Threads: known, ContextFile: contextFile, ThreadsFile: threadsFile,
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
