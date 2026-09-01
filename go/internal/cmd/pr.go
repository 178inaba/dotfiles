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
	c.AddCommand(prContextCmd(build), prFreshnessCmd(build), prPostReviewCmd(build), prReplyThreadsCmd(build))
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
			pr, err := worktree.ParsePullRequest([]byte(content))
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
// Standard output is the path and nothing else. The context itself runs to
// hundreds of kilobytes on a large pull request, so it is written to a file
// here rather than passed back through a redirection the model composes — and
// the name is composed here too, because parallel subagents share one scratch
// directory and a fixed name has already caused one to read another
// repository's pull request.
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
			repo, err := client.CurrentRepo(c.Context(), runner.Exec{})
			if err != nil {
				return silent(fmt.Errorf("failed to resolve repository (gh repo view)"))
			}
			meta, err := contextPR(c.Context(), client, repo, number)
			if err != nil {
				return silent(err)
			}

			fetched, err := pullrequest.Fetch(c.Context(), client, repo, meta, limits)
			if err != nil {
				return silent(err)
			}

			// owner and name are separated by @, which neither may contain:
			// with a hyphen, a-b/c and a/b-c would collapse onto one name and
			// the uniqueness this file's whole purpose rests on would have a
			// hole in it.
			name := fmt.Sprintf("pr-context-%s@%s-%d.json", repo.Owner, repo.Name, meta.Number)
			path, err := writeContext(outDir, name, fetched)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), struct {
				Path string `json:"path"`
			}{path}))
		},
	}
}

// contextPR resolves the pull request the context is about.
//
// The two ways it can fail need different answers, so they are reported apart:
// a number that names nothing is one thing, and a branch with no pull request
// is another, where naming a number is the way forward.
func contextPR(ctx context.Context, client *ghapi.Client, repo ghapi.Repo, number int) (ghapi.PullRequest, error) {
	if number == 0 {
		pr, err := client.PullRequestForCurrentBranch(ctx, runner.Exec{}, repo)
		if err != nil {
			return ghapi.PullRequest{}, fmt.Errorf("could not infer PR from current branch; specify <pr-number> explicitly")
		}
		return pr, nil
	}
	pr, err := client.PullRequest(ctx, repo, number)
	if err != nil {
		return ghapi.PullRequest{}, fmt.Errorf("failed to fetch PR #%d", number)
	}
	return pr, nil
}

// writeContext puts the context at its name, through a temporary file in the
// same directory so that a run interrupted halfway leaves no partial document
// where a complete one is expected.
func writeContext(outDir, name string, v any) (string, error) {
	tmp, err := os.CreateTemp(outDir, ".pr-context.*")
	if err != nil {
		return "", err
	}
	defer os.Remove(tmp.Name())

	if err := renderJSON(tmp, v); err != nil {
		tmp.Close()
		return "", err
	}
	if err := tmp.Close(); err != nil {
		return "", err
	}
	path := filepath.Join(outDir, name)
	if err := os.Rename(tmp.Name(), path); err != nil {
		return "", err
	}
	return path, nil
}

// contextLimits reads the three caps a caller raises when a pull request was
// cut short.
//
// They stay environment variables rather than becoming flags: the only time
// anybody sets one is to run the same command again with more room, and the
// command line belongs to the skill.
func contextLimits() (pullrequest.Limits, error) {
	limits := pullrequest.DefaultLimits
	for _, l := range []struct {
		name string
		out  *int
	}{
		{"MAX_COMMENTS", &limits.Comments},
		{"MAX_THREADS", &limits.Threads},
		{"MAX_THREAD_COMMENTS", &limits.ThreadComments},
	} {
		value := os.Getenv(l.name)
		if value == "" {
			continue
		}
		n, err := strconv.Atoi(value)
		if err != nil || n < 0 || strings.ContainsFunc(value, func(r rune) bool { return r < '0' || r > '9' }) {
			return pullrequest.Limits{}, fmt.Errorf("invalid %s: %s", l.name, value)
		}
		*l.out = n
	}
	return limits, nil
}

// prPostReviewCmd builds `ccx pr post-review`.
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

// prReplyThreadsCmd builds `ccx pr reply-threads`.
func prReplyThreadsCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "reply-threads <pr-context.json> <threads-file>",
		Short: "Reply to and resolve the review threads awaiting our confirmation",
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

			eligible, headOID, err := pullrequest.ParseEligible([]byte(context))
			if err != nil {
				return silent(fmt.Errorf("%v in %s", err, contextFile))
			}
			if err := pullrequest.RequireInWorkDir(threadsFile, "threads_path", contextFile); err != nil {
				return silent(err)
			}
			if err := pullrequest.RequireHead(c.Context(), runner.Exec{}, ".", headOID, "replying or resolving"); err != nil {
				return silent(err)
			}

			actions, err := pullrequest.ParseThreadActions([]byte(threads), threadsFile)
			if err != nil {
				return silent(err)
			}
			if err := pullrequest.ValidateThreadActions(actions, eligible, contextFile, threadsFile); err != nil {
				return silent(err)
			}
			// Nothing to do is an ordinary answer, and the one exit of this
			// command that renders indented rather than compact.
			if len(actions) == 0 {
				return silent(renderJSON(c.OutOrStdout(), pullrequest.ThreadReplies{
					Replied: []pullrequest.RepliedThread{}, Resolved: []string{},
					ResolveFailed: []pullrequest.FailedResolve{}, Warnings: []string{},
				}))
			}

			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			replies, err := pullrequest.Reply(c.Context(), client, actions, threadsFile)
			if err != nil {
				return silent(err)
			}
			return silent(renderCompactJSON(c.OutOrStdout(), replies))
		},
	}
}
