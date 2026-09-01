package cmd

import (
	"os"
	"path/filepath"

	"github.com/cli/go-gh/v2/pkg/config"
	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/reviewprs"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

// newReviewCmd builds `ccx review`, the three questions /review-assigned-prs
// asks around each pass of its loop: what to review, where to review it, and
// whether the review arrived.
func newReviewCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("review", "Review the pull requests assigned to this user")
	c.AddCommand(reviewPendingCmd(build), reviewVerifyCmd(build), reviewCloneCmd(build))
	return c
}

func reviewPendingCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "pending",
		Short: "List the pull requests waiting for this user's review",
		Long:  longFor("review pending"),
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			reportBuild(c, build)
			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			pending, err := reviewprs.ListPending(c.Context(), client)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), pending))
		},
	}
}

func reviewVerifyCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "verify <owner>/<repo>#<number>...",
		Short: "Check that this user's review reached each pull request",
		Long:  longFor("review verify"),
		Args:  cobra.MinimumNArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			specs := make([]reviewprs.Spec, 0, len(args))
			for _, arg := range args {
				s, err := reviewprs.ParseSpec(arg)
				if err != nil {
					return err
				}
				specs = append(specs, s)
			}

			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			verified, err := reviewprs.VerifyPosted(c.Context(), client, specs)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), verified))
		},
	}
}

func reviewCloneCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "clone <owner>/<repo>",
		Short: "Make a review clone of a repository available",
		Long:  longFor("review clone"),
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			repo, err := reviewprs.ParseOwnerRepo(args[0])
			if err != nil {
				return err
			}
			clone, err := reviewprs.EnsureClone(c.Context(), runner.Exec{}, cloneOptions(), repo)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), clone))
		},
	}
}

// cloneOptions reads the environment the review workspace lives in.
//
// Here rather than in the package, so that its tests can name a directory
// instead of setting a variable — t.Setenv changes the whole process and cannot
// be used from a parallel test.
func cloneOptions() reviewprs.CloneOptions {
	dataHome := os.Getenv("XDG_DATA_HOME")
	if dataHome == "" {
		home, err := os.UserHomeDir()
		if err == nil {
			dataHome = filepath.Join(home, ".local", "share")
		}
	}
	host := os.Getenv("GH_HOST")
	if host == "" {
		host = "github.com"
	}
	// go-gh's ConfigDir is a plain read of the environment, unlike its Read,
	// which memoises the parsed configuration for the life of the process.
	return reviewprs.CloneOptions{DataHome: dataHome, ConfigDir: config.ConfigDir(), Host: host}
}
