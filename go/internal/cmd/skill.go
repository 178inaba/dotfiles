package cmd

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/skill"
)

// newSkillCmd builds `ccx skill`, the checks /skill-authoring runs over the
// SKILL.md files themselves.
func newSkillCmd(deps Deps) *cobra.Command {
	c := newParentCmd("skill", "Check the SKILL.md files a skill is defined by")
	c.AddCommand(skillFrontmatterCmd(deps), skillContractCmd(deps))
	return c
}

func skillFrontmatterCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "frontmatter [<target>]",
		Short: "Check the frontmatter of a skill directory or one SKILL.md",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			target, err := skillTarget(c.Context(), deps.Dir, args)
			if err != nil {
				return silent(err)
			}
			// Violations are not a failure of the check: the caller reads them
			// and decides. Only being unable to check at all is.
			checked, err := skill.CheckFrontmatter(target)
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), checked))
		},
	}
}

// skillTarget is the one positional argument, or the default where it was left
// out.
func skillTarget(ctx context.Context, dir string, args []string) (string, error) {
	if len(args) == 1 && args[0] != "" {
		return args[0], nil
	}
	return skillsDir(ctx, dir)
}

// skillsDir is the default target: the skills of the checkout dir belongs to,
// and the skills of the repository this configuration is stowed from where dir
// belongs to no checkout of it.
//
// The checkout first, because that is the copy being edited and the one every
// other command already acts on. The stowed repository alone was the answer
// while the two could not differ; in a linked worktree they do, and checking
// the main tree there is a pass that never looked at the file that changed.
//
// The fallback is not a lesser case: run from the home directory, where
// ~/.claude/skills is the copy meant, there is no checkout to ask.
func skillsDir(ctx context.Context, dir string) (string, error) {
	if top, err := runner.Git(ctx, runner.Exec{}, dir, "rev-parse", "--show-toplevel"); err == nil {
		skills := filepath.Join(top, "claude", ".claude", "skills")
		if info, err := os.Stat(skills); err == nil && info.IsDir() {
			return skills, nil
		}
	}

	repo, ok := selfbuild.Repo()
	if !ok {
		return "", fmt.Errorf("this repository could not be located, so there is no default target to check")
	}
	return filepath.Join(repo, "claude", ".claude", "skills"), nil
}

// skillContractCmd builds `ccx skill contract`, whose reason for existing is
// in the help this renders.
func skillContractCmd(deps Deps) *cobra.Command {
	return &cobra.Command{
		Use:   "contract [<skills-dir>]",
		Short: "Check the contract identifiers skills name",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, deps.Build)
			target, err := skillTarget(c.Context(), deps.Dir, args)
			if err != nil {
				return silent(err)
			}
			checked, err := skill.CheckContract(target, published())
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), checked))
		},
	}
}
