package cmd

import (
	"fmt"
	"path/filepath"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/skill"
)

// newSkillCmd builds `ccx skill`, the checks /skill-authoring runs over the
// SKILL.md files themselves.
func newSkillCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("skill", "Check the SKILL.md files a skill is defined by")
	c.AddCommand(skillFrontmatterCmd(build))
	return c
}

func skillFrontmatterCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "frontmatter [<target>]",
		Short: "Check the frontmatter of a skill directory or one SKILL.md",
		Args:  cobra.MaximumNArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			target := ""
			if len(args) == 1 {
				target = args[0]
			}
			if target == "" {
				var err error
				if target, err = skillsDir(); err != nil {
					return silent(err)
				}
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

// skillsDir is the default target: the skills of the repository this
// configuration is stowed from.
//
// The shell walked up from its own file, which resolved to ~/.claude/skills
// through the stow symlink. A binary has no file to walk up from, so it asks
// where the repository is — the same resolution the hook has used since it
// moved to Go, and the copy worth editing either way.
func skillsDir() (string, error) {
	repo, ok := selfbuild.Repo()
	if !ok {
		return "", fmt.Errorf("this repository could not be located, so there is no default target to check")
	}
	return filepath.Join(repo, "claude", ".claude", "skills"), nil
}
