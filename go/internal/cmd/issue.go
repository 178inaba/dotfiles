package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/issue"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

func newIssueCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("issue", "Read GitHub issues")
	c.AddCommand(newSectionsCmd(build), newTreeCmd(build))
	return c
}

// newTreeCmd builds `ccx issue tree`, which resolves where an issue sits among
// its parent, its children and its blockers.
//
// The two annotations are flags rather than always-on because each costs a
// round trip per sub-issue, and this runs on every startup of the skills that
// read a leaf issue.
func newTreeCmd(build selfbuild.State) *cobra.Command {
	var repoName string
	var withPRs, withDeps bool
	c := &cobra.Command{
		Use:   "tree <issue-number>",
		Short: "Resolve an issue's parent, sub-issues and blockers",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			// Not silent: a number that is not one is a mistake at the
			// command line, and the usage text is the answer to it.
			number, err := issueNumber(args[0])
			if err != nil {
				return err
			}

			client, err := ghapi.New(ghapi.Options{})
			if err != nil {
				return silent(err)
			}
			repo, err := targetRepo(c.Context(), client, repoName)
			if err != nil {
				return silent(err)
			}

			t, err := issue.Tree(c.Context(), client, repo, number, issue.TreeOptions{WithPRs: withPRs, WithDeps: withDeps})
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), t))
		},
	}
	c.Flags().StringVarP(&repoName, "repo", "R", "", "repository to read the issue from, as owner/repo")
	c.Flags().BoolVar(&withPRs, "with-prs", false, "attach the pull requests closing each sub-issue")
	c.Flags().BoolVar(&withDeps, "with-deps", false, "attach each sub-issue's blockers")
	return c
}

// targetRepo is the repository a command works on: the one named on the command
// line, or the one the working directory belongs to.
func targetRepo(ctx context.Context, c *ghapi.Client, name string) (ghapi.Repo, error) {
	if name != "" {
		return ghapi.ParseRepo(name)
	}
	repo, err := c.CurrentRepo(ctx, runner.Exec{}, ".")
	if err != nil {
		return ghapi.Repo{}, fmt.Errorf("failed to resolve the current repository (run inside a GitHub repository or pass -R owner/repo): %w", err)
	}
	return repo, nil
}

// issueNumber parses the one positional argument.
//
// Digits and nothing else, which is what the shell required: Atoi alone would
// accept a leading sign, and a negative number would be read as a flag anyway.
func issueNumber(s string) (int, error) {
	if s == "" || strings.ContainsFunc(s, func(r rune) bool { return r < '0' || r > '9' }) {
		return 0, fmt.Errorf("invalid issue number: %s", s)
	}
	n, err := strconv.Atoi(s)
	if err != nil {
		return 0, fmt.Errorf("invalid issue number: %s", s)
	}
	return n, nil
}

// newSectionsCmd builds `ccx issue sections`, the schema an issue body's `## `
// headings are written against.
func newSectionsCmd(build selfbuild.State) *cobra.Command {
	c := newParentCmd("sections", "Resolve the sections of an issue body")
	c.AddCommand(sectionsSchemaCmd(build), sectionsListCmd(build), sectionsCheckCmd(build), sectionsFindCmd(build))
	return c
}

// sectionsSchemaCmd answers with one row of the table.
//
// It takes no locale because its callers are consumers: they accept a heading
// in either language, since they do not know which one the issue they are
// reading was written in.
func sectionsSchemaCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "schema <key>",
		Short: "Print one section's row of the schema",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			s, err := issue.Schema(args[0])
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), s))
		},
	}
}

// sectionsListCmd answers with the whole table for one locale and kind, which
// is what the drafting side renders from.
func sectionsListCmd(build selfbuild.State) *cobra.Command {
	var locale, kind string
	c := &cobra.Command{
		Use:   "list",
		Short: "Print every section for a locale and issue kind",
		Args:  cobra.NoArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			reportBuild(c, build)
			l, err := issue.List(issue.Locale(locale), issue.Kind(kind))
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), l))
		},
	}
	localeAndKind(c, &locale, &kind)
	return c
}

// sectionsCheckCmd is the one subcommand that answers with a status rather than
// with JSON.
//
// Its consumer needs only pass or fail, while the reasons are for a person and
// a model to read, so they go to standard error one per line and the status
// names the class. That is 178inaba/dotfiles#86 requirement 3.2, and the only
// deliberate exception to "standard output is JSON" in this command tree.
func sectionsCheckCmd(build selfbuild.State) *cobra.Command {
	var locale, kind, mappingFile string
	c := &cobra.Command{
		Use:   "check <draft-file>",
		Short: "Check a draft's headings against the schema",
		Args:  cobra.ExactArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			draft, err := readFile(args[0], "draft file")
			if err != nil {
				return silent(err)
			}

			var mapping []issue.Mapping
			if mappingFile != "" {
				content, err := readFile(mappingFile, "mapping file")
				if err != nil {
					return silent(err)
				}
				if mapping, err = issue.ParseMapping(content); err != nil {
					return silent(err)
				}
			}

			violations, err := issue.Check(draft, issue.Locale(locale), issue.Kind(kind), mapping)
			if err != nil {
				return silent(err)
			}
			for _, v := range violations {
				fmt.Fprintln(c.ErrOrStderr(), v.Message)
			}
			if class, found := issue.Worst(violations); found {
				return exitCode(checkStatus[class])
			}
			return nil
		},
	}
	localeAndKind(c, &locale, &kind)
	c.Flags().StringVar(&mappingFile, "mapping", "", "file mapping section keys to a repository template's headings")
	return c
}

func sectionsFindCmd(build selfbuild.State) *cobra.Command {
	return &cobra.Command{
		Use:   "find <file> <key>",
		Short: "Print one section of an issue body",
		Args:  cobra.ExactArgs(2),
		RunE: func(c *cobra.Command, args []string) error {
			reportBuild(c, build)
			body, err := readFile(args[0], "input file")
			if err != nil {
				return silent(err)
			}
			found, err := issue.Find(body, args[1])
			// The package reports an empty body without naming it, because the
			// path is the caller's; the shell said which file it read, and a
			// message that does not is worse at the one job it has.
			if errors.Is(err, issue.ErrEmptyInput) {
				return silent(fmt.Errorf("input file is empty: %s\nan empty body usually means the command that wrote it failed", args[0]))
			}
			// A body that does not carry the section gets its own status, so
			// that a caller can branch on it without reading the message — and
			// so that it stays distinct from the failures above, which mean the
			// body itself could not be trusted.
			if errors.Is(err, issue.ErrSectionNotFound) {
				return exitCode(6)
			}
			if err != nil {
				return silent(err)
			}
			return silent(renderJSON(c.OutOrStdout(), found))
		},
	}
}

// checkStatus is where the violation classes become process exit statuses.
//
// The numbers are the contract `ccx issue sections check` publishes and the
// package deliberately does not know them: a library function that returned 4
// would be carrying a process boundary around with it. 6 belongs to find, and
// is not in this table for that reason.
var checkStatus = map[issue.Class]int{
	issue.MissingSection:        2,
	issue.UnknownHeading:        3,
	issue.MappedMachineKey:      4,
	issue.HeadingLocaleMismatch: 5,
}

// localeAndKind adds the two flags that say which schema a draft is written
// against. Both are required, because neither has a defensible default: a wrong
// guess would report every heading as being in the wrong language.
func localeAndKind(c *cobra.Command, locale, kind *string) {
	c.Flags().StringVar(locale, "locale", "", "language of the issue body (ja or en)")
	c.Flags().StringVar(kind, "kind", "", "issue kind (leaf, sub or parent)")
	_ = c.MarkFlagRequired("locale")
	_ = c.MarkFlagRequired("kind")
}

// readFile reads an input, naming its role in the error so that a caller given
// two paths can tell which one it got wrong.
func readFile(path, role string) (string, error) {
	info, err := os.Stat(path)
	if err != nil || !info.Mode().IsRegular() {
		return "", fmt.Errorf("%s not found or not a regular file: %s", role, path)
	}
	b, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("read %s: %w", role, err)
	}
	return string(b), nil
}
