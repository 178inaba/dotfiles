package ghapi

import (
	"context"
	"errors"
	"fmt"
	"net/url"
	"strings"

	"github.com/cli/go-gh/v2/pkg/repository"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Repo names a GitHub repository.
//
// It carries no host. Every command in this tree talks to the single host the
// Client resolved, and a second copy of that on every value could only ever
// disagree with it.
type Repo struct {
	Owner string
	Name  string
}

// String returns the owner/name form, which is what -R accepts and what the
// output contracts print.
func (r Repo) String() string { return r.Owner + "/" + r.Name }

// ParseRepo reads a repository from any form gh takes for -R: OWNER/REPO,
// HOST/OWNER/REPO, or a URL — including the scp-like git@host:owner/repo a git
// remote carries once ~/.gitconfig has rewritten an https one.
func ParseRepo(s string) (Repo, error) {
	// The host argument is only consulted for the bare OWNER/REPO form, and
	// go-gh reads the user's config to supply one when it is not given. Empty
	// is deliberate: nothing here keeps a host, and asking go-gh for the
	// default would send a test to ~/.config/gh.
	r, err := repository.ParseWithHost(s, "")
	if err != nil {
		return Repo{}, fmt.Errorf("parse repository %q: %w", s, err)
	}
	return Repo{Owner: r.Owner, Name: r.Name}, nil
}

// RepoFromAPIURL reads a repository out of the api url GitHub puts on an issue
// or a pull request as repository_url.
//
// The object itself is what carries it: the repository object is not required
// by the issue schema, but this url is, which is why it is what the two
// commands reading cross-repository references go by.
func RepoFromAPIURL(apiURL string) (Repo, error) {
	u, err := url.Parse(apiURL)
	if err != nil {
		return Repo{}, fmt.Errorf("parse repository url %q: %w", apiURL, err)
	}
	parts := strings.Split(strings.Trim(u.Path, "/"), "/")
	if len(parts) < 2 {
		return Repo{}, fmt.Errorf("not a repository url: %s", apiURL)
	}
	return Repo{Owner: parts[len(parts)-2], Name: parts[len(parts)-1]}, nil
}

// CurrentRepo names the repository dir belongs to.
//
// This is the `gh repo view --json nameWithOwner` that three of the ported
// commands opened with. gh answers it from the git remotes and so does this;
// repository.Current would too, but it runs git through go-gh's own safeexec,
// which no injected Runner can reach — a test built on it would read the
// developer's checkout and pass.
//
// The remote supplies a candidate and GitHub supplies the answer, so a remote
// written in the wrong case or naming a repository that has since been renamed
// still resolves to the name the API uses. Every later round trip in a command
// is built from that name, so it has to be the one the API agrees with.
func (c *Client) CurrentRepo(ctx context.Context, r runner.Runner, dir string) (Repo, error) {
	candidate, err := RemoteRepo(ctx, r, dir)
	if err != nil {
		return Repo{}, err
	}

	var out struct {
		Name  string `json:"name"`
		Owner struct {
			Login string `json:"login"`
		} `json:"owner"`
	}
	if err := c.Get(ctx, "repos/"+candidate.Owner+"/"+candidate.Name, &out); err != nil {
		return Repo{}, fmt.Errorf("look up %s: %w", candidate, err)
	}
	return Repo{Owner: out.Owner.Login, Name: out.Name}, nil
}

// RemoteRepo picks the remote that names the repository a command is about.
//
// Whether a remote points at GitHub at all is left to the lookup that follows:
// a host this module cannot reach fails there anyway, and deciding it here
// would mean keeping a second opinion about which host that is.
//
// This is what CurrentRepo asks before it canonicalises, and it is exported for
// callers that do not need the canonical name — GitHub answers a miscased or
// since-renamed one anyway. A caller whose later requests or output are built
// from the name wants CurrentRepo instead.
func RemoteRepo(ctx context.Context, r runner.Runner, dir string) (Repo, error) {
	out, err := runner.Git(ctx, r, dir, "remote", "-v")
	if err != nil {
		return Repo{}, fmt.Errorf("list the git remotes: %w", err)
	}

	best, bestRank := Repo{}, -1
	seen := make(map[string]bool)
	for line := range strings.SplitSeq(out, "\n") {
		// Two lines per remote, fetch before push; only the first is read
		// because the two differ only where a repository is pushed somewhere
		// other than it is read from, and it is the source that identifies it.
		name, rest, ok := strings.Cut(line, "\t")
		if !ok || seen[name] {
			continue
		}
		seen[name] = true

		url, _, _ := strings.Cut(rest, " ")
		repo, err := ParseRepo(url)
		if err != nil {
			continue
		}
		// Strictly greater, so that remotes of equal standing keep the order
		// git listed them in.
		if rank := remoteRank(name); rank > bestRank {
			best, bestRank = repo, rank
		}
	}
	if bestRank < 0 {
		return Repo{}, errors.New("no git remote names a repository")
	}
	return best, nil
}

// remoteRank orders remotes the way gh orders them: the repository a fork was
// made from before the fork itself, and any name gh does not recognise last.
func remoteRank(name string) int {
	switch name {
	case "upstream":
		return 3
	case "github":
		return 2
	case "origin":
		return 1
	default:
		return 0
	}
}

// DefaultBranch returns a repository's default branch.
//
// Callers ask git first and come here only where origin/HEAD is missing, which
// a clone sets and only a repository whose remote was added by hand lacks.
func (c *Client) DefaultBranch(ctx context.Context, repo Repo) (string, error) {
	var out struct {
		DefaultBranch string `json:"default_branch"`
	}
	if err := c.Get(ctx, "repos/"+repo.Owner+"/"+repo.Name, &out); err != nil {
		return "", fmt.Errorf("look up the default branch of %s: %w", repo, err)
	}
	return out.DefaultBranch, nil
}
