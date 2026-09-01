package reviewprs

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"regexp"

	"github.com/goccy/go-yaml"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// workspace is the directory the review clones live under.
//
// Away from wherever the user keeps their own checkout, so that a worktree
// created for a review never appears in the repository they are working in.
const workspace = "claude-review-prs"

// OwnerRepo is a repository named as exactly owner/repo.
//
// Deliberately not ghapi.Repo, which also accepts a host and a url: this one
// becomes two path components under the review workspace, and a component that
// is . or .. would put the clone — and the removal that cleans up after a
// failed one — outside it.
type OwnerRepo struct {
	Owner string
	Name  string
}

func (r OwnerRepo) String() string { return r.Owner + "/" + r.Name }

// ownerRepoPattern is one slash and nothing else.
var ownerRepoPattern = regexp.MustCompile(`^([^/]+)/([^/]+)$`)

// ParseOwnerRepo reads an owner/repo argument.
func ParseOwnerRepo(s string) (OwnerRepo, error) {
	m := ownerRepoPattern.FindStringSubmatch(s)
	if m == nil {
		return OwnerRepo{}, fmt.Errorf("invalid repo reference (expected <owner>/<repo>): %s", s)
	}
	for _, part := range m[1:] {
		if part == "." || part == ".." {
			return OwnerRepo{}, fmt.Errorf("invalid repo reference (dot components not allowed): %s", s)
		}
	}
	return OwnerRepo{Owner: m[1], Name: m[2]}, nil
}

// CloneOptions are what the environment tells EnsureClone.
//
// Parameters rather than reads of os.Getenv, so that the tests for this can run
// in parallel: t.Setenv changes the whole process and forbids it.
type CloneOptions struct {
	// DataHome is XDG_DATA_HOME, or ~/.local/share where that is unset.
	DataHome string
	// ConfigDir is gh's configuration directory, which is where the choice
	// between ssh and https comes from.
	ConfigDir string
	// Host is the GitHub host to clone from.
	Host string
}

// Clone is where a repository has been made available for review.
type Clone struct {
	// The absolute path of the clone. Use this rather than composing the path
	// from the workspace layout, which is this command's to decide.
	Path string `json:"path"`
}

// EnsureClone makes a review clone of repo available and returns its path,
// fetching into one that is already there.
//
// Safe to run for the same repository at the same time, which happens whenever
// two subagents review two pull requests of one repository. There is no lock:
// the clone is completed in a hidden temporary directory beside its
// destination and moved into place in one step, so the destination only ever
// holds a finished clone, and whoever loses the race adopts the winner's rather
// than replacing it. macOS has no flock(1), and a lock built out of mkdir plus
// stale detection would cost more than the one wasted clone it saves.
func EnsureClone(ctx context.Context, r runner.Runner, o CloneOptions, repo OwnerRepo) (Clone, error) {
	parent := filepath.Join(o.DataHome, workspace, repo.Owner)
	path := filepath.Join(parent, repo.Name)

	if isRepo(path) {
		if _, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"-C", path, "fetch", "--prune"}}); err != nil {
			return Clone{}, fmt.Errorf("failed to fetch %s", repo)
		}
		return Clone{Path: path}, nil
	}

	if err := os.MkdirAll(parent, 0o755); err != nil {
		return Clone{}, fmt.Errorf("failed to create parent dir %s", parent)
	}
	// A directory without a .git is what an interrupted clone of the previous
	// implementation, which cloned straight into the destination, left behind.
	// Nothing writes a partial clone there any more, so the missing .git is
	// enough to call it debris.
	if _, err := os.Stat(path); err == nil {
		if err := os.RemoveAll(path); err != nil {
			return Clone{}, fmt.Errorf("failed to remove %s: %w", path, err)
		}
	}

	url, err := cloneURL(o, repo)
	if err != nil {
		return Clone{}, err
	}
	tmp, err := os.MkdirTemp(parent, "."+repo.Name+".")
	if err != nil {
		return Clone{}, fmt.Errorf("failed to create a temporary directory in %s: %w", parent, err)
	}
	defer os.RemoveAll(tmp)

	// git rather than `gh repo clone`, which only added the url this builds.
	// MkdirTemp has already created the directory, and git declines to clone
	// into one that exists unless it is empty — which this one is.
	if _, err := r.Run(ctx, runner.Command{Name: "git", Args: []string{"clone", url, tmp}}); err != nil {
		// Somebody else may have published while this was cloning, in which
		// case there is nothing left to do but use theirs.
		if isRepo(path) {
			return Clone{Path: path}, nil
		}
		return Clone{}, fmt.Errorf("failed to clone %s", repo)
	}

	// Rename refuses a destination that is a non-empty directory, so a clone
	// published in the meantime survives this rather than being moved into.
	if err := os.Rename(tmp, path); err != nil && !isRepo(path) {
		return Clone{}, fmt.Errorf("failed to publish clone for %s", repo)
	}
	return Clone{Path: path}, nil
}

// isRepo reports whether path holds a git repository, which is the only signal
// that distinguishes a finished clone from anything else at that path.
func isRepo(path string) bool {
	info, err := os.Stat(filepath.Join(path, ".git"))
	return err == nil && info.IsDir()
}

// cloneURL builds the url to clone from, honouring the protocol gh is
// configured with — this machine's remotes are ssh, and cloning over https
// would leave the review clone unlike every other one on it.
func cloneURL(o CloneOptions, repo OwnerRepo) (string, error) {
	protocol, err := gitProtocol(o.ConfigDir, o.Host)
	if err != nil {
		return "", err
	}
	if protocol == "ssh" {
		return fmt.Sprintf("git@%s:%s/%s.git", o.Host, repo.Owner, repo.Name), nil
	}
	return fmt.Sprintf("https://%s/%s/%s.git", o.Host, repo.Owner, repo.Name), nil
}

// gitProtocol reads gh's git_protocol for host: the per-host setting in
// hosts.yml, then the global one in config.yml, then gh's own default.
//
// Read here rather than through go-gh's config package, which memoises the
// answer in a package-level variable behind a sync.Once and takes its directory
// from the process environment. Inside a test binary the first call would win
// for every later one, and no t.Setenv could correct it.
func gitProtocol(dir, host string) (string, error) {
	var hosts map[string]struct {
		GitProtocol string `yaml:"git_protocol"`
	}
	if err := readYAML(filepath.Join(dir, "hosts.yml"), &hosts); err != nil {
		return "", err
	}
	if p := hosts[host].GitProtocol; p != "" {
		return p, nil
	}

	var config struct {
		GitProtocol string `yaml:"git_protocol"`
	}
	if err := readYAML(filepath.Join(dir, "config.yml"), &config); err != nil {
		return "", err
	}
	if config.GitProtocol != "" {
		return config.GitProtocol, nil
	}
	return "https", nil
}

// readYAML decodes a gh configuration file, treating one that is not there as
// one that says nothing — which is what gh does with a fresh installation.
//
// The two files are read separately rather than merged, because go-gh's own
// merge takes a single file and this needs both.
func readYAML(path string, out any) error {
	b, err := os.ReadFile(path)
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("read %s: %w", path, err)
	}
	if err := yaml.Unmarshal(b, out); err != nil {
		return fmt.Errorf("parse %s: %w", path, err)
	}
	return nil
}
