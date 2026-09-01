// Package statusline renders the status line Claude Code draws under the
// prompt.
//
// It is redrawn every five seconds in every open session, so nothing here waits
// on anything slow. git runs behind a five-second cache; the exchange rate and
// the pull request badge are served from their caches and refreshed by a
// detached child, so a redraw costs at most one git invocation and usually not
// even that.
package statusline

import (
	"context"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"time"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/statusline/fxrate"
	"github.com/178inaba/dotfiles/go/internal/statusline/gitstate"
	"github.com/178inaba/dotfiles/go/internal/statusline/payload"
	"github.com/178inaba/dotfiles/go/internal/statusline/prinfo"
)

// gitMaxAge matches statusLine.refreshInterval in settings.json, which keeps
// the redraw to at most one git invocation per cycle. Changing one without the
// other either wastes invocations or shows stale state.
const gitMaxAge = 5 * time.Second

// Config carries the seams and the cache locations. Use Default for the real
// ones.
type Config struct {
	Runner  runner.Runner
	Spawner runner.Spawner
	Now     func() time.Time
	Getwd   func() (string, error)
	Home    string

	// The cache directories, one per kind of state.
	GitCacheDir string
	PRCacheDir  string
	FXCacheDir  string

	// ChildEnv is what a detached refresh is started with; see
	// selfbuild.ChildEnv.
	ChildEnv []string
}

// Default returns the configuration the command runs with.
func Default() Config {
	home, _ := os.UserHomeDir()
	// Every status line cache lives under one directory, so that clearing the
	// lot is removing one tree.
	root := filepath.Join(cache.Dir(), "statusline")
	return Config{
		Runner:  runner.Exec{},
		Spawner: runner.Exec{},
		Now:     time.Now,
		// os.Getwd prefers $PWD when it names the same directory, which is the
		// logical path the shell reported and the one the user recognises.
		Getwd:       os.Getwd,
		Home:        home,
		GitCacheDir: filepath.Join(root, "git"),
		PRCacheDir:  filepath.Join(root, "pr"),
		FXCacheDir:  filepath.Join(root, fxrate.CacheName),
		ChildEnv:    selfbuild.ChildEnv(),
	}
}

// Run reads the payload and writes the status line. buildError is the first
// line of a failed self-rebuild, empty when the binary is current.
//
// It has no failure mode the caller can act on: every source of information is
// optional and a missing one simply leaves its segment out, so the command that
// wraps this always succeeds.
func Run(ctx context.Context, cfg Config, stdin io.Reader, stdout io.Writer, buildError string) error {
	in, _ := io.ReadAll(stdin)
	fields := payload.Parse(in)
	now := cfg.Now()

	// The payload names the directory; the working directory stands in when it
	// does not. Resolved once, because the cache keys and the rendered path
	// have to agree on it.
	current := fields.Workspace.CurrentDir
	if current == "" {
		current, _ = cfg.Getwd()
	}

	d := Data{
		Fields:     fields,
		Home:       cfg.Home,
		Current:    current,
		Now:        now,
		BuildError: buildError,
	}
	d.Git = repository(ctx, cfg, current, now)
	d.PR = pullRequestInfo(cfg, d.Git, current, now)
	d.Rate = exchangeRate(cfg, fields, now)

	_, err := stdout.Write(Render(d))
	return err
}

// repository returns the state of the repository the status line is standing
// in, or nil when it is not in one.
//
// The cache is keyed by directory so that parallel sessions do not overwrite
// each other's. git runs in the process working directory rather than in the
// one the payload named, which holds only because Claude Code starts the
// command there. The refresh child is told the directory outright, so the two
// can disagree; #133 reconciles them here.
func repository(ctx context.Context, cfg Config, current string, now time.Time) *gitstate.Status {
	dir := cache.Path(cfg.GitCacheDir, current)
	if rec, ok := cache.Read[*gitstate.Status](dir, current); ok && cache.Fresh(now, rec.At, gitMaxAge) {
		return rec.Value
	}

	var status *gitstate.Status
	out, err := cfg.Runner.Run(ctx, runner.Command{Name: "git", Args: gitstate.StatusArgs()})
	if err == nil {
		parsed := gitstate.Parse(string(out))
		status = &parsed
	}

	// Written even when there is no repository, so that a directory outside one
	// is not re-checked on every redraw. Best effort: a failed write costs one
	// git invocation next time.
	_ = cache.Write(dir, current, now, status)
	return status
}

// pullRequestInfo returns the cached badge and starts a refresh when it is
// stale.
func pullRequestInfo(cfg Config, status *gitstate.Status, current string, now time.Time) *prinfo.Info {
	if status == nil || status.Branch == "" {
		// Outside a repository, or on a detached head, there is no branch to
		// have a pull request for and no refresh is started.
		return nil
	}

	// Keying on the branch as well as the directory is what makes a branch
	// switch take effect at once rather than at the next expiry.
	key := current + ":" + status.Branch
	dir := cache.Path(cfg.PRCacheDir, current, status.Branch)
	info, refresh := prinfo.Lookup(dir, key, now)
	if refresh {
		spawn(cfg, RefreshPRCommandName,
			flagNow+"="+strconv.FormatInt(now.Unix(), 10),
			flagCache+"="+dir, flagKey+"="+key, flagBranch+"="+status.Branch,
			flagDir+"="+current)
	}
	return &info
}

// exchangeRate returns the cached rate and starts a refresh when it is stale.
func exchangeRate(cfg Config, fields payload.Fields, now time.Time) float64 {
	if !showsCost(fields) {
		// Nothing to convert, so nothing is fetched: a session below a cent
		// should not be starting network requests.
		return 0
	}
	rate, refresh := fxrate.Lookup(cfg.FXCacheDir, now)
	if refresh {
		spawn(cfg, RefreshFXCommandName,
			flagNow+"="+strconv.FormatInt(now.Unix(), 10), flagCache+"="+cfg.FXCacheDir)
	}
	return rate
}

// spawn starts a refresh and forgets about it. A failure to start is not worth
// reporting: the segment renders from the cache either way.
func spawn(cfg Config, args ...string) {
	if cfg.Spawner == nil {
		return
	}
	_ = cfg.Spawner.Spawn(cfg.ChildEnv, args...)
}
