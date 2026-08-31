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
	"strconv"
	"time"

	"github.com/178inaba/dotfiles/go/internal/cache"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/statusline/fxrate"
	"github.com/178inaba/dotfiles/go/internal/statusline/gitstate"
	"github.com/178inaba/dotfiles/go/internal/statusline/prinfo"
)

const (
	// gitCacheBase names the per-directory repository caches.
	gitCacheBase = "/tmp/claude-statusline-git-cache"
	// gitMaxAge matches statusLine.refreshInterval in settings.json, which
	// keeps the redraw to at most one git invocation per cycle. Changing one
	// without the other either wastes invocations or shows stale state.
	gitMaxAge = 5 * time.Second
)

// Config carries the seams and the cache locations. Use Default for the real
// ones.
type Config struct {
	Runner  runner.Runner
	Spawner runner.Spawner
	Now     func() time.Time
	Getwd   func() (string, error)
	Home    string

	GitCacheBase string
	PRCacheBase  string
	FXCachePath  string

	// ChildEnv is what a detached refresh is started with; see
	// selfbuild.ChildEnv.
	ChildEnv []string
}

// Default returns the configuration the command runs with.
func Default() Config {
	home, _ := os.UserHomeDir()
	return Config{
		Runner:  runner.Exec{},
		Spawner: runner.Exec{},
		Now:     time.Now,
		// os.Getwd prefers $PWD when it names the same directory, which is the
		// logical path the shell reported and the one the user recognises.
		Getwd:        os.Getwd,
		Home:         home,
		GitCacheBase: gitCacheBase,
		PRCacheBase:  prinfo.CacheBase,
		FXCachePath:  fxrate.CachePath,
		ChildEnv:     selfbuild.ChildEnv(),
	}
}

// Run reads the payload and writes the status line. buildError is the first
// line of a failed self-rebuild, empty when the binary is current.
//
// It has no failure mode the caller can act on: every source of information is
// optional and a missing one simply leaves its segment out, so the command that
// wraps this always succeeds.
func Run(ctx context.Context, cfg Config, stdin io.Reader, stdout io.Writer, buildError string) error {
	payload, _ := io.ReadAll(stdin)
	fields := ParseFields(payload)
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
// one the payload named; Claude Code starts the command there.
func repository(ctx context.Context, cfg Config, current string, now time.Time) *gitstate.Status {
	path := cache.Path(cfg.GitCacheBase, current)
	if rec, ok := cache.Read[*gitstate.Status](path, current); ok && cache.Fresh(now, rec.At, gitMaxAge) {
		return rec.Value
	}

	var status *gitstate.Status
	out, err := cfg.Runner.Run(ctx, runner.Command{Name: "git", Args: gitstate.StatusArgs()})
	if err == nil {
		parsed := gitstate.Parse(out)
		status = &parsed
	}

	// Written even when there is no repository, so that a directory outside one
	// is not re-checked on every redraw. Best effort: a failed write costs one
	// git invocation next time.
	_ = cache.Write(path, current, now, status)
	return status
}

// pullRequestInfo returns the cached badge and starts a refresh when it is
// stale.
func pullRequestInfo(cfg Config, status *gitstate.Status, current string, now time.Time) *prinfo.Info {
	if status == nil || status.Branch == "" {
		// Outside a repository, or on a detached head, there is no branch to
		// have a pull request for and gh is never started.
		return nil
	}

	// Keying on the branch as well as the directory is what makes a branch
	// switch take effect at once rather than at the next expiry.
	key := current + ":" + status.Branch
	path := cache.Path(cfg.PRCacheBase, key)
	info, refresh := prinfo.Lookup(path, key, now)
	if refresh {
		spawn(cfg, RefreshPRCommandName,
			flagNow+"="+strconv.FormatInt(now.Unix(), 10),
			flagCache+"="+path, flagKey+"="+key, flagBranch+"="+status.Branch)
	}
	return &info
}

// exchangeRate returns the cached rate and starts a refresh when it is stale.
func exchangeRate(cfg Config, fields Fields, now time.Time) float64 {
	if !showsCost(fields) {
		// Nothing to convert, so nothing is fetched: a session below a cent
		// should not be starting network requests.
		return 0
	}
	rate, refresh := fxrate.Lookup(cfg.FXCachePath, now)
	if refresh {
		spawn(cfg, RefreshFXCommandName,
			flagNow+"="+strconv.FormatInt(now.Unix(), 10), flagCache+"="+cfg.FXCachePath)
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
