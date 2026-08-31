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
	// GitCacheBase names the per-directory repository caches.
	GitCacheBase = "/tmp/claude-statusline-git-cache"
	// gitMaxAge matches statusLine.refreshInterval in settings.json, which
	// keeps the redraw to at most one git invocation per cycle. Changing one
	// without the other either wastes invocations or shows stale state.
	gitMaxAge = 5
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
		GitCacheBase: GitCacheBase,
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
	now := cfg.Now().Unix()

	// The payload names the directory; the working directory stands in when it
	// does not. Resolved once, because the cache keys and the rendered path
	// have to agree on it.
	current := fields.CurrentDir
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
	d.Git = gitSegment(ctx, cfg, current, now)
	d.PR = pullRequestRecord(cfg, d.Git, current, now)
	d.Rate = exchangeRate(cfg, fields, now)

	_, err := stdout.Write(Render(d))
	return err
}

// gitSegment returns the repository fragment, from the cache when it is fresh.
//
// The cache is keyed by directory so that parallel sessions do not overwrite
// each other's.
func gitSegment(ctx context.Context, cfg Config, current string, now int64) string {
	path := cache.Path(cfg.GitCacheBase, current)
	if rec, ok := cache.ReadKeyed(path); ok && rec.Key == current && cache.Fresh(now, rec.At, gitMaxAge) {
		return rec.Result
	}

	segment := ""
	out, err := cfg.Runner.Run(ctx, runner.Command{Name: "git", Args: gitstate.StatusArgs})
	if err == nil {
		segment = gitstate.Parse(out).Segment()
	}

	// Written even when there is no repository, so that a directory outside one
	// is not re-checked on every redraw.
	// Best effort: a write that fails only costs one git invocation next time.
	_ = cache.WriteKeyed(path, cache.Keyed{At: now, Key: current, Result: segment})
	return segment
}

// pullRequestRecord returns the cached badge and starts a refresh when it is
// stale.
func pullRequestRecord(cfg Config, gitSegment, current string, now int64) string {
	branch := gitstate.BranchOf(gitSegment)
	if branch == "" {
		// Outside a repository, or on a detached head, there is no branch to
		// have a pull request for and gh is never started.
		return ""
	}

	key := current + ":" + branch
	path := cache.Path(cfg.PRCacheBase, key)
	record, refresh := prinfo.Lookup(path, key, now)
	if refresh {
		spawn(cfg, RefreshPRCommandName,
			flagNow+"="+strconv.FormatInt(now, 10),
			flagCache+"="+path, flagKey+"="+key, flagBranch+"="+branch)
	}
	return record
}

// exchangeRate returns the cached rate and starts a refresh when it is stale.
func exchangeRate(cfg Config, fields Fields, now int64) string {
	if !ShowsCost(fields) {
		return ""
	}
	rate, refresh := fxrate.Lookup(cfg.FXCachePath, now)
	if refresh {
		spawn(cfg, RefreshFXCommandName,
			flagNow+"="+strconv.FormatInt(now, 10), flagCache+"="+cfg.FXCachePath)
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
