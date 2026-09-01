package statusline

import (
	"context"
	"net/http"
	"time"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
	"github.com/178inaba/dotfiles/go/internal/statusline/fxrate"
	"github.com/178inaba/dotfiles/go/internal/statusline/prinfo"
)

// The subcommand names the detached refreshes are spawned as.
//
// The shell version put the refresh in a subshell, which outlives its parent;
// a goroutine does not, so the binary re-runs itself instead. The names are
// prefixed and hidden from the command tree: they are an implementation detail
// of the redraw, not something to be run by hand, and the prefix keeps them out
// of the way of the ported scripts that will take the plain names.
const (
	RefreshFXCommandName = "internal-refresh-fx"
	RefreshPRCommandName = "internal-refresh-pr"
)

// The child's flags. The parent builds the argv and the command tree declares
// the flags, so they are named here rather than spelled out at both ends:
// renaming one otherwise compiles cleanly and silently stops a refresh that
// reports nothing by design.
const (
	FlagNow    = "now"
	FlagCache  = "cache"
	FlagKey    = "key"
	FlagBranch = "branch"
	FlagDir    = "dir"
)

const (
	flagNow    = "--" + FlagNow
	flagCache  = "--" + FlagCache
	flagKey    = "--" + FlagKey
	flagBranch = "--" + FlagBranch
	flagDir    = "--" + FlagDir
)

// RefreshFX fetches the exchange rate. The caller passes the cache directory
// and the timestamp rather than deriving them again, so that the child cannot
// disagree with the parent about which entry it is writing.
func RefreshFX(ctx context.Context, cacheDir string, now time.Time) error {
	return fxrate.Refresh(ctx, http.DefaultClient, fxrate.APIURL, cacheDir, now)
}

// RefreshPR asks GitHub about a branch's pull request. The client is a
// constructor rather than a client for the reason prinfo.Refresh documents.
func RefreshPR(ctx context.Context, r runner.Runner, cacheDir, cacheKey, branch, dir string, now time.Time) error {
	newClient := func() (*ghapi.Client, error) { return ghapi.New(ghapi.Options{}) }
	return prinfo.Refresh(ctx, r, newClient, cacheDir, cacheKey, branch, dir, now)
}
