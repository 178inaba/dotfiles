package statusline

import (
	"context"
	"net/http"

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

// RefreshFX fetches the exchange rate. The caller passes the cache path and the
// timestamp rather than deriving them again, so that the child cannot disagree
// with the parent about which file it is writing.
func RefreshFX(ctx context.Context, cachePath string, now int64) {
	fxrate.Refresh(ctx, http.DefaultClient, fxrate.APIURL, cachePath, now)
}

// RefreshPR asks gh about a branch's pull request.
//
// The working directory is inherited rather than set: gh and git both resolve
// the repository from it, and the parent was already standing in the right one.
func RefreshPR(ctx context.Context, cfg Config, cachePath, cacheKey, branch string, now int64) {
	prinfo.Refresh(ctx, cfg.Runner, cachePath, cacheKey, branch, now)
}
