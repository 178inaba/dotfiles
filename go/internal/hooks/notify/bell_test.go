package notify

import (
	"testing"

	"github.com/178inaba/dotfiles/go/internal/hooks"
)

// The JSON this becomes, and the exit status that carries it, are pinned by
// the dispatcher's test; what matters here is that idle-notify and the hook
// itself ring the same bell.
func TestRing(t *testing.T) {
	t.Parallel()

	if got, want := ring().TerminalSequence, "\a"; got != want {
		t.Errorf("TerminalSequence = %q, want %q", got, want)
	}
	if got, want := NewBell().Run(t.Context(), hooks.Payload{}), (hooks.Result{Directive: ring()}); got != want {
		t.Errorf("Run() = %+v, want %+v", got, want)
	}
}
