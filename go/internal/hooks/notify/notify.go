// Package notify tells the user that a session is waiting for them, and only
// when it is a human that is waited on.
//
// Four hook registrations, one mechanism. The tracker records which subagents
// are running, and idle-notify reads those markers to tell a session waiting
// for a person from one waiting for an agent it started; when it does notify,
// the sound, the bell and the Slack post are the ways it reaches them. The
// bell and Slack are hooks in their own right as well, because other events
// want one without the whole decision.
package notify

import (
	"net/http"
	"os"

	"github.com/178inaba/dotfiles/go/internal/hooks/state"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Deps are the seams of the whole mechanism. Use Default for the real ones.
//
// One set for the package rather than one per hook: the entry points are four
// views of a single thing, and a seam belongs to what it cuts through.
type Deps struct {
	// Dir is the state tree the markers live in; see state.Dir.
	Dir string
	// Sound plays the notification sound.
	Sound runner.Detacher
	// Client posts to the Slack webhook.
	Client *http.Client
	// Runner asks git which project a notification came from, and ps whether a
	// marker's process is still there.
	Runner runner.Runner
	// Signaller answers whether a recorded pid is alive.
	Signaller runner.Signaller
	// Getenv reads the webhook out of the environment.
	Getenv func(string) string
	// Getppid names the process a marker records.
	Getppid func() int
}

// Default wires the real implementations.
func Default() Deps {
	return Deps{
		Dir:       state.Dir,
		Sound:     runner.Exec{},
		Client:    http.DefaultClient,
		Runner:    runner.Exec{},
		Signaller: runner.Exec{},
		Getenv:    os.Getenv,
		Getppid:   os.Getppid,
	}
}
