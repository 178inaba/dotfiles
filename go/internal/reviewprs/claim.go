package reviewprs

import (
	"crypto/rand"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"time"

	"golang.org/x/sys/unix"

	"github.com/178inaba/dotfiles/go/internal/atomicfile"
	"github.com/178inaba/dotfiles/go/internal/filelock"
)

// A claim says that a pull request is being reviewed.
//
// GitHub keeps a pull request in the list until the review is posted, so a
// review that outlasts the loop interval comes back on the next iteration —
// of this session or of another one. The claim is what keeps it from being
// reviewed twice, and it lives on disk so that neither a compacted
// conversation nor a second session loses sight of it.

// claimTTL is how long a claim holds whatever its holder's state: the bound on
// how long a claim whose holder's pid was reused can park a pull request.
const claimTTL = time.Hour

// Holder is who takes and releases claims: the Claude Code process and session
// the command runs under.
type Holder struct {
	// PID is the Claude Code process, zero outside Claude Code. A claim with
	// no pid is judged by its age alone.
	PID int `json:"pid"`
	// SessionID is the Claude Code session, for a person reading the claim.
	SessionID string `json:"session_id"`
}

// ClaimOptions are what the environment tells the claim store.
//
// Parameters rather than reads of os.Getenv, the clock and the process table,
// so that the tests can run in parallel and name a holder that has gone.
type ClaimOptions struct {
	// StateHome is XDG_STATE_HOME, or ~/.local/state where that is unset; empty
	// where there is no home directory to build either on.
	StateHome string
	Holder    Holder
	Now       func() time.Time
	// Alive reports whether a process is still running; ProcessAlive in
	// production.
	Alive func(pid int) bool
}

// claim is one claim file.
type claim struct {
	Owner   string    `json:"owner"`
	Repo    string    `json:"repo"`
	Number  int       `json:"number"`
	Holder            // the pid and the session id
	TakenAt time.Time `json:"taken_at"`
	// Token tells this claim from any later one on the same pull request. A
	// session id cannot: the same session takes a pull request over again
	// when its own first review outlives the claim.
	Token string `json:"token"`

	// unreadable is a file that is there but does not parse. Its zero TakenAt
	// is what makes it stale; the flag is for saying so.
	unreadable bool
}

func (c claim) String() string {
	if c.unreadable {
		return "an unreadable claim"
	}
	return fmt.Sprintf("the claim of pid %d, session %q, taken at %s", c.PID, c.SessionID, c.TakenAt.Format(time.RFC3339))
}

func claimsDir(stateHome string) string {
	return filepath.Join(stateHome, "ccx", "review-claims")
}

// ClaimPath is one pull request's claim file under stateHome.
//
// A component that is empty, . or .. is refused: the path is removed on
// release, and any of them would put it outside the store.
func ClaimPath(stateHome, owner, repo string, number int) (string, error) {
	if owner == "" || repo == "" || dotComponent(owner, repo) {
		return "", fmt.Errorf("invalid repository %s/%s for a claim", owner, repo)
	}
	return filepath.Join(claimsDir(stateHome), owner, repo, strconv.Itoa(number)+".json"), nil
}

// ProcessAlive reports whether pid is a running process. A process of another
// user answers EPERM, which still means it is there.
func ProcessAlive(pid int) bool {
	if pid <= 0 {
		return false
	}
	err := unix.Kill(pid, 0)
	return err == nil || errors.Is(err, unix.EPERM)
}

// live reports whether c still keeps its pull request from being reviewed.
func (o ClaimOptions) live(c *claim) bool {
	if c == nil {
		return false
	}
	if c.PID != 0 && !o.Alive(c.PID) {
		return false
	}
	return o.Now().Sub(c.TakenAt) <= claimTTL
}

// readClaim reads the claim at path, nil where there is none. Every claim is
// published by a rename, so a file that does not parse was not written by a
// claim in progress.
func readClaim(path string) (*claim, error) {
	b, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	var c claim
	if err := json.Unmarshal(b, &c); err != nil || c.TakenAt.IsZero() {
		return &claim{unreadable: true}, nil
	}
	return &c, nil
}

// lockClaims holds the store's lock. Every write to the store — taking, taking
// over, releasing — is made under it, which is what makes the read, the
// judgement and the write one step.
func lockClaims(stateHome string) (func(), error) {
	if stateHome == "" {
		return nil, errors.New("no state directory to keep review claims in")
	}
	dir := claimsDir(stateHome)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("failed to create %s: %w", dir, err)
	}
	return filelock.Lock(filepath.Join(dir, ".lock"))
}

// lookup reads the claim on one pull request, nil where there is none. The
// error is ready to report as it is.
func (o ClaimOptions) lookup(owner, repo string, number int) (string, *claim, error) {
	path, err := ClaimPath(o.StateHome, owner, repo, number)
	if err != nil {
		return "", nil, err
	}
	c, err := readClaim(path)
	if err != nil {
		return "", nil, fmt.Errorf("failed to read the claim on %s/%s#%d: %w", owner, repo, number, err)
	}
	return path, c, nil
}

// sortClaims splits the pull requests found waiting into out.PRs and
// out.InFlight by their claims, taking a claim on each one that has no live
// claim when take is set.
//
// Without take nothing is opened for writing, the lock file included: the
// claims are only read, and the rename that publishes each one means a read
// never sees a torn file.
func (o ClaimOptions) sortClaims(found []PR, take bool, out *Pending) error {
	if take {
		release, err := lockClaims(o.StateHome)
		if err != nil {
			return err
		}
		defer release()
	} else if o.StateHome == "" {
		out.PRs = append(out.PRs, found...)
		return nil
	}

	for _, pr := range found {
		path, old, err := o.lookup(pr.Owner, pr.Repo, pr.Number)
		if err != nil {
			out.degrade(err.Error())
			continue
		}
		if o.live(old) {
			out.InFlight = append(out.InFlight, pr)
			continue
		}
		if take {
			mine := claim{
				Owner: pr.Owner, Repo: pr.Repo, Number: pr.Number,
				Holder: o.Holder, TakenAt: o.Now().UTC(), Token: rand.Text(),
			}
			if err := writeClaim(path, mine); err != nil {
				out.degrade(fmt.Sprintf("failed to claim %s: %v", pr, err))
				continue
			}
			pr.Claim = mine.Token
			if old != nil {
				out.Warnings = append(out.Warnings, fmt.Sprintf("took over %s from %s", pr, old))
			}
		}
		out.PRs = append(out.PRs, pr)
	}
	return nil
}

func writeClaim(path string, c claim) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	// Unsynced: a claim a crash empties reads as unreadable, and so stale,
	// which costs one review taken again after the crash killed the first.
	return atomicfile.WriteNoSync(path, 0o644, func(f *os.File) error {
		return json.NewEncoder(f).Encode(c)
	})
}

// Release removes the claim each spec names and returns a warning for each one
// it could not or would not remove.
//
// A claim is removed only while it is still the one named. A pull request taken
// over since — by another session, or by this one after its own claim went
// stale — carries a different claim, which is left in place. A spec that names
// no claim releases nothing, and says so when its pull request has a live
// claim: a claim whose token was lost on the way would otherwise stay until it
// went stale without anyone hearing of it.
func Release(o ClaimOptions, specs []Spec) []string {
	var named []Spec
	var warnings []string
	for _, s := range specs {
		if s.Claim != "" {
			named = append(named, s)
			continue
		}
		// Read only, as a pending without --claim reads: nothing is released
		// here, so nothing needs the lock.
		if o.StateHome == "" {
			continue
		}
		_, c, err := o.lookup(s.Owner, s.Repo, s.Number)
		switch {
		case err != nil:
			warnings = append(warnings, err.Error())
		case o.live(c):
			warnings = append(warnings, fmt.Sprintf("%s has a live claim, but no claim was named to release", s))
		}
	}
	if len(named) == 0 {
		return warnings
	}

	release, err := lockClaims(o.StateHome)
	if err != nil {
		return append(warnings, fmt.Sprintf("no claim was released: %v", err))
	}
	defer release()
	for _, s := range named {
		path, c, err := o.lookup(s.Owner, s.Repo, s.Number)
		if err != nil {
			warnings = append(warnings, err.Error())
			continue
		}
		if c == nil {
			continue
		}
		if c.Token != s.Claim {
			warnings = append(warnings, fmt.Sprintf("left %s on %s in place: it is not the claim named", c, s))
			continue
		}
		if err := os.Remove(path); err != nil {
			warnings = append(warnings, fmt.Sprintf("failed to release the claim on %s: %v", s, err))
		}
	}
	return warnings
}
