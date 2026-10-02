package reviewprs_test

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/reviewprs"
)

var (
	// claimed is the one pull request the claim tests' GitHub asks this user
	// to review.
	claimed = reviewprs.PR{Owner: "acme", Repo: "foo", Number: 100, URL: "https://github.com/acme/foo/pull/100"}
	// epoch is when the first claim in each test is taken.
	epoch = time.Date(2026, 10, 2, 9, 0, 0, 0, time.UTC)
)

// asking serves a GitHub on which claimed is waiting for this user's review.
func asking(t *testing.T) *ghapi.Client {
	t.Helper()

	_, c := serve(t, fixtures{login: "me", items: []string{hit("acme", "foo", 100, "author1")}})
	return c
}

// holder is one session that takes claims: its pid, its session id, the
// clock it reads and the pids it finds gone.
func holder(stateHome string, pid int, session string, now time.Time, dead ...int) reviewprs.ClaimOptions {
	return reviewprs.ClaimOptions{
		StateHome: stateHome,
		Holder:    reviewprs.Holder{PID: pid, SessionID: session},
		Now:       func() time.Time { return now },
		Alive:     func(pid int) bool { return !slices.Contains(dead, pid) },
	}
}

func pending(t *testing.T, c *ghapi.Client, o reviewprs.ClaimOptions, take bool) reviewprs.Pending {
	t.Helper()

	got, err := reviewprs.ListPending(t.Context(), c, o, take)
	if err != nil {
		t.Fatalf("ListPending: %v", err)
	}
	return got
}

// stripped is prs without the claims a --claim run hands out, which are
// random, so that a test can compare the rest.
func stripped(prs []reviewprs.PR) []reviewprs.PR {
	out := slices.Clone(prs)
	for i := range out {
		out[i].Claim = ""
	}
	return out
}

// tree lists every file under dir with its content, so that a test can say
// nothing was written.
func tree(t *testing.T, dir string) map[string]string {
	t.Helper()

	files := map[string]string{}
	if err := filepath.WalkDir(dir, func(path string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		b, err := os.ReadFile(path)
		files[path] = string(b)
		return err
	}); err != nil {
		t.Fatalf("walk %s: %v", dir, err)
	}
	return files
}

func TestClaimIsTakenOnce(t *testing.T) {
	t.Parallel()

	state, c := t.TempDir(), asking(t)

	first := pending(t, c, holder(state, 10, "s1", epoch), true)
	if len(first.PRs) != 1 || first.PRs[0].Claim == "" {
		t.Fatalf("first run prs = %+v, want one pull request with its claim", first.PRs)
	}
	first.PRs = stripped(first.PRs)
	if diff := cmp.Diff(reviewprs.Pending{PRs: []reviewprs.PR{claimed}, InFlight: []reviewprs.PR{}}, first); diff != "" {
		t.Errorf("first run (-want +got):\n%s", diff)
	}
	// The same session's next iteration counts too: the review it started is
	// still running.
	for _, session := range []string{"s1", "s2"} {
		again := pending(t, c, holder(state, 10, session, epoch.Add(time.Minute)), true)
		if diff := cmp.Diff(reviewprs.Pending{PRs: []reviewprs.PR{}, InFlight: []reviewprs.PR{claimed}}, again); diff != "" {
			t.Errorf("run by %s after the claim (-want +got):\n%s", session, diff)
		}
	}
}

func TestPendingWithoutClaimWritesNothing(t *testing.T) {
	t.Parallel()

	state, c := t.TempDir(), asking(t)

	unclaimed := pending(t, c, holder(state, 10, "s1", epoch), false)
	if diff := cmp.Diff(reviewprs.Pending{PRs: []reviewprs.PR{claimed}, InFlight: []reviewprs.PR{}}, unclaimed); diff != "" {
		t.Errorf("before any claim (-want +got):\n%s", diff)
	}
	if got := tree(t, state); len(got) != 0 {
		t.Errorf("a read-only run wrote %v", got)
	}

	pending(t, c, holder(state, 10, "s1", epoch), true)
	before := tree(t, state)
	got := pending(t, c, holder(state, 20, "s2", epoch.Add(time.Minute)), false)
	if diff := cmp.Diff(reviewprs.Pending{PRs: []reviewprs.PR{}, InFlight: []reviewprs.PR{claimed}}, got); diff != "" {
		t.Errorf("after a claim (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff(before, tree(t, state)); diff != "" {
		t.Errorf("a read-only run changed the claims (-before +after):\n%s", diff)
	}
}

func TestStaleClaimIsTakenOver(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		next reviewprs.ClaimOptions
	}{
		{name: "the holder is gone", next: holder("", 20, "s2", epoch.Add(time.Minute), 10)},
		{name: "the claim is older than four hours", next: holder("", 20, "s2", epoch.Add(4*time.Hour+time.Second))},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			state, c := t.TempDir(), asking(t)
			pending(t, c, holder(state, 10, "s1", epoch), true)
			tc.next.StateHome = state

			// A read-only run already counts a stale claim as none.
			if got := pending(t, c, tc.next, false); !slices.Equal(got.PRs, []reviewprs.PR{claimed}) {
				t.Errorf("read-only run = %+v, want the pull request in prs", got)
			}

			got := pending(t, c, tc.next, true)
			if !slices.Equal(stripped(got.PRs), []reviewprs.PR{claimed}) || len(got.InFlight) != 0 {
				t.Errorf("ListPending = %+v, want the pull request taken over", got)
			}
			if len(got.Warnings) != 1 || !strings.Contains(got.Warnings[0], "acme/foo#100") {
				t.Errorf("warnings = %q, want one line naming acme/foo#100", got.Warnings)
			}
			if got.Degraded {
				t.Error("a takeover degraded the run")
			}

			// The new claim is the second session's.
			third := pending(t, c, holder(state, 30, "s3", tc.next.Now(), 10), true)
			if !slices.Equal(third.InFlight, []reviewprs.PR{claimed}) {
				t.Errorf("third run = %+v, want the taken-over claim live", third)
			}
		})
	}
}

func TestUnreadableClaimIsTakenOver(t *testing.T) {
	t.Parallel()

	state, c := t.TempDir(), asking(t)
	path, err := reviewprs.ClaimPath(state, "acme", "foo", 100)
	if err != nil {
		t.Fatalf("ClaimPath: %v", err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("not json"), 0o644); err != nil {
		t.Fatal(err)
	}

	got := pending(t, c, holder(state, 10, "s1", epoch), true)
	if !slices.Equal(stripped(got.PRs), []reviewprs.PR{claimed}) || len(got.Warnings) != 1 {
		t.Errorf("ListPending = %+v, want the pull request taken over with one warning", got)
	}
}

// TestRacingRunsClaimOnce runs several sessions' --claim at once over the same
// pull request, unclaimed and with a stale claim: exactly one of them gets it.
func TestRacingRunsClaimOnce(t *testing.T) {
	t.Parallel()

	for _, stale := range []bool{false, true} {
		t.Run(map[bool]string{false: "unclaimed", true: "stale"}[stale], func(t *testing.T) {
			t.Parallel()

			state, c := t.TempDir(), asking(t)
			if stale {
				pending(t, c, holder(state, 10, "gone", epoch), true)
			}

			const racers = 8
			var (
				wg       sync.WaitGroup
				mu       sync.Mutex
				prs      int
				inFlight int
			)
			// One client per racer, as each run is a process of its own.
			clients := make([]*ghapi.Client, racers)
			for i := range clients {
				clients[i] = asking(t)
			}
			for i, c := range clients {
				wg.Go(func() {
					// Only the holder of the stale claim is gone; a racer that
					// took it over is alive to the others.
					o := holder(state, 100+i, "racer", epoch.Add(time.Minute), 10)
					got, err := reviewprs.ListPending(t.Context(), c, o, true)
					if err != nil {
						t.Errorf("ListPending: %v", err)
						return
					}
					mu.Lock()
					prs += len(got.PRs)
					inFlight += len(got.InFlight)
					mu.Unlock()
				})
			}
			wg.Wait()

			if prs != 1 || inFlight != racers-1 {
				t.Errorf("%d runs listed it in prs and %d in in_flight, want 1 and %d", prs, inFlight, racers-1)
			}
		})
	}
}

func TestClaimWithoutAStateDirectory(t *testing.T) {
	t.Parallel()

	c := asking(t)
	if got, err := reviewprs.ListPending(t.Context(), c, holder("", 10, "s1", epoch), true); err == nil {
		t.Errorf("ListPending --claim = %+v, want a failure with nowhere to keep claims", got)
	}
	got := pending(t, c, holder("", 10, "s1", epoch), false)
	if diff := cmp.Diff(reviewprs.Pending{PRs: []reviewprs.PR{claimed}, InFlight: []reviewprs.PR{}}, got); diff != "" {
		t.Errorf("read-only run (-want +got):\n%s", diff)
	}
}

func TestRelease(t *testing.T) {
	t.Parallel()

	// take claims claimed and returns the spec that names the claim it got.
	take := func(t *testing.T, c *ghapi.Client, o reviewprs.ClaimOptions) reviewprs.Spec {
		t.Helper()

		got := pending(t, c, o, true)
		if len(got.PRs) != 1 {
			t.Fatalf("ListPending = %+v, want the pull request claimed", got)
		}
		return reviewprs.Spec{Owner: "acme", Repo: "foo", Number: 100, Claim: got.PRs[0].Claim}
	}
	unclaimedNow := func(t *testing.T, c *ghapi.Client, state string, at time.Time) bool {
		t.Helper()
		return len(pending(t, c, holder(state, 99, "reader", at), false).PRs) == 1
	}

	t.Run("the claim named is removed", func(t *testing.T) {
		t.Parallel()

		state, c := t.TempDir(), asking(t)
		spec := take(t, c, holder(state, 10, "s1", epoch))

		if got := reviewprs.Release(holder(state, 10, "s1", epoch), []reviewprs.Spec{spec}); len(got) != 0 {
			t.Errorf("Release warnings = %q, want none", got)
		}
		if !unclaimedNow(t, c, state, epoch) {
			t.Error("the pull request is still claimed after the release")
		}
	})

	// The same session's first review outlives its claim, a later iteration
	// takes the pull request over, and then the first review finishes: its
	// release must not end the second review's claim.
	t.Run("a claim taken over since is kept", func(t *testing.T) {
		t.Parallel()

		state, c := t.TempDir(), asking(t)
		first := take(t, c, holder(state, 10, "s1", epoch))
		later := epoch.Add(4*time.Hour + time.Minute)
		take(t, c, holder(state, 10, "s1", later))

		got := reviewprs.Release(holder(state, 10, "s1", later), []reviewprs.Spec{first})
		if len(got) != 1 || !strings.Contains(got[0], "acme/foo#100") {
			t.Errorf("Release warnings = %q, want one line naming acme/foo#100", got)
		}
		if unclaimedNow(t, c, state, later) {
			t.Error("the release removed the claim taken over since")
		}
	})

	t.Run("a spec naming no claim releases nothing", func(t *testing.T) {
		t.Parallel()

		state, c := t.TempDir(), asking(t)
		take(t, c, holder(state, 10, "s1", epoch))

		// The token was lost on the way, which a warning makes visible.
		bare := reviewprs.Spec{Owner: "acme", Repo: "foo", Number: 100}
		if got := reviewprs.Release(holder(state, 10, "s1", epoch), []reviewprs.Spec{bare}); len(got) != 1 || !strings.Contains(got[0], "acme/foo#100") {
			t.Errorf("Release warnings = %q, want one line naming acme/foo#100", got)
		}
		if unclaimedNow(t, c, state, epoch) {
			t.Error("a spec naming no claim released one")
		}
	})

	t.Run("no claim is nothing to release", func(t *testing.T) {
		t.Parallel()

		for _, claim := range []string{"x", ""} {
			state := t.TempDir()
			spec := reviewprs.Spec{Owner: "acme", Repo: "foo", Number: 100, Claim: claim}
			if got := reviewprs.Release(holder(state, 10, "s1", epoch), []reviewprs.Spec{spec}); len(got) != 0 {
				t.Errorf("Release(%v) warnings = %q, want none", spec, got)
			}
			// Naming no claim only reads, as pending without --claim does.
			if got := tree(t, state); claim == "" && len(got) != 0 {
				t.Errorf("a release naming no claim wrote %v", got)
			}
		}
	})

	t.Run("a claim that cannot be read is reported", func(t *testing.T) {
		t.Parallel()

		state := t.TempDir()
		path, err := reviewprs.ClaimPath(state, "acme", "foo", 100)
		if err != nil {
			t.Fatalf("ClaimPath: %v", err)
		}
		// A directory where the file goes makes the read fail every time.
		if err := os.MkdirAll(path, 0o755); err != nil {
			t.Fatal(err)
		}
		spec := reviewprs.Spec{Owner: "acme", Repo: "foo", Number: 100, Claim: "x"}
		if got := reviewprs.Release(holder(state, 10, "s1", epoch), []reviewprs.Spec{spec}); len(got) != 1 {
			t.Errorf("Release warnings = %q, want one line", got)
		}
	})

	t.Run("without a state directory", func(t *testing.T) {
		t.Parallel()

		spec := reviewprs.Spec{Owner: "acme", Repo: "foo", Number: 100, Claim: "x"}
		if got := reviewprs.Release(holder("", 10, "s1", epoch), []reviewprs.Spec{spec}); len(got) != 1 {
			t.Errorf("Release warnings = %q, want one line", got)
		}
	})
}

// TestUnreadableClaimDegrades is a claim the run cannot read at all: the pull
// request is left out of both lists rather than guessed at.
func TestUnreadableClaimDegrades(t *testing.T) {
	t.Parallel()

	state, c := t.TempDir(), asking(t)
	path, err := reviewprs.ClaimPath(state, "acme", "foo", 100)
	if err != nil {
		t.Fatalf("ClaimPath: %v", err)
	}
	if err := os.MkdirAll(path, 0o755); err != nil {
		t.Fatal(err)
	}
	for _, take := range []bool{false, true} {
		got := pending(t, c, holder(state, 10, "s1", epoch), take)
		if len(got.PRs) != 0 || len(got.InFlight) != 0 || !got.Degraded || len(got.Warnings) != 1 {
			t.Errorf("ListPending(take=%v) = %+v, want it left out with one warning", take, got)
		}
	}
}

func TestClaimPathRejectsDotComponents(t *testing.T) {
	t.Parallel()

	for _, tc := range [][2]string{{"..", "foo"}, {"acme", "."}} {
		if got, err := reviewprs.ClaimPath(t.TempDir(), tc[0], tc[1], 1); err == nil {
			t.Errorf("ClaimPath(%q, %q) = %q, want a refusal", tc[0], tc[1], got)
		}
	}
}

func TestProcessAlive(t *testing.T) {
	t.Parallel()

	if !reviewprs.ProcessAlive(os.Getpid()) {
		t.Error("this process reads as gone")
	}
	if reviewprs.ProcessAlive(0) {
		t.Error("pid 0 reads as alive")
	}
}
