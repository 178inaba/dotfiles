// Package gitstate reads and renders the repository segment of the status
// line.
//
// One `git status --porcelain=v2 --branch` supplies the branch name, the
// ahead/behind counts and the staged and unstaged totals together. The shell
// version started as six separate git invocations and was collapsed into this
// one because the status line redraws every five seconds.
package gitstate

import (
	"strconv"
	"strings"
)

// StatusArgs is the git invocation this package parses. It is exported so that
// the caller running it and the tests checking the parser against real git
// cannot drift apart from each other.
//
// One report carries the branch, the ahead and behind counts and both change
// totals; the shell version started as six separate git invocations and was
// collapsed into this because the status line redraws every five seconds.
var StatusArgs = []string{"--no-optional-locks", "status", "--porcelain=v2", "--branch"}

// Status is what a single porcelain v2 report said.
type Status struct {
	// Branch is empty on a detached head, which is also how the report reads.
	Branch string
	// HasUpstream is whether the branch tracks anything. git prints the
	// upstream and the ahead/behind counts as separate records, so a pruned
	// remote reference leaves this true with both counts at zero.
	HasUpstream bool
	Ahead       int
	Behind      int
	Staged      int
	Modified    int
}

// Parse reads a porcelain v2 report.
func Parse(out []byte) Status {
	var s Status
	for line := range strings.SplitSeq(string(out), "\n") {
		switch {
		case strings.HasPrefix(line, "# branch.head "):
			s.Branch = strings.TrimPrefix(line, "# branch.head ")
			if s.Branch == "(detached)" {
				s.Branch = ""
			}
		case strings.HasPrefix(line, "# branch.upstream "):
			// Only the presence matters; which remote branch it is never
			// reaches the display.
			s.HasUpstream = true
		case strings.HasPrefix(line, "# branch.ab "):
			ahead, behind, _ := strings.Cut(strings.TrimPrefix(line, "# branch.ab "), " ")
			s.Ahead = count(strings.TrimPrefix(ahead, "+"))
			s.Behind = count(strings.TrimPrefix(behind, "-"))
		case strings.HasPrefix(line, "1"), strings.HasPrefix(line, "2"), strings.HasPrefix(line, "u"):
			// 1 is a change, 2 a rename and u a conflict. The two letters after
			// the record type are the staged and unstaged states, and a dot
			// means unchanged — so a conflict, whose code is UU, counts on both
			// sides. A line too short to hold them counts on both sides too,
			// which shows a malformed record rather than hiding it.
			staged, modified := states(line)
			if staged != '.' {
				s.Staged++
			}
			if modified != '.' {
				s.Modified++
			}
		}
	}
	return s
}

// states returns the two status letters of a record, using a rune that is not a
// dot when the line is too short to hold them.
func states(line string) (byte, byte) {
	const missing = ' '
	staged, modified := byte(missing), byte(missing)
	if len(line) > 2 {
		staged = line[2]
	}
	if len(line) > 3 {
		modified = line[3]
	}
	return staged, modified
}

func count(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		return 0
	}
	return n
}

// Segment renders the repository fragment, uncolored and with its leading
// space: " (main +1 ~1 ↑1)".
//
// This exact string is what the cache holds, which is why the colouring belongs
// to the caller and why BranchOf can recover the branch from it.
func (s Status) Segment() string {
	var b strings.Builder
	b.WriteString(" (")
	b.WriteString(s.Branch)
	if s.Staged > 0 {
		b.WriteString(" +" + strconv.Itoa(s.Staged))
	}
	if s.Modified > 0 {
		b.WriteString(" ~" + strconv.Itoa(s.Modified))
	}
	b.WriteString(s.sync())
	b.WriteString(")")
	return b.String()
}

// sync renders the ahead, behind and no-upstream markers.
//
// Ahead replaces the no-upstream marker instead of joining it: a branch with
// commits to push has an upstream to push them to, so the two are exclusive.
func (s Status) sync() string {
	out := ""
	if s.Branch != "" && !s.HasUpstream {
		// Commits on this branch exist on this machine only, which a bare
		// (main) — the in-sync rendering — would not distinguish.
		out = " ↑∅"
	}
	if s.Ahead > 0 {
		out = " ↑" + strconv.Itoa(s.Ahead)
	}
	if s.Behind > 0 {
		out += " ↓" + strconv.Itoa(s.Behind)
	}
	return out
}

// BranchOf recovers the branch name from a rendered segment.
//
// It reads the string rather than the struct because the segment is what the
// cache stores: on a hit there is no parsed status to ask, and the pull request
// lookup still needs the branch.
func BranchOf(segment string) string {
	rest, ok := strings.CutPrefix(segment, " (")
	if !ok {
		return ""
	}
	rest, _, _ = strings.Cut(rest, ")")
	branch, _, _ := strings.Cut(rest, " ")
	return branch
}
