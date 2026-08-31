package gitstate

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

// The fixtures below were captured from git 2.54 rather than written by hand,
// so the parser is checked against what git actually emits.
const (
	noUpstream = `# branch.oid 05a469b49bf44da8a9c82d9437417a5278bb8688
# branch.head main
`
	stagedModifiedAhead = `# branch.oid 34b4895626d2a5a781c67edb6fcf7022aea83bbe
# branch.head main
# branch.upstream origin/main
# branch.ab +1 -0
1 A. N... 000000 100644 100644 0000000000000000000000000000000000000000 b4785957bc986dc39c629de9fac9df46972c00fc staged.txt
1 .M N... 100644 100644 100644 78981922613b2afb6025042ff6bd878ac1994e85 78981922613b2afb6025042ff6bd878ac1994e85 tracked.txt
? untracked.txt
`
	inSync = `# branch.oid 7fc439f664ed9ea4001058fcd5bb7e49bdd88a60
# branch.head main
# branch.upstream origin/main
# branch.ab +0 -0
`
	behind = `# branch.oid 7fc439f664ed9ea4001058fcd5bb7e49bdd88a60
# branch.head main
# branch.upstream origin/main
# branch.ab +0 -1
`
	detached = `# branch.oid 7fc439f664ed9ea4001058fcd5bb7e49bdd88a60
# branch.head (detached)
`
	unmerged = `# branch.oid 2027204de3cd22a67d13f311238ec1f8142f54be
# branch.head main
u UU N... 100644 100644 100644 100644 df967b96a579e45a18b8251732d16804b2e56a55 ba2906d0666cf726c7eaadd2cd3db615dedfdf3a 2299c37978265a95cbe835a4b0f0bbf15aad5549 cf
`
)

func TestParse(t *testing.T) {
	tests := []struct {
		name string
		out  string
		want Status
	}{
		{
			name: "no upstream",
			out:  noUpstream,
			want: Status{Branch: "main"},
		},
		{
			// An untracked file is not a change: only the 1, 2 and u records
			// are counted, and ? is skipped like the # headers.
			name: "staged, modified and ahead",
			out:  stagedModifiedAhead,
			want: Status{Branch: "main", HasUpstream: true, Ahead: 1, Staged: 1, Modified: 1},
		},
		{
			name: "in sync",
			out:  inSync,
			want: Status{Branch: "main", HasUpstream: true},
		},
		{
			name: "behind",
			out:  behind,
			want: Status{Branch: "main", HasUpstream: true, Behind: 1},
		},
		{
			// A detached head has no branch name and no upstream, so it must
			// not be mistaken for a branch that was never pushed.
			name: "detached",
			out:  detached,
			want: Status{},
		},
		{
			// A conflict is both staged and unstaged, because neither half of
			// its two-letter code is a dot.
			name: "unmerged",
			out:  unmerged,
			want: Status{Branch: "main", Staged: 1, Modified: 1},
		},
		{
			// git prints branch.upstream and branch.ab independently, so a
			// pruned upstream reference leaves the first without the second.
			name: "an upstream without ahead or behind counts",
			out:  "# branch.head main\n# branch.upstream origin/gone\n",
			want: Status{Branch: "main", HasUpstream: true},
		},
		{
			name: "both ahead and behind",
			out:  "# branch.head main\n# branch.upstream origin/main\n# branch.ab +2 -3\n",
			want: Status{Branch: "main", HasUpstream: true, Ahead: 2, Behind: 3},
		},
		{
			// The shortest record git emits is about a hundred characters, so
			// anything too short to hold the two status letters is not one.
			name: "a record too short to be one is ignored",
			out:  "# branch.head main\n1\n",
			want: Status{Branch: "main"},
		},
		{
			name: "empty output",
			out:  "",
			want: Status{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if diff := cmp.Diff(tt.want, Parse([]byte(tt.out))); diff != "" {
				t.Errorf("Parse mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestStatusSegment(t *testing.T) {
	tests := []struct {
		name string
		out  string
		want string
	}{
		{name: "clean and in sync", out: inSync, want: "(main)"},
		{name: "staged, modified and ahead", out: stagedModifiedAhead, want: "(main +1 ~1 ↑1)"},
		{name: "behind", out: behind, want: "(main ↓1)"},
		{name: "detached", out: detached, want: "()"},
		{name: "unmerged", out: unmerged, want: "(main +1 ~1 ↑∅)"},
		{
			// A branch with no upstream exists only on this machine, which the
			// empty-set marker says and a bare (main) would not.
			name: "no upstream", out: noUpstream, want: "(main ↑∅)",
		},
		{
			// Ahead replaces the marker rather than joining it: a branch with
			// commits to push necessarily has an upstream to push them to.
			name: "ahead replaces the no-upstream marker",
			out:  "# branch.head main\n# branch.upstream origin/main\n# branch.ab +3 -0\n",
			want: "(main ↑3)",
		},
		{
			name: "ahead and behind",
			out:  "# branch.head main\n# branch.upstream origin/main\n# branch.ab +2 -3\n",
			want: "(main ↑2 ↓3)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := Parse([]byte(tt.out)).Segment(); got != tt.want {
				t.Errorf("Segment() = %q, want %q", got, tt.want)
			}
		})
	}
}
