package ghmd

// In the package rather than beside the rest of the tests, because the walk
// both judgements are filtered out of does not leave it: the run a reference
// sits in is what this asserts, and the exported reader has already dropped it.
// What that reader hands out is tested from outside, in ghmd_test.go, where the
// rest of the exported surface is.

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestKeywordReferences(t *testing.T) {
	t.Parallel()

	// The two consumers take opposite halves of this by Kind, so what a
	// reference is attributed to is the whole of what is asserted here, beside
	// the reference itself.
	tests := []struct {
		name string
		body string
		want []keywordReference
	}{
		{name: "empty", body: ""},
		{
			name: "a bare reference names no repository",
			body: "Closes #5\n",
			want: []keywordReference{{Number: 5, Kind: Prose}},
		},
		{
			name: "a qualified reference carries its repository",
			body: "Resolves other/repo#12\n",
			want: []keywordReference{{Repo: "other/repo", Number: 12, Kind: Prose}},
		},
		{
			// An underscore is not alphanumeric, so it does not join the
			// keyword to a longer word — which is where this reading and the
			// \b one it replaces used to disagree.
			//
			// Whether GitHub closes on this form is unverified: it takes a
			// real pull request merged into a default branch to find out, and
			// what is pinned here is the one implementation that survived the
			// two, not GitHub's behaviour. This is the case to rewrite first
			// if GitHub's own reading is ever established.
			name: "an underscore leaves the keyword whole",
			body: "_Closes #5\n",
			want: []keywordReference{{Number: 5, Kind: Prose}},
		},
		{
			name: "a code span is where GitHub does not read it",
			body: "see `Closes #11` here\n",
			want: []keywordReference{{Number: 11, Kind: Span}},
		},
		{
			name: "a fenced block is the other such place",
			body: "a\n```\ncloses #656\n```\n",
			want: []keywordReference{{Number: 656, Kind: Fence}},
		},
		{
			// Every spelling GitHub accepts, in one body: the three verbs,
			// their inflections, the optional colon and either case.
			name: "the spellings the keyword takes",
			body: "close #1\nCloses: #2\nclosed #3\nfix #4\nFIXES #5\nfixed: #6\n" +
				"resolve #7\nResolves #8\nresolved #9\n",
			want: []keywordReference{
				{Number: 1, Kind: Prose}, {Number: 2, Kind: Prose},
				{Number: 3, Kind: Prose}, {Number: 4, Kind: Prose},
				{Number: 5, Kind: Prose}, {Number: 6, Kind: Prose},
				{Number: 7, Kind: Prose}, {Number: 8, Kind: Prose},
				{Number: 9, Kind: Prose},
			},
		},
		{
			// References come in the order the body writes them, and the
			// caller that deduplicates does so itself: #10 twice is twice
			// here.
			name: "references come in the order the body writes them",
			body: "Closes #10\nResolves other/repo#12\nfix #10\n",
			want: []keywordReference{
				{Number: 10, Kind: Prose},
				{Repo: "other/repo", Number: 12, Kind: Prose},
				{Number: 10, Kind: Prose},
			},
		},
		{
			// The case each filter actually meets: one body holding both the
			// references it means and the ones it quotes, interleaved, so that
			// neither filter can pass by reading the body as a whole.
			name: "one body holds the meant and the quoted alike",
			body: "Closes #10\nas in `Closes #11`\n```\nCloses #12\n```\nFixes other/repo#13\n",
			want: []keywordReference{
				{Number: 10, Kind: Prose},
				{Number: 11, Kind: Span},
				{Number: 12, Kind: Fence},
				{Repo: "other/repo", Number: 13, Kind: Prose},
			},
		},
		// The forms that name no issue to close. Four of them hold a # and so
		// reach the pattern, which turns them away; the two that hold none —
		// the keyword with nothing to refer to, and the url — are skipped
		// before it by the run having no # in it at all.
		{name: "a line break separates the keyword from the reference", body: "Closes\n#5\n"},
		{name: "a placeholder names no issue", body: "docs update: `Closes #N` placeholder\n"},
		{name: "a longer word merely ends in the keyword", body: "word `discloses #656` here\n"},
		// GitHub links a bare reference and closes on none.
		{name: "a reference without a keyword", body: "See #5\n"},
		{name: "the keyword without a reference", body: "call `closes the stream` explicitly\n"},
		{name: "a url is not a reference GitHub closes on", body: "Fixes https://github.com/owner/repo/issues/14\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			if diff := cmp.Diff(tt.want, keywordReferences(tt.body)); diff != "" {
				t.Errorf("keywordReferences(%q) (-want +got):\n%s", tt.body, diff)
			}
		})
	}
}
