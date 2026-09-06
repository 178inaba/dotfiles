package ghmd_test

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

func TestRefuseBareHashRefs(t *testing.T) {
	t.Parallel()

	// Every accepted case holds three candidates that a reading GitHub does not
	// make would have counted, so that accepting it says something.
	tests := []struct {
		name    string
		body    string
		refused bool
	}{
		{name: "empty", body: ""},
		{name: "a mention of one issue", body: "see #1"},
		{name: "three distinct", body: "#1 then #2 then #3", refused: true},
		{name: "punctuation around them still counts", body: "(#1) [#2] #3.", refused: true},
		// The count is of distinct leading digits, not of distinct issues:
		// what it is looking for is a run of item numbers.
		{name: "repeats are one number", body: "#1 and #1 and #1"},
		{name: "ten and up are not item numbers", body: "#10 #11 #12"},
		{name: "a suffix is not an item number", body: "#1a2b3c #2d4e6f #3g5h7i"},
		{name: "qualified references are left alone", body: "o/r#1 o/r#2 o/r#3"},
		{name: "a fenced block hides its lines", body: "#1\n```\n#2 #3\n```\n"},
		{name: "a tilde fence hides its lines too", body: "#1\n~~~\n#2 #3\n~~~\n"},
		{name: "an indented fence still opens one", body: "#1\n  ```\n#2 #3\n  ```\n"},
		{name: "a code span hides what it holds", body: "`#1 #2` #3"},
		// The span is removed before the line is split, so the text either
		// side of it joins into one token.
		{name: "removing a span joins its neighbours", body: "a`x`#1 b`y`#2 c`z`#3"},
		// A known limit, kept deliberately: the shell version behaved this
		// way and a body with an unclosed fence is broken anyway.
		{name: "an unclosed fence hides the rest", body: "#1\n```\n#2 #3\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			refusal := ghmd.RefuseBareHashRefs(tt.body)
			if refused := refusal != ""; refused != tt.refused {
				t.Errorf("RefuseBareHashRefs(%q) = %q, want refused = %v", tt.body, refusal, tt.refused)
			}
		})
	}
}

func TestRefuseBareHashRefsSaysWhatWasFoundAndWhatToDo(t *testing.T) {
	t.Parallel()

	got := ghmd.RefuseBareHashRefs("#1 one #2 two #3 three")
	for _, want := range []string{"3 distinct", "OWNER/REPO#N", "Fix:"} {
		if !strings.Contains(got, want) {
			t.Errorf("the refusal = %q, want it to hold %q", got, want)
		}
	}
	// Each caller puts its own lines above the refusal and ends the message
	// itself, so a newline of its own at either end would show up as a gap in
	// both of them.
	if strings.HasPrefix(got, "\n") || strings.HasSuffix(got, "\n") {
		t.Errorf("the refusal = %q, want no leading or trailing newline", got)
	}
}

// text is one segment rendered as the caller sees it: its kind, its line and
// the bytes it covers, so a case reads as the body it describes.
type text struct {
	Kind ghmd.Kind
	Line int
	Text string
}

func segments(body string) []text {
	var out []text
	for s := range ghmd.Segments(body) {
		out = append(out, text{Kind: s.Kind, Line: s.Line, Text: body[s.Start:s.End]})
	}
	return out
}

func TestSegments(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		want []text
	}{
		{name: "empty", body: "", want: nil},
		{
			name: "prose only",
			body: "one\ntwo\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "one\n"},
				{Kind: ghmd.Prose, Line: 2, Text: "two\n"},
			},
		},
		{
			// The backticks belong to the span, because what asks for it reads
			// the span as GitHub renders it.
			name: "a span splits its line",
			body: "a `b` c\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "a "},
				{Kind: ghmd.Span, Line: 1, Text: "`b`"},
				{Kind: ghmd.Prose, Line: 1, Text: " c\n"},
			},
		},
		{
			// The marker lines are neither: nothing reads them, and calling
			// them prose would put a fence's own backticks in the text a
			// substitution rewrites.
			name: "a fence marker is not a segment",
			body: "a\n```\nb\n```\nc\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "a\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "b\n"},
				{Kind: ghmd.Prose, Line: 5, Text: "c\n"},
			},
		},
		{
			name: "an unclosed fence runs to the end",
			body: "a\n```\nb\nc\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "a\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "b\n"},
				{Kind: ghmd.Fence, Line: 4, Text: "c\n"},
			},
		},
		{
			name: "a line with no trailing newline still ends",
			body: "a `b`",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "a "},
				{Kind: ghmd.Span, Line: 1, Text: "`b`"},
			},
		},
		{
			name: "an unpaired backtick is prose",
			body: "a `b\n",
			want: []text{{Kind: ghmd.Prose, Line: 1, Text: "a `b\n"}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			if diff := cmp.Diff(tt.want, segments(tt.body)); diff != "" {
				t.Errorf("Segments(%q) (-want +got):\n%s", tt.body, diff)
			}
		})
	}
}

// TestSegmentsAreOrderedAndDisjoint is what lets a caller rewrite a body from
// its segments: it can copy the bytes between them through untouched, which
// only works if they arrive in order and never overlap.
func TestSegmentsAreOrderedAndDisjoint(t *testing.T) {
	t.Parallel()

	const body = "intro #1\n```go\nfmt.Println(\"#2\")\n```\ntail `#3` end\n"

	end := 0
	for s := range ghmd.Segments(body) {
		if s.Start < end || s.End < s.Start || s.End > len(body) {
			t.Errorf("segment [%d,%d) does not follow the one ending at %d, within %d bytes",
				s.Start, s.End, end, len(body))
		}
		end = s.End
	}
	if end != len(body) {
		t.Errorf("the last segment ends at %d, want the end of the body at %d", end, len(body))
	}
}
