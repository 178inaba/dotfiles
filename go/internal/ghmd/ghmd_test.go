package ghmd_test

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

func TestRefuseBareHashRefs(t *testing.T) {
	t.Parallel()

	// An accepted case that is about the reading holds three candidates a
	// coarser one would have counted, so that accepting it says something.
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
		// The tilde line does not close the backtick block, so the numbers on
		// the line after it are still inside it.
		{name: "a tilde line does not close a backtick block", body: "```\n~~~\n#1 #2 #3\n```\n"},
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

func TestRefuseQuotedClosingKeyword(t *testing.T) {
	t.Parallel()

	// GitHub reads a closing keyword only where it renders as prose, so what
	// is refused is one written where it will not be read: the merge then
	// leaves the issue open and nobody is told.
	tests := []struct {
		name    string
		body    string
		refused bool
	}{
		{name: "empty", body: ""},
		{name: "a code span holds one", body: "Related\n\n`Closes #656`\n", refused: true},
		{name: "a fenced block holds one", body: "before\n```\ncloses #656\n```\nafter\n", refused: true},
		{name: "a qualified reference in a span", body: "see `Resolves foo/bar#12` here\n", refused: true},
		// The three GitHub does read, or does not read as a keyword at all.
		{name: "prose is where it works", body: "Closes #656\n"},
		{name: "a placeholder names no issue", body: "docs update: `Closes #N` placeholder\n"},
		{name: "the keyword without a reference", body: "call `closes the stream` explicitly\n"},
		{name: "a longer word merely ends in one", body: "word `discloses #656` here\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			refusal := ghmd.RefuseQuotedClosingKeyword(tt.body)
			if refused := refusal != ""; refused != tt.refused {
				t.Errorf("RefuseQuotedClosingKeyword(%q) = %q, want refused = %v", tt.body, refusal, tt.refused)
			}
		})
	}
}

func TestRefuseQuotedClosingKeywordSaysWhyAndWhatToDo(t *testing.T) {
	t.Parallel()

	got := ghmd.RefuseQuotedClosingKeyword("Related\n\n`Closes #656`\n")
	for _, want := range []string{"leaves the issue open", "Fix:", "placeholder"} {
		if !strings.Contains(got, want) {
			t.Errorf("the refusal = %q, want it to hold %q", got, want)
		}
	}
	// As the other refusal does: each caller puts its own lines above this one
	// and ends the message itself.
	if strings.HasPrefix(got, "\n") || strings.HasSuffix(got, "\n") {
		t.Errorf("the refusal = %q, want no leading or trailing newline", got)
	}
}

func TestClosingReferences(t *testing.T) {
	t.Parallel()

	// The reading GitHub acts on, and no way to ask for the rest: a body that
	// documents a closing keyword beside the one it means gets back only the
	// one it means. Which run each reference sat in is asserted where the walk
	// is, in closing_internal_test.go — a caller of this cannot see it, which
	// is the point of the shape.
	body := "Closes #10\nas in `Closes #11`\n```\nCloses #12\n```\nFixes other/repo#13\n"

	want := []ghmd.ClosingReference{{Number: 10}, {Repo: "other/repo", Number: 13}}
	if diff := cmp.Diff(want, ghmd.ClosingReferences(body)); diff != "" {
		t.Errorf("ClosingReferences(%q) (-want +got):\n%s", body, diff)
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
			// A marker line is Fence like the lines it delimits: no consumer
			// needs the distinction, and calling it prose would put a fence's
			// own backticks in the text a substitution rewrites.
			name: "a fence marker is a Fence segment",
			body: "a\n```\nb\n```\nc\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "a\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "```\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "b\n"},
				{Kind: ghmd.Fence, Line: 4, Text: "```\n"},
				{Kind: ghmd.Prose, Line: 5, Text: "c\n"},
			},
		},
		{
			name: "an unclosed fence runs to the end",
			body: "a\n```\nb\nc\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "a\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "```\n"},
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
		{
			// The CommonMark way to quote text that itself holds a backtick,
			// which this repository's own pull request bodies write: a run of
			// two is closed by the next run of two, and the single run inside
			// is content rather than a span of its own.
			name: "a double-backtick span holds a single backtick",
			body: "a ``b `c` d`` e\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "a "},
				{Kind: ghmd.Span, Line: 1, Text: "``b `c` d``"},
				{Kind: ghmd.Prose, Line: 1, Text: " e\n"},
			},
		},
		{
			// The run of two has no partner, so it is literal text and the
			// scan resumes after it — which is what lets the two single runs
			// it was searching past pair with each other.
			name: "an unmatched run is literal and the scan resumes after it",
			body: "a `` b ` c`\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "a `` b "},
				{Kind: ghmd.Span, Line: 1, Text: "` c`"},
				{Kind: ghmd.Prose, Line: 1, Text: "\n"},
			},
		},
		{
			name: "three backticks are closed by three, not by one",
			body: "x ```a`b``` y\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "x "},
				{Kind: ghmd.Span, Line: 1, Text: "```a`b```"},
				{Kind: ghmd.Prose, Line: 1, Text: " y\n"},
			},
		},
		{
			// CommonMark's example 145: a backtick fence's info string may not
			// hold a backtick, so this opens no block and the line is read as
			// prose with a span in it. A tilde fence's info string may hold
			// both characters, which is why the rule is the backtick's alone.
			name: "a backtick run whose remainder holds a backtick opens no fence",
			body: "``` x ` y ```",
			want: []text{{Kind: ghmd.Span, Line: 1, Text: "``` x ` y ```"}},
		},
		{
			// The misreading this replaces: a fence used to toggle on any
			// marker line, so the tilde line closed the backtick block and
			// left the rest of the body as prose.
			name: "a tilde line inside a backtick block is content",
			body: "```\n~~~\n#1 #2 #3\n```\n",
			want: []text{
				{Kind: ghmd.Fence, Line: 1, Text: "```\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "~~~\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "#1 #2 #3\n"},
				{Kind: ghmd.Fence, Line: 4, Text: "```\n"},
			},
		},
		{
			// The other half of the rule above, and CommonMark's example 146:
			// only a backtick fence's info string is restricted, so refusing a
			// backtick in a tilde one would open no block here at all.
			name: "a tilde fence's info string may hold a backtick",
			body: "~~~ aa ``` ~~~\nx\n~~~\n",
			want: []text{
				{Kind: ghmd.Fence, Line: 1, Text: "~~~ aa ``` ~~~\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "x\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "~~~\n"},
			},
		},
		{
			// A closing fence carries no info string, but trailing whitespace
			// is not one: the line still closes the block.
			name: "whitespace after a closing run still closes the block",
			body: "```\nx\n```   \ny\n",
			want: []text{
				{Kind: ghmd.Fence, Line: 1, Text: "```\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "x\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "```   \n"},
				{Kind: ghmd.Prose, Line: 4, Text: "y\n"},
			},
		},
		{
			name: "a longer closing run still closes the block",
			body: "````\nx\n`````\n",
			want: []text{
				{Kind: ghmd.Fence, Line: 1, Text: "````\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "x\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "`````\n"},
			},
		},
		{
			// The deviation markerRun names: CommonMark stops at three spaces
			// and reads a fourth as an indented code block, so this is the one
			// place the reading answers differently on purpose.
			name: "a fence marker indented past three spaces still opens a block",
			body: "    ```\nx\n    ```\ny\n",
			want: []text{
				{Kind: ghmd.Fence, Line: 1, Text: "    ```\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "x\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "    ```\n"},
				{Kind: ghmd.Prose, Line: 4, Text: "y\n"},
			},
		},
		{
			name: "a run shorter than the opening one is content",
			body: "````\n```\nx\n````\n",
			want: []text{
				{Kind: ghmd.Fence, Line: 1, Text: "````\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "```\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "x\n"},
				{Kind: ghmd.Fence, Line: 4, Text: "````\n"},
			},
		},
		{
			// A closing fence carries no info string, so a run of the right
			// length followed by text closes nothing.
			name: "a closing-length run followed by text is content",
			body: "```\n``` x\ny\n```\n",
			want: []text{
				{Kind: ghmd.Fence, Line: 1, Text: "```\n"},
				{Kind: ghmd.Fence, Line: 2, Text: "``` x\n"},
				{Kind: ghmd.Fence, Line: 3, Text: "y\n"},
				{Kind: ghmd.Fence, Line: 4, Text: "```\n"},
			},
		},
		{
			// A fence needs three, so neither line opens one; and neither run
			// has a partner on its own line, so neither is a span either.
			name: "a line opening with one or two backticks is not a fence",
			body: "`\nx\n`\n``\ny\n``\n",
			want: []text{
				{Kind: ghmd.Prose, Line: 1, Text: "`\n"},
				{Kind: ghmd.Prose, Line: 2, Text: "x\n"},
				{Kind: ghmd.Prose, Line: 3, Text: "`\n"},
				{Kind: ghmd.Prose, Line: 4, Text: "``\n"},
				{Kind: ghmd.Prose, Line: 5, Text: "y\n"},
				{Kind: ghmd.Prose, Line: 6, Text: "``\n"},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got := segments(tt.body)
			if diff := cmp.Diff(tt.want, got); diff != "" {
				t.Errorf("Segments(%q) (-want +got):\n%s", tt.body, diff)
			}
			// The partition read off the table: what a case declares is
			// yielded is also the whole of the body it was read from.
			var whole strings.Builder
			for _, s := range got {
				whole.WriteString(s.Text)
			}
			if whole.String() != tt.body {
				t.Errorf("the segments concatenate to %q, want the body itself", whole.String())
			}
		})
	}
}

// TestSegmentsPartitionTheBody is what lets a caller account for every byte of
// a body from its segments alone: they arrive in order, never overlap, and
// leave no gap for a reader to forget.
func TestSegmentsPartitionTheBody(t *testing.T) {
	t.Parallel()

	const body = "intro #1\n```go\nfmt.Println(\"#2\")\n```\ntail `#3` end\n"

	end := 0
	for s := range ghmd.Segments(body) {
		if s.Start != end || s.End < s.Start || s.End > len(body) {
			t.Errorf("segment [%d,%d) does not continue the one ending at %d, within %d bytes",
				s.Start, s.End, end, len(body))
		}
		end = s.End
	}
	if end != len(body) {
		t.Errorf("the last segment ends at %d, want the end of the body at %d", end, len(body))
	}
}

// TestBlocks reads a block back as its info string and its content, so that a
// case says what a caller sees rather than a pair of offsets.
func TestBlocks(t *testing.T) {
	t.Parallel()

	type block struct {
		info    string
		line    int
		content string
	}
	tests := map[string]struct {
		body string
		want []block
	}{
		"a block with a language": {
			body: "text\n```bash\nls\n```\n",
			want: []block{{info: "bash\n", line: 2, content: "ls\n"}},
		},
		// The info string is what follows the marker, as written, so a block
		// declaring nothing carries the line end and no more.
		"a block with no info string at all": {
			body: "```\nls\n```\n",
			want: []block{{info: "\n", line: 1, content: "ls\n"}},
		},
		"a block with nothing between its markers": {
			body: "```go\n```\n",
			want: []block{{info: "go\n", line: 1}},
		},
		// The closing marker of the first may not be read as the opening one
		// of the second, which is what a caller splitting fenced lines into
		// blocks for itself gets wrong.
		"two blocks written back to back": {
			body: "```sh\na\n```\n```zsh\nb\n```\n",
			want: []block{{info: "sh\n", line: 1, content: "a\n"}, {info: "zsh\n", line: 4, content: "b\n"}},
		},
		// A shorter run, or one of the other character, is content.
		"a block quoting a marker": {
			body: "````md\n```\ninner\n```\n````\n",
			want: []block{{info: "md\n", line: 1, content: "```\ninner\n```\n"}},
		},
		"an unclosed block runs to the end": {
			body: "```bash\nls\npwd\n",
			want: []block{{info: "bash\n", line: 1, content: "ls\npwd\n"}},
		},
		"a body with no block in it": {body: "text and `a span`\n"},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()

			var got []block
			for b := range ghmd.Blocks(tt.body) {
				got = append(got, block{info: b.Info, line: b.Line, content: tt.body[b.Start:b.End]})
			}
			if diff := cmp.Diff(tt.want, got, cmp.AllowUnexported(block{})); diff != "" {
				t.Errorf("blocks mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// TestBlankCode reads the results as literals rather than as a length and a
// count, because what a caller depends on is not only that the code is gone
// but that everything else sits where the body put it: the widths below are
// the offsets a match against the result reports.
func TestBlankCode(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		want string
	}{
		{name: "empty", body: "", want: ""},
		{name: "prose is what comes back", body: "one\ntwo\n", want: "one\ntwo\n"},
		{
			// The backticks go with the span, so what follows keeps the
			// character in front of it: an import written after a span is
			// still preceded by a space, and still an import.
			name: "a span goes, backticks and all",
			body: "a `b` c\n",
			want: "a     c\n",
		},
		{
			// The marker lines are Fence like the content between them, so
			// blanking whatever Segments does not call prose takes them too.
			name: "a fence goes with its marker lines",
			body: "a\n```\nb\n```\nc\n",
			want: "a\n   \n \n   \nc\n",
		},
		{
			// The last line of a body need not end in one, and prose that
			// runs to the final byte comes back whole.
			name: "a body that does not end in a newline",
			body: "no newline here",
			want: "no newline here",
		},
		{
			// Only the newline is kept, so the \r of a marker line is blanked
			// like any other byte of code while the one in prose is not.
			name: "CRLF keeps its newline and loses the rest",
			body: "```\r\n@a.md\r\n```\r\n@c.md\r\n",
			want: "    \n      \n    \n@c.md\r\n",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			if diff := cmp.Diff(tt.want, ghmd.BlankCode(tt.body)); diff != "" {
				t.Errorf("BlankCode(%q) (-want +got):\n%s", tt.body, diff)
			}
		})
	}
}
