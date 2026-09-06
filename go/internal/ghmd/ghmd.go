// Package ghmd reads a body the way GitHub renders it.
//
// Two things in this module have to agree about that reading. The gh shim
// refuses a body that numbers its items with bare #N, because GitHub autolinks
// those and notifies unrelated issues; `ccx issue publish` writes issue bodies
// in process, where the shim never sees them, so it has to reach the same
// verdict about the same text. Neither of them owns the reading, so it lives
// here rather than in one of them: what is shared is how a body reads, not
// what either caller decides about it.
//
// The scan is deliberately not a markdown parser. It knows the two things that
// decide whether a reference is live — a fenced block and an inline code span —
// and nothing else, because that is the whole of what the callers ask.
package ghmd

import (
	"bytes"
	"iter"
	"regexp"
	"strings"
)

// BareHashRefLimit is how many distinct bare references make a run of item
// numbers rather than a mention of an issue.
//
// Three, because a body that means to cite an issue cites one or two, and the
// mistake being caught is a numbered list written as #1 #2 #3.
const BareHashRefLimit = 3

var (
	fenceLine = regexp.MustCompile("^[[:space:]]*(```|~~~)")
	codeSpan  = regexp.MustCompile("`[^`]*`")
	// The trailing class is what excludes #12 and up, #1a2b3c and #1st; the
	// leading one is what leaves OWNER/REPO#1 alone, by skipping any token
	// that opens with an alphanumeric.
	bareHashToken = regexp.MustCompile(`^[^[:alnum:]#]*#[1-9]([^[:alnum:]]|$)`)
)

// Kind is how GitHub reads a run of a body.
type Kind int

const (
	// Prose is text GitHub renders as prose, where a reference is live and a
	// substitution belongs.
	Prose Kind = iota
	// Fence is a line inside a fenced code block. The fence's own marker lines
	// are no segment at all: nothing reads them, and calling them prose would
	// hand a rewriter the backticks that delimit the block.
	Fence
	// Span is an inline code span, its backticks included, so that a caller
	// matching against it sees what GitHub shows.
	Span
)

// Segment is one run of a body, and where it is.
type Segment struct {
	Kind Kind
	// Line is the 1-based line the segment starts on, which is what a report
	// about a body names.
	Line int
	// Start and End are the byte offsets into the body itself, not into the
	// line, so that a rewriter can copy what lies between segments through
	// untouched.
	Start, End int
}

// Segments walks a body, yielding its runs in order and without overlap.
//
// A known limit, carried over from the shell version this replaces: an
// unclosed fence hides everything after it. A body with one is broken in a way
// the writer will see, and closing over it would make the reading disagree
// with what the shim already decided.
func Segments(body string) iter.Seq[Segment] {
	return func(yield func(Segment) bool) {
		fence := false
		line, offset := 1, 0
		for text := range strings.Lines(body) {
			start := offset
			offset += len(text)
			switch {
			case fenceLine.MatchString(text):
				fence = !fence
			case fence:
				if !yield(Segment{Kind: Fence, Line: line, Start: start, End: offset}) {
					return
				}
			default:
				if !yieldProseAndSpans(yield, text, line, start) {
					return
				}
			}
			line++
		}
	}
}

// yieldProseAndSpans splits one prose line around the code spans in it.
func yieldProseAndSpans(yield func(Segment) bool, text string, line, start int) bool {
	at := 0
	// The search allocates, and most lines hold no code span.
	if strings.IndexByte(text, '`') >= 0 {
		for _, span := range codeSpan.FindAllStringIndex(text, -1) {
			if span[0] > at && !yield(Segment{Kind: Prose, Line: line, Start: start + at, End: start + span[0]}) {
				return false
			}
			if !yield(Segment{Kind: Span, Line: line, Start: start + span[0], End: start + span[1]}) {
				return false
			}
			at = span[1]
		}
	}
	if at >= len(text) {
		return true
	}
	return yield(Segment{Kind: Prose, Line: line, Start: start + at, End: start + len(text)})
}

// BareHashRefs counts the distinct digits of the bare #1 to #9 in body,
// ignoring the places GitHub does not autolink.
//
// Distinct digits rather than distinct issues, because what it is looking for
// is a run of item numbers and #1 #1 #1 is not one.
//
// [[:alnum:]] is ASCII here while the awk this replaced used whatever the
// locale said, so a digit followed by a multibyte letter counts where it might
// not have. That errs towards blocking, and a decision that does not move with
// the locale is worth more than the agreement — macOS awk compares multibyte
// text unreliably.
func BareHashRefs(body string) int {
	seen := map[byte]bool{}
	// Per line, and with the spans taken out rather than skipped: text either
	// side of a span joins into one token, which is what decides whether that
	// token opens with an alphanumeric. The buffer is reused across lines
	// because this runs on the gh shim's path, before every write it guards.
	var line []byte
	at := 0
	flush := func() {
		for token := range bytes.FieldsSeq(line) {
			if bareHashToken.Match(token) {
				seen[token[bytes.IndexByte(token, '#')+1]] = true
			}
		}
		line = line[:0]
	}
	for s := range Segments(body) {
		if s.Line != at {
			flush()
			at = s.Line
		}
		if s.Kind == Prose {
			line = append(line, body[s.Start:s.End]...)
		}
	}
	flush()
	return len(seen)
}
