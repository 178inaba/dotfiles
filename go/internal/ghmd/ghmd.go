// Package ghmd reads a body the way GitHub renders it.
//
// Two things in this module have to agree about that reading. The gh shim
// refuses a body that numbers its items with bare #N, because GitHub autolinks
// those and notifies unrelated issues; ghapi writes bodies in process, where
// the shim never sees them, so it has to reach the same verdict about the same
// text. Neither of them owns the reading, so it lives here rather than in one
// of them: what is shared is how a body reads, not what either caller decides
// about it. The words the refusal is written in are here for the same reason —
// both refusers say the same thing about the same body.
//
// A notation whose meaning depends on that reading belongs here too, even
// where GitHub has never heard of it: the #{NAME} placeholder in placeholder.go
// means nothing inside a code span or a fenced block, and deciding that is
// this package's job rather than its writer's.
//
// The scan is deliberately not a markdown parser. It knows the two things that
// decide whether a reference is live — a fenced block and an inline code span —
// and nothing else, because that is the whole of what the callers ask.
package ghmd

import (
	"bytes"
	"fmt"
	"iter"
	"regexp"
	"strings"
)

// bareHashRefLimit is how many distinct bare references make a run of item
// numbers rather than a mention of an issue.
//
// Three, because a body that means to cite an issue cites one or two, and the
// mistake being caught is a numbered list written as #1 #2 #3.
const bareHashRefLimit = 3

var (
	fenceLine = regexp.MustCompile("^[[:space:]]*(```|~~~)")
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
	// The scan walks every byte, and most lines hold no code span.
	if strings.IndexByte(text, '`') >= 0 {
		for _, span := range codeSpans(text) {
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

// RefuseBareHashRefs is what to say about a body that numbers its items with
// bare #N, and the empty string for one that does not.
//
// The whole rule in one place, rather than a count and a threshold each caller
// compares for itself: the gh shim and ghapi.NewBody both have to reach the
// same verdict about the same body, and a third judge added later would be a
// third copy of the comparison. Where the body came from is the caller's to
// name, above or in front of this, since only the caller knows whether it is a
// file, a field or a flag.
func RefuseBareHashRefs(body string) string {
	distinct := bareHashRefs(body)
	if distinct < bareHashRefLimit {
		return ""
	}
	return fmt.Sprintf(`%d distinct bare #N in #1 to #9 number the items of this body.

GitHub autolinks a bare #number, so using one to number a list of remarks
(#1, #2, ...) sends a reference notification to unrelated issues and pull
requests. A notification cannot be taken back.

Fix: if the numbering is the point, write it in a form without # — an
ordered list (1. 2. ...), say. If an issue or a pull request is really
being referenced, name it as OWNER/REPO#N:
  178inaba/dotfiles#3
That keeps the link and does not trip this guard.`, distinct)
}

// codeSpans returns the byte ranges of the code spans in one line, backticks
// included, in order and without overlap.
//
// CommonMark pairs a span by the length of its backtick run: a run of N opens
// a span that the next run of exactly N closes, which is how text that itself
// holds a backtick is quoted — inside a run of two, a lone backtick is content
// rather than a delimiter. A run with no partner is literal text, and the scan
// resumes just after it rather than after the runs it searched past, so that
// those runs are still free to pair with each other.
//
// What CommonMark does beyond this — stripping a space from each end of the
// content, and allowing a span to continue across a soft line break — changes
// what a span reads as but not where one starts or ends, and only the
// boundaries are asked for here. Staying within the line is also what keeps
// Segment.Line meaning what it says.
func codeSpans(text string) [][2]int {
	var out [][2]int
	for i := 0; i < len(text); {
		if text[i] != '`' {
			i++
			continue
		}
		n := backtickRun(text, i)
		j, ok := closingRun(text, i+n, n)
		if !ok {
			i += n
			continue
		}
		out = append(out, [2]int{i, j + n})
		i = j + n
	}
	return out
}

// closingRun finds the run of exactly n backticks that closes a span opened at
// from, skipping over the runs of any other length between.
func closingRun(text string, from, n int) (int, bool) {
	for i := from; i < len(text); {
		if text[i] != '`' {
			i++
			continue
		}
		m := backtickRun(text, i)
		if m == n {
			return i, true
		}
		i += m
	}
	return 0, false
}

// backtickRun is the length of the run of backticks starting at i.
func backtickRun(text string, i int) int {
	j := i
	for j < len(text) && text[j] == '`' {
		j++
	}
	return j - i
}

// bareHashRefs counts the distinct digits of the bare #1 to #9 in body,
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
func bareHashRefs(body string) int {
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
