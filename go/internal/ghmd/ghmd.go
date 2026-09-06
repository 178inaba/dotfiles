// Package ghmd reads a body the way GitHub renders it.
//
// Two things in this module have to agree about a verdict. The gh shim
// refuses a body that numbers its items with bare #N, because GitHub autolinks
// those and notifies unrelated issues, and a pull request body that hides a
// closing keyword inside code, because GitHub does not read it there and the
// merge leaves the issue open; ghapi writes bodies in process, where the shim
// never sees them, so it has to reach the same verdicts about the same text.
// The words the refusals are written in are here for that reason — both
// refusers say the same thing about the same body.
//
// More than those two read a body, though, and the reading is what they all
// share rather than the verdict: issue's section check asks which lines of a
// draft are prose, plandocs blanks a plan document's code out before looking
// for the links and imports in it, skill's contract check wants the names
// a SKILL.md writes in a code span, and pullrequest's linked_issues asks for
// the closing keywords a body means, which is the same question the refusal
// asks with the answer kept the other way round. None of them owns the
// reading, so it lives here rather than in whichever of them wrote it first —
// a copy in a caller drifts the next time this one is corrected, which is what
// plandocs' own scanner had done in six places by the time it was retired.
//
// A notation whose meaning depends on that reading belongs here too, even
// where GitHub has never heard of it: the #{NAME} placeholder in placeholder.go
// means nothing inside a code span or a fenced block, and deciding that is
// this package's job rather than its writer's.
//
// The scan is deliberately not a markdown parser. It knows the two things that
// decide whether a reference is live — a fenced block and an inline code span —
// and nothing else, because that is the whole of what the callers ask. The two
// it does know it delimits by CommonMark's measurement rather than by an
// approximation of it: both are marked by a run of one character, and a
// reading that looks at the character without its length puts a boundary where
// GitHub shows none.
//
// Two deviations remain, both deliberate and both named where they are made: a
// fence marker may be indented by any amount rather than by three spaces
// (markerRun), and a code span does not continue across a line end
// (nextCodeSpan). The second is the one that can still misread a body.
package ghmd

import (
	"bytes"
	"fmt"
	"iter"
	"regexp"
	"strconv"
	"strings"
)

// bareHashRefLimit is how many distinct bare references make a run of item
// numbers rather than a mention of an issue.
//
// Three, because a body that means to cite an issue cites one or two, and the
// mistake being caught is a numbered list written as #1 #2 #3.
const bareHashRefLimit = 3

var (
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
	// Fence is a line of a fenced code block, the lines that open and close it
	// included. Nothing distinguishes a marker from the content it delimits:
	// no consumer needs the distinction, and a kind of its own would grow a
	// case in every switch over one. What a marker may not be is prose, which
	// would hand a rewriter the backticks that delimit the block.
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
	// line, so that a rewriter can splice its output out of the body at the
	// offsets it was handed.
	Start, End int
}

// Segments partitions a body: the runs come in order, never overlap, and
// cover every byte from the first to the last.
//
// The covering is what a caller is entitled to rely on rather than a property
// of how this happens to be written. A caller that has to account for every
// byte accounts for it by walking the segments, and a gap nobody yields is
// where such a reader forgets one.
//
// A known limit, carried over from the shell version this replaces: an
// unclosed fence hides everything after it, running to the end as Fence. A
// body with one is broken in a way the writer will see, and closing over it
// would make the reading disagree with what the shim already decided.
func Segments(body string) iter.Seq[Segment] {
	return func(yield func(Segment) bool) {
		var open fence
		line, offset := 1, 0
		for text := range strings.Lines(body) {
			start := offset
			offset += len(text)
			if open.step(text) {
				if !yield(Segment{Kind: Fence, Line: line, Start: start, End: offset}) {
					return
				}
			} else if !yieldProseAndSpans(yield, text, line, start) {
				return
			}
			line++
		}
	}
}

// BlankCode returns body with every byte Segments does not yield as Prose — a
// Fence segment or a Span segment — replaced by a space, and every newline
// kept.
//
// Blanked rather than deleted, so that the result has the body's own length
// and line structure and each remaining character sits at its original
// offset. That is what a caller matching a pattern against the result asks
// for: an import written after a code span is still preceded by a space, and
// still an import. A \r is a byte like any other, so one inside code goes and
// one in prose stays.
//
// Writing the segments out in turn is what keeps the length, and it is
// Segments' covering the body that makes that true — a run yielded by nobody
// would be a run missing from here.
func BlankCode(body string) string {
	var b strings.Builder
	// Exactly len(body) bytes are written, because the segments cover the
	// body and each is written at its own length, so this is the only
	// allocation the builder makes.
	b.Grow(len(body))
	for s := range Segments(body) {
		text := body[s.Start:s.End]
		if s.Kind == Prose {
			b.WriteString(text)
			continue
		}
		for i := range len(text) {
			if text[i] == '\n' {
				b.WriteByte('\n')
				continue
			}
			b.WriteByte(' ')
		}
	}
	return b.String()
}

// fence is the block a body is currently inside, or the zero value outside
// one. Both the marker's character and the length of its run are carried,
// because both decide what closes it.
type fence struct {
	char byte
	n    int
}

// step advances f over one line and reports whether that line belongs to a
// fenced block — the markers that open and close it included, since they are
// Fence like the content between them.
//
// Whether a line opens a block is asked only outside one, and whether it
// closes one only inside: a marker of the other character, or a shorter one,
// is content rather than a nested block, and that is the difference from the
// toggle this replaces.
func (f *fence) step(text string) bool {
	switch {
	case f.n == 0:
		opened, ok := opensFence(text)
		if ok {
			*f = opened
		}
		return ok
	case f.closedBy(text):
		*f = fence{}
	}
	return true
}

// opensFence reports whether a line outside a block opens one.
//
// A backtick fence is refused where the rest of the line holds a backtick,
// which is CommonMark's rule that a backtick fence's info string may not
// contain one — its example 145 is the line "``` aa ```", read as a code
// span and not as a block. A tilde fence's info string may hold either
// character, so the rule is the backtick's alone.
func opensFence(text string) (fence, bool) {
	char, n, rest := markerRun(text)
	if n < 3 {
		return fence{}, false
	}
	if char == '`' && strings.IndexByte(rest, '`') >= 0 {
		return fence{}, false
	}
	return fence{char: char, n: n}, true
}

// closedBy reports whether a line closes the block f opens.
//
// A closing fence carries no info string, so a run of the right character and
// length still closes nothing if anything but whitespace follows it.
func (f fence) closedBy(text string) bool {
	char, n, rest := markerRun(text)
	return char == f.char && n >= f.n && strings.TrimSpace(rest) == ""
}

// markerRun reads the fence marker a line opens with: its character, the
// length of its run, and what follows the run.
//
// The leading whitespace it skips is any amount, where CommonMark allows three
// spaces. Nothing here depends on the difference, and the tolerance is what
// the expression this replaces already had.
func markerRun(text string) (char byte, n int, rest string) {
	i := 0
	for i < len(text) && (text[i] == ' ' || text[i] == '\t') {
		i++
	}
	if i == len(text) || (text[i] != '`' && text[i] != '~') {
		return 0, 0, ""
	}
	n = runLength(text, i)
	return text[i], n, text[i+n:]
}

// yieldProseAndSpans splits one prose line around the code spans in it.
func yieldProseAndSpans(yield func(Segment) bool, text string, line, start int) bool {
	at := 0
	for {
		i, j, ok := nextCodeSpan(text, at)
		if !ok {
			break
		}
		if i > at && !yield(Segment{Kind: Prose, Line: line, Start: start + at, End: start + i}) {
			return false
		}
		if !yield(Segment{Kind: Span, Line: line, Start: start + i, End: start + j}) {
			return false
		}
		at = j
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

// closingKeyword matches a reference GitHub would close an issue on. It reads
// only the direct adjacency of keyword, optional colon, space and reference,
// so the detection is limited to it as well.
//
// Unexported, and the reading exported instead as ClosingReferences: a caller
// handed the pattern would derive the boundary and the keyword set from it for
// itself, which is the second implementation this one exists to be instead of.
var closingKeyword = regexp.MustCompile(
	`(?i)(?:^|[^[:alnum:]])(?:close[sd]?|fix(?:e[sd])?|resolve[sd]?):?[[:space:]]+([[:alnum:]_.-]+/[[:alnum:]_.-]+)?#([0-9]+)`)

// ClosingReference is one reference a closing keyword names, and the run of the
// body it sits in.
type ClosingReference struct {
	// Repo is the owner/repo a qualified reference names, and empty for a bare
	// #N — which is how the body wrote it.
	Repo string
	// Number is the issue or pull request the reference names.
	Number int
	// Kind is the run the reference sits in. GitHub closes on a Prose one and
	// on no other, so a consumer keeps one side of this and drops the rest.
	Kind Kind
}

// ClosingReferences returns every closing-keyword reference in body, in the
// order the body writes them.
//
// One reading for both the judgements made about a closing keyword: the
// refusal below wants the references GitHub will not read, and pr context's
// linked_issues the ones it will. Split in two, the two would come to disagree
// about the same body — as they did, over an underscore before the keyword and
// over a keyword quoted in code.
//
// The match runs on each segment's own text rather than on the body at the
// segment's offsets, which is what makes the pattern's leading ^ mean the start
// of a run. A reference is therefore attributed to the run it sits in, a
// keyword parted from its number by a line end is read by nobody, and both
// follow from Segments rather than from a rule of their own.
func ClosingReferences(body string) []ClosingReference {
	var out []ClosingReference
	for s := range Segments(body) {
		for _, m := range closingKeyword.FindAllStringSubmatch(body[s.Start:s.End], -1) {
			out = append(out, ClosingReference{Repo: m[1], Number: atoi(m[2]), Kind: s.Kind})
		}
	}
	return out
}

// hasQuotedClosingKeyword reports whether body holds a closing keyword where
// GitHub will not read it as one: inside a fence, or inside a code span.
func hasQuotedClosingKeyword(body string) bool {
	for _, ref := range ClosingReferences(body) {
		if ref.Kind != Prose {
			return true
		}
	}
	return false
}

// atoi reads the digits of a reference.
//
// The pattern matched digits, so this cannot fail on anything that reaches it;
// a number too large for an int comes back as zero rather than as a reason to
// abandon the whole body.
func atoi(s string) int {
	n, _ := strconv.Atoi(s)
	return n
}

// RefuseQuotedClosingKeyword is what to say about a pull request body that
// holds a closing keyword where GitHub will not read it as one — inside a
// fence, or inside a code span — and the empty string for one that does not.
//
// Here rather than with the gh shim that first made the judgement, for the
// reason RefuseBareHashRefs is: ghapi writes pull request bodies in process,
// where the shim never sees them, and two implementations of the same rule are
// how the two ends of it come to disagree. Where the body came from is the
// caller's to name, above or in front of this.
func RefuseQuotedClosingKeyword(body string) string {
	if !hasQuotedClosingKeyword(body) {
		return ""
	}
	// An interpreted string because it quotes a backtick, which a raw one
	// cannot hold.
	return "GitHub does not read Closes/Fixes/Resolves #N as a closing keyword inside a\n" +
		"code span or a code block, so merging the pull request leaves the issue open.\n" +
		"\n" +
		"Fix: if the issue is meant to close on the merge, write the keyword bare:\n" +
		"  Closes #656\n" +
		"To quote or document the keyword instead, replace the real number with a\n" +
		"placeholder (`Closes #N` — without a number it is not detected)."
}

// nextCodeSpan returns the bounds of the first code span at or after from,
// backticks included.
//
// CommonMark pairs a span by the length of its backtick run: a run of N opens
// a span that the next run of exactly N closes, which is how text that itself
// holds a backtick is quoted — inside a run of two, a lone backtick is content
// rather than a delimiter. A run with no partner is literal text, and the scan
// resumes just after it rather than after the runs it searched past, so that
// those runs are still free to pair with each other.
//
// Where this departs from CommonMark is the line: a span there may continue
// across a soft break, and one that does is missed here — the text either side
// comes back as prose, so a bare reference inside it is counted and a
// placeholder inside it is substituted. That is the deviation and not a
// simplification of one. It is kept because the line is the unit every caller
// reads in: BareHashRefs tokenises per line, and the section check maps a
// segment's line onto a line index. No draft has written such a span, and the
// day one does the fix is a wider change than this reader.
//
// The one thing CommonMark does that genuinely does not move a boundary is
// stripping a space from each end of a span's content, and only the boundaries
// are asked for here.
func nextCodeSpan(text string, from int) (start, end int, ok bool) {
	for i := from; ; {
		k := strings.IndexByte(text[i:], '`')
		if k < 0 {
			return 0, 0, false
		}
		i += k
		n := runLength(text, i)
		if j, found := closingRun(text, i+n, n); found {
			return i, j + n, true
		}
		i += n
	}
}

// closingRun finds the run of exactly n backticks that closes a span opened at
// from, skipping over the runs of any other length between.
func closingRun(text string, from, n int) (int, bool) {
	for i := from; ; {
		k := strings.IndexByte(text[i:], '`')
		if k < 0 {
			return 0, false
		}
		i += k
		m := runLength(text, i)
		if m == n {
			return i, true
		}
		i += m
	}
}

// runLength is the length of the run of text[i] starting at i. It is the one
// measurement both constructs are delimited by.
func runLength(text string, i int) int {
	j := i
	for j < len(text) && text[j] == text[i] {
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
