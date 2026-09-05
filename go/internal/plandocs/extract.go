package plandocs

import (
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

// reference is one mention of another file, whichever of the two forms wrote
// it. The walk treats them alike; only the depth-0 closure cares which is
// which, since the harness expands imports and nothing expands links.
type reference struct {
	// target is the path as written, with the @ sigil and any #fragment
	// already off. It is what a warning names, because it is what the author
	// has to correct.
	target   string
	isImport bool
	at       int
}

var (
	// A Markdown inline link. The target stops at whitespace so that a title —
	// [x](path "Title") — leaves the title behind, and reference-style links,
	// [x][ref], never match at all.
	linkPattern = regexp.MustCompile(`\[[^\]\n]*\]\(([^)\s]+)\)`)
	// An import. The @ has to open a word, which is what separates
	// @docs/x.md from the one in an e-mail address.
	importPattern = regexp.MustCompile(`(?:^|\s)@(\S+)`)
	// A URL or a mailto:, which name something that is not a file here.
	schemePattern = regexp.MustCompile(`^[a-zA-Z][a-zA-Z0-9+.\-]*:`)
	// A fence opening or closing a code block, at either of the two spellings.
	fencePattern = regexp.MustCompile("^\\s{0,3}(```|~~~)")
)

// references reads every link and import out of text, in the order they
// appear.
//
// Code is stripped first, exactly as Claude Code's import parser skips it, so
// that a backticked path is a mention rather than a reference.
func references(text string) []reference {
	text = stripCode(text)

	out := []reference{}
	for _, m := range linkPattern.FindAllStringSubmatchIndex(text, -1) {
		out = append(out, reference{target: text[m[2]:m[3]], at: m[0]})
	}
	for _, m := range importPattern.FindAllStringSubmatchIndex(text, -1) {
		out = append(out, reference{target: text[m[2]:m[3]], isImport: true, at: m[0]})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].at < out[j].at })
	return out
}

// stripCode blanks out fenced blocks and inline code spans, keeping the rest
// of the text where it was so that positions still order the references.
func stripCode(text string) string {
	lines := strings.Split(text, "\n")
	fence := ""
	for i, line := range lines {
		switch open := fencePattern.FindStringSubmatch(line); {
		case fence != "":
			// Only the fence that opened the block can close it, so that a ```
			// inside a ~~~ block stays content.
			if len(open) > 0 && open[1] == fence {
				fence = ""
			}
			lines[i] = ""
		case len(open) > 0:
			fence = open[1]
			lines[i] = ""
		default:
			lines[i] = stripSpans(line)
		}
	}
	return strings.Join(lines, "\n")
}

// stripSpans blanks out the code spans in one line. A run of backticks is
// closed by a run of the same length, which is how a span holds a backtick.
func stripSpans(line string) string {
	var b strings.Builder
	for i := 0; i < len(line); {
		if line[i] != '`' {
			b.WriteByte(line[i])
			i++
			continue
		}
		open := i
		for i < len(line) && line[i] == '`' {
			i++
		}
		run := line[open:i]
		shut := strings.Index(line[i:], run)
		if shut < 0 {
			// An unmatched run is ordinary text, and so is everything after
			// it.
			b.WriteString(line[open:])
			return b.String()
		}
		end := i + shut + len(run)
		b.WriteString(strings.Repeat(" ", end-open))
		i = end
	}
	return b.String()
}

// resolve turns a target written in file into the absolute path it names, and
// reports whether it names a file at all.
//
// A fragment is dropped first, so that a link to one heading of a document and
// a link to the document are the same target.
func resolve(target, file, home string) (string, bool) {
	target, _, _ = strings.Cut(target, "#")
	if target == "" || schemePattern.MatchString(target) {
		return "", false
	}
	switch {
	case target == "~":
		return "", false
	case strings.HasPrefix(target, "~/"):
		return filepath.Join(home, target[2:]), true
	case filepath.IsAbs(target):
		return filepath.Clean(target), true
	}
	return filepath.Join(filepath.Dir(file), target), true
}

// isDocument reports whether a target is one of the documents this walk
// lists. The closure follows every import regardless of extension, as the
// harness does; only what a planner is told to read is held to .md.
func isDocument(target string) bool {
	target, _, _ = strings.Cut(target, "#")
	return strings.HasSuffix(target, ".md")
}
