package plandocs

import (
	"path/filepath"
	"regexp"
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
}

var (
	// Both forms in one pass, so that the references come out in the order
	// they were written and a link's own label cannot be read a second time
	// as an import. Group 1 is a link's target, group 2 an import's.
	//
	// A link target stops at whitespace, which leaves a title — [x](p "T") —
	// behind and never matches a reference-style link, [x][ref]. An import's @
	// has to open a word, which is what separates @docs/x.md from the one in
	// an e-mail address.
	referencePattern = regexp.MustCompile(`\[[^\]\n]*\]\(([^)\s]+)\)|(?:^|\s)@(\S+)`)
	// A URL or a mailto:, which name something that is not a file here.
	schemePattern = regexp.MustCompile(`^[a-zA-Z][a-zA-Z0-9+.\-]*:`)
	// A fence opening or closing a code block, at either of the two spellings.
	fencePattern = regexp.MustCompile("^\\s{0,3}(```|~~~)")
)

// references reads every link and import out of text, in the order they
// appear.
//
// Code is stripped first, exactly as Claude Code's import parser skips it, so
// that a backticked path is a mention rather than a reference. A fragment
// comes off here and nowhere else, which is what makes a link to one heading
// of a document and a link to the document the same reference.
func references(text string) []reference {
	var out []reference
	for _, m := range referencePattern.FindAllStringSubmatch(stripCode(text), -1) {
		target, isImport := m[1], false
		if target == "" {
			target, isImport = m[2], true
		}
		if target, _, _ = strings.Cut(target, "#"); target == "" {
			continue
		}
		out = append(out, reference{target: target, isImport: isImport})
	}
	return out
}

// stripCode blanks out fenced blocks and inline code spans, so that what is
// left is the prose the two forms are read out of.
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
		case strings.IndexByte(line, '`') >= 0:
			lines[i] = stripSpans(line)
		}
	}
	return strings.Join(lines, "\n")
}

// stripSpans blanks out the code spans in one line. A run of backticks is
// closed by a run of the same length, which is how a span holds a backtick.
//
// A span is padded to its own width rather than deleted, so that what follows
// it keeps the character in front of it: an import written after a span is
// still preceded by a space, and still an import.
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
func resolve(target, file, home string) (string, bool) {
	if schemePattern.MatchString(target) {
		return "", false
	}
	switch {
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
	return strings.HasSuffix(target, ".md")
}
