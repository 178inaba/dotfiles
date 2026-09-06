package plandocs

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghmd"
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
	// A link target stops at whitespace, so a title — [x](p "T") — is matched
	// beside it rather than taken for part of the path, and a reference-style
	// link, [x][ref], never matches at all. An import's @ has to open a word,
	// which is what separates @docs/x.md from the one in an e-mail address.
	referencePattern = regexp.MustCompile(`\[[^\]\n]*\]\(([^)\s]+)(?:\s+"[^"\n]*")?\)|(?:^|\s)@(\S+)`)
	// Sentence punctuation an import written in prose ends up carrying, since
	// nothing but whitespace closes one. A path never ends in any of these.
	importTail = regexp.MustCompile(`[.,;:!?)\]]+$`)
	// A URL or a mailto:, which name something that is not a file here.
	schemePattern = regexp.MustCompile(`^[a-zA-Z][a-zA-Z0-9+.\-]*:`)
)

// references reads every link and import out of text, in the order they
// appear.
//
// Code is blanked out first, so that a backticked path is a mention rather
// than a reference. Which is code is ghmd's to say and not this package's:
// the specification asks for the code Claude Code's import parser skips, and
// the edge rules of that parser are written down nowhere, so the reading here
// is CommonMark's — the one written definition, and the one the rest of this
// module already reads a body by. A fragment comes off here and nowhere else,
// which is what makes a link to one heading of a document and a link to the
// document the same reference.
func references(text string) []reference {
	var out []reference
	for _, m := range referencePattern.FindAllStringSubmatch(ghmd.BlankCode(text), -1) {
		target, isImport := m[1], false
		if target == "" {
			target, isImport = importTail.ReplaceAllString(m[2], ""), true
		}
		if target, _, _ = strings.Cut(target, "#"); target == "" {
			continue
		}
		out = append(out, reference{target: target, isImport: isImport})
	}
	return out
}

// resolve turns a target written in file into the absolute path it names, and
// reports whether it names a file at all.
//
// A leading / is the filesystem root, which is what an import means by it and
// not what GitHub renders a link with one as — GitHub reads it against the
// repository root. Nothing here writes that form, and one that did would be
// reported as a link to a file that is not there rather than read silently
// from the wrong place.
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
