package plandocs

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

var (
	// An import, captured in group 1. A Markdown link is matched beside it
	// and captured nowhere, so that a link's label is consumed whole rather
	// than read as prose: an @ there would otherwise take the rest of the link
	// for its path. A link target stops at whitespace, so a title — [x](p "T")
	// — is consumed with it. An import's @ has to open a word, which is what
	// separates @docs/x.md from the one in an e-mail address.
	importPattern = regexp.MustCompile(`\[[^\]\n]*\]\([^)\s]+(?:\s+"[^"\n]*")?\)|(?:^|\s)@(\S+)`)
	// Sentence punctuation an import written in prose ends up carrying, since
	// nothing but whitespace closes one. A path never ends in any of these.
	importTail = regexp.MustCompile(`[.,;:!?)\]]+$`)
	// A URL or a mailto:, which name something that is not a file here.
	schemePattern = regexp.MustCompile(`^[a-zA-Z][a-zA-Z0-9+.\-]*:`)
)

// imports reads every @ import out of text, in the order they appear, each as
// the path it writes with the @ and any #fragment already off. That is what a
// warning names, because it is what the author has to correct.
//
// Code is blanked out first, since the harness does not expand an import
// written in code. Which is code is ghmd's to say and not this package's: the
// edge rules of the harness's import parser are written down nowhere, so the
// reading here is CommonMark's — the one written definition, and the one the
// rest of this module already reads a body by.
func imports(text string) []string {
	var out []string
	for _, m := range importPattern.FindAllStringSubmatch(ghmd.BlankCode(text), -1) {
		if m[1] == "" {
			continue
		}
		target, _, _ := strings.Cut(importTail.ReplaceAllString(m[1], ""), "#")
		if target == "" {
			continue
		}
		out = append(out, target)
	}
	return out
}

// resolve turns a target written in file into the absolute path it names, and
// reports whether it names a file at all. A leading / is the filesystem root,
// which is what an import means by it.
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
