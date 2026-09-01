package ghshim

import (
	"regexp"
	"strings"
)

// scanned is one walk of the argv: whether the repository was named, the first
// positional argument, and whichever body the command carries. One walk answers
// all four because the flags have to be parsed the same way for each of them.
type scanned struct {
	hasRepo    bool
	positional string
	inlineBody string
	bodyFile   string
}

// scan walks the arguments that follow the noun and the verb.
//
// GH_REPO never appears in the argv — the shell consumes an assignment before
// the command sees it — so a value there is folded in at the start. Whether it
// was written in front of the command or exported earlier cannot be told apart,
// and need not be: either way gh resolves the repository from it rather than
// from the working directory.
func scan(args []string, vf valueFlags, bf bodyFlags, ghRepo string) scanned {
	s := scanned{hasRepo: ghRepo != ""}
	endOfFlags := false

	for i := 0; i < len(args); i++ {
		tok := args[i]
		if endOfFlags {
			if s.positional == "" {
				s.positional = tok
			}
			continue
		}

		switch {
		case tok == "--":
			endOfFlags = true

		case strings.HasPrefix(tok, "--") && strings.Contains(tok, "="):
			name, value, _ := strings.Cut(tok, "=")
			if name == "--repo" {
				s.hasRepo = true
			}
			s.recordBody(strings.TrimPrefix(name, "--"), value, bf.inlineLong, bf.fileLong)

		case strings.HasPrefix(tok, "--"):
			if tok == "--repo" {
				s.hasRepo = true
			}
			name := strings.TrimPrefix(tok, "--")
			if vf.long[name] && i+1 < len(args) {
				i++
				s.recordBody(name, args[i], bf.inlineLong, bf.fileLong)
			}

		case strings.HasPrefix(tok, "-") && len(tok) > 1:
			// A run of short options can be written as one token, attached to
			// its value or not: -yd, -Rowner/repo, -F=path.
			rest := tok[1:]
			for rest != "" {
				ch := rest[0]
				rest = rest[1:]
				if ch == 'R' {
					s.hasRepo = true
				}
				if strings.IndexByte(vf.short, ch) < 0 {
					continue
				}
				var value string
				switch {
				case rest != "":
					// One leading = is dropped, as pflag does: the value of
					// -F=x is x and the value of -F==x is =x. Keeping it made
					// the body file "=path", which then failed to open and
					// blocked the command for the wrong reason.
					value = strings.TrimPrefix(rest, "=")
				case i+1 < len(args):
					i++
					value = args[i]
				}
				s.recordBody(string(ch), value, bf.inlineShort, bf.fileShort)
				rest = ""
			}

		default:
			if s.positional == "" {
				s.positional = tok
			}
		}
	}
	return s
}

// recordBody keeps the value if the flag is one of the two that carry a body.
// The names to compare against are passed in because the long and the short
// spelling of the same flag register under different names.
func (s *scanned) recordBody(name, value, inlineName, fileName string) {
	if name == "" {
		return
	}
	switch name {
	case inlineName:
		s.inlineBody = value
	case fileName:
		s.bodyFile = value
	}
}

// A positional argument naming a repository. OWNER/REPO and HOST/OWNER/REPO are
// told from a bare REPO by the number of slashes.
var repoPositional = regexp.MustCompile(
	`^(https?://[^/[:space:]]+/[^/[:space:]]+/[^/[:space:]]+(\.git)?/?` +
		`|[A-Za-z0-9_.][A-Za-z0-9_.-]*/[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)?)$`)

// A positional argument naming one issue or pull request in full. The shape of
// the URL is matched rather than "does it contain a slash", because a branch
// name contains one too.
var issueURL = regexp.MustCompile(
	`^https?://[^/[:space:]]+/[^/[:space:]]+/[^/[:space:]]+/(issues|pull)/[0-9]+/?$`)

// isExplicit reports whether the command names its own repository.
//
// The invariant is that the repository is not resolved from the working
// directory; -R is one sufficient condition for that and not the only one, and
// which conditions exist differs per noun, so the test does too.
func isExplicit(c command, s scanned) bool {
	switch {
	case c.noun == "repo" && c.verb == "rename":
		// The positional is the new repository name, so it names nothing.
		return s.hasRepo
	case c.noun == "repo":
		// These have no -R at all, and gh resolves GH_REPO only as a default
		// for -R, so the positional is the only way left.
		return repoPositional.MatchString(s.positional)
	case (c.noun == "issue" || c.noun == "pr") && c.verb == "create":
		// No selector, so there is no URL to carry a repository.
		return s.hasRepo
	case c.noun == "issue" || c.noun == "pr":
		return s.hasRepo || issueURL.MatchString(s.positional)
	default:
		// A release tag and a label name are not repositories.
		return s.hasRepo
	}
}
