package ghshim

import (
	"regexp"
	"strings"
)

// arg is one item a walk found: an option, or a positional argument, which
// arrives with an empty name.
type arg struct {
	// long says the option was spelled with two dashes, which is what tells
	// --body from the -b of a run of short options; name is the long name in
	// that case and the one letter otherwise.
	long bool
	name string
	// value is what the option carried, or the positional itself. hasValue
	// says whether an option carried one at all: one written at the end with
	// nothing left to take is reported without a value rather than with an
	// empty one, so that it cannot erase a value the same flag gave earlier.
	value    string
	hasValue bool
}

// walk reports every option and every positional in the arguments that follow
// a command, in the order they were written.
//
// It is the one place that knows gh's argument grammar — the runs of short
// options, the value written attached or apart, the one = that pflag drops,
// and the -- that ends the flags. The rules that judge a noun and a verb and
// the one that judges gh api read the same argv for different things, and a
// second walk written for the second reader is how the two would come to
// disagree; the -F=path mis-parse of #71 was one such disagreement inside a
// single walk.
func walk(args []string, vf valueFlags, visit func(arg)) {
	endOfFlags := false

	for i := 0; i < len(args); i++ {
		tok := args[i]
		if endOfFlags {
			visit(arg{value: tok, hasValue: true})
			continue
		}

		switch {
		case tok == "--":
			endOfFlags = true

		case strings.HasPrefix(tok, "--"):
			name, value, attached := strings.Cut(tok[2:], "=")
			if !attached && vf.long[name] && i+1 < len(args) {
				i++
				value, attached = args[i], true
			}
			visit(arg{long: true, name: name, value: value, hasValue: attached})

		case strings.HasPrefix(tok, "-") && len(tok) > 1:
			// A run of short options can be written as one token, attached to
			// its value or not: -yd, -Rowner/repo, -F=path.
			rest := tok[1:]
			for rest != "" {
				ch := rest[0]
				rest = rest[1:]
				if strings.IndexByte(vf.short, ch) < 0 {
					visit(arg{name: string(ch)})
					continue
				}
				var value string
				var hasValue bool
				switch {
				case rest != "":
					// One leading = is dropped, as pflag does: the value of
					// -F=x is x and of -F==x is =x. Keeping it made the body
					// file "=path" and blocked the command for the wrong
					// reason.
					value, hasValue = strings.TrimPrefix(rest, "="), true
				case i+1 < len(args):
					i++
					value, hasValue = args[i], true
				}
				visit(arg{name: string(ch), value: value, hasValue: hasValue})
				rest = ""
			}

		default:
			visit(arg{value: tok, hasValue: true})
		}
	}
}

// scanned is what one walk of the argv found for the rules that judge a noun
// and a verb. One walk answers all four, because the flags have to be parsed
// the same way for each of them.
type scanned struct {
	hasRepo    bool
	positional string
	inlineBody string
	bodyFile   string
}

// scan reads the arguments that follow the noun and the verb. ghRepo is folded
// in at the start, since it never appears in the argv; see Env.GHRepo.
func scan(args []string, vf valueFlags, bf bodyFlags, ghRepo string) scanned {
	s := scanned{hasRepo: ghRepo != ""}

	walk(args, vf, func(a arg) {
		switch {
		case a.name == "":
			if s.positional == "" {
				s.positional = a.value
			}
			return
		// gh spells its own repository flag -R/--repo, and an option after --
		// is a positional rather than that flag, which the case above catches.
		case a.long && a.name == "repo", !a.long && a.name == "R":
			s.hasRepo = true
		}
		if !a.hasValue {
			return
		}
		if a.long {
			s.recordBody(a.name, a.value, bf.inlineLong, bf.fileLong)
		} else {
			s.recordBody(a.name, a.value, bf.inlineShort, bf.fileShort)
		}
	})
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

// explicitness is how a command can name its own repository, which differs per
// noun. Naming the ways once is what keeps the test and the guidance a refusal
// offers from drifting apart: both switch over this.
type explicitness int

const (
	// byFlag: -R/--repo or GH_REPO. Release tags and label names are not
	// repositories.
	byFlag explicitness = iota
	// byFlagOrURL: also a selector given as a full issue or pull request URL.
	byFlagOrURL
	// byFlagNoSelector: gh issue create and gh pr create take no selector, so
	// no URL can carry a repository.
	byFlagNoSelector
	// byPositional: gh repo edit and its siblings have no -R at all, so the
	// positional is the only way left.
	byPositional
	// byFlagNotPositional: gh repo rename's positional is the new repository
	// name, so it names nothing.
	byFlagNotPositional
)

func classify(c command) explicitness {
	switch {
	case c.noun == "repo" && c.verb == "rename":
		return byFlagNotPositional
	case c.noun == "repo":
		return byPositional
	case c.noun != "issue" && c.noun != "pr":
		return byFlag
	case c.verb == "create":
		return byFlagNoSelector
	default:
		return byFlagOrURL
	}
}

// isExplicit reports whether the command names its own repository.
func isExplicit(c command, s scanned) bool {
	switch classify(c) {
	case byPositional:
		return repoPositional.MatchString(s.positional)
	case byFlagOrURL:
		return s.hasRepo || issueURL.MatchString(s.positional)
	default:
		return s.hasRepo
	}
}
