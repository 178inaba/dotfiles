package issue

import (
	"fmt"
	"strings"
)

// Violation is one thing wrong with a draft.
//
// Class exists because the caller reports the classes apart: it exits with the
// status of the earliest one present, so that a script can tell "a required
// section is missing" from "a heading is in the wrong language" without parsing
// the messages.
type Violation struct {
	Class   Class
	Message string
}

// Class is what kind of violation a check found.
//
// Not a status: turning one into a number a process exits with is a boundary
// this package does not sit on, and internal/cmd owns that mapping. The
// declaration order is how early each class stops a draft from being usable,
// which is what lets one answer stand for a set of them.
type Class int

const (
	// MissingSection is a section the kind requires and the draft lacks.
	MissingSection Class = iota
	// UnknownHeading is a `## ` heading that is in neither the table nor the
	// template mapping.
	UnknownHeading
	// MappedMachineKey is a template renaming a heading that other skills find
	// by its text.
	MappedMachineKey
	// HeadingLocaleMismatch is a canonical heading in the other language.
	HeadingLocaleMismatch
)

// Mapping is one entry of a repository issue template's heading mapping: the
// schema key, and the heading that template uses for it.
type Mapping struct {
	Key     string
	Heading string
}

// ParseMapping reads the mapping file, one `<key> <heading>` per line, blank
// lines and `#` comments ignored.
//
// The format is this loose because the person writing it is drafting an issue
// against a repository template, not maintaining a data file. Every way of
// getting it wrong is an error rather than a best effort: silently keeping one
// of two entries for the same key would fail later as "a required section is
// missing", which points at the draft instead of at the mapping.
func ParseMapping(content string) ([]Mapping, error) {
	var out []Mapping
	byKey := map[string]bool{}
	byHeading := map[string]bool{}
	for line := range strings.Lines(content) {
		line = strings.TrimRight(line, "\n")
		if t := strings.TrimSpace(line); t == "" || strings.HasPrefix(t, "#") {
			continue
		}
		key, heading, ok := strings.Cut(strings.TrimLeft(line, " \t"), " ")
		heading = strings.TrimSpace(heading)
		if !ok || key == "" || heading == "" {
			return nil, fmt.Errorf("malformed mapping line (expected: <key> <template heading>): %s", line)
		}
		if _, known := row(key); !known {
			return nil, fmt.Errorf("unknown section key in the mapping: %s (see the section table in internal/issue)", key)
		}
		if byKey[key] {
			return nil, fmt.Errorf("section key mapped more than once: %s", key)
		}
		if byHeading[heading] {
			return nil, fmt.Errorf("template heading mapped from more than one key: %s", heading)
		}
		byKey[key], byHeading[heading] = true, true
		out = append(out, Mapping{Key: key, Heading: heading})
	}
	return out, nil
}

// Check reports what is wrong with a draft written for locale and kind.
//
// Four rules, and the division of labour between the first two is deliberate.
// A heading counts as known if it is the canonical heading in *either* locale,
// not only in the one being checked — read strictly, every heading that is not
// in the mapping would have to match the checked locale exactly, rule 4 could
// never fire, and a draft with its headings in two languages would pass as a
// pile of unknown ones. Loosened, rule 2 catches headings from outside the
// table and rule 4 catches the ones from the other locale, which is what a
// mixed draft actually is.
//
// Mapped headings are exempt from rule 4: where a repository template names the
// sections, the template's language wins.
func Check(draft string, l Locale, k Kind, mapping []Mapping) ([]Violation, error) {
	if err := validLocale(l); err != nil {
		return nil, err
	}
	if err := validKind(k); err != nil {
		return nil, err
	}

	keyOfMapped := map[string]string{}
	headingOfMapped := map[string]string{}
	for _, m := range mapping {
		keyOfMapped[m.Heading] = m.Key
		headingOfMapped[m.Key] = m.Heading
	}

	// resolved pairs each heading in the draft with the key it names, if any.
	type resolved struct {
		heading string
		key     string
		mapped  bool
	}
	var found []resolved
	present := map[string]bool{}
	for _, h := range headingsIn(strings.Split(draft, "\n")) {
		r := resolved{heading: h.text}
		if key, ok := keyOfMapped[h.text]; ok {
			r.key, r.mapped = key, true
		} else {
			for _, s := range table {
				if _, ok := s.Headings.locale(h.text); ok {
					r.key = s.Key
					break
				}
			}
		}
		if r.key != "" {
			present[r.key] = true
		}
		found = append(found, r)
	}

	var out []Violation
	for _, s := range table {
		if !s.requiredOn(k) || present[s.Key] {
			continue
		}
		expected := s.Headings.For(l)
		if h, ok := headingOfMapped[s.Key]; ok {
			expected = h
		}
		out = append(out, Violation{MissingSection,
			fmt.Sprintf("missing required section: %s (expected heading: %q)", s.Key, expected)})
	}
	for _, r := range found {
		if r.key == "" {
			out = append(out, Violation{UnknownHeading, fmt.Sprintf("unknown heading: %q", r.heading)})
		}
	}
	for _, m := range mapping {
		if s, ok := row(m.Key); ok && !s.TemplateMappable {
			out = append(out, Violation{MappedMachineKey,
				fmt.Sprintf("machine-consumed key must keep its canonical heading: %s (mapped to %q)", m.Key, m.Heading)})
		}
	}
	for _, r := range found {
		if r.key == "" || r.mapped || hasJapaneseScript(r.heading) == (l == JA) {
			continue
		}
		s, _ := row(r.key)
		out = append(out, Violation{HeadingLocaleMismatch,
			fmt.Sprintf("heading locale mismatch: %q is not %s (canonical %s heading for %s: %q)",
				r.heading, l, l, r.key, s.Headings.For(l))})
	}
	return out, nil
}

// Worst returns the class a set of violations reports as a whole, which is the
// earliest present, and whether there were any at all.
func Worst(vs []Violation) (Class, bool) {
	if len(vs) == 0 {
		return 0, false
	}
	worst := vs[0].Class
	for _, v := range vs[1:] {
		if v.Class < worst {
			worst = v.Class
		}
	}
	return worst, true
}
