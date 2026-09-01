// Package issue reads GitHub issues the way this repository's skills need
// them: the section schema that issue bodies are written against, and the
// parent/child hierarchy behind them.
//
// issue-draft, issue-handle, deep-review and github-sub-issues all read it.
package issue

import (
	"errors"
	"fmt"
	"slices"
	"strings"
	"unicode"
)

// Locale is the language an issue body is written in.
type Locale string

// The locales the section table carries a heading for.
const (
	JA Locale = "ja"
	EN Locale = "en"
)

// locales is the order the two appear in wherever both are rendered.
var locales = []Locale{JA, EN}

// Kind is what an issue is in the parent/child scheme: a leaf issue that one
// pull request closes, a sub of a parent, or the parent itself.
type Kind string

// The issue kinds. The set is declared here rather than derived from the
// section table, because a kind may require no section at all.
const (
	Leaf   Kind = "leaf"
	Sub    Kind = "sub"
	Parent Kind = "parent"
)

var kinds = []Kind{Leaf, Sub, Parent}

// Headings is one section's heading in each locale.
//
// A struct rather than a map because the locales are a closed set — the check
// below needs per-locale script detection, which a table column cannot carry —
// and because a map would render its keys in sorted order, while the contract
// is the order they are declared in.
type Headings struct {
	JA string `json:"ja"`
	EN string `json:"en"`
}

// For returns the heading in l.
func (h Headings) For(l Locale) string {
	if l == JA {
		return h.JA
	}
	return h.EN
}

// locale reports which locale h uses for heading, if either.
func (h Headings) locale(heading string) (Locale, bool) {
	switch heading {
	case h.JA:
		return JA, true
	case h.EN:
		return EN, true
	}
	return "", false
}

// Section is one row of the table: a key that skills identify a section by, and
// everything that depends on the key.
//
// The key exists because an issue body's `## ` headings are read by people and
// searched for by other skills at the same time. Depending on the heading text
// breaks in both directions in an English repository — the author cannot choose
// the language the reader sees, and the reader cannot find a translated
// heading — so the identity is the key and this is the only place that maps it
// to words.
type Section struct {
	Key      string   `json:"key"`
	Headings Headings `json:"headings"`
	// The table's "required on" column verbatim. Empty means
	// optional everywhere, which is not the same as "optional for this kind":
	// consumers need both, and deriving one from the other would put a copy of
	// the table on their side.
	RequiredOn []Kind `json:"required_on"`
	// Whether a repository issue template may rename this
	// section's heading. A machine-consumed section may not.
	TemplateMappable bool `json:"template_mappable"`
	// The fixed text that says the section has nothing in it,
	// null for a section that has no such marker.
	NoneMarkers *Headings `json:"none_markers"`
}

// table is the schema. Its source is 178inaba/dotfiles#85.
var table = []Section{
	{Key: "background", Headings: Headings{"背景・目的", "Background / Purpose"}, RequiredOn: []Kind{Leaf, Sub, Parent}, TemplateMappable: true},
	{Key: "depends_on", Headings: Headings{"依存", "Depends on"}, RequiredOn: []Kind{Sub}, NoneMarkers: &Headings{"なし", "None"}},
	{Key: "requirements", Headings: Headings{"要件", "Requirements"}, RequiredOn: []Kind{Leaf, Sub}, TemplateMappable: true},
	{Key: "acceptance", Headings: Headings{"受け入れ条件", "Acceptance criteria"}, RequiredOn: []Kind{Leaf, Sub, Parent}, TemplateMappable: true},
	{Key: "affected_code", Headings: Headings{"影響範囲・関連コード", "Affected code"}, RequiredOn: []Kind{Leaf, Sub}, TemplateMappable: true},
	{Key: "impl_notes", Headings: Headings{"実装方針の示唆", "Implementation notes (suggestions)"}, RequiredOn: []Kind{}, TemplateMappable: true},
	{Key: "deferred", Headings: Headings{"実装時判断に委ねる事項", "Deferred to implementer judgment"}, RequiredOn: []Kind{}, TemplateMappable: true},
	{Key: "out_of_scope", Headings: Headings{"スコープ外", "Out of scope"}, RequiredOn: []Kind{Leaf, Sub, Parent}, TemplateMappable: true},
	{Key: "composition", Headings: Headings{"構成（Sub-Issues）", "Structure (Sub-Issues)"}, RequiredOn: []Kind{Parent}},
	{Key: "cross_cutting", Headings: Headings{"横断ルール", "Cross-cutting rules"}, RequiredOn: []Kind{Parent}, TemplateMappable: true},
	{Key: "release_manual_steps", Headings: Headings{"リリース時の手動作業", "Manual release steps"}, RequiredOn: []Kind{Parent}, NoneMarkers: &Headings{"なし（全 Sub のマージで完了）", "None (completed by merging all Subs)"}},
}

// ErrSectionNotFound is Find's answer for a body that has no such section.
//
// A distinct error rather than an empty result, because the caller branches on
// it: an issue that does not carry the section is an ordinary case, while a
// body that could not be read is not, and conflating them would let a failed
// fetch pass as "this issue has no dependencies".
var ErrSectionNotFound = errors.New("section not found")

// ErrEmptyInput is Find's answer for a body with nothing in it.
//
// It is not a missing section. `gh issue view > file` creates the file before
// it fails, so the two are easy to confuse, and a caller that read "no such
// section" from a failed fetch would decide something it has no grounds for.
// The caller names the file, since it is the one holding the path.
var ErrEmptyInput = errors.New("input file is empty")

// Schema returns one row of the table.
//
// It takes no locale: the callers are consumers, which accept a heading in
// either language because they do not know which one the issue they are
// reading was written in.
func Schema(key string) (Section, error) {
	s, ok := row(key)
	if !ok {
		return Section{}, unknownKey(key)
	}
	return s, nil
}

// Keys is every section key, in the order the schema declares them.
//
// The keys are as much a part of the contract as any field name: `ccx issue
// sections schema` takes one, and the skills that read an issue body name them.
func Keys() []string {
	out := make([]string, 0, len(table))
	for _, s := range table {
		out = append(out, s.Key)
	}
	return out
}

// ListedSection is one row as the drafting side needs it, with the heading
// already chosen and the requirement already decided.
type ListedSection struct {
	Key string `json:"key"`
	// The heading for the locale asked for, so that the drafting side does not
	// choose between the two itself.
	Heading string `json:"heading"`
	// Whether the kind asked for requires this section. Every section is
	// listed, required or not, because one that is optional may still appear.
	Required bool `json:"required"`
	// The kinds that require the section, which is what a caller checking a
	// kind other than the one asked for needs.
	RequiredOn []Kind `json:"required_on"`
	// Whether a repository issue template may rename this section's heading.
	// A machine-consumed section may not.
	TemplateMappable bool `json:"template_mappable"`
	// The fixed text that says the section has nothing in it, null for a
	// section that has no such marker.
	NoneMarkers *Headings `json:"none_markers"`
}

// Listing is the whole table rendered for one locale and kind.
type Listing struct {
	Locale   Locale          `json:"locale"`
	Kind     Kind            `json:"kind"`
	Sections []ListedSection `json:"sections"`
}

// List returns every section, not only the ones kind requires: a key that is
// not required may still appear, and filtering here would take that choice
// away from whoever is drafting.
func List(l Locale, k Kind) (Listing, error) {
	if err := validLocale(l); err != nil {
		return Listing{}, err
	}
	if err := validKind(k); err != nil {
		return Listing{}, err
	}

	out := Listing{Locale: l, Kind: k, Sections: make([]ListedSection, 0, len(table))}
	for _, s := range table {
		out.Sections = append(out.Sections, ListedSection{
			Key:              s.Key,
			Heading:          s.Headings.For(l),
			Required:         s.requiredOn(k),
			RequiredOn:       s.RequiredOn,
			TemplateMappable: s.TemplateMappable,
			NoneMarkers:      s.NoneMarkers,
		})
	}
	return out, nil
}

// Found is a section located in a body.
type Found struct {
	Key string `json:"key"`
	// The language of the heading that matched, which tells the
	// caller nothing it asked for and everything about what it is reading.
	Locale  Locale `json:"locale"`
	Heading string `json:"heading"`
	// Excludes the heading line, ends before the next `## `, and has its
	// surrounding blank lines and any carriage returns removed — the last
	// because a body written in GitHub's web editor arrives over the API with
	// CRLF, and consumers compare it against fixed markers.
	Body string `json:"body"`
}

// Find locates the section key in a body, accepting the heading in either
// locale.
func Find(body string, key string) (Found, error) {
	s, ok := row(key)
	if !ok {
		return Found{}, unknownKey(key)
	}
	if strings.TrimSpace(body) == "" {
		return Found{}, ErrEmptyInput
	}

	lines := strings.Split(body, "\n")
	start, end := -1, len(lines)
	var matched string
	var matchedLocale Locale
	for _, h := range headingsIn(lines) {
		if start < 0 {
			if l, ok := s.Headings.locale(h.text); ok {
				start, matched, matchedLocale = h.line, h.text, l
			}
			continue
		}
		end = h.line
		break
	}
	if start < 0 {
		return Found{}, fmt.Errorf("%q: %w", key, ErrSectionNotFound)
	}

	return Found{
		Key:     key,
		Locale:  matchedLocale,
		Heading: matched,
		Body:    normalizeBody(lines[start+1 : end]),
	}, nil
}

// heading is one `## ` line and where it was.
type heading struct {
	// line is the index in the slice that was scanned, not a line number: only
	// the distance between two headings matters here.
	line int
	text string
}

// headingsIn returns the section headings of a body.
//
// Only `## ` opens a section — a deeper heading belongs to the section it is
// in — and a line inside a fenced block is text, so that a draft showing the
// body template in an example does not appear to declare the sections it
// mentions. Only backtick fences count, as they are the only ones the drafts
// use.
func headingsIn(lines []string) []heading {
	var out []heading
	fenced := false
	for i, line := range lines {
		if strings.HasPrefix(strings.TrimLeft(line, " \t"), "```") {
			fenced = !fenced
			continue
		}
		if fenced || !strings.HasPrefix(line, "## ") {
			continue
		}
		out = append(out, heading{line: i, text: strings.Trim(line[len("## "):], " \t\r")})
	}
	return out
}

// normalizeBody drops the blank lines around a section and the carriage
// returns inside it.
func normalizeBody(lines []string) string {
	trimmed := make([]string, 0, len(lines))
	for _, line := range lines {
		trimmed = append(trimmed, strings.TrimSuffix(line, "\r"))
	}
	first, last := -1, -1
	for i, line := range trimmed {
		if strings.TrimLeft(line, " \t") != "" {
			if first < 0 {
				first = i
			}
			last = i
		}
	}
	if first < 0 {
		return ""
	}
	return strings.Join(trimmed[first:last+1], "\n")
}

func row(key string) (Section, bool) {
	for _, s := range table {
		if s.Key == key {
			return s, true
		}
	}
	return Section{}, false
}

func (s Section) requiredOn(k Kind) bool {
	return slices.Contains(s.RequiredOn, k)
}

func unknownKey(key string) error {
	return fmt.Errorf("unknown section key: %s (see the section table in internal/issue)", key)
}

func validLocale(l Locale) error {
	if slices.Contains(locales, l) {
		return nil
	}
	return fmt.Errorf("unsupported locale: %s (supported: %s)\n"+
		"add a locale by extending the section table in internal/issue; its Headings type lists every place to touch", l, join(locales))
}

func validKind(k Kind) error {
	if slices.Contains(kinds, k) {
		return nil
	}
	return fmt.Errorf("unsupported kind: %s (supported: %s)", k, join(kinds))
}

// join renders a set for an error message.
func join[T ~string](vs []T) string {
	s := make([]string, len(vs))
	for i, v := range vs {
		s[i] = string(v)
	}
	return strings.Join(s, ", ")
}

// hasJapaneseScript reports whether a heading contains a character only
// Japanese uses.
//
// The check is by character class rather than by dictionary: a heading is one
// short phrase, and asking whether it contains kana or a Han character
// separates the two locales without knowing either language.
func hasJapaneseScript(s string) bool {
	for _, r := range s {
		if unicode.In(r, unicode.Hiragana, unicode.Katakana, unicode.Han) {
			return true
		}
	}
	return false
}
