package issue_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/issue"
)

// A leaf draft needs background, requirements, acceptance, affected_code and
// out_of_scope; these are those five, in each locale.
const (
	jaLeaf = `## 背景・目的

なぜやるか。

## 要件

- ひとつ

## 受け入れ条件

- [ ] 通る

## 影響範囲・関連コード

` + "`a/b.go`" + `

## スコープ外

なし
`
	enLeaf = `## Background / Purpose

Why.

## Requirements

- one

## Acceptance criteria

- [ ] passes

## Affected code

` + "`a/b.go`" + `

## Out of scope

None
`
)

// swap replaces one heading in a draft, for the cases that need a single
// section to differ.
func swap(draft, from, to string) string {
	return strings.Replace(draft, "## "+from, "## "+to, 1)
}

func TestSchema(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		key  string
		want issue.Section
	}{
		{
			name: "carries both headings, the markers and the raw columns",
			key:  "depends_on",
			want: issue.Section{
				Key:              "depends_on",
				Headings:         issue.Headings{JA: "依存", EN: "Depends on"},
				RequiredOn:       []issue.Kind{issue.Sub},
				TemplateMappable: false,
				NoneMarkers:      &issue.Headings{JA: "なし", EN: "None"},
			},
		},
		{
			name: "a key without markers reports none",
			key:  "background",
			want: issue.Section{
				Key:              "background",
				Headings:         issue.Headings{JA: "背景・目的", EN: "Background / Purpose"},
				RequiredOn:       []issue.Kind{issue.Leaf, issue.Sub, issue.Parent},
				TemplateMappable: true,
				NoneMarkers:      nil,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, err := issue.Schema(tt.key)
			if err != nil {
				t.Fatalf("Schema(%q): %v", tt.key, err)
			}
			if diff := cmp.Diff(tt.want, got); diff != "" {
				t.Errorf("Schema(%q) (-want +got):\n%s", tt.key, diff)
			}
		})
	}
}

func TestSchemaUnknownKey(t *testing.T) {
	t.Parallel()

	_, err := issue.Schema("nope")
	if err == nil {
		t.Fatal("want an error, got nil")
	}
	if !strings.Contains(err.Error(), "unknown section key") {
		t.Errorf("Schema error = %v, want it to say the key is unknown", err)
	}
}

func TestList(t *testing.T) {
	t.Parallel()

	got, err := issue.List(issue.JA, issue.Sub)
	if err != nil {
		t.Fatalf("List: %v", err)
	}

	// Every key, in table order: the drafting side renders them in this order,
	// so a reordering is a visible change rather than an implementation detail.
	wantKeys := []string{
		"background", "depends_on", "requirements", "acceptance", "affected_code",
		"impl_notes", "deferred", "out_of_scope", "composition", "cross_cutting",
		"release_manual_steps",
	}
	var gotKeys []string
	for _, s := range got.Sections {
		gotKeys = append(gotKeys, s.Key)
	}
	if diff := cmp.Diff(wantKeys, gotKeys); diff != "" {
		t.Errorf("List keys (-want +got):\n%s", diff)
	}

	byKey := map[string]issue.ListedSection{}
	for _, s := range got.Sections {
		byKey[s.Key] = s
	}

	tests := []struct {
		name string
		key  string
		want issue.ListedSection
	}{
		{
			name: "renders the requested locale's heading and computes required for the kind",
			key:  "depends_on",
			want: issue.ListedSection{
				Key: "depends_on", Heading: "依存", Required: true,
				RequiredOn: []issue.Kind{issue.Sub}, TemplateMappable: false,
				NoneMarkers: &issue.Headings{JA: "なし", EN: "None"},
			},
		},
		{
			// required is per kind, required_on is the column: a consumer needs
			// both, which is why the row carries the raw one too.
			name: "a parent-only key is not required on sub but keeps its column",
			key:  "composition",
			want: issue.ListedSection{
				Key: "composition", Heading: "構成（Sub-Issues）", Required: false,
				RequiredOn: []issue.Kind{issue.Parent}, TemplateMappable: false, NoneMarkers: nil,
			},
		},
		{
			name: "an optional key has an empty required_on",
			key:  "impl_notes",
			want: issue.ListedSection{
				Key: "impl_notes", Heading: "実装方針の示唆", Required: false,
				RequiredOn: []issue.Kind{}, TemplateMappable: true, NoneMarkers: nil,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			if diff := cmp.Diff(tt.want, byKey[tt.key]); diff != "" {
				t.Errorf("List section %q (-want +got):\n%s", tt.key, diff)
			}
		})
	}
}

func TestListRendersTheRequestedLocale(t *testing.T) {
	t.Parallel()

	got, err := issue.List(issue.EN, issue.Leaf)
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if want := "Background / Purpose"; got.Sections[0].Heading != want {
		t.Errorf("first heading = %q, want %q", got.Sections[0].Heading, want)
	}
	if !got.Sections[0].Required {
		t.Error("background should be required on a leaf")
	}
}

func TestListRejectsUnsupportedInput(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		locale issue.Locale
		kind   issue.Kind
		want   string
	}{
		{name: "unsupported locale", locale: "fr", kind: issue.Leaf, want: "unsupported locale: fr"},
		{name: "unsupported kind", locale: issue.JA, kind: "epic", want: "unsupported kind: epic"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			_, err := issue.List(tt.locale, tt.kind)
			if err == nil {
				t.Fatal("want an error, got nil")
			}
			if !strings.Contains(err.Error(), tt.want) {
				t.Errorf("List error = %v, want it to contain %q", err, tt.want)
			}
			// The message has to name the supported set, or the caller is left
			// guessing what it may pass.
			for _, s := range []string{"supported:"} {
				if !strings.Contains(err.Error(), s) {
					t.Errorf("List error = %v, want it to contain %q", err, s)
				}
			}
		})
	}
}

func TestFind(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		key  string
		want issue.Found
	}{
		{
			name: "resolves the ja canonical heading and excludes the heading line",
			body: jaLeaf, key: "requirements",
			want: issue.Found{Key: "requirements", Locale: issue.JA, Heading: "要件", Body: "- ひとつ"},
		},
		{
			name: "resolves the en canonical heading without being told the locale",
			body: enLeaf, key: "requirements",
			want: issue.Found{Key: "requirements", Locale: issue.EN, Heading: "Requirements", Body: "- one"},
		},
		{
			name: "the last section runs to the end of the body",
			body: enLeaf, key: "out_of_scope",
			want: issue.Found{Key: "out_of_scope", Locale: issue.EN, Heading: "Out of scope", Body: "None"},
		},
		{
			// An empty section is where an off-by-one shows up: the next
			// heading must not be swept into the body.
			name: "an empty section does not leak the next heading",
			body: "## Requirements\n## Out of scope\n\nNone\n", key: "requirements",
			want: issue.Found{Key: "requirements", Locale: issue.EN, Heading: "Requirements", Body: ""},
		},
		{
			// A body written in GitHub's web editor arrives with CRLF, and
			// consumers compare it against fixed markers.
			name: "a CRLF body carries no carriage return",
			body: "## Depends on\r\n\r\nNone\r\n\r\n## Out of scope\r\n\r\nNone\r\n", key: "depends_on",
			want: issue.Found{Key: "depends_on", Locale: issue.EN, Heading: "Depends on", Body: "None"},
		},
		{
			// The drafts show the body template in fenced examples, and those
			// headings do not declare sections.
			name: "a heading inside a fence does not end the section",
			body: "## Requirements\n\n```\n## Out of scope\n```\n\ndone\n", key: "requirements",
			want: issue.Found{Key: "requirements", Locale: issue.EN, Heading: "Requirements",
				Body: "```\n## Out of scope\n```\n\ndone"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, err := issue.Find(tt.body, tt.key)
			if err != nil {
				t.Fatalf("Find(_, %q): %v", tt.key, err)
			}
			if diff := cmp.Diff(tt.want, got); diff != "" {
				t.Errorf("Find(_, %q) (-want +got):\n%s", tt.key, diff)
			}
		})
	}
}

func TestFindErrors(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		body    string
		key     string
		wantErr error
		wantMsg string
	}{
		{
			// Its own error, because the caller reads "no such section" as an
			// ordinary answer and must not read a failed fetch as one.
			name: "a section the body does not have", body: enLeaf, key: "composition",
			wantErr: issue.ErrSectionNotFound,
		},
		{name: "an empty body", body: "", key: "requirements", wantErr: issue.ErrEmptyInput},
		{name: "a whitespace-only body", body: "  \n\t\n", key: "requirements", wantErr: issue.ErrEmptyInput},
		{name: "an unknown key", body: enLeaf, key: "nope", wantMsg: "unknown section key"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			_, err := issue.Find(tt.body, tt.key)
			if err == nil {
				t.Fatal("want an error, got nil")
			}
			if tt.wantErr != nil && !errors.Is(err, tt.wantErr) {
				t.Errorf("Find error = %v, want errors.Is(_, %v)", err, tt.wantErr)
			}
			if tt.wantMsg != "" && !strings.Contains(err.Error(), tt.wantMsg) {
				t.Errorf("Find error = %v, want it to contain %q", err, tt.wantMsg)
			}
		})
	}
}
