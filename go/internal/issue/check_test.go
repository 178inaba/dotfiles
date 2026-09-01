package issue_test

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/issue"
)

func TestCheck(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		draft   string
		locale  issue.Locale
		kind    issue.Kind
		mapping []issue.Mapping
		// wantClasses is the class of each violation, in the order reported.
		wantClasses []issue.Class
		// wantIn are fragments each message must name, so that a reader can
		// tell which heading or key is at fault.
		wantIn []string
	}{
		{
			name: "a clean ja draft passes", draft: jaLeaf, locale: issue.JA, kind: issue.Leaf,
		},
		{
			name: "a clean en draft passes", draft: enLeaf, locale: issue.EN, kind: issue.Leaf,
		},
		{
			// The draft is otherwise a valid ja leaf, with one heading left in
			// English: rule 2 knows the heading, so only rule 4 fires.
			name:  "one heading in the other language is a locale mismatch",
			draft: swap(jaLeaf, "要件", "Requirements"), locale: issue.JA, kind: issue.Leaf,
			wantClasses: []issue.Class{issue.HeadingLocaleMismatch},
			wantIn:      []string{"Requirements", "requirements"},
		},
		{
			name:   "a required section the draft lacks",
			draft:  strings.Replace(jaLeaf, "## 受け入れ条件\n\n- [ ] 通る\n\n", "", 1),
			locale: issue.JA, kind: issue.Leaf,
			wantClasses: []issue.Class{issue.MissingSection},
			wantIn:      []string{"acceptance", "受け入れ条件"},
		},
		{
			// A repository template may rename a section, and then its heading
			// is the expected one.
			name:  "a template heading passes when the mapping declares it",
			draft: swap(jaLeaf, "受け入れ条件", "Definition of Done"), locale: issue.JA, kind: issue.Leaf,
			mapping: []issue.Mapping{{Key: "acceptance", Heading: "Definition of Done"}},
		},
		{
			name:  "the same draft without the mapping is rejected",
			draft: swap(jaLeaf, "受け入れ条件", "Definition of Done"), locale: issue.JA, kind: issue.Leaf,
			// The renamed heading is unknown, and the section it renamed is
			// therefore missing.
			wantClasses: []issue.Class{issue.MissingSection, issue.UnknownHeading},
			wantIn:      []string{"acceptance", "Definition of Done"},
		},
		{
			// depends_on is machine-consumed: other skills find it by its
			// heading, so a template may not rename it.
			name:  "mapping a machine-consumed key is refused",
			draft: jaLeaf, locale: issue.JA, kind: issue.Leaf,
			mapping:     []issue.Mapping{{Key: "depends_on", Heading: "Prerequisites"}},
			wantClasses: []issue.Class{issue.MappedMachineKey},
			wantIn:      []string{"depends_on", "Prerequisites"},
		},
		{
			name:   "headings inside a fence declare nothing",
			draft:  jaLeaf + "\n```\n## Out of scope\n## 存在しない見出し\n```\n",
			locale: issue.JA, kind: issue.Leaf,
		},
		{
			// Three classes at once, so that the order they are reported in and
			// the status they collapse to are both pinned.
			name: "every class is reported, missing first",
			draft: swap(
				strings.Replace(jaLeaf, "## 受け入れ条件\n\n- [ ] 通る\n\n", "", 1),
				"要件", "Requirements") + "\n## 知らない見出し\n\nx\n",
			locale: issue.JA, kind: issue.Leaf,
			wantClasses: []issue.Class{issue.MissingSection, issue.UnknownHeading, issue.HeadingLocaleMismatch},
			wantIn:      []string{"acceptance", "知らない見出し", "Requirements"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, err := issue.Check(tt.draft, tt.locale, tt.kind, tt.mapping)
			if err != nil {
				t.Fatalf("Check: %v", err)
			}

			var gotClasses []issue.Class
			var messages []string
			for _, v := range got {
				gotClasses = append(gotClasses, v.Class)
				messages = append(messages, v.Message)
			}
			if diff := cmp.Diff(tt.wantClasses, gotClasses); diff != "" {
				t.Errorf("Check codes (-want +got):\n%s\nmessages: %q", diff, messages)
			}

			joined := strings.Join(messages, "\n")
			for _, want := range tt.wantIn {
				if !strings.Contains(joined, want) {
					t.Errorf("Check messages %q, want one naming %q", messages, want)
				}
			}
			// Each message is one line: the caller prints them one per line on
			// standard error, and an embedded newline would split a reason in
			// two.
			for _, m := range messages {
				if strings.Contains(m, "\n") {
					t.Errorf("message %q spans lines, want one line", m)
				}
			}
		})
	}
}

func TestWorst(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name      string
		in        []issue.Violation
		want      issue.Class
		wantFound bool
	}{
		{name: "no violations is success", in: nil},
		{
			// The earliest class present, not the first reported, so that the
			// answer does not depend on the order the rules ran in.
			name: "the earliest class present",
			in: []issue.Violation{
				{Class: issue.HeadingLocaleMismatch}, {Class: issue.UnknownHeading},
			},
			want: issue.UnknownHeading, wantFound: true,
		},
		{
			name: "a single class is itself",
			in:   []issue.Violation{{Class: issue.MappedMachineKey}},
			want: issue.MappedMachineKey, wantFound: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, found := issue.Worst(tt.in)
			if found != tt.wantFound {
				t.Fatalf("Worst(%v) found = %v, want %v", tt.in, found, tt.wantFound)
			}
			if found && got != tt.want {
				t.Errorf("Worst(%v) = %d, want %d", tt.in, got, tt.want)
			}
		})
	}
}

func TestCheckRejectsUnsupportedInput(t *testing.T) {
	t.Parallel()

	if _, err := issue.Check(enLeaf, "fr", issue.Leaf, nil); err == nil {
		t.Error("Check with an unsupported locale: want an error, got nil")
	}
	if _, err := issue.Check(enLeaf, issue.EN, "epic", nil); err == nil {
		t.Error("Check with an unsupported kind: want an error, got nil")
	}
}

func TestParseMapping(t *testing.T) {
	t.Parallel()

	t.Run("reads entries and ignores blanks and comments", func(t *testing.T) {
		t.Parallel()

		got, err := issue.ParseMapping("# a comment\n\nacceptance Definition of Done\n  background   Why now\n")
		if err != nil {
			t.Fatalf("ParseMapping: %v", err)
		}
		want := []issue.Mapping{
			{Key: "acceptance", Heading: "Definition of Done"},
			{Key: "background", Heading: "Why now"},
		}
		if diff := cmp.Diff(want, got); diff != "" {
			t.Errorf("ParseMapping (-want +got):\n%s", diff)
		}
	})

	// Every malformed mapping is an error rather than a best effort: quietly
	// dropping one entry surfaces later as "a required section is missing",
	// which points at the draft instead of at the mapping.
	tests := []struct {
		name string
		in   string
		want string
	}{
		{name: "a line with no heading", in: "acceptance\n", want: "malformed mapping line"},
		{name: "an unknown key", in: "nope Something\n", want: "unknown section key in the mapping"},
		{name: "one key mapped twice", in: "acceptance A\nacceptance B\n", want: "mapped more than once"},
		{name: "two keys on one heading", in: "acceptance Same\nbackground Same\n", want: "more than one key"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			_, err := issue.ParseMapping(tt.in)
			if err == nil {
				t.Fatal("want an error, got nil")
			}
			if !strings.Contains(err.Error(), tt.want) {
				t.Errorf("ParseMapping error = %v, want it to contain %q", err, tt.want)
			}
		})
	}
}
