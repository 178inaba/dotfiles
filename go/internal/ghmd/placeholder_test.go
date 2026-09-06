package ghmd_test

import (
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

func TestPlaceholders(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		want []ghmd.Placeholder
	}{
		{name: "none", body: "nothing here\n"},
		{
			name: "one on the first line",
			body: "waits for #{SUB_A}\n",
			want: []ghmd.Placeholder{{Name: "SUB_A", Line: 1}},
		},
		{
			name: "two on one line, in order",
			body: "x\n#{A} then #{B}\n",
			want: []ghmd.Placeholder{{Name: "A", Line: 2}, {Name: "B", Line: 2}},
		},
		// #{NAME} is string interpolation in Ruby and Elixir, so a draft
		// showing some is not naming an issue.
		{name: "a code span holds none", body: "`#{A}` and `#{B}`\n"},
		{name: "a fenced block holds none", body: "```\n#{A}\n```\n"},
		{name: "a lowercase name is not one", body: "#{sub_a}\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			if diff := cmp.Diff(tt.want, ghmd.Placeholders(tt.body)); diff != "" {
				t.Errorf("Placeholders(%q) (-want +got):\n%s", tt.body, diff)
			}
		})
	}
}

func TestSubstitute(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		body    string
		numbers map[string]int
		want    string
		left    []ghmd.Placeholder
	}{
		{
			name:    "every name is known",
			body:    "closes #{A} and #{B}\n",
			numbers: map[string]int{"A": 12, "B": 13},
			want:    "closes #12 and #13\n",
		},
		{
			name:    "a name with no number is left as it is",
			body:    "closes #{A} and #{B}\n",
			numbers: map[string]int{"A": 12},
			want:    "closes #12 and #{B}\n",
			left:    []ghmd.Placeholder{{Name: "B", Line: 1}},
		},
		{
			name:    "nothing to fill leaves the body alone",
			body:    "no placeholder\n",
			numbers: map[string]int{"A": 12},
			want:    "no placeholder\n",
		},
		{
			name:    "code is left alone",
			body:    "#{A}\n`#{A}`\n```\n#{A}\n```\n",
			numbers: map[string]int{"A": 12},
			want:    "#12\n`#{A}`\n```\n#{A}\n```\n",
		},
		{
			// The whole of what makes a run of new issues publishable: the
			// numbers a run assigns may well be 1, 2 and 3, and nothing here
			// judges the text it produces.
			name:    "the first three issues of a repository",
			body:    "subs: #{A} #{B} #{C}\n",
			numbers: map[string]int{"A": 1, "B": 2, "C": 3},
			want:    "subs: #1 #2 #3\n",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, left := ghmd.Substitute(tt.body, tt.numbers)
			if got != tt.want {
				t.Errorf("Substitute(%q) = %q, want %q", tt.body, got, tt.want)
			}
			if diff := cmp.Diff(tt.left, left); diff != "" {
				t.Errorf("Substitute(%q) left (-want +got):\n%s", tt.body, diff)
			}
		})
	}
}
