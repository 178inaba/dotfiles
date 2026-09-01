package pullrequest_test

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

func TestContextFileName(t *testing.T) {
	t.Parallel()

	// The @ is what keeps a-b/c and a/b-c from collapsing onto one name, which
	// is the whole reason a parallel review of another pull request cannot
	// read this one's file.
	got := pullrequest.ContextFileName(ghapi.Repo{Owner: "a-b", Name: "c"}, 5)
	if want := "pr-context-a-b@c-5.json"; got != want {
		t.Errorf("ContextFileName = %q, want %q", got, want)
	}
	if other := pullrequest.ContextFileName(ghapi.Repo{Owner: "a", Name: "b-c"}, 5); other == got {
		t.Errorf("a-b/c and a/b-c both answer %q", got)
	}
}

func TestParseCheckout(t *testing.T) {
	t.Parallel()

	const full = `{"pr":{"head_oid":"abc123","head_ref":"feature/x","base_ref":"main"},"is_own_pr":true}`

	tests := []struct {
		name    string
		in      string
		want    worktree.PullRequest
		wantErr string
	}{
		{
			name: "every field",
			in:   full,
			want: worktree.PullRequest{HeadRef: "feature/x", HeadOID: "abc123", BaseRef: "main", IsOwnPR: true},
		},
		{
			// false is an answer, and reading its absence as one would treat a
			// reviewer's checkout as the author's.
			name: "is_own_pr false",
			in:   `{"pr":{"head_oid":"abc123","head_ref":"feature/x","base_ref":"main"},"is_own_pr":false}`,
			want: worktree.PullRequest{HeadRef: "feature/x", HeadOID: "abc123", BaseRef: "main"},
		},
		{name: "no head_oid", in: `{"pr":{"head_ref":"feature/x","base_ref":"main"},"is_own_pr":true}`, wantErr: "pr.head_oid missing"},
		{name: "no head_ref", in: `{"pr":{"head_oid":"abc123","base_ref":"main"},"is_own_pr":true}`, wantErr: "pr.head_ref missing"},
		{name: "no base_ref", in: `{"pr":{"head_oid":"abc123","head_ref":"feature/x"},"is_own_pr":true}`, wantErr: "pr.base_ref missing"},
		{name: "no is_own_pr", in: `{"pr":{"head_oid":"abc123","head_ref":"feature/x","base_ref":"main"}}`, wantErr: "is_own_pr missing"},
		{name: "not json at all", in: `not json`, wantErr: "decode"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := pullrequest.ParseCheckout([]byte(tc.in))
			if tc.wantErr != "" {
				if err == nil {
					t.Fatalf("ParseCheckout = %+v, want an error mentioning %q", got, tc.wantErr)
				}
				if !strings.Contains(err.Error(), tc.wantErr) {
					t.Errorf("ParseCheckout error = %q, want it to mention %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseCheckout: %v", err)
			}
			if diff := cmp.Diff(tc.want, got); diff != "" {
				t.Errorf("ParseCheckout (-want +got):\n%s", diff)
			}
		})
	}
}
