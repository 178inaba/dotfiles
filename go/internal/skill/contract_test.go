package skill_test

import (
	"testing"

	"github.com/178inaba/dotfiles/go/internal/skill"
)

// published stands in for what the commands publish. internal/cmd assembles
// the real set; written out here so the check can be exercised without it.
var published = skill.Contract{
	Commands:    []string{"worktree collect", "issue tree"},
	Identifiers: []string{"head_oid", "in_use_by_process", "all_sub_issues_closed", "release_manual_steps"},
}

func TestCheckRefsContractFields(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		body string
		want []string
	}{
		{
			name: "a field that exists",
			body: "Run `ccx worktree collect` and keep `head_oid` when you thin the list.\n",
		},
		{
			// The point of the whole check.
			name: "a field that no longer exists",
			body: "Run `ccx worktree collect` and keep `head_sha` when you thin the list.\n",
			want: []string{"head_sha"},
		},
		{
			// issue-handle names the section keys while delegating the lookup
			// to another skill's procedure.
			name: "a field of a command this skill does not run",
			body: "Run `ccx worktree collect`, then read the `release_manual_steps` section.\n",
		},
		{
			name: "a value of a set",
			body: "Run `ccx issue tree`; a reason of `in_use_by_process` is left alone.\n",
		},
		{
			// Without the gate, a skill about somebody else's schema would fail
			// on its own vocabulary.
			name: "a skill that runs no command at all",
			body: "A history table needs a `valid_from` and a `valid_to`.\n",
		},
		{
			name: "a token that is not a contract identifier but is allowed",
			body: "Run `ccx issue tree`, then start it with `run_in_background`.\n",
		},
		{
			// The usual form; matching a span whole would let it through.
			name: "a field written with its value",
			body: "Run `ccx issue tree`; stop when `all_sub_issues_closed: false`.\n",
		},
		{
			name: "a bad field written with its value",
			body: "Run `ccx issue tree`; stop when `all_subissues_closed: false`.\n",
			want: []string{"all_subissues_closed"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			root := t.TempDir()
			write(t, root, "sample", "---\nname: sample\ndescription: x\n---\n\n"+tc.body)

			got, err := skill.CheckRefs(root, published)
			if err != nil {
				t.Fatalf("CheckRefs: %v", err)
			}

			var unknown []string
			for _, v := range got.Violations {
				if v.Type == skill.UnknownContractField {
					unknown = append(unknown, v.Ref)
				}
			}
			if len(unknown) != len(tc.want) {
				t.Fatalf("unknown contract fields = %v, want %v", unknown, tc.want)
			}
			for i, w := range tc.want {
				if unknown[i] != w {
					t.Errorf("unknown contract field %d = %q, want %q", i, unknown[i], w)
				}
			}
		})
	}
}
