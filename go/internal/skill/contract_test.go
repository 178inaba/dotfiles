package skill_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/skill"
)

// published stands in for what the commands publish. internal/cmd assembles
// the real set; written out here so the check can be exercised without it.
var published = skill.Published{
	Commands:    []string{"worktree collect", "issue tree"},
	Identifiers: []string{"head_oid", "in_use_by_process", "all_sub_issues_closed", "release_manual_steps"},
}

func TestCheckContract(t *testing.T) {
	t.Parallel()

	// Every body is one line, and the frontmatter written around it puts that
	// line at 6.
	const bodyLine = 6

	tests := []struct {
		name string
		body string
		want []skill.ContractFinding
	}{
		{
			name: "a field that exists",
			body: "Run `ccx worktree collect` and keep `head_oid` when you thin the list.\n",
		},
		{
			// The point of the whole check.
			name: "a field that no longer exists",
			body: "Run `ccx worktree collect` and keep `head_sha` when you thin the list.\n",
			want: []skill.ContractFinding{{
				Type: skill.UnknownContractField, File: "sample/SKILL.md",
				Line: bodyLine, Ref: "head_sha",
			}},
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
			want: []skill.ContractFinding{{
				Type: skill.UnknownContractField, File: "sample/SKILL.md",
				Line: bodyLine, Ref: "all_subissues_closed",
			}},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			root := t.TempDir()
			write(t, root, "sample", "---\nname: sample\ndescription: x\n---\n\n"+tc.body)

			got, err := skill.CheckContract(root, published)
			if err != nil {
				t.Fatalf("CheckContract: %v", err)
			}

			want := tc.want
			if want == nil {
				want = []skill.ContractFinding{}
			}
			if diff := cmp.Diff(want, got.Violations); diff != "" {
				t.Errorf("violations (-want +got):\n%s", diff)
			}
			// The path is made absolute, so the output alone says which copy
			// was read.
			if got.SkillsDir != root {
				t.Errorf("skills_dir = %q, want %q", got.SkillsDir, root)
			}
		})
	}
}

func TestCheckContractFails(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	if err := os.MkdirAll(filepath.Join(root, "sub"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	tests := []struct {
		name    string
		target  string
		wantErr string
	}{
		{name: "a directory that is not there", target: filepath.Join(root, "nope"), wantErr: "skills directory not found"},
		{name: "a directory holding no skills", target: root, wantErr: "no */SKILL.md found"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := skill.CheckContract(tc.target, published)
			if err == nil {
				t.Fatalf("CheckContract = %+v, want an error mentioning %q", got, tc.wantErr)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error = %q, want it to mention %q", err, tc.wantErr)
			}
		})
	}
}
