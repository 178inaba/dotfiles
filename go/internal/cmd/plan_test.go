package cmd

import (
	"bytes"
	"encoding/json/v2"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/gittest"
	"github.com/178inaba/dotfiles/go/internal/plandocs"
)

// The paths a caller gives reach the package, and nothing else does: the home
// the command reads is the process's, so the test gives it one of its own
// rather than letting the machine's answer decide what the rules are.
func TestPlanDocsPassesItsArgumentsThrough(t *testing.T) {
	dir, home := t.TempDir(), t.TempDir()
	t.Setenv("HOME", home)
	gittest.Write(t, filepath.Join(dir, ".git", "HEAD"), "ref: refs/heads/main\n")
	gittest.Write(t, filepath.Join(dir, ".claude", "rules", "scoped.md"), "---\npaths:\n  - \"**/go/**\"\n---\n")
	gittest.Write(t, filepath.Join(home, ".claude", "rules", "user.md"), "---\npaths:\n  - \"**/go/**\"\n---\n")

	tests := map[string]struct {
		args      []string
		documents []string
	}{
		"with a path the rules match": {
			args: []string{"plan", "docs", "go/x.go"},
			documents: []string{
				filepath.Join(dir, ".claude", "rules", "scoped.md"),
				filepath.Join(home, ".claude", "rules", "user.md"),
			},
		},
		// Empty rather than nil: the list crossed the wire, where the absence
		// of a document is the empty array the skills read.
		"with no path at all": {args: []string{"plan", "docs"}, documents: []string{}},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			var stdout, stderr bytes.Buffer
			if code := run(t.Context(), tt.args,
				strings.NewReader(""), &stdout, &stderr, Deps{Dir: dir}); code != 0 {
				t.Fatalf("`ccx %s` = %d, want 0: %s", strings.Join(tt.args, " "), code, stderr.String())
			}

			var got plandocs.Collection
			if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
				t.Fatalf("decode: %v\n%s", err, stdout.String())
			}
			if diff := cmp.Diff(tt.documents, got.Documents); diff != "" {
				t.Errorf("documents mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

// The findings leave through two doors and both have to open: the report goes
// out whole, and the status says whether there was anything in it, since the
// skill that runs this reads the number to decide whether to run it again.
//
// What counts as a finding is the package's own and is tested there; this is
// the wiring between the two doors.
func TestPlanCheckAnswersOnStandardOutputAndInTheStatus(t *testing.T) {
	dir, home := t.TempDir(), t.TempDir()
	t.Setenv("HOME", home)
	gittest.Write(t, filepath.Join(dir, ".git", "HEAD"), "ref: refs/heads/main\n")

	tests := map[string]struct {
		plan string
		code int
		want []plandocs.Finding
	}{
		// Empty rather than nil: the lists crossed the wire, where the absence
		// of a finding is the empty array the skills read.
		"a plan with nothing wrong in it": {plan: "All good.\n", want: []plandocs.Finding{}},
		"a plan citing a path that is not there": {
			plan: "Edit `go/nope.go`.\n",
			code: 2,
			want: []plandocs.Finding{{Line: 1, Ref: "go/nope.go"}},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			plan := filepath.Join(dir, "plan.md")
			gittest.Write(t, plan, tt.plan)

			var stdout, stderr bytes.Buffer
			args := []string{"plan", "check", plan}
			if code := run(t.Context(), args,
				strings.NewReader(""), &stdout, &stderr, Deps{Dir: dir}); code != tt.code {
				t.Fatalf("`ccx plan check` = %d, want %d: %s", code, tt.code, stderr.String())
			}

			var got plandocs.Checked
			if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
				t.Fatalf("decode: %v\n%s", err, stdout.String())
			}
			if diff := cmp.Diff(tt.want, got.MissingPaths); diff != "" {
				t.Errorf("missing paths mismatch (-want +got):\n%s", diff)
			}
			if got.UnresolvedSymbols == nil || got.UnrecordedCommands == nil {
				t.Errorf("a list reached the wire absent rather than empty: %+v", got)
			}
		})
	}
}
