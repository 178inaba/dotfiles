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
