package cmd

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestRun(t *testing.T) {
	tests := []struct {
		name string
		args []string

		wantCode      int
		wantStdout    []string
		wantNotStdout []string
		wantStderr    []string
		// bare says the other stream must be empty, which is what keeps a
		// diagnostic out of the pipe a subcommand renders into.
		bareStdout bool
		bareStderr bool
	}{
		{
			name:       "no arguments prints help and succeeds",
			args:       nil,
			wantCode:   0,
			wantStdout: []string{"Usage:", "ccx"},
			bareStderr: true,
		},
		{
			name:       "help flag prints help and succeeds",
			args:       []string{"--help"},
			wantCode:   0,
			wantStdout: []string{"Usage:"},
			bareStderr: true,
		},
		{
			// The refresh commands exist so the status line can re-run itself
			// in the background; running one by hand does nothing useful, so
			// they stay out of the listing while the real subcommand is in it.
			name:          "help lists the subcommands but not the internal ones",
			args:          []string{"--help"},
			wantCode:      0,
			wantStdout:    []string{"statusline"},
			wantNotStdout: []string{"internal-refresh-fx", "internal-refresh-pr"},
			bareStderr:    true,
		},
		{
			name:       "unknown subcommand fails with usage on stderr",
			args:       []string{"bogus"},
			wantCode:   1,
			wantStderr: []string{`unknown command "bogus"`, "Usage:"},
			bareStdout: true,
		},
		{
			name:       "unknown flag fails with usage on stderr",
			args:       []string{"--nope"},
			wantCode:   1,
			wantStderr: []string{"unknown flag", "Usage:"},
			bareStdout: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var stdout, stderr bytes.Buffer
			code := run(t.Context(), tt.args, strings.NewReader(""), &stdout, &stderr, Deps{})

			if code != tt.wantCode {
				t.Errorf("exit code = %d, want %d (stdout=%q stderr=%q)", code, tt.wantCode, stdout.String(), stderr.String())
			}
			for _, want := range tt.wantStdout {
				if !strings.Contains(stdout.String(), want) {
					t.Errorf("stdout does not contain %q:\n%s", want, stdout.String())
				}
			}
			for _, unwanted := range tt.wantNotStdout {
				if strings.Contains(stdout.String(), unwanted) {
					t.Errorf("stdout contains %q:\n%s", unwanted, stdout.String())
				}
			}
			for _, want := range tt.wantStderr {
				if !strings.Contains(stderr.String(), want) {
					t.Errorf("stderr does not contain %q:\n%s", want, stderr.String())
				}
			}
			if tt.bareStdout && stdout.Len() != 0 {
				t.Errorf("stdout = %q, want empty", stdout.String())
			}
			if tt.bareStderr && stderr.Len() != 0 {
				t.Errorf("stderr = %q, want empty", stderr.String())
			}
		})
	}
}

// clientPatterns reach the two packages the rule is about: the command tree,
// and the statusline package the detached refreshes are wired in. Not the
// subpackages below the latter — prinfo and fxrate are handed a constructor,
// so building one is not something their code is in a position to do.
var clientPatterns = []string{"*.go", filepath.Join("..", "statusline", "*.go")}

// TestOnlyExecuteBuildsTheClient holds the whole tree to one construction of
// the GitHub client, in Execute, where the dependency is assembled.
//
// A test rather than a depguard rule in .golangci.yml, which is where this
// repository puts its package-boundary rules: depguard works per import, and
// root.go goes on importing ghapi. "One call in one file" is not something it
// can say.
//
// The text is scanned rather than parsed, because the call written out is what
// the rule is about and what a reader greps for. Two things it therefore does
// not see: an aliased import, and the call named inside a comment — which is
// why the comments that talk about it leave the parenthesis off.
func TestOnlyExecuteBuildsTheClient(t *testing.T) {
	t.Parallel()

	var found []string
	for _, pattern := range clientPatterns {
		paths, err := filepath.Glob(pattern)
		if err != nil {
			t.Fatalf("Glob %s: %v", pattern, err)
		}
		for _, path := range paths {
			if strings.HasSuffix(path, "_test.go") {
				continue
			}
			b, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("ReadFile %s: %v", path, err)
			}
			for i, line := range strings.Split(string(b), "\n") {
				if strings.Contains(line, "ghapi.New(") {
					found = append(found, fmt.Sprintf("%s:%d", path, i+1))
				}
			}
		}
	}

	if len(found) != 1 || !strings.HasPrefix(found[0], "root.go:") {
		t.Errorf("a client is built at %v, want only the one in root.go that Execute puts in Deps; "+
			"a command takes its client from Deps rather than building its own", found)
	}
}
