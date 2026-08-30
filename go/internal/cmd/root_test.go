package cmd

import (
	"bytes"
	"strings"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/selfbuild"
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
			code := run(tt.args, strings.NewReader(""), &stdout, &stderr, selfbuild.State{})

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
