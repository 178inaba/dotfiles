package ghshim

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// These are the two cases of the shell suite that never reached the decision —
// it stubbed the real gh with GH_BIN and asked whether the shim found it —
// together with the paths around them that only a table can reach.

// ghTree lays out a directory holding an executable named gh, and returns it.
func ghTree(t *testing.T, mode os.FileMode) string {
	t.Helper()

	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "gh"), []byte("stub"), mode); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	// The shim compares resolved directories, so the fixture has to be
	// resolved too: a t.TempDir under macOS lives below /var, which is a
	// symlink to /private/var.
	resolved, err := filepath.EvalSymlinks(dir)
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	return resolved
}

func TestReal(t *testing.T) {
	t.Parallel()

	real := ghTree(t, 0o755)
	self := ghTree(t, 0o755)
	notExecutable := ghTree(t, 0o644)
	empty := t.TempDir()

	tests := []struct {
		name     string
		ghBin    string
		pathList []string
		want     string
		wantErr  bool
	}{
		{
			name:     "GH_BIN names a path outside this directory",
			ghBin:    filepath.Join(real, "gh"),
			pathList: []string{empty},
			want:     filepath.Join(real, "gh"),
		},
		{
			// The shell case: GH_BIN pointing at the shim falls back to PATH.
			name:     "GH_BIN pointing at the shim falls back to PATH",
			ghBin:    filepath.Join(self, "gh"),
			pathList: []string{self, real},
			want:     filepath.Join(real, "gh"),
		},
		{
			name:     "GH_BIN without a separator is looked up on PATH",
			ghBin:    "gh",
			pathList: []string{real},
			want:     filepath.Join(real, "gh"),
		},
		{
			name:     "a GH_BIN that resolves to nothing falls back to PATH",
			ghBin:    filepath.Join(empty, "gh"),
			pathList: []string{real},
			want:     filepath.Join(real, "gh"),
		},
		{
			name:     "PATH is walked in order, skipping this directory",
			pathList: []string{empty, self, real},
			want:     filepath.Join(real, "gh"),
		},
		{
			name:     "a file with no execute bit is not a candidate",
			pathList: []string{notExecutable, real},
			want:     filepath.Join(real, "gh"),
		},
		{
			// The shell case: a missing real gh does not silently succeed.
			name:     "a missing real gh is an error",
			pathList: []string{empty},
			wantErr:  true,
		},
		{
			name:    "an empty PATH is an error",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, err := Real(tt.ghBin, strings.Join(tt.pathList, string(filepath.ListSeparator)), self)
			if tt.wantErr {
				if !errors.Is(err, ErrNoRealGH) {
					t.Fatalf("Real = %q, %v; want %v", got, err, ErrNoRealGH)
				}
				return
			}
			if err != nil {
				t.Fatalf("Real: %v", err)
			}
			if got != tt.want {
				t.Errorf("Real = %q, want %q", got, tt.want)
			}
		})
	}
}
