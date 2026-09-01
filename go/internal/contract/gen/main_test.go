package main

import (
	"os"
	"path/filepath"
	"testing"
)

// TestGeneratedFileIsCurrent is what makes the contract a rendering rather
// than a copy: change a doc comment or a constant and the file on disk stops
// matching what the extractor now produces.
//
// It is a test rather than a generate step in CI so that the failure arrives
// where the change was made, with the command to fix it.
func TestGeneratedFileIsCurrent(t *testing.T) {
	contractDir := filepath.Join("..")

	want, err := generate(contractDir)
	if err != nil {
		t.Fatalf("generate: %v", err)
	}
	got, err := os.ReadFile(filepath.Join(contractDir, outputFile))
	if err != nil {
		t.Fatalf("read %s: %v", outputFile, err)
	}
	if string(got) != string(want) {
		t.Errorf("%s is out of date; run `go generate ./internal/contract/...`", outputFile)
	}
}
