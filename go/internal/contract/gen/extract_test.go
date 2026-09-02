package gen

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"
)

// Written to disk rather than kept as a testdata package: the extractor's job
// is to read a directory of Go source, and two of the cases below are about
// source layout rather than about the AST.
const fixture = `package sample

// Kind is what sort of thing this is.
type Kind string

const (
	// Alpha is the first.
	Alpha Kind = "alpha"
	Beta  Kind = "beta"
)

// Loose is a named string with no constants, which is not an enum.
type Loose string

// Thing is the fixture's output type.
type Thing struct {
	// Name is what it is called.
	Name string ` + "`json:\"name\"`" + `
	// Parent is null when there is none, and also when it could not be read
	// — the warning tells those apart.
	Parent *Ref ` + "`json:\"parent\"`" + `
	Count  int  ` + "`json:\"count\"`" + `
	// Hidden carries no json tag and is not part of any contract.
	Hidden string
	Kind   Kind ` + "`json:\"kind\"`" + `
}

// Ref is a reference to something else.
type Ref struct {
	Number int ` + "`json:\"number\"`" + `
}

// hidden is unexported, so it cannot be any command's input or output and
// nothing about it belongs in the table.
type hidden struct {
	Secret string ` + "`json:\"secret\"`" + `
}

type quiet string

const quietOne quiet = "one"
`

func TestExtract(t *testing.T) {
	dir := t.TempDir()
	write(t, filepath.Join(dir, "sample.go"), fixture)
	// A test file in the same directory must not contribute: its types are not
	// part of any contract, and a fixture struct there would silently join one.
	write(t, filepath.Join(dir, "sample_test.go"), "package sample\n\n// Fake is a test helper.\ntype Fake struct {\n\tX int `json:\"x\"`\n}\n")

	got, err := extract([]pkg{{path: "example.com/sample", dir: dir}})
	if err != nil {
		t.Fatalf("extract: %v", err)
	}

	// The Go name each comment opens with is replaced by the JSON one, and a
	// field with no comment is absent rather than present and empty.
	wantFields := map[string]string{
		"example.com/sample.Thing.Name":   "What it is called.",
		"example.com/sample.Thing.Parent": "Null when there is none, and also when it could not be read — the warning tells those apart.",
	}
	if diff := cmp.Diff(wantFields, got.Fields); diff != "" {
		t.Errorf("fields (-want +got):\n%s", diff)
	}

	// Loose has no constants, so it is absent: an empty entry would answer
	// "is this an enum" with yes and nothing to show.
	wantEnums := map[string][]string{"example.com/sample.Kind": {"alpha", "beta"}}
	if diff := cmp.Diff(wantEnums, got.Enums); diff != "" {
		t.Errorf("enums (-want +got):\n%s", diff)
	}
	wantEnumDocs := map[string]string{"example.com/sample.Kind.alpha": "The first."}
	if diff := cmp.Diff(wantEnumDocs, got.EnumDocs); diff != "" {
		t.Errorf("enum docs (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff([]string{"example.com/sample"}, got.Packages); diff != "" {
		t.Errorf("packages (-want +got):\n%s", diff)
	}
}

// TestExtractUntypedConstIsNotAMember covers the shape that looks like a value
// set and is not: without a type of its own a constant is untyped, whatever
// the specification above it said.
func TestExtractUntypedConstIsNotAMember(t *testing.T) {
	dir := t.TempDir()
	write(t, filepath.Join(dir, "sample.go"), "package sample\n\ntype Verdict string\n\nconst (\n\tA Verdict = \"a\"\n\tB          = \"b\"\n)\n")

	got, err := extract([]pkg{{path: "example.com/sample", dir: dir}})
	if err != nil {
		t.Fatalf("extract: %v", err)
	}
	want := map[string][]string{"example.com/sample.Verdict": {"a"}}
	if diff := cmp.Diff(want, got.Enums); diff != "" {
		t.Errorf("enums (-want +got):\n%s", diff)
	}
}

func write(t *testing.T, name, content string) {
	t.Helper()
	if err := os.WriteFile(name, []byte(content), 0o600); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
}
