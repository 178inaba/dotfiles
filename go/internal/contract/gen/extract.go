package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type pkg struct {
	path string
	dir  string
}

// docs is everything the types cannot say about themselves at run time:
// reflection sees a field's json tag but not the sentence above it, nor a
// named type's constants. Both are syntactic, so they are read from source.
//
// A field or a type with nothing to say is absent rather than empty — the
// empties were two thirds of the table, and the renderer asks Enums whether a
// type is an enum at all.
type docs struct {
	Fields   map[string]string
	Types    map[string]string
	Enums    map[string][]string
	EnumDocs map[string]string
	// Every import path that was read. Without it a type from a package
	// nobody added to the source list would render blank and say nothing.
	Packages []string
}

func extract(pkgs []pkg) (docs, error) {
	out := docs{Fields: map[string]string{}, Types: map[string]string{}, Enums: map[string][]string{}, EnumDocs: map[string]string{}}
	for _, p := range pkgs {
		if err := extractPkg(p, out); err != nil {
			return docs{}, err
		}
		out.Packages = append(out.Packages, p.path)
	}
	return out, nil
}

func extractPkg(p pkg, out docs) error {
	files, err := parseDir(p.dir)
	if err != nil {
		return err
	}

	// Before the const blocks: a constant may be declared above its type.
	stringTypes := map[string]bool{}
	for _, f := range files {
		forEachDecl(f, token.TYPE, func(gd *ast.GenDecl) {
			for _, spec := range gd.Specs {
				if ts, ok := spec.(*ast.TypeSpec); ok && ts.Name.IsExported() && isStringIdent(ts.Type) {
					stringTypes[ts.Name.Name] = true
				}
			}
		})
	}

	for _, f := range files {
		forEachDecl(f, token.TYPE, func(gd *ast.GenDecl) { collectFields(p.path, gd, out) })
		forEachDecl(f, token.CONST, func(gd *ast.GenDecl) { collectEnums(p.path, gd, stringTypes, out) })
	}
	return nil
}

// parseDir reads a package's non-test Go files, sorted by name so the table
// does not move with the filesystem.
//
// go/parser's own ParseDir is deprecated for not honouring build tags, and
// reading the directory here makes the test-file exclusion explicit — a
// fixture struct would otherwise join a contract it has nothing to do with.
func parseDir(dir string) ([]*ast.File, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", dir, err)
	}
	fset := token.NewFileSet()
	var files []*ast.File
	for _, e := range entries {
		name := e.Name()
		if e.IsDir() || !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		f, err := parser.ParseFile(fset, filepath.Join(dir, name), nil, parser.ParseComments)
		if err != nil {
			return nil, fmt.Errorf("parse %s: %w", filepath.Join(dir, name), err)
		}
		files = append(files, f)
	}
	return files, nil
}

func forEachDecl(f *ast.File, tok token.Token, fn func(*ast.GenDecl)) {
	for _, d := range f.Decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok == tok {
			fn(gd)
		}
	}
}

func collectFields(path string, gd *ast.GenDecl, out docs) {
	for _, spec := range gd.Specs {
		ts, ok := spec.(*ast.TypeSpec)
		// An unexported type cannot be a contract: nothing outside its own
		// package can be registered as a command's input or output.
		if !ok || !ts.Name.IsExported() {
			continue
		}
		st, ok := ts.Type.(*ast.StructType)
		if !ok {
			continue
		}
		// A single-spec declaration keeps its comment on the declaration; one
		// in a parenthesised block keeps it on the spec.
		if doc := docText(ts.Doc, ts.Name.Name, ""); doc != "" {
			out.Types[path+"."+ts.Name.Name] = doc
		} else if doc := docText(gd.Doc, ts.Name.Name, ""); doc != "" && len(gd.Specs) == 1 {
			out.Types[path+"."+ts.Name.Name] = doc
		}

		for _, field := range st.Fields.List {
			// Not on the wire, so not in a contract.
			if field.Tag == nil || !strings.Contains(field.Tag.Value, "json:") {
				continue
			}
			for _, name := range field.Names {
				if doc := docText(field.Doc, name.Name, jsonName(field.Tag.Value)); doc != "" {
					out.Fields[path+"."+ts.Name.Name+"."+name.Name] = doc
				}
			}
		}
	}
}

func collectEnums(path string, gd *ast.GenDecl, stringTypes map[string]bool, out docs) {
	for _, spec := range gd.Specs {
		vs, ok := spec.(*ast.ValueSpec)
		if !ok {
			continue
		}
		// Only a specification naming its own type: without one a constant is
		// untyped, whatever the one above it said, and carrying the type down
		// would collect an unrelated string into the value set.
		id, ok := vs.Type.(*ast.Ident)
		if !ok || !stringTypes[id.Name] {
			continue
		}
		current := id.Name
		for _, v := range vs.Values {
			lit, ok := v.(*ast.BasicLit)
			if !ok || lit.Kind != token.STRING {
				continue
			}
			s, err := strconv.Unquote(lit.Value)
			if err != nil {
				continue
			}
			out.Enums[path+"."+current] = append(out.Enums[path+"."+current], s)
			if doc := docText(vs.Doc, name(vs), s); doc != "" {
				out.EnumDocs[path+"."+current+"."+s] = doc
			}
		}
	}
}

func isStringIdent(e ast.Expr) bool {
	id, ok := e.(*ast.Ident)
	return ok && id.Name == "string"
}

// jsonName is read from the tag's source text, since this side has no
// reflection.
func jsonName(tag string) string {
	_, rest, found := strings.Cut(tag, `json:"`)
	if !found {
		return ""
	}
	value, _, _ := strings.Cut(rest, `"`)
	key, _, _ := strings.Cut(value, ",")
	return key
}

// name is what a value specification's doc comment opens with.
func name(vs *ast.ValueSpec) string {
	if len(vs.Names) == 0 {
		return ""
	}
	return vs.Names[0].Name
}

// docText flattens a doc comment to one line and puts the name it opens with
// into the words the reader has.
//
// Two readers, one comment. Go convention — and revive, which the lint step
// enables for exactly this — is to open with the declaration's own name, which
// the reader of a --help has never seen; what they have is the JSON key, or
// the constant's value. So the name is replaced rather than removed, since
// dropping the subject alone would leave "Are the degradations that…".
//
// The line breaks go because the render decides the column width, not this.
func docText(g *ast.CommentGroup, decl, wire string) string {
	if g == nil {
		return ""
	}
	text := strings.Join(strings.Fields(g.Text()), " ")
	rest, found := strings.CutPrefix(text, decl+" ")
	if !found || rest == "" {
		return text
	}
	// The row already carries the name, so "X is a thing" reads best with the
	// subject and its verb both gone.
	for _, verb := range []string{"is ", "are "} {
		if tail, ok := strings.CutPrefix(rest, verb); ok && tail != "" {
			return strings.ToUpper(tail[:1]) + tail[1:]
		}
	}
	// Any other verb would leave a fragment, so replace rather than remove.
	if wire != "" {
		return wire + " " + rest
	}
	return text
}
