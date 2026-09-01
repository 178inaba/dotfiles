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

// pkg is one package to read: its import path, which is how the renderer will
// ask for it, and where its source sits.
type pkg struct {
	path string
	dir  string
}

// docs is everything the types cannot say about themselves at run time.
//
// Reflection sees a field's json tag and its type but not the sentence above
// it, and it sees a named string type but not which constants were declared
// for it. Both are purely syntactic, so they are read from source here and
// looked up by name at render time.
type docs struct {
	// Fields is keyed "<import path>.<Type>.<Field>". A field with no doc
	// comment is present with an empty value, so that the renderer can tell a
	// field it knows about from one the table has never heard of.
	Fields map[string]string
	// Types is keyed "<import path>.<Type>" and holds the type's own doc
	// comment, for the sentences that are about the document as a whole rather
	// than about any one field.
	Types map[string]string
	// Enums is keyed "<import path>.<Type>" and holds the values in
	// declaration order. A named string type with no constants is absent
	// rather than empty: the renderer asks whether a type is an enum.
	Enums map[string][]string
	// EnumDocs is keyed "<import path>.<Type>.<value>" and holds the
	// constant's doc comment. What a value means is as much a part of the
	// contract as the value itself.
	EnumDocs map[string]string
}

// extract reads every package and merges the result.
func extract(pkgs []pkg) (docs, error) {
	out := docs{Fields: map[string]string{}, Types: map[string]string{}, Enums: map[string][]string{}, EnumDocs: map[string]string{}}
	for _, p := range pkgs {
		if err := extractPkg(p, out); err != nil {
			return docs{}, err
		}
	}
	return out, nil
}

func extractPkg(p pkg, out docs) error {
	files, err := parseDir(p.dir)
	if err != nil {
		return err
	}

	// The string types have to be known before the const blocks are read,
	// because a constant may be declared above the type it belongs to.
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

// parseDir reads a package's non-test Go files, sorted by name so that the
// generated table does not move when a filesystem changes its mind about order.
//
// go/parser has a ParseDir for this, but it is deprecated for not honouring
// build tags, and reading the directory here also makes the test-file
// exclusion explicit — a fixture struct in one would otherwise join a contract
// it has nothing to do with.
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
		// A single-spec declaration keeps its doc comment on the declaration;
		// one inside a parenthesised block keeps it on the spec.
		if doc := docText(ts.Doc); doc != "" {
			out.Types[path+"."+ts.Name.Name] = doc
		} else if len(gd.Specs) == 1 {
			out.Types[path+"."+ts.Name.Name] = docText(gd.Doc)
		}

		for _, field := range st.Fields.List {
			// A field with no json tag is not on the wire, whatever else it
			// is, so nothing about it belongs in a contract.
			if field.Tag == nil || !strings.Contains(field.Tag.Value, "json:") {
				continue
			}
			for _, name := range field.Names {
				out.Fields[path+"."+ts.Name.Name+"."+name.Name] = docText(field.Doc)
			}
		}
	}
}

func collectEnums(path string, gd *ast.GenDecl, stringTypes map[string]bool, out docs) {
	// Go lets a typed const block state its type on the first spec only, so
	// the type carries down until another one replaces it.
	current := ""
	for _, spec := range gd.Specs {
		vs, ok := spec.(*ast.ValueSpec)
		if !ok {
			continue
		}
		if id, ok := vs.Type.(*ast.Ident); ok {
			current = id.Name
		}
		if !stringTypes[current] {
			continue
		}
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
			out.EnumDocs[path+"."+current+"."+s] = docText(vs.Doc)
		}
	}
}

func isStringIdent(e ast.Expr) bool {
	id, ok := e.(*ast.Ident)
	return ok && id.Name == "string"
}

// docText flattens a doc comment to one line.
//
// Help is rendered into a column whose width is decided at render time, so the
// comment's own line breaks would fight it; they are joined here and the
// renderer wraps.
func docText(g *ast.CommentGroup) string {
	if g == nil {
		return ""
	}
	return strings.Join(strings.Fields(g.Text()), " ")
}
