package cmd

import (
	"bytes"
	encodingjson "encoding/json"
	json "encoding/json/v2"
	"fmt"
	"io"
	"maps"
	"reflect"
	"regexp"
	"slices"
	"strings"
	"testing"

	"github.com/spf13/cobra"

	"github.com/178inaba/dotfiles/go/internal/contract"

	"github.com/178inaba/dotfiles/go/internal/pullrequest"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
	"github.com/178inaba/dotfiles/go/internal/worktree"
)

// TestContractsRender is what stops the degradation in help.String shipping:
// a type that cannot be rendered says so instead of panicking.
func TestContractsRender(t *testing.T) {
	for path, h := range contracts {
		text := h.String()
		if strings.Contains(text, renderFailed) {
			t.Errorf("%s: %s", path, text)
		}
		for _, line := range strings.Split(text, "\n") {
			if len([]rune(line)) > contract.LineWidth {
				t.Errorf("%s: line is %d wide, over %d:\n%s", path, len([]rune(line)), contract.LineWidth, line)
			}
		}
	}
}

// goIdentifier is a word only a Go reader could resolve: capital-led parts run
// together, which is how this module spells a declaration and not English.
var goIdentifier = regexp.MustCompile(`\b[A-Z][a-z0-9]+([A-Z][A-Za-z0-9]*)+\b`)

var notAnIdentifier = map[string]bool{
	"GitHub": true, "GraphQL": true, "SemanticError": true,
}

// TestNoGoNamesInTheRenderedContract is the guard behind the rule that a doc
// comment now has two readers. Three Go names had reached a published help
// before it existed.
//
// Only the rendered blocks: an intro is hand-written English and may
// legitimately name a Go type it is telling the reader about.
func TestNoGoNamesInTheRenderedContract(t *testing.T) {
	for path, h := range contracts {
		for _, blk := range h.blocks {
			text, err := contract.Render(blk.typ, blk.mode)
			if err != nil {
				t.Errorf("%s: %v", path, err)
				continue
			}
			for _, word := range goIdentifier.FindAllString(text, -1) {
				if !notAnIdentifier[word] {
					t.Errorf("%s: the contract names %q, which only a Go reader can resolve", path, word)
				}
			}
		}
	}
}

// TestContractsNameRealCommands catches the one way a contract goes unprinted:
// the help hook looks a command up by its path, so a key matching no command
// is a contract nobody reaches.
func TestContractsNameRealCommands(t *testing.T) {
	root := newRootCmd(selfbuild.State{})

	paths := map[string]bool{}
	var walk func(*cobra.Command)
	walk = func(c *cobra.Command) {
		for _, sub := range c.Commands() {
			paths[commandPath(sub)] = true
			walk(sub)
		}
	}
	walk(root)

	for path := range contracts {
		if !paths[path] {
			t.Errorf("contracts has %q, which is not a command", path)
		}
	}
}

// TestHelpRendersTheContract is the other half: asking for help has to reach
// the table, which is what the hook on the root is for.
func TestHelpRendersTheContract(t *testing.T) {
	var out bytes.Buffer
	if code := run(t.Context(), []string{"worktree", "collect", "--help"}, nil, &out, io.Discard, selfbuild.State{}); code != 0 {
		t.Fatalf("--help exited %d", code)
	}
	for _, want := range []string{"Output (JSON on standard output)", "in_use_by_process", "Exit status:"} {
		if !strings.Contains(out.String(), want) {
			t.Errorf("help does not carry %q", want)
		}
	}
}

// TestEverySkillFacingCommandHasAContract is the list itself, written out: the
// commands a skill reads the output of. The statusline, the hooks and the
// refresh commands are not among them.
func TestEverySkillFacingCommandHasAContract(t *testing.T) {
	want := []string{
		"issue tree",
		"issue sections schema", "issue sections list", "issue sections check", "issue sections find",
		"pr context", "pr freshness", "pr prepare-review", "pr post-review", "pr reply-threads",
		"worktree detect", "worktree create", "worktree resolve", "worktree checkout",
		"worktree collect", "worktree delete",
		"review pending", "review verify", "review clone",
		"skill frontmatter", "skill refs",
	}
	for _, path := range want {
		if _, ok := contracts[path]; !ok {
			t.Errorf("%q has no contract", path)
		}
	}
	if len(contracts) != len(want) {
		t.Errorf("contracts has %d entries, want the %d listed here", len(contracts), len(want))
	}
}

// inputDocument is one registered input type's sample document and the parser
// that is supposed to enforce what its tags claim.
type inputDocument struct {
	sample string
	parse  func(t *testing.T, b []byte) error
}

// inputDocuments is a parser and a sample for each type a command reads. The
// set it has to cover is derived rather than listed, so a new reads(...)
// registration fails here until it is given both.
//
// Every sample gives its bodies inline, so a parser that resolves a named body
// returns before it reaches the work dir. One is passed anyway: a change that
// made it read the dir would fail loudly rather than read the repository.
var inputDocuments = map[reflect.Type]inputDocument{
	reflect.TypeFor[pullrequest.ReviewFile](): {
		sample: `{
			"assessment": "Approve可能",
			"body": "The review body.",
			"comments": [{"path": "internal/cache/cache.go", "line": 12, "body": "A remark."}]
		}`,
		parse: func(t *testing.T, b []byte) error {
			_, err := pullrequest.ParseSubmission(b, t.TempDir(), "review.json")
			return err
		},
	},
	reflect.TypeFor[pullrequest.ThreadsFile](): {
		sample: `{
			"threads": [{
				"path": "internal/cache/cache.go", "line": 12,
				"id": "PRRT_kwOA", "body": "A reply.", "resolve": true
			}]
		}`,
		parse: func(t *testing.T, b []byte) error {
			_, err := pullrequest.ParseThreadActions(b, t.TempDir(), "threads.json")
			return err
		},
	},
	reflect.TypeFor[worktree.DeleteInput](): {
		sample: `{
			"candidates": {
				"worktrees": [{
					"path": "/tmp/wt", "branch": "feature/1-a", "verdict": "pr_merged",
					"detail": "Its pull request merged.", "is_current": false, "head_oid": "0f00"
				}],
				"branches": [{
					"branch": "feature/2-b", "verdict": "merged_no_pr",
					"detail": "Merged into the default branch.", "is_current": false, "head_oid": "0f01"
				}]
			}
		}`,
		parse: func(_ *testing.T, b []byte) error {
			_, err := worktree.ParseCandidates(b)
			return err
		},
	},
}

// TestEveryInputContractHasAParser keeps the table above honest: the tags of a
// newly registered input document would otherwise go unchecked, and nothing
// else would say so.
func TestEveryInputContractHasAParser(t *testing.T) {
	read := map[reflect.Type]bool{}
	for _, path := range slices.Sorted(maps.Keys(contracts)) {
		for _, blk := range contracts[path].blocks {
			if blk.mode != contract.Input {
				continue
			}
			read[blk.typ] = true
			if _, ok := inputDocuments[blk.typ]; !ok {
				t.Errorf("%s reads %s, which has no entry in inputDocuments", path, blk.typ)
			}
		}
	}
	// The other direction, because an entry for a type nothing reads any more
	// keeps running and reads as coverage of a contract that is gone.
	for _, typ := range slices.SortedFunc(maps.Keys(inputDocuments), byTypeName) {
		if !read[typ] {
			t.Errorf("inputDocuments has %s, which no command reads", typ)
		}
	}
}

// groupConstrained are the json fields that are optional on their own but
// constrained as a pair: exactly one of body and body_file on a review, at most
// one on a thread reply. Omitting either is the pair's answer rather than the
// field's, so neither the completeness check nor the parser check applies.
//
// The entries are checked against the fields the walk actually reaches, so a
// renamed type or json field empties this list loudly rather than silently.
// The list is provisional: a tag saying "exactly one of these" would put the
// constraint in the rendered contract and leave nothing here to maintain, and
// 178inaba/dotfiles#139 defers that to an issue of its own.
var groupConstrained = []string{
	"pullrequest.ReviewFile.body",
	"pullrequest.ReviewFile.body_file",
	"pullrequest.ReviewFileComment.body",
	"pullrequest.ReviewFileComment.body_file",
	"pullrequest.ThreadsFileEntry.body",
	"pullrequest.ThreadsFileEntry.body_file",
}

// fieldCase is one json field to omit, and where it sits in its sample.
type fieldCase struct {
	// name is "<package>.<Type>.<json field>", the way the exclusion list
	// names one too.
	name     string
	path     []any
	required bool
}

// sampleWalk descends one sample document alongside the type it was written
// for, collecting a case per json field.
type sampleWalk struct {
	t    *testing.T
	seen map[reflect.Type]bool
	// names is every field the walk reached, the excluded ones included, and
	// is what the exclusion list is checked against. Shared across documents,
	// since the list names fields of several.
	names map[string]bool
	cases []fieldCase
}

func (w *sampleWalk) walk(typ reflect.Type, doc any, path []any) {
	w.t.Helper()
	if w.seen[typ] {
		return
	}
	// Scoped rather than global, so the same type appearing twice side by side
	// is walked at both of them.
	w.seen[typ] = true
	defer delete(w.seen, typ)

	obj, ok := doc.(map[string]any)
	if !ok {
		w.t.Errorf("the sample at %s is not an object, so %s cannot be walked", at(path), typ)
		return
	}

	for i := range typ.NumField() {
		f := typ.Field(i)
		name, ok := jsonFieldName(f)
		if !ok {
			continue
		}
		full := typ.String() + "." + name
		required := f.Tag.Get("contract") == "required"
		excluded := slices.Contains(groupConstrained, full)
		w.names[full] = true
		here := append(slices.Clone(path), name)
		value, present := obj[name]

		// An excluded field gets no case, so nothing else would notice a tag
		// on one — and no parser can enforce it, since the pair answers for
		// the field and a document may legitimately name the other half.
		if excluded && required {
			w.t.Errorf("%s is constrained as a pair but tagged required, which no parser can enforce on its own", full)
		}
		if !present {
			if !excluded {
				w.t.Errorf("the sample has no %s, so omitting %s would prove nothing", at(here), full)
			}
			continue
		}
		if !excluded {
			w.cases = append(w.cases, fieldCase{name: full, path: here, required: required})
		}

		inner, isList := structUnder(f.Type)
		if inner == nil {
			continue
		}
		// No input document holds one today. If one ever does, its Go fields
		// are not what reaches the wire, so descending into them would bind
		// tags to keys no parser ever sees — say so instead of guessing.
		if serialisesItself(inner) {
			w.t.Errorf("%s serialises itself, so the fields under %s are not its wire shape", inner, at(here))
			continue
		}
		if !isList {
			w.walk(inner, value, here)
			continue
		}
		// An empty list would leave every field of the element type
		// unexercised, which is the same hole as a missing key.
		elems, ok := value.([]any)
		if !ok || len(elems) == 0 {
			w.t.Errorf("the sample's %s is empty, so no field of %s is exercised", at(here), inner)
			continue
		}
		w.walk(inner, elems[0], append(slices.Clone(here), 0))
	}
}

// TestRequiredTagsMatchTheParsers binds contract:"required" to the check that
// enforces it. The tag decides only how a field prints in a --help; what
// refuses a document missing it is a hand-written nil check inside the parser,
// and nothing failed when the two disagreed in either direction — a tag with no
// check accepts what the help calls required, a check with no tag refuses what
// the help says may be left out.
func TestRequiredTagsMatchTheParsers(t *testing.T) {
	reached := map[string]bool{}
	for _, typ := range slices.SortedFunc(maps.Keys(inputDocuments), byTypeName) {
		doc := inputDocuments[typ]
		var decoded any
		if err := json.Unmarshal([]byte(doc.sample), &decoded); err != nil {
			t.Fatalf("%s: the sample is not valid JSON: %v", typ, err)
		}
		w := &sampleWalk{t: t, seen: map[reflect.Type]bool{}, names: reached}
		w.walk(typ, decoded, nil)

		for _, c := range w.cases {
			panicked, err := callParser(t, doc.parse, withoutField(t, doc.sample, c.path))
			switch {
			case panicked:
				t.Errorf("%s: the parser panicked on a document without %s, so its check for the field is gone", c.name, at(c.path))
			case c.required && err == nil:
				t.Errorf("%s is tagged required, but the parser accepted a document without %s", c.name, at(c.path))
			case !c.required && err != nil:
				t.Errorf("%s is not tagged required, but the parser refused a document without %s: %v", c.name, at(c.path), err)
			}
		}
	}

	for _, name := range groupConstrained {
		if !reached[name] {
			t.Errorf("the exclusion list names %s, which no input document has", name)
		}
	}
}

// TestNoRequiredTagOnAnOutputOnlyType catches the tag that reads as a promise
// and does nothing: it is looked at only in Input mode, so on a type no command
// reads it is a no-op nobody would notice.
//
// Reachability rather than the registration decides, because worktree.Candidates
// is reached from an input contract and an output one both.
func TestNoRequiredTagOnAnOutputOnlyType(t *testing.T) {
	read, written := map[reflect.Type]bool{}, map[reflect.Type]bool{}
	for _, h := range contracts {
		for _, blk := range h.blocks {
			if blk.mode == contract.Input {
				reachableStructs(blk.typ, read)
				continue
			}
			reachableStructs(blk.typ, written)
		}
	}

	for _, typ := range slices.SortedFunc(maps.Keys(written), byTypeName) {
		if read[typ] {
			continue
		}
		for i := range typ.NumField() {
			if f := typ.Field(i); f.Tag.Get("contract") == "required" {
				t.Errorf("%s.%s is tagged required, but %s is reached only from an output contract, where the tag is never read",
					typ, f.Name, typ)
			}
		}
	}
}

// reachableStructs collects every struct typ reaches, through fields of any
// kind, pointers, lists and map values.
//
// Deliberately wider than what a help prints, and every field is followed
// whether or not it has a json name. A type that serialises itself reaches its
// wire form through a Go field the contract never names — issue.PRList holds
// its issue.PR values in an untagged field, and the renderer descends to them
// through the marshaler table instead. Narrowing this to published fields
// hides issue.PR here, and a required tag on it would then pass.
//
// Being wide costs nothing, because the question asked of the result is only
// whether a type is reached from an input contract as well. A type this finds
// and the help does not is one whose tag is even more certainly never read.
func reachableStructs(typ reflect.Type, into map[reflect.Type]bool) {
	switch typ.Kind() {
	case reflect.Pointer, reflect.Slice, reflect.Array, reflect.Map:
		reachableStructs(typ.Elem(), into)
	case reflect.Struct:
		if into[typ] {
			return
		}
		into[typ] = true
		for i := range typ.NumField() {
			reachableStructs(typ.Field(i).Type, into)
		}
	}
}

// callParser keeps a panic apart from a refusal. A parser whose nil check is
// gone derefs nil rather than returning an error, and reporting that as an
// error would make the missing check look like the very rejection it stopped
// doing.
func callParser(t *testing.T, parse func(*testing.T, []byte) error, b []byte) (panicked bool, err error) {
	t.Helper()
	defer func() {
		if recover() != nil {
			panicked = true
		}
	}()
	return false, parse(t, b)
}

// withoutField is the sample with one key gone, decoded fresh each time so that
// no case can see another's deletion.
func withoutField(t *testing.T, sample string, path []any) []byte {
	t.Helper()
	var doc any
	if err := json.Unmarshal([]byte(sample), &doc); err != nil {
		t.Fatalf("the sample is not valid JSON: %v", err)
	}
	// The path came from walking this same sample, so every step is there.
	node := doc
	for _, step := range path[:len(path)-1] {
		if i, ok := step.(int); ok {
			node = node.([]any)[i]
			continue
		}
		node = node.(map[string]any)[step.(string)]
	}
	delete(node.(map[string]any), path[len(path)-1].(string))

	b, err := json.Marshal(doc)
	if err != nil {
		t.Fatalf("re-encoding the sample failed: %v", err)
	}
	return b
}

// jsonFieldName is a field's key on the wire, read the way the renderer reads
// it. A field the contract does not publish has no case to answer.
//
// A copy of the renderer's own rule rather than a call to it: the rule lives in
// an unexported function of internal/contract, and exporting a view of a walk
// that does not itself back the rendering would put a third answer in the
// module rather than one. The one shape the two would describe differently is
// a type that serialises itself, and the sample walk refuses those outright
// rather than guessing at a wire form it cannot see.
func jsonFieldName(f reflect.StructField) (string, bool) {
	tag, ok := f.Tag.Lookup("json")
	if !ok || tag == "-" {
		return "", false
	}
	name, _, _ := strings.Cut(tag, ",")
	return name, name != ""
}

// serialisesItself reports whether a type takes its own serialisation over,
// checked against both marshaler interfaces for the reason the renderer checks
// both: jsontext.Value implements only the older one.
func serialisesItself(typ reflect.Type) bool {
	for _, iface := range []reflect.Type{
		reflect.TypeFor[json.MarshalerTo](),
		reflect.TypeFor[encodingjson.Marshaler](),
	} {
		if typ.Implements(iface) || reflect.PointerTo(typ).Implements(iface) {
			return true
		}
	}
	return false
}

// structUnder is the struct a field's json value is made of, past any pointer
// and list, and whether the value is a list of them. Nil where the value is
// not made of a struct at all.
func structUnder(typ reflect.Type) (elem reflect.Type, isList bool) {
	for {
		switch typ.Kind() {
		case reflect.Pointer:
			typ = typ.Elem()
		case reflect.Slice, reflect.Array:
			isList, typ = true, typ.Elem()
		case reflect.Struct:
			return typ, isList
		default:
			return nil, false
		}
	}
}

// at spells a path the way the sample reads, so a failure names the key rather
// than the walk.
func at(path []any) string {
	var b strings.Builder
	for _, step := range path {
		if i, ok := step.(int); ok {
			fmt.Fprintf(&b, "[%d]", i)
			continue
		}
		if b.Len() > 0 {
			b.WriteString(".")
		}
		fmt.Fprint(&b, step)
	}
	return b.String()
}

func byTypeName(a, b reflect.Type) int { return strings.Compare(a.String(), b.String()) }
