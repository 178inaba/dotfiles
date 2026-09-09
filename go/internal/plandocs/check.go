package plandocs

import (
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"unicode"

	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

// Checked is what a plan's references and commands came to.
//
// Three lists rather than one, because the three are fixed differently: a
// reference is corrected or annotated, and a command is run and its result
// written down. Every list is present even when it is empty, so that a reader
// branches on what is in one rather than on whether it is there.
type Checked struct {
	// The plan that was read, as it was named on the command line.
	Plan string `json:"plan"`
	// Code spans that read as a path and name no file or directory. A path
	// with a glob or a placeholder in it is not one of these: it names a shape
	// rather than a file, so nothing about it can be looked up.
	MissingPaths []Finding `json:"missing_paths"`
	// Code spans that read as a name and that no file in the repository holds
	// as a whole word. A qualified name whose last part is held somewhere is
	// not one of these, since that is where an unexported one is written.
	UnresolvedSymbols []Finding `json:"unresolved_symbols"`
	// Commands in a shell block that carry no recorded result. The record says
	// either what running it did or why it could not be run; both count, and
	// the help spells out how each is written.
	UnrecordedCommands []Finding `json:"unrecorded_commands"`
	// A place under the repository the walk could not read. Reported rather
	// than raised: one unreadable directory narrows the search, and a narrower
	// search is still worth answering with.
	Warnings []string `json:"warnings"`
}

// Finding is one thing in the plan to fix, and where in the plan it is.
type Finding struct {
	// The 1-based line of the plan the finding is on.
	Line int `json:"line"`
	// The text as the plan writes it, so that it can be found in the plan and
	// corrected there. A reference is not normalised at all — one carrying
	// line numbers is reported with them on — and a command carries only the
	// indentation of its block stripped.
	Ref string `json:"ref"`
}

// knownExtensions decide whether a span with no slash in it reads as a path.
//
// An allow list rather than any dot-and-letters suffix, so that a qualified
// name is a name: nothing here spells the last part of one, which is what
// keeps a package-qualified reference out of the path check and in the symbol
// check where it can be resolved.
var knownExtensions = []string{
	"bash", "conf", "css", "go", "golden", "html", "ini", "js", "json",
	"jsx", "lock", "md", "mod", "proto", "py", "rb", "sh", "sql", "sum",
	"toml", "ts", "tsx", "txt", "yaml", "yml", "zsh",
}

// shellLanguages are the info strings whose blocks hold commands. A block in
// any other language is a code example rather than something anybody ran.
var shellLanguages = []string{"bash", "console", "sh", "shell", "zsh"}

// recordedResultForms are how a command's result is written down, as a reader
// of the help has to type them.
var recordedResultForms = []string{"# => exit <status>: <what it printed>", "# => not run: <why not>"}

// branchTypes open a branch name. They are this repository's own convention
// for a working branch, decided in the prose of the skill that names one;
// nothing publishes them to share, so this is a copy and a test pins it to
// that prose. Written in the skill's order for that test's sake — nothing
// here reads them in order.
var branchTypes = []string{"feature", "fix", "hotfix", "refactor", "chore", "docs"}

var (
	// A span that reads as a name: no whitespace, and none of the punctuation
	// that would make it prose or a call.
	symbolSpan = regexp.MustCompile(`^[A-Za-z0-9_.-]+$`)
	// The line numbers a reference to a place in a file carries, in the
	// spellings this repository's own documents use.
	lineNumbers = regexp.MustCompile(`:[0-9]+(?:[-,][0-9]+)*$`)
	// A skill invoked by name, which is written like an absolute path and is
	// not one.
	slashCommand = regexp.MustCompile(`^/[a-z][a-z0-9-]*$`)
	// A recorded result, in either of the two forms. What follows the colon
	// has to say something, so that an empty record is no record.
	recordedResult = regexp.MustCompile(`^#\s*=>\s*(?:exit\s+[0-9]+:\s*\S|not run:\s*\S)`)
	// The annotation that marks everything on a line as something the plan is
	// about to create rather than a reference to something that exists.
	plannedArtifact = regexp.MustCompile(`\(new\)|（新規）`)
)

// KnownExtensions are the suffixes that make a span with no slash in it read
// as a path.
//
// This and the two below are the parts of the check that are data rather than
// types, so the help cannot render them off a declaration. Read from here so
// that adding one reaches the help without anybody remembering to retype it
// there.
func KnownExtensions() []string { return slices.Clone(knownExtensions) }

// ShellLanguages are the info strings whose blocks are read as commands.
func ShellLanguages() []string { return slices.Clone(shellLanguages) }

// RecordedResultForms are the two ways a command's result is written down.
func RecordedResultForms() []string { return slices.Clone(recordedResultForms) }

// Check reads a plan and answers with what it gets wrong about the repository
// it is a plan for.
//
// dir is where the command was run and may be anywhere inside the repository,
// as it is for the collection walk: the top is found from it here rather than
// taken as given, because a relative path in a plan is written from the top
// and a caller standing in a subdirectory would otherwise resolve it and walk
// from there.
//
// home resolves the ~/ form and is a parameter for the reason it is there: a
// test has a fixture home and no business reading the real one.
//
// Nothing in the plan is executed. The commands it lists are read for whether
// their result was written down, which is a question about the text.
func Check(planFile, dir, home string) (Checked, error) {
	b, err := os.ReadFile(planFile)
	if err != nil {
		return Checked{}, err
	}

	// Outside a repository the walk up finds no top and answers with the
	// directory it started in, which here would mean searching whatever
	// happens to be under it and calling a plan clean on that evidence. The
	// collection walk can degrade to it — its answer is then "no documents" —
	// but a gate may not: this is a precondition, and a precondition that
	// does not hold is a failure with a reason rather than an answer.
	top := directories(dir)[0]
	if !hasGitEntry(top) {
		return Checked{}, fmt.Errorf("%s is not inside a repository", dir)
	}

	c := checker{top: top, home: home, out: Checked{Plan: planFile}}
	body := string(b)

	c.readSpans(body)
	c.readCommands(body)

	tree, err := c.walkTree(planFile)
	if err != nil {
		return Checked{}, err
	}
	c.resolve(tree)
	return c.out, nil
}

// checker carries the candidates a plan's spans raised and the answer being
// assembled. The candidates are held rather than answered as they are found,
// so that the tree is walked once for all of them.
type checker struct {
	top, home string
	paths     []Finding
	symbols   []Finding
	out       Checked
}

// readSpans reads every code span the plan writes and keeps the ones that
// could name something.
//
// Only the spans, and not the fenced blocks a code span reading of them would
// also yield: what is inside a block is either a command, which the command
// check reads under its own rules, or an example, which names nothing the
// plan is claiming exists.
func (c *checker) readSpans(body string) {
	annotated := plannedLines(body)
	for s := range ghmd.Segments(body) {
		if s.Kind != ghmd.Span || annotated[s.Line] {
			continue
		}
		text := strings.Trim(body[s.Start:s.End], "`")
		switch {
		// Whitespace by Unicode's reading and not ASCII's: a plan written in
		// Japanese separates words in a span with a full-width space, and one
		// read as a single token would be looked up as a name.
		case strings.ContainsFunc(text, unicode.IsSpace):
		case pathLike(text):
			c.paths = append(c.paths, Finding{Line: s.Line, Ref: text})
		case symbolLike(text):
			c.symbols = append(c.symbols, Finding{Line: s.Line, Ref: text})
		}
	}
}

// resolve drops the candidates the tree accounts for and keeps the rest, in
// the order the plan writes them.
func (c *checker) resolve(t tree) {
	for _, f := range c.paths {
		if !c.found(t, f.Ref) {
			c.out.MissingPaths = append(c.out.MissingPaths, f)
		}
	}
	for _, f := range c.symbols {
		if !t.holds(f.Ref) {
			c.out.UnresolvedSymbols = append(c.out.UnresolvedSymbols, f)
		}
	}
}

// found reports whether a path-like span names something.
//
// The line numbers come off first, since a reference to a place in a file is
// a reference to the file. What is left is read from the home directory, from
// the filesystem root or from the top of the repository, and a relative one
// the top does not have is looked for at the end of any path in the tree —
// which is what makes a plan naming a file by its bare name, as a plan does
// once it has named it in full, resolve to the file it means.
//
// The two spellings that survive all of that and still name nothing are a
// skill and a branch, neither of which is a file. They are dropped here
// rather than before the lookup, so that a real file spelled like one of them
// is found rather than excused.
func (c *checker) found(t tree, raw string) bool {
	s := lineNumbers.ReplaceAllString(raw, "")
	switch {
	case strings.HasPrefix(s, "~/"):
		return exists(filepath.Join(c.home, filepath.FromSlash(withoutTrailingSlash(s[2:]))))
	case path.IsAbs(s):
		return exists(filepath.FromSlash(s)) || slashCommand.MatchString(s)
	}

	s = strings.TrimPrefix(withoutTrailingSlash(s), "./")
	return exists(filepath.Join(c.top, filepath.FromSlash(s))) ||
		t.endsWith(s) ||
		branchLike(s, "/") ||
		c.namesSomethingElse(s)
}

// namesSomethingElse reports whether a path nothing in the tree answers for is
// one this check cannot answer for either.
//
// Two of those. The repository's own directory is not walked, so nothing under
// it can be found here however plainly it is there. And a path with no known
// extension whose first segment is not at the top of the repository is not a
// path into this checkout at all: it is a module path, a directory in another
// project, a branch. Reporting either would hand the author a finding with
// nothing to do about it, which costs more than the miss.
//
// Held to paths with no known extension because that is what separates the two
// cases from a file: a plan writing a filename means a file, wherever it puts
// it, and one of those under a first segment nobody has is worth saying.
//
// Held to paths with a directory in them for the same kind of reason. Every
// spelling the second case is about — a module path, another project's
// directory, a branch — has one, while a span of a single segment that reads
// as a path is a file or directory at the top of this repository and nothing
// else. Without that, the first segment is the whole span and the rule says
// "not there, so not ours" of every one of them, which would leave the
// dotfile spelling that makes such a span path-like unable to report anything.
func (c *checker) namesSomethingElse(s string) bool {
	first, _, nested := strings.Cut(s, "/")
	if hasKnownExtension(s) || !nested {
		return false
	}
	return first == ".git" || !exists(filepath.Join(c.top, first))
}

// withoutTrailingSlash is a path written as a directory, read as a path. An
// empty result names the directory it was relative to.
func withoutTrailingSlash(s string) string { return strings.TrimSuffix(s, "/") }

// pathLike reports whether a span reads as a path: it has a directory in it,
// it opens with a dot the way a configuration file at the top of a repository
// does, or it ends in one of the extensions above.
//
// A span carrying a glob or a placeholder is not path-like at all, rather than
// path-like and skipped: it names a shape, and the symbol check would then
// read it as a name.
func pathLike(s string) bool {
	if strings.ContainsAny(s, "*?<>{}") {
		return false
	}
	if strings.Contains(s, "/") {
		return true
	}
	if len(s) > 1 && s[0] == '.' && isLetter(s[1]) {
		return true
	}
	return hasKnownExtension(s)
}

// hasKnownExtension reports whether a span ends in one of the extensions this
// check reads as naming a file.
func hasKnownExtension(s string) bool {
	return slices.Contains(knownExtensions, strings.TrimPrefix(path.Ext(s), "."))
}

// symbolLike reports whether a span reads as a name to look for.
//
// Three things spelled like a name are not one, and they are refused here
// rather than excused after the search, so that nothing is looked for that
// could not be found: a flag, which belongs to a command line; a span with no
// letter in it, which is a number or a date; and a worktree, which is named
// after the branch in it with the separator changed and is written nowhere in
// the tree under either spelling.
func symbolLike(s string) bool {
	return symbolSpan.MatchString(s) && containsLetter(s) &&
		!strings.HasPrefix(s, "-") && !branchLike(s, "-")
}

// branchLike reports whether a span opens with a branch type followed by sep.
// A span carrying a known extension is a file under a directory that happens
// to be spelled like a branch type, not a branch.
func branchLike(s, sep string) bool {
	if hasKnownExtension(s) {
		return false
	}
	for _, t := range branchTypes {
		if strings.HasPrefix(s, t+sep) {
			return true
		}
	}
	return false
}

// plannedLines are the lines whose spans name something the plan is about to
// create. The whole line is marked rather than the span before the
// annotation: a line names one artifact and then says what it is, and asking
// an author to annotate each span of such a line separately buys nothing.
func plannedLines(body string) map[int]bool {
	out := map[int]bool{}
	for i, line := range strings.Split(body, "\n") {
		if plannedArtifact.MatchString(line) {
			out[i+1] = true
		}
	}
	return out
}

// readCommands reads the shell blocks and reports the commands in them whose
// result was never written down.
//
// The blocks come from the shared reading, which is what says where one ends
// and what its opening marker declared. Which declarations name a shell is
// this check's judgement and stays here.
//
// The findings come out in line order because the blocks do and each yields
// at most one.
func (c *checker) readCommands(body string) {
	for blk := range ghmd.Blocks(body) {
		if !slices.Contains(shellLanguages, language(blk.Info)) {
			continue
		}

		open, at := Finding{}, blk.Line
		for text := range strings.Lines(body[blk.Start:blk.End]) {
			at++
			text = strings.TrimSpace(text)
			switch {
			case recordedResult.MatchString(text):
				open = Finding{}
			case text == "" || strings.HasPrefix(text, "#"):
			case open.Line == 0:
				open = Finding{Line: at, Ref: text}
			}
		}
		if open.Line != 0 {
			c.out.UnrecordedCommands = append(c.out.UnrecordedCommands, open)
		}
	}
}

// language is the first word of an info string, lowercased, which is what
// names a block's language when it names anything.
func language(info string) string {
	fields := strings.Fields(info)
	if len(fields) == 0 {
		return ""
	}
	return strings.ToLower(fields[0])
}

// tree is what one walk of the repository answers: the paths it holds and the
// names its files write.
type tree struct {
	paths []string
	// Every name that was looked for, and whether a file wrote it. A
	// qualified name is looked for whole and its last part separately, so
	// both spellings are keys here.
	holding map[string]bool
}

// holds reports whether the tree writes a name. A qualified name the tree
// does not write whole is held if it writes the last part of it, which is
// where an unexported declaration is spelled — a plan naming one by its
// package would otherwise never resolve.
func (t tree) holds(symbol string) bool {
	if t.holding[symbol] {
		return true
	}
	last, qualified := lastSegment(symbol)
	return qualified && t.holding[last]
}

// lastSegment is what follows the final dot of a qualified name, and whether
// there was one.
func lastSegment(symbol string) (string, bool) {
	i := strings.LastIndexByte(symbol, '.')
	if i < 0 || i == len(symbol)-1 {
		return "", false
	}
	return symbol[i+1:], true
}

// endsWith reports whether any path in the tree ends at s, on a segment
// boundary so that a name is not matched inside another one.
func (t tree) endsWith(s string) bool {
	return slices.ContainsFunc(t.paths, func(p string) bool {
		return p == s || strings.HasSuffix(p, "/"+s)
	})
}

// walkTree reads the repository once: every path in it, and every file's text
// searched for the names still being looked for.
//
// Once rather than once per name, since a name is looked up by reading files
// and there is no index to ask. What is skipped is what is not this branch's
// text: the repository's own directory, a nested checkout — a linked worktree
// marks itself with a file rather than a directory, so the entry is looked
// for either way — and anything holding a zero byte early on, which is a
// binary and not somewhere a name is written.
//
// The plan itself is skipped even when it is kept inside the repository. A
// plan naming something it invented would otherwise be its own evidence that
// the name exists, which is the opposite of what this walk is for.
func (c *checker) walkTree(planFile string) (tree, error) {
	t := tree{holding: map[string]bool{}}
	for _, f := range c.symbols {
		t.holding[f.Ref] = false
		if last, qualified := lastSegment(f.Ref); qualified {
			t.holding[last] = false
		}
	}

	plan, err := filepath.Abs(planFile)
	if err != nil {
		return tree{}, err
	}

	err = filepath.WalkDir(c.top, func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			c.warn(p, err)
			if d != nil && d.IsDir() {
				return fs.SkipDir
			}
			return nil
		}
		if p == c.top {
			return nil
		}
		if d.IsDir() && (d.Name() == ".git" || hasGitEntry(p)) {
			return fs.SkipDir
		}

		rel, err := filepath.Rel(c.top, p)
		if err != nil {
			return err
		}
		t.paths = append(t.paths, filepath.ToSlash(rel))
		if d.IsDir() || p == plan {
			return nil
		}
		c.read(t, p)
		return nil
	})
	return t, err
}

// read searches one file for the names not yet found.
func (c *checker) read(t tree, p string) {
	b, err := os.ReadFile(p)
	if err != nil {
		c.warn(p, err)
		return
	}
	if isBinary(b) {
		return
	}
	for symbol, found := range t.holding {
		if !found && wholeWord(b, symbol) {
			t.holding[symbol] = true
		}
	}
}

// warn records a place under the repository that could not be read.
func (c *checker) warn(p string, err error) {
	c.out.Warnings = append(c.out.Warnings, fmt.Sprintf("%s could not be read: %v", p, err))
}

// isBinary reports whether the head of a file holds a zero byte.
func isBinary(b []byte) bool {
	return bytes.IndexByte(b[:min(len(b), 8192)], 0) >= 0
}

// wholeWord reports whether b writes s with neither a letter, a digit nor an
// underscore against either end of it.
func wholeWord(b []byte, s string) bool {
	for at := 0; ; {
		i := bytes.Index(b[at:], []byte(s))
		if i < 0 {
			return false
		}
		i += at
		if (i == 0 || !isWord(b[i-1])) && (i+len(s) == len(b) || !isWord(b[i+len(s)])) {
			return true
		}
		at = i + 1
	}
}

func isWord(c byte) bool {
	return c == '_' || ('0' <= c && c <= '9') || isLetter(c)
}

func isLetter(c byte) bool { return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') }

// containsLetter reports whether a span holds a letter. Only the span grammar
// above reaches this, which admits nothing outside ASCII, so the byte reading
// is the whole of it.
func containsLetter(s string) bool {
	return strings.IndexFunc(s, func(r rune) bool { return r < 128 && isLetter(byte(r)) }) >= 0
}

// exists reports whether a path is there, as a file or as a directory: a plan
// names both, and a directory that is there is not a missing path.
func exists(p string) bool {
	_, err := os.Stat(p)
	return err == nil
}
