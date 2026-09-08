// Package frontmatter reads the YAML block at the top of a markdown file.
//
// Two commands need it and had a copy each: ccx skill frontmatter reads a
// SKILL.md for the fields a skill is loaded by, and ccx plan docs reads a
// rule for the paths it is scoped to. The copies drifted, and the drift was
// only visible from outside — plandocs normalised CRLF before looking for the
// fences, skill did not, so a SKILL.md saved with CRLF found no fence at all
// and was reported as missing both of its fields, which the save-time hook
// then refused. What the two share is the format: where the block ends, what
// its line endings mean, and what a block holding no mapping amounts to. What
// each one requires of the fields it finds is its own, and stays with it.
package frontmatter

import (
	"strings"

	"github.com/goccy/go-yaml"
)

// fence opens and closes the block.
const fence = "---"

// Block is the frontmatter of one file.
type Block struct {
	// The lines between the fences, with the file's \r\n line endings
	// normalised to \n.
	Lines []string
	// The line number of the first line inside the fences, counting from the
	// start of the file. Always 2, since the opening fence has to be the first
	// line, but returned rather than assumed so that the line numbers a caller
	// reports come from the reader that found them.
	Start int
}

// Split returns the block at the top of content, and whether there is one.
//
// The extent is decided from the raw text. A parser's verdict cannot stand in
// for it: with no fence at all it would read the body instead, and the answer
// would depend on what the body happened to say.
//
// Carriage returns come off first, so that a file saved with CRLF has the
// frontmatter it looks like it has. The alternative — reporting CRLF rather
// than accepting it — would be a rule about line endings, which is the
// business of the .editorconfig a checked-out tree may or may not have, and
// this reader is run on trees that have none.
func Split(content []byte) (Block, bool) {
	lines := strings.Split(strings.ReplaceAll(string(content), "\r\n", "\n"), "\n")
	if lines[0] != fence {
		return Block{}, false
	}
	for i, line := range lines[1:] {
		if line == fence {
			return Block{Lines: lines[1 : i+1], Start: 2}, true
		}
	}
	return Block{}, false
}

// Fields parses the block into the values it declares.
//
// Into any rather than straight into a map: frontmatter that parses to a
// sequence or a scalar is not a parse failure, it is a block with no fields —
// which is the same as having none, and comes back empty. The parse error is
// returned as it is, for the caller that reports the parser's own words.
func (b Block) Fields() (map[string]any, error) {
	var document any
	if err := yaml.Unmarshal([]byte(strings.Join(b.Lines, "\n")), &document); err != nil {
		return nil, err
	}
	fields, _ := document.(map[string]any)
	return fields, nil
}
