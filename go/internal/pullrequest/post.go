package pullrequest

import (
	"context"
	"encoding/json/jsontext"
	"encoding/json/v2"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Assessment is the verdict a review carries, in the words the skill writes.
type Assessment string

const (
	// AssessmentApprove is a review with nothing that has to change. The
	// values are Japanese because the review file they are read out of is
	// written by a skill that speaks Japanese, and these are what it writes.
	AssessmentApprove Assessment = "Approve可能"
	// AssessmentChanges is a review with something that has to change.
	AssessmentChanges Assessment = "修正が必要"
	// AssessmentDiscuss is a review that asks rather than tells.
	AssessmentDiscuss Assessment = "要議論"
)

// event is what GitHub is told to do with the review.
//
// The mapping lives here rather than in the prompt so that it cannot drift with
// a reviewer's politeness: a verdict of "changes needed" posts as a request for
// changes whether or not the prose reads gently.
func (a Assessment) event() (string, error) {
	switch a {
	case AssessmentApprove:
		return "APPROVE", nil
	case AssessmentChanges:
		return "REQUEST_CHANGES", nil
	case AssessmentDiscuss:
		return "COMMENT", nil
	default:
		return "", fmt.Errorf("invalid assessment: %s (expected: %s | %s | %s)",
			a, AssessmentApprove, AssessmentChanges, AssessmentDiscuss)
	}
}

// SubmissionComment is one comment anchored to a line of the diff.
type SubmissionComment struct {
	Path string
	Line int
	Body string
}

// Submission is a review ready to post, with every body already resolved.
type Submission struct {
	Assessment Assessment
	Body       string
	Comments   []SubmissionComment
}

// ParseSubmission reads a review file and resolves the bodies it names.
//
// Each body may be given inline or as the name of a file beside it. Long prose
// hand-written as a JSON string loses its whole meaning to one missed escape,
// so writing plain markdown and naming it is the supported way round. A named
// body has to be a bare file name in the work dir: allowing a path would let a
// review reach round the directory binding that keeps parallel reviews of
// different pull requests apart.
func ParseSubmission(b []byte, workDir, file string) (Submission, error) {
	// The fields are read as raw JSON and checked by hand, because the shapes
	// this rejects have their own messages: a review whose resolve is a string
	// or whose comments are an object needs to be told what the field should
	// have been, not what a decoder made of it.
	var wire struct {
		Assessment jsontext.Value `json:"assessment"`
		Body       jsontext.Value `json:"body"`
		BodyFile   jsontext.Value `json:"body_file"`
		Comments   jsontext.Value `json:"comments"`
	}
	if err := json.Unmarshal(b, &wire); err != nil {
		return Submission{}, fmt.Errorf("invalid JSON in %s (%v)", file, err)
	}
	if len(wire.Assessment) == 0 || wire.Assessment.Kind() == 'n' {
		return Submission{}, fmt.Errorf("assessment missing in %s", file)
	}

	if !bodyShapeOK(wire.Body, wire.BodyFile) {
		return Submission{}, fmt.Errorf("exactly one of body (string) / body_file (non-empty string) is required in %s", file)
	}
	body, err := resolveBody(wire.Body, wire.BodyFile, workDir)
	if err != nil {
		return Submission{}, err
	}

	if wire.Comments.Kind() != '[' {
		return Submission{}, commentsError(file)
	}
	var comments []struct {
		Path     jsontext.Value `json:"path"`
		Line     jsontext.Value `json:"line"`
		Body     jsontext.Value `json:"body"`
		BodyFile jsontext.Value `json:"body_file"`
	}
	if err := json.Unmarshal(wire.Comments, &comments); err != nil {
		return Submission{}, commentsError(file)
	}

	out := Submission{Assessment: Assessment(text(wire.Assessment)), Body: body, Comments: []SubmissionComment{}}
	for _, c := range comments {
		if c.Path.Kind() != '"' || c.Line.Kind() != '0' || !bodyShapeOK(c.Body, c.BodyFile) {
			return Submission{}, commentsError(file)
		}
		line, err := strconv.Atoi(string(c.Line))
		if err != nil {
			return Submission{}, commentsError(file)
		}
		commentBody, err := resolveBody(c.Body, c.BodyFile, workDir)
		if err != nil {
			return Submission{}, err
		}
		out.Comments = append(out.Comments, SubmissionComment{Path: text(c.Path), Line: line, Body: commentBody})
	}
	return out, nil
}

func commentsError(file string) error {
	return fmt.Errorf("comments must be an array of {path: string, line: number, body xor body_file: string} in %s", file)
}

// bodyShapeOK reports whether exactly one of the two forms is present and is a
// usable string.
func bodyShapeOK(body, file jsontext.Value) bool {
	switch {
	case len(body) > 0 && len(file) > 0:
		return false
	case len(body) > 0:
		return body.Kind() == '"'
	case len(file) > 0:
		return file.Kind() == '"' && text(file) != ""
	default:
		return false
	}
}

// resolveBody turns whichever form was used into the text.
func resolveBody(body, file jsontext.Value, workDir string) (string, error) {
	if len(body) > 0 {
		return text(body), nil
	}
	name := text(file)
	if strings.ContainsRune(name, filepath.Separator) {
		return "", fmt.Errorf("body_file must be a bare filename in the review work dir (no path separators): %s", name)
	}
	content, err := os.ReadFile(filepath.Join(workDir, name))
	if err != nil {
		return "", fmt.Errorf("body_file not found in the review work dir: %s", filepath.Join(workDir, name))
	}
	return string(content), nil
}

// text unquotes a JSON string, and hands back anything else as it was written
// so that an error naming it says what the file actually holds.
func text(v jsontext.Value) string {
	var s string
	if err := json.Unmarshal(v, &s); err != nil {
		return string(v)
	}
	return s
}

// Posted is where the review ended up.
type Posted struct {
	URL string `json:"url"`
}

// Post submits a review, after checking that every comment still points at a
// line of the diff.
//
// GitHub rejects a comment on a line outside the diff with a 422, and by then
// the review is half posted and the reviewer is looking at an error rather than
// at the anchoring problem. Checking first turns that into something a person
// can fix before anything is published.
func Post(ctx context.Context, r runner.Runner, c *ghapi.Client, dir string, target Target, sub Submission) (Posted, error) {
	event, err := sub.Assessment.event()
	if err != nil {
		return Posted{}, err
	}
	if err := RequireHead(ctx, r, dir, target.HeadOID, "posting"); err != nil {
		return Posted{}, err
	}
	if err := checkAnchors(ctx, r, dir, target.BaseRef, sub.Comments); err != nil {
		return Posted{}, err
	}

	type comment struct {
		Path string `json:"path"`
		Line int    `json:"line"`
		Body string `json:"body"`
	}
	payload := struct {
		CommitID string    `json:"commit_id"`
		Event    string    `json:"event"`
		Body     string    `json:"body"`
		Comments []comment `json:"comments"`
	}{CommitID: target.HeadOID, Event: event, Body: sub.Body, Comments: []comment{}}
	for _, s := range sub.Comments {
		payload.Comments = append(payload.Comments, comment(s))
	}

	var response struct {
		HTMLURL string `json:"html_url"`
	}
	if err := c.Post(ctx, fmt.Sprintf("repos/%s/pulls/%d/reviews", target.Repo, target.Number), payload, &response); err != nil {
		return Posted{}, fmt.Errorf("failed to post review (gh api): %v", err)
	}
	if response.HTMLURL == "" {
		return Posted{}, fmt.Errorf("review posted but html_url missing in the API response")
	}
	return Posted{URL: response.HTMLURL}, nil
}

// checkAnchors reports the comments that point at lines the diff does not have.
func checkAnchors(ctx context.Context, r runner.Runner, dir, baseRef string, comments []SubmissionComment) error {
	if len(comments) == 0 {
		return nil
	}
	span := "origin/" + baseRef + "...HEAD"
	// The colour and external-diff settings are overridden rather than
	// inherited: whichever a person has configured, this has to read the same
	// unified diff.
	out, err := r.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", dir, "-c", "color.diff=false", "diff", "--no-ext-diff", span},
	})
	if err != nil {
		return fmt.Errorf("failed to read the diff for %s: %v", span, err)
	}

	lines := diffLines(string(out))
	var invalid []string
	for _, c := range comments {
		if !lines[c.Path][c.Line] {
			invalid = append(invalid, c.Path+":"+strconv.Itoa(c.Line))
		}
	}
	if len(invalid) == 0 {
		return nil
	}
	return fmt.Errorf(
		"the following review comments point to lines absent from the current diff (origin/%s...HEAD); re-anchor them before posting:\n%s",
		baseRef, strings.Join(invalid, "\n"))
}

// diffLines reads a unified diff into the lines a comment may be anchored to:
// per file, the added lines and the context around them, numbered on the new
// side.
//
// The order of the cases is load-bearing. An added line whose own text starts
// with "++" is printed as "+++ …" and is indistinguishable from a file header
// by its prefix alone, so a line inside a hunk is read as content first. A real
// header only ever appears after a "diff --git" line has ended the hunk.
func diffLines(diff string) map[string]map[int]bool {
	out := map[string]map[int]bool{}
	var file string
	var n int
	var inHunk bool

	for line := range strings.SplitSeq(diff, "\n") {
		switch {
		case strings.HasPrefix(line, `\ `):
			// "\ No newline at end of file" belongs to the line before it.
		case inHunk && strings.HasPrefix(line, "+"), inHunk && strings.HasPrefix(line, " "):
			if out[file] == nil {
				out[file] = map[int]bool{}
			}
			out[file][n] = true
			n++
		case inHunk && strings.HasPrefix(line, "-"):
			// A removed line has no number on the new side to point at.
		case strings.HasPrefix(line, "+++ "):
			file = strings.TrimPrefix(strings.TrimPrefix(line, "+++ "), "b/")
		case strings.HasPrefix(line, "@@ "):
			if start, ok := hunkStart(line); ok {
				n, inHunk = start, true
			}
		default:
			inHunk = false
		}
	}
	return out
}

// hunkStart reads the first line number of a hunk's new side out of its header.
func hunkStart(header string) (int, bool) {
	_, rest, ok := strings.Cut(header, "+")
	if !ok {
		return 0, false
	}
	digits := rest
	if i := strings.IndexFunc(rest, func(r rune) bool { return r < '0' || r > '9' }); i >= 0 {
		digits = rest[:i]
	}
	n, err := strconv.Atoi(digits)
	if err != nil {
		return 0, false
	}
	return n, true
}
