package pullrequest

import (
	"context"
	"encoding/json/jsontext"
	"encoding/json/v2"
	"errors"
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

// ReviewFile is the document `ccx pr post-review` reads.
//
// Each body is given inline or as the name of a file beside it, exactly one of
// the two. Long prose hand-written as a JSON string loses its whole meaning to
// one missed escape, so writing plain markdown and naming it is the supported
// way round.
type ReviewFile struct {
	// The verdict, which decides whether the review is posted as an approval,
	// a request for changes or a comment.
	Assessment *Assessment `json:"assessment" contract:"required"`
	// The review body, written inline.
	Body *string `json:"body"`
	// The name of a markdown file in the review work dir holding the body. A
	// bare file name: a path would let a review reach round the directory
	// binding that keeps parallel reviews of different pull requests apart.
	BodyFile *string `json:"body_file"`
	// The remarks anchored to lines of the diff, empty for a review that is
	// all body.
	Comments []ReviewFileComment `json:"comments" contract:"required"`
}

// ReviewFileComment is one remark anchored to a line of the diff.
type ReviewFileComment struct {
	Path *string `json:"path" contract:"required"`
	// The line number on the new side of the diff.
	Line *int `json:"line" contract:"required"`
	// The remark, inline or named, exactly as the review body is.
	Body     *string `json:"body"`
	BodyFile *string `json:"body_file"`
}

// ParseSubmission reads a review file and resolves the bodies it names.
func ParseSubmission(b []byte, workDir, file string) (Submission, error) {
	var wire ReviewFile
	if err := json.Unmarshal(b, &wire); err != nil {
		return Submission{}, submissionError(err, file)
	}
	if wire.Assessment == nil {
		return Submission{}, fmt.Errorf("assessment missing in %s", file)
	}
	if !bodyShapeOK(wire.Body, wire.BodyFile) {
		return Submission{}, bodyShapeError(file)
	}
	body, err := resolveBody(wire.Body, wire.BodyFile, workDir)
	if err != nil {
		return Submission{}, err
	}
	// Absent and null both arrive as nil, and neither is an empty list of
	// comments: a review file that forgot the key has not said there are none.
	if wire.Comments == nil {
		return Submission{}, commentsError(file)
	}

	out := Submission{Assessment: *wire.Assessment, Body: body, Comments: []SubmissionComment{}}
	for _, c := range wire.Comments {
		if c.Path == nil || c.Line == nil || !bodyShapeOK(c.Body, c.BodyFile) {
			return Submission{}, commentsError(file)
		}
		commentBody, err := resolveBody(c.Body, c.BodyFile, workDir)
		if err != nil {
			return Submission{}, err
		}
		out.Comments = append(out.Comments, SubmissionComment{Path: *c.Path, Line: *c.Line, Body: commentBody})
	}
	return out, nil
}

// submissionError turns the decoder's complaint back into the message the
// offending field has of its own.
//
// The fields carry their real types so that the contract can be rendered from
// them rather than written out beside them. What that costs is this: a review
// whose comments are an object still has to be told what the field should have
// held, not what a decoder made of it, so the pointer the decoder reports is
// mapped back to the field's own message.
func submissionError(err error, file string) error {
	var se *json.SemanticError
	if errors.As(err, &se) {
		switch firstToken(se.JSONPointer) {
		case "comments":
			return commentsError(file)
		case "assessment":
			return fmt.Errorf("assessment must be a string in %s", file)
		case "body", "body_file":
			return bodyShapeError(file)
		}
	}
	return fmt.Errorf("invalid JSON in %s (%v)", file, err)
}

// firstToken is the top-level field a pointer reaches into, which is as deep
// as any of these messages distinguishes.
func firstToken(p jsontext.Pointer) string {
	for tok := range p.Tokens() {
		return tok
	}
	return ""
}

func bodyShapeError(file string) error {
	return fmt.Errorf("exactly one of body (string) / body_file (non-empty string) is required in %s", file)
}

func commentsError(file string) error {
	return fmt.Errorf("comments must be an array of {path: string, line: number, body xor body_file: string} in %s", file)
}

// bodyShapeOK reports whether exactly one of the two forms is present and is a
// usable string.
func bodyShapeOK(body, file *string) bool {
	switch {
	case body != nil && file != nil:
		return false
	case body != nil:
		return true
	case file != nil:
		return *file != ""
	default:
		return false
	}
}

// resolveBody turns whichever form was used into the text.
func resolveBody(body, file *string, workDir string) (string, error) {
	if body != nil {
		return *body, nil
	}
	if strings.ContainsRune(*file, filepath.Separator) {
		return "", fmt.Errorf("body_file must be a bare filename in the review work dir (no path separators): %s", *file)
	}
	content, err := os.ReadFile(filepath.Join(workDir, *file))
	if err != nil {
		return "", fmt.Errorf("body_file not found in the review work dir: %s", filepath.Join(workDir, *file))
	}
	return string(content), nil
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
