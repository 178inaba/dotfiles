package pullrequest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/contract"
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
// Long prose hand-written as a JSON string loses its whole meaning to one
// missed escape, so writing plain markdown and naming it is the supported way
// round.
type ReviewFile struct {
	// The verdict, which decides whether the review is posted as an approval,
	// a request for changes or a comment.
	Assessment *Assessment `json:"assessment" contract:"required"`
	ReviewBody `contract:"exclusive,required"`
	// The remarks anchored to lines of the diff, empty for a review that is
	// all body.
	Comments []ReviewFileComment `json:"comments" contract:"required"`
}

// ReviewBody is the review's own prose, the two ways of giving it declared
// together so that their adjacency is the type rather than a rule about it.
//
// Not shared with CommentBody below: the wire shape is the same, and what each
// key means to the reader of a --help is not.
type ReviewBody struct {
	// The review body, written inline.
	Body *string `json:"body"`
	// The name of a markdown file in the work dir holding the body. A path
	// would let a review reach round the directory binding that keeps
	// parallel runs on different pull requests apart.
	BodyFile *string `json:"body_file" contract:"nonempty,barefilename"`
}

// ReviewFileComment is one remark anchored to a line of the diff.
type ReviewFileComment struct {
	Path *string `json:"path" contract:"required"`
	// The line number on the new side of the diff.
	Line        *int `json:"line" contract:"required"`
	CommentBody `contract:"exclusive,required"`
}

// CommentBody is one remark's prose, given the same two ways the review body is.
type CommentBody struct {
	// The remark, inline or named, exactly as the review body is.
	Body     *string `json:"body"`
	BodyFile *string `json:"body_file" contract:"nonempty,barefilename"`
}

// ParseSubmission reads a review file and resolves the bodies it names.
func ParseSubmission(b []byte, workDir, file string) (Submission, error) {
	var wire ReviewFile
	if err := contract.Unmarshal(b, &wire, file); err != nil {
		return Submission{}, err
	}
	body, err := resolveBody(wire.Body, wire.BodyFile, workDir)
	if err != nil {
		return Submission{}, err
	}

	// Every field the loop below dereferences is one the declaration required,
	// which is what Unmarshal has just held the document to.
	out := Submission{Assessment: *wire.Assessment, Body: body, Comments: []SubmissionComment{}}
	for _, c := range wire.Comments {
		commentBody, err := resolveBody(c.Body, c.BodyFile, workDir)
		if err != nil {
			return Submission{}, err
		}
		out.Comments = append(out.Comments, SubmissionComment{Path: *c.Path, Line: *c.Line, Body: commentBody})
	}
	return out, nil
}

// resolveBody turns whichever form was used into the text.
//
// All that is left of the body checks: which of the two forms a document may
// give, and what a named one may look like, are the declaration's. Whether the
// file is there is not something the document can answer.
func resolveBody(body, file *string, workDir string) (string, error) {
	if body != nil {
		return *body, nil
	}
	content, err := os.ReadFile(filepath.Join(workDir, *file))
	if err != nil {
		return "", fmt.Errorf("body_file not found in the work dir: %s", filepath.Join(workDir, *file))
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
