package pullrequest

import (
	"context"
	"encoding/json/v2"
	"errors"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/contract"
	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// Resolving a review thread takes it off somebody's list of things to answer,
// and it is not something to undo. Which threads may be touched is therefore
// settled here rather than in a prompt, and so is which thread an entry means:
// the input names a path and a line, which is what the writer was reasoning
// about, and the opaque id is looked up rather than transcribed. Reply checks
// its whole request before it sends anything, so a run either acts on all of
// its input or on none of it. The checking is inside Reply rather than beside
// it precisely so that no caller can leave it out.

// KnownThread is one review thread as the pull request context recorded it.
//
// A subset of what a context holds, deliberately: naming only what a selector
// matches on and what a refusal has to print keeps a field this never looks at
// from becoming a reason it fails.
type KnownThread struct {
	ID           string
	Path         string
	Line         *int
	OriginalLine *int
	OpenedBy     *string
	Ball         Ball
	// ResolvableByMe is the context's judgement, not one made again here from
	// different information.
	ResolvableByMe bool
	// LastCommentURL is what the live read is compared against; empty for a
	// thread the context found no comments on.
	LastCommentURL string
}

// ThreadAction is one entry of ThreadsFile, with its body already read and its
// selector not yet resolved.
//
// What each field means is on ThreadsFileEntry, which is the half the contract
// publishes; the difference here is that body and body_file have collapsed into
// the text they named.
type ThreadAction struct {
	Path    string
	Line    *int
	ID      *string
	Body    *string
	Resolve bool
}

// selector is how an entry is named back to the caller, which is what it wrote
// rather than what it resolved to.
func (a ThreadAction) selector() string {
	if a.Line == nil {
		return a.Path
	}
	return a.Path + ":" + strconv.Itoa(*a.Line)
}

// plannedAction is one entry with its thread found.
type plannedAction struct {
	thread  KnownThread
	body    *string
	resolve bool
}

// RepliedThread is a reply that was posted.
type RepliedThread struct {
	ID string `json:"id"`
	// path, line and original_line are the thread as the context described it,
	// so that a report reads as the file that was written rather than as ids.
	Path         string `json:"path"`
	Line         *int   `json:"line"`
	OriginalLine *int   `json:"original_line"`
	URL          string `json:"url"`
}

// FailedResolve is a reply that landed with a resolve that did not.
type FailedResolve struct {
	ID string `json:"id"`
	// What GitHub said, which is usually about write access.
	Error string `json:"error"`
}

// ThreadReplies is what one run did.
type ThreadReplies struct {
	Replied  []RepliedThread `json:"replied"`
	Resolved []string        `json:"resolved"`
	// A degradation rather than a failure: the replies are
	// posted, and a fork or a repository without write access cannot resolve
	// at all. Stopping the whole review over it would help nobody.
	ResolveFailed []FailedResolve `json:"resolve_failed"`
	Warnings      []string        `json:"warnings"`
}

// PlannedThread is one thread a run would act on.
type PlannedThread struct {
	ID           string  `json:"id"`
	Path         string  `json:"path"`
	Line         *int    `json:"line"`
	OriginalLine *int    `json:"original_line"`
	OpenedBy     *string `json:"opened_by"`
	// Whether a reply would be posted; false for an entry that only
	// resolves.
	Reply   bool `json:"reply"`
	Resolve bool `json:"resolve"`
}

// ReplyPlan is what a run would do, in the order it would do it.
type ReplyPlan struct {
	Plan []PlannedThread `json:"plan"`
}

// ThreadsFile is the document `ccx pr reply-threads` reads.
type ThreadsFile struct {
	// The threads to act on, named by where they are rather than by their
	// ids. Only threads the pull request context flagged as ours to move on
	// are reachable, so that one run cannot settle somebody else's remark or
	// reopen a resolved one.
	Threads []ThreadsFileEntry `json:"threads" contract:"required"`
}

// ThreadsFileEntry is one thread's reply, its resolve, or both.
type ThreadsFileEntry struct {
	// The thread's path, as the context records it.
	Path *string `json:"path" contract:"required"`
	// The line, matching either line or original_line. Leave it out where the
	// path has only one thread we may act on; a path with several and no line
	// is refused rather than guessed at.
	Line *int `json:"line"`
	// The thread's id, to break a tie where a path and a line reach more than
	// one. It has to be one of them: an id from elsewhere is refused, which is
	// what catches an id copied from the wrong thread.
	ID        *string `json:"id"`
	ReplyBody `contract:"exclusive"`
	// Whether to mark the thread resolved. Required even when it is false,
	// since an entry that neither replies nor resolves does nothing.
	Resolve *bool `json:"resolve" contract:"required"`
}

// ReplyBody is the reply's prose, or neither of its two keys where the entry
// only resolves.
type ReplyBody struct {
	// The reply, written inline. Resolving without replying is how a repeated
	// run avoids saying the same thing twice, and how a resolve is retried
	// where the reply already landed.
	Body *string `json:"body"`
	// The name of a markdown file in the work dir holding the reply. A bare
	// file name: a path would let an entry reach round the directory binding
	// that keeps parallel runs on different pull requests apart.
	BodyFile *string `json:"body_file"`
}

// ParseThreadActions reads the threads file, resolving the bodies it names
// against the work dir.
func ParseThreadActions(b []byte, workDir, file string) ([]ThreadAction, error) {
	notArray := fmt.Errorf("threads must be an array in %s", file)
	shape := fmt.Errorf(
		"threads must be an array of {path: string, line?: number, id?: string, resolve: boolean, body xor body_file?: string} in %s", file)

	var wire ThreadsFile
	if err := contract.Unmarshal(b, &wire, file); err != nil {
		// A field-level refusal names its field and travels as its own type;
		// the wrapping below would take both away.
		var ve *contract.ViolationError
		if errors.As(err, &ve) {
			return nil, err
		}
		// Mapping the decoder's pointer back is what keeps the field's own
		// message rather than the decoder's.
		var se *json.SemanticError
		if errors.As(err, &se) && firstToken(se.JSONPointer) == "threads" {
			if se.JSONPointer == "/threads" {
				return nil, notArray
			}
			return nil, shape
		}
		return nil, fmt.Errorf("invalid JSON in %s (%v)", file, err)
	}

	out := make([]ThreadAction, 0, len(wire.Threads))
	for _, e := range wire.Threads {
		// Both absent is the resolve-only form rather than a violation, which
		// is why this group is declared without required.
		var body *string
		if e.Body != nil || e.BodyFile != nil {
			if !bodyShapeOK(e.BodyFile) {
				return nil, bodyShapeError(file)
			}
			text, err := resolveBody(e.Body, e.BodyFile, workDir)
			if err != nil {
				return nil, err
			}
			body = &text
		}
		out = append(out, ThreadAction{Path: *e.Path, Line: e.Line, ID: e.ID, Body: body, Resolve: *e.Resolve})
	}
	return out, nil
}

// ReplyRequest is one run's whole input.
type ReplyRequest struct {
	Actions []ThreadAction
	// Threads is every thread the pull request context holds, not only the
	// ones a run may act on: an id that belongs to another thread is refused
	// by naming where it does belong, which needs the rest of them.
	Threads []KnownThread
	// ContextFile and ThreadsFile are named in the refusals, because what a
	// caller does about one is to edit the file it names. ThreadsFile also
	// decides where the record of what has been posted lives.
	ContextFile string
	ThreadsFile string
}

// plan resolves every selector and runs every check, up to but not including
// the first mutation.
//
// Shared whole by Reply and DryRun, which is what makes "the plan shown is the
// plan executed" true rather than a claim.
func plan(ctx context.Context, c *ghapi.Client, req ReplyRequest) ([]plannedAction, error) {
	if err := checkEntries(req.Actions); err != nil {
		return nil, err
	}
	planned, err := resolveSelectors(req.Actions, req.Threads, req.ContextFile)
	if err != nil {
		return nil, err
	}
	if err := checkResolved(planned, req.ThreadsFile); err != nil {
		return nil, err
	}
	if err := checkLive(ctx, c, planned, req.ContextFile); err != nil {
		return nil, err
	}
	return planned, nil
}

// checkEntries rejects what is wrong with an entry on its own, before any
// thread is looked up.
func checkEntries(actions []ThreadAction) error {
	var blank, noop []string
	for _, a := range actions {
		if a.Body != nil && strings.TrimSpace(*a.Body) == "" {
			blank = append(blank, a.selector())
		}
		if a.Body == nil && !a.Resolve {
			noop = append(noop, a.selector())
		}
	}
	if len(blank) > 0 {
		return fmt.Errorf("reply body is present but blank for thread(s): %s (omit body entirely to resolve without replying)",
			strings.Join(blank, ", "))
	}
	if len(noop) > 0 {
		return fmt.Errorf("thread(s) with neither a reply body nor resolve: true do nothing: %s", strings.Join(noop, ", "))
	}
	return nil
}

// resolveSelectors turns each entry's path, line and id into the one thread it
// names, or refuses with the threads it could have meant.
func resolveSelectors(actions []ThreadAction, threads []KnownThread, contextFile string) ([]plannedAction, error) {
	out := make([]plannedAction, 0, len(actions))
	for _, a := range actions {
		candidates := matching(threads, a)
		if a.ID != nil {
			candidates = slices.DeleteFunc(candidates, func(t KnownThread) bool { return t.ID != *a.ID })
			if len(candidates) == 0 {
				return nil, wrongID(*a.ID, a, threads, contextFile)
			}
		}

		switch len(candidates) {
		case 1:
			out = append(out, plannedAction{thread: candidates[0], body: a.Body, resolve: a.Resolve})
		case 0:
			return nil, fmt.Errorf("no thread we may act on matches %s\n%s", a.selector(), atPath(threads, a.Path))
		default:
			return nil, fmt.Errorf("%d threads we may act on match %s; add \"id\" to say which:\n%s",
				len(candidates), a.selector(), list(candidates))
		}
	}
	return out, nil
}

// matching is the threads a selector reaches: ours to move on, at that path,
// and on that line if one was given.
//
// A line matches either line or original_line, because line is null on a thread
// whose lines have left the diff — which for the author is the state right
// after the push that fixed them.
func matching(threads []KnownThread, a ThreadAction) []KnownThread {
	var out []KnownThread
	for _, t := range threads {
		if t.Ball != BallMine || t.Path != a.Path {
			continue
		}
		if a.Line != nil && !onLine(t, *a.Line) {
			continue
		}
		out = append(out, t)
	}
	return out
}

func onLine(t KnownThread, line int) bool {
	return (t.Line != nil && *t.Line == line) || (t.OriginalLine != nil && *t.OriginalLine == line)
}

// wrongID is the refusal for an id that is not among the threads the rest of
// the selector reached.
//
// It names where the id does belong, since the mistake it catches is an id
// copied from another thread — the one that put a reply about a clock skew onto
// a remark about a cache test.
func wrongID(id string, a ThreadAction, threads []KnownThread, contextFile string) error {
	for _, t := range threads {
		if t.ID != id {
			continue
		}
		return fmt.Errorf("id %s is not a thread we may act on at %s: it is %s, opened by %s, ball %s\n%s",
			id, a.selector(), position(t), login(t.OpenedBy), t.Ball, atPath(threads, a.Path))
	}
	return fmt.Errorf("id %s is not a thread in %s", id, contextFile)
}

// atPath is the "you could have meant these" half of a refusal.
//
// Where the path holds nothing we may act on it names the threads that are
// there with the ball each is waiting on, rather than reporting an empty match:
// "there is a thread here, it is just not yours to move" sends the caller to
// the protocol, and "nothing matches" sends them back to try another line.
func atPath(threads []KnownThread, path string) string {
	if ours := matching(threads, ThreadAction{Path: path}); len(ours) > 0 {
		return "threads we may act on at " + path + ":\n" + list(ours)
	}

	var others []string
	for _, t := range threads {
		if t.Path == path {
			others = append(others, fmt.Sprintf("  %s  %s  opened by %s  ball %s",
				t.ID, position(t), login(t.OpenedBy), t.Ball))
		}
	}
	if len(others) == 0 {
		return "no thread at all is recorded at " + path
	}
	return "no thread at " + path + " is ours to act on; the threads there are:\n" + strings.Join(others, "\n")
}

// list renders threads one to a line, with everything a caller picks between
// them by.
func list(threads []KnownThread) string {
	lines := make([]string, 0, len(threads))
	for _, t := range threads {
		lines = append(lines, fmt.Sprintf("  %s  %s  opened by %s", t.ID, position(t), login(t.OpenedBy)))
	}
	return strings.Join(lines, "\n")
}

// position is a thread's path with both of its line numbers, since a selector
// may be written against either.
func position(t KnownThread) string {
	return fmt.Sprintf("%s line %s original_line %s", t.Path, number(t.Line), number(t.OriginalLine))
}

func number(n *int) string {
	if n == nil {
		return "null"
	}
	return strconv.Itoa(*n)
}

func login(l *string) string {
	if l == nil {
		return "an account that no longer exists"
	}
	return *l
}

// checkResolved rejects what only becomes visible once the threads are known.
func checkResolved(planned []plannedAction, threadsFile string) error {
	var unresolvable, dupes []string
	seen := map[string]bool{}
	for _, p := range planned {
		if p.resolve && !p.thread.ResolvableByMe {
			unresolvable = append(unresolvable, describe(p.thread))
		}
		if seen[p.thread.ID] {
			dupes = append(dupes, describe(p.thread))
		}
		seen[p.thread.ID] = true
	}
	if len(unresolvable) > 0 {
		return fmt.Errorf("thread(s) we may not resolve (a person's remark is closed by that person, not by us): %s\nreply without resolve: true, and leave the thread open",
			strings.Join(unresolvable, ", "))
	}
	if len(dupes) > 0 {
		return fmt.Errorf("duplicate thread(s) would post duplicate replies: %s", strings.Join(dupes, ", "))
	}

	// Eligibility was frozen when the context was fetched, so running the same
	// file again would pass every check and reply twice. The record of what was
	// posted is what stops that.
	log := PostedLog(threadsFile)
	if resent := wouldResend(log, planned); len(resent) > 0 {
		return fmt.Errorf("thread(s) already replied to in an earlier run of this file, with nothing said since: %s\nremove them from %s (resending would post duplicate replies); the record is in %s",
			strings.Join(resent, ","), threadsFile, log)
	}
	return nil
}

// describe names a thread the way a caller wrote it, with the id it resolved to.
func describe(t KnownThread) string {
	where := t.Path
	if t.Line != nil {
		where += ":" + strconv.Itoa(*t.Line)
	} else if t.OriginalLine != nil {
		where += ":" + strconv.Itoa(*t.OriginalLine)
	}
	return where + " (" + t.ID + ")"
}

// checkLive re-reads every thread a run would touch and stops the whole run if
// any of them has moved since the context was fetched.
//
// The context is a snapshot, and between fetching it and writing the replies a
// reviewer may answer or resolve. Replying then puts an answer under a remark
// that has already been withdrawn, and resolving discards an answer nobody has
// read. One thread stops all of them, since the file was written as one
// judgement of one view.
func checkLive(ctx context.Context, c *ghapi.Client, planned []plannedAction, contextFile string) error {
	var moved []string
	for _, p := range planned {
		var live struct {
			Node struct {
				IsResolved bool `json:"isResolved"`
				Comments   struct {
					Nodes []struct {
						URL string `json:"url"`
					} `json:"nodes"`
				} `json:"comments"`
			} `json:"node"`
		}
		if err := c.GraphQL(ctx, liveThreadQuery, map[string]any{"threadId": p.thread.ID}, &live); err != nil {
			return fmt.Errorf("failed to re-read thread %s before posting (GraphQL): %v", p.thread.ID, err)
		}

		var url string
		if nodes := live.Node.Comments.Nodes; len(nodes) > 0 {
			url = nodes[0].URL
		}
		switch {
		case live.Node.IsResolved:
			moved = append(moved, describe(p.thread)+": resolved since the context was fetched")
		case url != p.thread.LastCommentURL:
			moved = append(moved, describe(p.thread)+": answered since the context was fetched")
		}
	}
	if len(moved) == 0 {
		return nil
	}
	return fmt.Errorf("the pull request has moved since %s was fetched:\n  %s\nrerun `ccx pr context` and write the replies against the new view (nothing was posted)",
		contextFile, strings.Join(moved, "\n  "))
}

// PostedLog is where the threads replied to from one file are recorded.
//
// Beside the file itself, which is already bound to one pull request's work
// dir, so the record is bound to it too.
func PostedLog(threadsFile string) string { return threadsFile + ".posted" }

const replyMutation = `
mutation($threadId: ID!, $body: String!) {
  addPullRequestReviewThreadReply(input: {pullRequestReviewThreadId: $threadId, body: $body}) {
    comment { url }
  }
}`

const resolveMutation = `
mutation($threadId: ID!) {
  resolveReviewThread(input: {threadId: $threadId}) {
    thread { isResolved }
  }
}`

// AbortedReply is a run that stopped partway through replying.
//
// It carries what was posted and what was not, because the way out is to run
// again with the posted ones removed, and a person needs to know which those
// are even though the record on disk already prevents the mistake.
type AbortedReply struct {
	Message string
}

func (e *AbortedReply) Error() string { return e.Message }

// DryRun runs every check and sends no mutation, answering with what a run
// would do.
func DryRun(ctx context.Context, c *ghapi.Client, req ReplyRequest) (ReplyPlan, error) {
	planned, err := plan(ctx, c, req)
	if err != nil {
		return ReplyPlan{}, err
	}

	out := ReplyPlan{Plan: make([]PlannedThread, 0, len(planned))}
	for _, p := range planned {
		out.Plan = append(out.Plan, PlannedThread{
			ID: p.thread.ID, Path: p.thread.Path, Line: p.thread.Line, OriginalLine: p.thread.OriginalLine,
			OpenedBy: p.thread.OpenedBy, Reply: p.body != nil, Resolve: p.resolve,
		})
	}
	return out, nil
}

// Reply posts the replies and resolves the threads, one thread at a time.
//
// Sequential on purpose. A single request with aliases would let one resolve's
// permission error take the replies beside it down, and there would be no way
// to tell how much of it had applied; the number of threads in a real run is a
// single digit.
func Reply(ctx context.Context, c *ghapi.Client, req ReplyRequest) (ThreadReplies, error) {
	planned, err := plan(ctx, c, req)
	if err != nil {
		return ThreadReplies{}, err
	}

	out := ThreadReplies{Replied: []RepliedThread{}, Resolved: []string{}, ResolveFailed: []FailedResolve{}, Warnings: []string{}}
	log := PostedLog(req.ThreadsFile)

	for _, p := range planned {
		id := p.thread.ID
		if p.body != nil {
			var reply struct {
				AddPullRequestReviewThreadReply struct {
					Comment struct {
						URL string `json:"url"`
					} `json:"comment"`
				} `json:"addPullRequestReviewThreadReply"`
			}
			vars := map[string]any{"threadId": id, "body": *p.body}
			if err := c.GraphQL(ctx, replyMutation, vars, &reply); err != nil {
				return ThreadReplies{}, abort(p.thread, err.Error(), log, planned)
			}
			// Recorded before anything else can fail, so that a run which
			// stops after this still refuses to resend it. A missing url is
			// recorded as one, which is the unconditional refusal.
			url := reply.AddPullRequestReviewThreadReply.Comment.URL
			if err := record(log, id, url); err != nil {
				return ThreadReplies{}, err
			}
			if url == "" {
				return ThreadReplies{}, abort(p.thread, "reply was posted but comment url is missing in the API response", log, planned)
			}
			out.Replied = append(out.Replied, RepliedThread{
				ID: id, Path: p.thread.Path, Line: p.thread.Line, OriginalLine: p.thread.OriginalLine,
				URL: reply.AddPullRequestReviewThreadReply.Comment.URL,
			})
		}

		if p.resolve {
			var resolved struct {
				ResolveReviewThread struct {
					Thread struct {
						IsResolved bool `json:"isResolved"`
					} `json:"thread"`
				} `json:"resolveReviewThread"`
			}
			if err := c.GraphQL(ctx, resolveMutation, map[string]any{"threadId": id}, &resolved); err != nil {
				out.ResolveFailed = append(out.ResolveFailed, FailedResolve{ID: id, Error: err.Error()})
				continue
			}
			out.Resolved = append(out.Resolved, id)
		}
	}

	if len(out.ResolveFailed) > 0 {
		ids := make([]string, 0, len(out.ResolveFailed))
		for _, f := range out.ResolveFailed {
			ids = append(ids, f.ID)
		}
		out.Warnings = append(out.Warnings, fmt.Sprintf(
			"replied but could not resolve %d thread(s) (write access to the repository is required to resolve): %s. The replies are posted; resolve them manually or ask the author to.",
			len(out.ResolveFailed), strings.Join(ids, ", ")))
	}
	return out, nil
}

// abort builds the message a stopped run leaves behind.
//
// The unprocessed threads are named the way the file names them, since writing
// a file holding only those is what a retry does; the id goes beside each so
// the record on disk can be read against it.
func abort(t KnownThread, reason, log string, planned []plannedAction) error {
	done := postedHere(log, planned)
	var b strings.Builder
	fmt.Fprintf(&b, "failed to reply to thread %s:\n%s\n", describe(t), reason)
	if len(done) > 0 {
		fmt.Fprintf(&b, "already replied (do NOT resend on retry): %s\n", strings.Join(done, ", "))
	}
	fmt.Fprintf(&b, "not processed: %s\n", strings.Join(remaining(done, planned), ", "))
	return &AbortedReply{Message: strings.TrimSuffix(b.String(), "\n")}
}

// record notes a reply in the log, by the thread it landed on and the url it
// landed at.
//
// The url is what makes the record answer the question it is asked. Written
// before the reply's url is known — a mutation that failed after posting — the
// line carries the id alone, which reads as "posted, whereabouts unknown" and
// is refused unconditionally.
func record(log, id, url string) error {
	f, err := os.OpenFile(log, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("record the reply to %s: %w", id, err)
	}
	defer f.Close()
	if _, err := fmt.Fprintln(f, strings.TrimSpace(id+" "+url)); err != nil {
		return fmt.Errorf("record the reply to %s: %w", id, err)
	}
	return nil
}

// postedReply is one line of the log: a thread, and where our reply to it
// landed.
type postedReply struct{ id, url string }

// postedReplies reads the log, treating its absence as nothing having been
// posted.
func postedReplies(log string) []postedReply {
	b, err := os.ReadFile(log)
	if err != nil {
		return nil
	}
	var out []postedReply
	for line := range strings.SplitSeq(string(b), "\n") {
		if line == "" {
			continue
		}
		id, url, _ := strings.Cut(line, " ")
		out = append(out, postedReply{id: id, url: url})
	}
	return out
}

// wouldResend is the threads an entry would say the same thing on twice.
//
// Two entries the record must not refuse, because neither can duplicate
// anything and both are what a caller is told to do next: one that resolves
// without replying, which is how a resolve is retried where only it failed, and
// a fresh reply to a thread somebody has spoken in since — a later /loop
// iteration answering a new remark, not resending the old one. What is left is
// a reply to a thread whose newest comment is still the reply we posted, and
// that is the mistake worth stopping.
func wouldResend(log string, planned []plannedAction) []string {
	posted := postedReplies(log)
	var found []string
	for _, p := range planned {
		if p.body == nil {
			continue
		}
		for _, r := range posted {
			if r.id == p.thread.ID && (r.url == "" || r.url == p.thread.LastCommentURL) {
				found = append(found, describe(p.thread))
				break
			}
		}
	}
	slices.Sort(found)
	return slices.Compact(found)
}

// postedHere is the threads of this run the log already holds, whatever was
// said since: what a stopped run reports as done, rather than what a fresh one
// would refuse.
func postedHere(log string, planned []plannedAction) []string {
	var found []string
	for _, r := range postedReplies(log) {
		if slices.ContainsFunc(planned, func(p plannedAction) bool { return p.thread.ID == r.id }) {
			found = append(found, r.id)
		}
	}
	slices.Sort(found)
	return slices.Compact(found)
}

// remaining is the threads the log does not hold, in the order they were given.
func remaining(done []string, planned []plannedAction) []string {
	var left []string
	for _, p := range planned {
		if !slices.Contains(done, p.thread.ID) {
			left = append(left, describe(p.thread))
		}
	}
	return left
}

// ParseThreads reads the review threads a context holds, and the head they were
// judged at.
func ParseThreads(b []byte) (threads []KnownThread, headOID string, err error) {
	var wire struct {
		PR struct {
			HeadOID string `json:"head_oid"`
		} `json:"pr"`
		ReviewThreads *[]struct {
			ID             string  `json:"id"`
			Path           string  `json:"path"`
			Line           *int    `json:"line"`
			OriginalLine   *int    `json:"original_line"`
			OpenedBy       *string `json:"opened_by"`
			Ball           Ball    `json:"ball"`
			ResolvableByMe bool    `json:"resolvable_by_me"`
			LastComment    *struct {
				URL string `json:"url"`
			} `json:"last_comment"`
		} `json:"review_threads"`
	}
	if err := json.Unmarshal(b, &wire); err != nil {
		return nil, "", fmt.Errorf("decode the pull request context: %w", err)
	}
	if wire.ReviewThreads == nil {
		return nil, "", fmt.Errorf("review_threads missing")
	}
	if wire.PR.HeadOID == "" {
		return nil, "", fmt.Errorf("pr.head_oid missing")
	}

	for _, t := range *wire.ReviewThreads {
		known := KnownThread{
			ID: t.ID, Path: t.Path, Line: t.Line, OriginalLine: t.OriginalLine,
			OpenedBy: t.OpenedBy, Ball: t.Ball, ResolvableByMe: t.ResolvableByMe,
		}
		if t.LastComment != nil {
			known.LastCommentURL = t.LastComment.URL
		}
		threads = append(threads, known)
	}
	return threads, wire.PR.HeadOID, nil
}
