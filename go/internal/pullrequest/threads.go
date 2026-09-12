package pullrequest

import (
	"context"
	"fmt"
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

// checkedAction is one entry with its reply judged: the body travels on the
// entry rather than in a slice beside it, so that nothing has to keep two
// lists in step to know whose reply is whose.
type checkedAction struct {
	ThreadAction
	// reply is nil for an entry that only resolves. Not "body", which the
	// embedded entry already has as the text it was read from.
	reply *ghapi.Body
}

// plannedAction is one entry with its thread found and its reply judged.
type plannedAction struct {
	thread  KnownThread
	body    *ghapi.Body
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
	// ids. Reachable are the unresolved threads on our own pull request and
	// the ones we opened — whether the ball is ours or theirs, since a change
	// of design is answered on a thread we have already replied in. A thread
	// the context flagged as nobody's move is not, so that one run cannot
	// settle somebody else's remark or reopen a resolved one.
	Threads []ThreadsFileEntry `json:"threads" contract:"required"`
}

// ThreadsFileEntry is one thread's reply, its resolve, or both.
type ThreadsFileEntry struct {
	// The thread's path, as the context records it.
	Path *string `json:"path" contract:"required"`
	// The line, matching either line or original_line. Leave it out where the
	// path has only one reachable thread; a path with several and no line is
	// refused rather than guessed at.
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
	// The name of a markdown file in the work dir holding the reply. A path
	// would let an entry reach round the directory binding that keeps
	// parallel runs on different pull requests apart.
	BodyFile *string `json:"body_file" contract:"nonempty,barefilename"`
}

// ParseThreadActions reads the threads file, resolving the bodies it names
// against the work dir.
func ParseThreadActions(b []byte, workDir, file string) ([]ThreadAction, error) {
	var wire ThreadsFile
	if err := contract.Unmarshal(b, &wire, file); err != nil {
		return nil, err
	}

	out := make([]ThreadAction, 0, len(wire.Threads))
	for _, e := range wire.Threads {
		// Both absent is the resolve-only form rather than a violation, which
		// is why this group is declared without required.
		var body *string
		if e.Body != nil || e.BodyFile != nil {
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
	// caller does about one is to edit the file it names.
	ContextFile string
	ThreadsFile string
	// CurrentUser is the login the newest comment's author is compared against.
	CurrentUser string
}

// plan resolves every selector and runs every check, up to but not including
// the first mutation.
//
// Shared whole by Reply and DryRun, which is what makes "the plan shown is the
// plan executed" true rather than a claim.
func plan(ctx context.Context, c *ghapi.Client, req ReplyRequest) ([]plannedAction, error) {
	checked, err := checkEntries(req.Actions)
	if err != nil {
		return nil, err
	}
	planned, err := resolveSelectors(checked, req.Threads, req.ContextFile)
	if err != nil {
		return nil, err
	}
	if err := checkResolved(planned); err != nil {
		return nil, err
	}
	if err := checkLive(ctx, c, planned, req); err != nil {
		return nil, err
	}
	return planned, nil
}

// checkEntries rejects what is wrong with an entry on its own, before any
// thread is looked up, and answers with the reply each one would post — nil
// where the entry only resolves.
//
// The replies are judged here, with the rest of what one entry can be wrong
// about, so that a file holding one body GitHub would turn into notifications
// on unrelated issues is refused before any of its replies has been sent.
func checkEntries(actions []ThreadAction) ([]checkedAction, error) {
	checked := make([]checkedAction, 0, len(actions))
	var blank, noop, refused []string
	for _, a := range actions {
		entry := checkedAction{ThreadAction: a}
		switch {
		case a.Body == nil:
			if !a.Resolve {
				noop = append(noop, a.selector())
			}
		case strings.TrimSpace(*a.Body) == "":
			blank = append(blank, a.selector())
		default:
			body, err := ghapi.NewBody(*a.Body)
			if err != nil {
				refused = append(refused, fmt.Sprintf("%s: %v", a.selector(), err))
				break
			}
			entry.reply = &body
		}
		checked = append(checked, entry)
	}
	if len(blank) > 0 {
		return nil, fmt.Errorf("reply body is present but blank for thread(s): %s (omit body entirely to resolve without replying)",
			strings.Join(blank, ", "))
	}
	if len(noop) > 0 {
		return nil, fmt.Errorf("thread(s) with neither a reply body nor resolve: true do nothing: %s", strings.Join(noop, ", "))
	}
	if len(refused) > 0 {
		return nil, fmt.Errorf("reply body refused for thread(s):\n%s", strings.Join(refused, "\n"))
	}
	return checked, nil
}

// resolveSelectors turns each entry's path, line and id into the one thread it
// names, or refuses with the threads it could have meant.
func resolveSelectors(actions []checkedAction, threads []KnownThread, contextFile string) ([]plannedAction, error) {
	out := make([]plannedAction, 0, len(actions))
	for _, a := range actions {
		candidates := matching(threads, a.ThreadAction)
		if a.ID != nil {
			candidates = slices.DeleteFunc(candidates, func(t KnownThread) bool { return t.ID != *a.ID })
			if len(candidates) == 0 {
				return nil, wrongID(*a.ID, a.ThreadAction, threads, contextFile)
			}
		}

		switch len(candidates) {
		case 1:
			out = append(out, plannedAction{thread: candidates[0], body: a.reply, resolve: a.Resolve})
		case 0:
			return nil, fmt.Errorf("no thread we may reach matches %s\n%s", a.selector(), atPath(threads, a.Path))
		default:
			return nil, fmt.Errorf("%d threads we may reach match %s; add \"id\" to say which:\n%s",
				len(candidates), a.selector(), list(candidates))
		}
	}
	return out, nil
}

// matching is the threads a selector reaches: every one somebody still owes
// something on, at that path, and on that line if one was given.
//
// Whose move it is does not narrow this, only whether anybody's does: a thread
// waiting on the reviewer is one we have answered, and answering it again is
// what a change of design calls for. What is left out is the thread nobody owes
// anything on — resolved, or somebody else's remark on somebody else's pull
// request.
//
// A line matches either line or original_line, because line is null on a thread
// whose lines have left the diff — which for the author is the state right
// after the push that fixed them.
func matching(threads []KnownThread, a ThreadAction) []KnownThread {
	var out []KnownThread
	for _, t := range threads {
		if t.Ball == BallNone || t.Path != a.Path {
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
		return fmt.Errorf("id %s is not a thread we may reach at %s: it is %s, opened by %s, ball %s\n%s",
			id, a.selector(), position(t), login(t.OpenedBy), t.Ball, atPath(threads, a.Path))
	}
	return fmt.Errorf("id %s is not a thread in %s", id, contextFile)
}

// atPath is the "you could have meant these" half of a refusal.
//
// Where the path holds nothing reachable it names the threads that are there,
// rather than reporting an empty match: "there is a thread here, it is just
// settled" sends the caller to the protocol, and "nothing matches" sends them
// back to try another line. Unreachable is one state and not several, so it is
// said once in the sentence rather than in a column beside each thread.
func atPath(threads []KnownThread, path string) string {
	if ours := matching(threads, ThreadAction{Path: path}); len(ours) > 0 {
		return "threads we may reach at " + path + ":\n" + list(ours)
	}

	var others []KnownThread
	for _, t := range threads {
		if t.Path == path {
			others = append(others, t)
		}
	}
	if len(others) == 0 {
		return "no thread at all is recorded at " + path
	}
	return "no thread at " + path + " is one we may reach (every one there is ball none):\n" + list(others)
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
func checkResolved(planned []plannedAction) error {
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

// checkLive re-reads every thread a run would touch, stopping the whole run if
// any of them has moved since the context was fetched, and refusing an entry
// whose reply would say, again, what our own newest comment already says.
//
// The context is a snapshot, and between fetching it and writing the replies a
// reviewer may answer or resolve. Replying then puts an answer under a remark
// that has already been withdrawn, and resolving discards an answer nobody has
// read. One thread stops all of them, since the file was written as one
// judgement of one view.
//
// Eligibility is otherwise frozen at the moment the context was fetched, so
// running the same file again would pass every other check and reply twice.
// The live newest comment is what stops that, without needing a record of
// what a previous run posted: the resend check runs only where staleness does
// not already stop the thread, so a thread somebody else has since answered
// is caught above and the body comparison only ever sees a newest comment the
// context also saw.
func checkLive(ctx context.Context, c *ghapi.Client, planned []plannedAction, req ReplyRequest) error {
	var moved, repeats []string
	for _, p := range planned {
		var live struct {
			Node struct {
				IsResolved bool `json:"isResolved"`
				Comments   struct {
					Nodes []commentNode `json:"nodes"`
				} `json:"comments"`
			} `json:"node"`
		}
		if err := c.GraphQL(ctx, liveThreadQuery, map[string]any{"threadId": p.thread.ID}, &live); err != nil {
			return fmt.Errorf("failed to re-read thread %s before posting (GraphQL): %v", p.thread.ID, err)
		}

		var newest commentNode
		if nodes := live.Node.Comments.Nodes; len(nodes) > 0 {
			newest = nodes[0]
		}
		switch {
		case live.Node.IsResolved:
			moved = append(moved, describe(p.thread)+": resolved since the context was fetched")
		case newest.URL != p.thread.LastCommentURL:
			moved = append(moved, describe(p.thread)+": answered since the context was fetched")
		case p.body != nil && isLogin(newest.Author.login(), req.CurrentUser) && newest.Body == p.body.String():
			repeats = append(repeats, describe(p.thread))
		}
	}
	if len(moved) > 0 {
		return fmt.Errorf("the pull request has moved since %s was fetched:\n  %s\nrerun `ccx pr context` and write the replies against the new view (nothing was posted)",
			req.ContextFile, strings.Join(moved, "\n  "))
	}
	if len(repeats) > 0 {
		return fmt.Errorf("thread(s) whose newest comment is the reply this entry would post again: %s\nsay something else in %s, or remove the entry (posting it again would say it twice)",
			strings.Join(repeats, ", "), req.ThreadsFile)
	}
	return nil
}

// AbortedReply is a run that stopped partway through replying.
//
// It carries what was posted and what was not, because the way out is to run
// again with the posted ones removed, and a person needs to know which those
// are.
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

	for _, p := range planned {
		id := p.thread.ID
		if p.body != nil {
			url, err := c.ReplyToReviewThread(ctx, id, *p.body)
			if err != nil {
				return ThreadReplies{}, abort(p.thread, err.Error(), out.Replied, planned)
			}
			// Added before the url is checked, so a run that stops right
			// after still reports this reply as posted rather than
			// unprocessed.
			out.Replied = append(out.Replied, RepliedThread{
				ID: id, Path: p.thread.Path, Line: p.thread.Line, OriginalLine: p.thread.OriginalLine,
				URL: url,
			})
			if url == "" {
				return ThreadReplies{}, abort(p.thread, "reply was posted but comment url is missing in the API response", out.Replied, planned)
			}
		}

		if p.resolve {
			if err := c.ResolveReviewThread(ctx, id); err != nil {
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
// a person can read it against what already landed.
func abort(t KnownThread, reason string, replied []RepliedThread, planned []plannedAction) error {
	done := make([]string, 0, len(replied))
	for _, r := range replied {
		done = append(done, r.ID)
	}
	var b strings.Builder
	fmt.Fprintf(&b, "failed to reply to thread %s:\n%s\n", describe(t), reason)
	if len(done) > 0 {
		fmt.Fprintf(&b, "already replied (do NOT resend on retry): %s\n", strings.Join(done, ", "))
	}
	fmt.Fprintf(&b, "not processed: %s\n", strings.Join(remaining(done, planned), ", "))
	return &AbortedReply{Message: strings.TrimSuffix(b.String(), "\n")}
}

// remaining is the planned threads not among done, in the order they were given.
func remaining(done []string, planned []plannedAction) []string {
	var left []string
	for _, p := range planned {
		if !slices.Contains(done, p.thread.ID) {
			left = append(left, describe(p.thread))
		}
	}
	return left
}

// KnownThreads is the review threads a context holds, in the shape a run
// selects against.
func (c Context) KnownThreads() []KnownThread {
	threads := make([]KnownThread, 0, len(c.ReviewThreads))
	for _, t := range c.ReviewThreads {
		known := KnownThread{
			ID: t.ID, Path: t.Path, Line: t.Line, OriginalLine: t.OriginalLine,
			OpenedBy: t.OpenedBy, Ball: t.Ball, ResolvableByMe: t.ResolvableByMe,
		}
		if t.LastComment != nil {
			known.LastCommentURL = t.LastComment.URL
		}
		threads = append(threads, known)
	}
	return threads
}
