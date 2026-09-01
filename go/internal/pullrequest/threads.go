package pullrequest

import (
	"context"
	"encoding/json/jsontext"
	"encoding/json/v2"
	"fmt"
	"os"
	"slices"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// Resolving a review thread takes it off the author's list of things to answer,
// and it is not something to undo. Which threads may be touched is therefore
// settled here rather than in a prompt: Reply checks its whole request before
// it sends anything, so a run either acts on all of its input or on none of
// it. The checking is inside Reply rather than beside it precisely so that no
// caller can leave it out.

// ThreadAction is one thread to reply to, to resolve, or both.
type ThreadAction struct {
	ID string
	// Body absent means resolving without replying, which is how a repeated
	// run avoids saying the same thing twice, and how a resolve is retried
	// where the reply already landed.
	Body    *string
	Resolve bool
}

// RepliedThread is a reply that was posted.
type RepliedThread struct {
	ID  string `json:"id"`
	URL string `json:"url"`
}

// FailedResolve is a reply that landed with a resolve that did not.
type FailedResolve struct {
	ID string `json:"id"`
	// Error is what GitHub said, which is usually about write access.
	Error string `json:"error"`
}

// ThreadReplies is what one run did.
type ThreadReplies struct {
	Replied  []RepliedThread `json:"replied"`
	Resolved []string        `json:"resolved"`
	// ResolveFailed is a degradation rather than a failure: the replies are
	// posted, and a fork or a repository without write access cannot resolve
	// at all. Stopping the whole review over it would help nobody.
	ResolveFailed []FailedResolve `json:"resolve_failed"`
	Warnings      []string        `json:"warnings"`
}

// ParseThreadActions reads the threads file.
//
// The fields are read as raw JSON and checked by hand, so that a resolve
// written as a string is answered with what the field should have held rather
// than with a decoder's complaint about it.
func ParseThreadActions(b []byte, file string) ([]ThreadAction, error) {
	var wire struct {
		Threads jsontext.Value `json:"threads"`
	}
	if err := json.Unmarshal(b, &wire); err != nil {
		return nil, fmt.Errorf("invalid JSON in %s (%v)", file, err)
	}
	if wire.Threads.Kind() != '[' {
		return nil, fmt.Errorf("threads must be an array in %s", file)
	}

	var entries []struct {
		ID      jsontext.Value `json:"id"`
		Body    jsontext.Value `json:"body"`
		Resolve jsontext.Value `json:"resolve"`
	}
	shape := fmt.Errorf("threads must be an array of {id: string, resolve: boolean, body?: string} in %s", file)
	if err := json.Unmarshal(wire.Threads, &entries); err != nil {
		return nil, shape
	}

	out := make([]ThreadAction, 0, len(entries))
	for _, e := range entries {
		resolve := e.Resolve.Kind()
		if e.ID.Kind() != '"' || (resolve != 't' && resolve != 'f') {
			return nil, shape
		}
		if len(e.Body) > 0 && e.Body.Kind() != '"' {
			return nil, shape
		}
		action := ThreadAction{ID: text(e.ID), Resolve: resolve == 't'}
		if len(e.Body) > 0 {
			body := text(e.Body)
			action.Body = &body
		}
		out = append(out, action)
	}
	return out, nil
}

// ReplyRequest is one run's whole input.
type ReplyRequest struct {
	Actions []ThreadAction
	// Eligible is what the pull request context flagged as awaiting our
	// confirmation: the judgement of whose thread this is, and whether it is
	// still open, belongs to whatever produced that file rather than being
	// made again here from different information.
	Eligible []string
	// ContextFile and ThreadsFile are named in the refusals, because what a
	// caller does about one is to edit the file it names. ThreadsFile also
	// decides where the record of what has been posted lives.
	ContextFile string
	ThreadsFile string
}

// validate rejects everything that would make a run act wrongly, before any of
// it is acted on.
func validate(actions []ThreadAction, eligible []string, contextFile, threadsFile string) error {
	var blank, noop, dupes []string
	seen := map[string]bool{}
	for _, a := range actions {
		if a.Body != nil && strings.TrimSpace(*a.Body) == "" {
			blank = append(blank, a.ID)
		}
		if a.Body == nil && !a.Resolve {
			noop = append(noop, a.ID)
		}
		if seen[a.ID] {
			dupes = append(dupes, a.ID)
		}
		seen[a.ID] = true
	}
	if len(blank) > 0 {
		return fmt.Errorf("reply body is present but blank for thread(s): %s (omit body entirely to resolve without replying)",
			strings.Join(blank, ", "))
	}
	if len(noop) > 0 {
		return fmt.Errorf("thread(s) with neither a reply body nor resolve: true do nothing: %s", strings.Join(noop, ", "))
	}
	if len(dupes) > 0 {
		return fmt.Errorf("duplicate thread id(s) would post duplicate replies: %s", strings.Join(dupes, ", "))
	}

	// Eligibility was frozen when the context was fetched, so running the same
	// file again would pass every check and reply twice. The record of what was
	// posted is what stops that.
	log := PostedLog(threadsFile)
	if resent := alreadyPosted(log, actions); len(resent) > 0 {
		return fmt.Errorf("thread(s) already replied to in an earlier run of this file: %s\nremove them from %s (resending would post duplicate replies); the record is in %s",
			strings.Join(resent, ","), threadsFile, log)
	}

	var ineligible []string
	for _, a := range actions {
		if !slices.Contains(eligible, a.ID) {
			ineligible = append(ineligible, a.ID)
		}
	}
	if len(ineligible) > 0 {
		return fmt.Errorf("thread(s) not awaiting our confirmation (not opened by us, already resolved, or we replied last): %s\nresolve/reply is limited to threads flagged awaiting_my_confirmation in %s",
			strings.Join(ineligible, ", "), contextFile)
	}
	return nil
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

// Reply posts the replies and resolves the threads, one thread at a time.
//
// Sequential on purpose. A single request with aliases would let one resolve's
// permission error take the replies beside it down, and there would be no way
// to tell how much of it had applied; the number of threads in a real run is a
// single digit.
func Reply(ctx context.Context, c *ghapi.Client, req ReplyRequest) (ThreadReplies, error) {
	if err := validate(req.Actions, req.Eligible, req.ContextFile, req.ThreadsFile); err != nil {
		return ThreadReplies{}, err
	}

	out := ThreadReplies{Replied: []RepliedThread{}, Resolved: []string{}, ResolveFailed: []FailedResolve{}, Warnings: []string{}}
	log := PostedLog(req.ThreadsFile)

	for _, a := range req.Actions {
		if a.Body != nil {
			var reply struct {
				AddPullRequestReviewThreadReply struct {
					Comment struct {
						URL string `json:"url"`
					} `json:"comment"`
				} `json:"addPullRequestReviewThreadReply"`
			}
			vars := map[string]any{"threadId": a.ID, "body": *a.Body}
			if err := c.GraphQL(ctx, replyMutation, vars, &reply); err != nil {
				return ThreadReplies{}, abort(a.ID, err.Error(), log, req.Actions)
			}
			// Recorded before anything else can fail, so that a run which
			// stops after this still refuses to resend it.
			if err := record(log, a.ID); err != nil {
				return ThreadReplies{}, err
			}
			if reply.AddPullRequestReviewThreadReply.Comment.URL == "" {
				return ThreadReplies{}, abort(a.ID, "reply was posted but comment url is missing in the API response", log, req.Actions)
			}
			out.Replied = append(out.Replied, RepliedThread{ID: a.ID, URL: reply.AddPullRequestReviewThreadReply.Comment.URL})
		}

		if a.Resolve {
			var resolved struct {
				ResolveReviewThread struct {
					Thread struct {
						IsResolved bool `json:"isResolved"`
					} `json:"thread"`
				} `json:"resolveReviewThread"`
			}
			if err := c.GraphQL(ctx, resolveMutation, map[string]any{"threadId": a.ID}, &resolved); err != nil {
				out.ResolveFailed = append(out.ResolveFailed, FailedResolve{ID: a.ID, Error: err.Error()})
				continue
			}
			out.Resolved = append(out.Resolved, a.ID)
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
func abort(id, reason, log string, actions []ThreadAction) error {
	done := alreadyPosted(log, actions)
	var b strings.Builder
	fmt.Fprintf(&b, "failed to reply to thread %s:\n%s\n", id, reason)
	if len(done) > 0 {
		fmt.Fprintf(&b, "already replied (do NOT resend on retry): %s\n", strings.Join(done, ","))
	}
	fmt.Fprintf(&b, "not processed: %s\n", strings.Join(remaining(done, actions), ","))
	return &AbortedReply{Message: strings.TrimSuffix(b.String(), "\n")}
}

// record appends a thread id to the log of what has been replied to.
func record(log, id string) error {
	f, err := os.OpenFile(log, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("record the reply to %s: %w", id, err)
	}
	defer f.Close()
	if _, err := fmt.Fprintln(f, id); err != nil {
		return fmt.Errorf("record the reply to %s: %w", id, err)
	}
	return nil
}

// postedIDs reads the log, treating its absence as nothing having been posted.
func postedIDs(log string) []string {
	b, err := os.ReadFile(log)
	if err != nil {
		return nil
	}
	var ids []string
	for line := range strings.SplitSeq(string(b), "\n") {
		if line != "" {
			ids = append(ids, line)
		}
	}
	return ids
}

// alreadyPosted is the ids of actions the log already holds, sorted and
// deduplicated.
func alreadyPosted(log string, actions []ThreadAction) []string {
	var found []string
	for _, id := range postedIDs(log) {
		if slices.ContainsFunc(actions, func(a ThreadAction) bool { return a.ID == id }) {
			found = append(found, id)
		}
	}
	slices.Sort(found)
	return slices.Compact(found)
}

// remaining is the ids of actions the log does not hold, in the order they were
// given.
func remaining(done []string, actions []ThreadAction) []string {
	var left []string
	for _, a := range actions {
		if !slices.Contains(done, a.ID) {
			left = append(left, a.ID)
		}
	}
	return left
}

// ParseEligible reads the threads a context flags as awaiting our confirmation,
// and the head they were judged at.
func ParseEligible(b []byte) (ids []string, headOID string, err error) {
	var wire struct {
		PR struct {
			HeadOID string `json:"head_oid"`
		} `json:"pr"`
		ReviewThreads *[]struct {
			ID                     string `json:"id"`
			AwaitingMyConfirmation bool   `json:"awaiting_my_confirmation"`
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
		if t.AwaitingMyConfirmation {
			ids = append(ids, t.ID)
		}
	}
	return ids, wire.PR.HeadOID, nil
}
