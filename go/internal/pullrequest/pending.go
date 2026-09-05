package pullrequest

import "time"

// What is waiting on us, counted rather than read.
//
// Whether a run has anything to judge is answered from these three lists and
// nothing else. That is why no body text appears in them: reading a remark to
// decide whether it counts is the very thing the reading order exists to
// prevent — a remark read before the change frames the change, and the reading
// becomes a search for evidence about the remark.

// PendingSet is what is waiting on us at the instant the document was read.
type PendingSet struct {
	// The point everything below is measured against: what the
	// last run recorded, or null where nothing has been recorded for this pull
	// request — a fresh machine, or a first run — in which case everything
	// counts.
	Since *string `json:"since"`
	// The threads it is our move on, by state rather than by
	// time: one left on our side keeps coming back until it is answered or
	// resolved, which is the point of it.
	Threads []PendingThread `json:"threads" contract:"required"`
	// The reviews with something to answer that have arrived or
	// been rewritten since. An approval, a dismissal, one of ours, and one
	// whose body is empty because its remarks were left inline are not among
	// them.
	Reviews []PendingReview `json:"reviews" contract:"required"`
	// The conversation's comments that have arrived or been
	// rewritten since, our own marked posts aside. A bot's counts, and so does
	// one of ours that carries no marker: a hand-written follow-up is a remark
	// like any other.
	Comments []PendingComment `json:"comments" contract:"required"`
}

// PendingThread names one thread it is our move on.
type PendingThread struct {
	Path string `json:"path"`
	// Null once the commented lines have left the diff, which for
	// the author is right after the fixing push.
	Line *int `json:"line"`
	// The line the thread was opened on, which survives that.
	OriginalLine *int    `json:"original_line"`
	OpenedBy     *string `json:"opened_by"`
	// The thread's first comment, which is where a reader goes to
	// see what was asked.
	URL string `json:"url"`
}

// PendingReview names one review with something still to answer.
type PendingReview struct {
	Author     *string `json:"author"`
	AuthorType *string `json:"author_type"`
	State      string  `json:"state"`
	// submitted_at and last_edited_at are both here because the
	// later of the two is what made it count, and a reader wondering why sees
	// which.
	SubmittedAt  string  `json:"submitted_at"`
	LastEditedAt *string `json:"last_edited_at"`
	URL          string  `json:"url"`
}

// PendingComment names one comment in the conversation still to answer.
type PendingComment struct {
	Author       *string `json:"author"`
	AuthorType   *string `json:"author_type"`
	CreatedAt    string  `json:"created_at"`
	LastEditedAt *string `json:"last_edited_at"`
	URL          string  `json:"url"`
}

// Pending counts what is waiting on us in a document, given the instant the
// last run judged from.
//
// A pure function of the two, deliberately: it is what makes the rules above
// testable against a document written by hand, and it is what lets the two
// commands that write a document both get the count by calling one thing at
// one point rather than by remembering to.
func Pending(c Context, since *string) PendingSet {
	p := PendingSet{
		Since:    since,
		Threads:  []PendingThread{},
		Reviews:  []PendingReview{},
		Comments: []PendingComment{},
	}

	for _, t := range c.ReviewThreads {
		if t.Ball != BallMine {
			continue
		}
		// The first comment is what opened the thread, and it survives
		// truncation because the comments are paginated forwards. A thread
		// with none cannot be ours to move — ball would be none — so this is a
		// guard rather than a case.
		url := ""
		if len(t.Comments) > 0 {
			url = t.Comments[0].URL
		}
		p.Threads = append(p.Threads, PendingThread{
			Path: t.Path, Line: t.Line, OriginalLine: t.OriginalLine,
			OpenedBy: t.OpenedBy, URL: url,
		})
	}

	for _, r := range c.Reviews {
		if isLogin(r.Author, c.CurrentUser) || r.Body == "" ||
			r.State == "APPROVED" || r.State == "DISMISSED" {
			continue
		}
		if !arrivedSince(r.SubmittedAt, r.LastEditedAt, since) {
			continue
		}
		p.Reviews = append(p.Reviews, PendingReview{
			Author: r.Author, AuthorType: r.AuthorType, State: r.State,
			SubmittedAt: r.SubmittedAt, LastEditedAt: r.LastEditedAt, URL: r.URL,
		})
	}

	for _, comment := range c.Comments {
		if comment.IsSkillComment || !arrivedSince(comment.CreatedAt, comment.LastEditedAt, since) {
			continue
		}
		p.Comments = append(p.Comments, PendingComment{
			Author: comment.Author, AuthorType: comment.AuthorType,
			CreatedAt: comment.CreatedAt, LastEditedAt: comment.LastEditedAt, URL: comment.URL,
		})
	}
	return p
}

// arrivedSince reports whether something written at made or last changed its
// point at or after the watermark.
//
// The later of the two dates, so that a remark rewritten after a run had
// already judged it is a remark again. Equality counts: the watermark is the
// instant a read began, truncated to the second, and excluding that second
// would lose whatever was submitted inside it.
//
// A date that will not parse counts, on either side. Everything else here
// degrades towards reading a remark twice rather than towards never seeing it,
// and one unreadable timestamp should not silently retire a review.
func arrivedSince(at string, edited, since *string) bool {
	if since == nil {
		return true
	}
	mark, err := time.Parse(time.RFC3339, *since)
	if err != nil {
		return true
	}
	newest, ok := latest(at, edited)
	if !ok {
		return true
	}
	return !newest.Before(mark)
}

// latest is the later of a creation date and an edit date, and whether either
// could be read at all.
func latest(at string, edited *string) (time.Time, bool) {
	newest, err := time.Parse(time.RFC3339, at)
	ok := err == nil
	if edited == nil {
		return newest, ok
	}
	editedAt, err := time.Parse(time.RFC3339, *edited)
	if err != nil {
		return newest, ok
	}
	if !ok || editedAt.After(newest) {
		return editedAt, true
	}
	return newest, true
}
