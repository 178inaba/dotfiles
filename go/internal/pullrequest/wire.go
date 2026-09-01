package pullrequest

// The GraphQL shapes, kept apart from the output contract below them. GitHub
// nests an author inside every comment and a login inside every author; the
// rest of this module passes a login around. Writing one set of types for both
// would make the nesting the contract.

type pageInfo struct {
	HasNextPage bool   `json:"hasNextPage"`
	EndCursor   string `json:"endCursor"`
}

// actor is null for an account that no longer exists, which is why every
// reference to one is a pointer.
type actor struct {
	Login string `json:"login"`
	// Typename tells a person from a bot without a second request, and is the
	// only place the raw GraphQL name reaches this module.
	Typename string `json:"__typename"`
}

func (a *actor) login() *string {
	if a == nil {
		return nil
	}
	return &a.Login
}

func (a *actor) typename() *string {
	if a == nil || a.Typename == "" {
		return nil
	}
	return &a.Typename
}

type commentNode struct {
	Author    *actor `json:"author"`
	Body      string `json:"body"`
	CreatedAt string `json:"createdAt"`
	URL       string `json:"url"`
}

type reviewNode struct {
	Author      *actor `json:"author"`
	State       string `json:"state"`
	Body        string `json:"body"`
	URL         string `json:"url"`
	SubmittedAt string `json:"submittedAt"`
}

type threadNode struct {
	ID         string `json:"id"`
	IsResolved bool   `json:"isResolved"`
	IsOutdated bool   `json:"isOutdated"`
	Path       string `json:"path"`
	// Line is null on a thread whose lines no longer exist in the diff.
	Line       *int   `json:"line"`
	ResolvedBy *actor `json:"resolvedBy"`
	Comments   struct {
		TotalCount int           `json:"totalCount"`
		PageInfo   pageInfo      `json:"pageInfo"`
		Nodes      []commentNode `json:"nodes"`
	} `json:"comments"`
	Tail struct {
		Nodes []commentNode `json:"nodes"`
	} `json:"tail"`
}

// body is the answer to the one query that carries most of the context.
type body struct {
	Viewer struct {
		Login string `json:"login"`
	} `json:"viewer"`
	Repository struct {
		// HeadCommit is null when the head is not an ordinary commit — a
		// force-push mid-run, or a head this token cannot see.
		HeadCommit *struct {
			CommittedDate string `json:"committedDate"`
		} `json:"headCommit"`
		PullRequest struct {
			Comments struct {
				TotalCount int           `json:"totalCount"`
				PageInfo   pageInfo      `json:"pageInfo"`
				Nodes      []commentNode `json:"nodes"`
			} `json:"comments"`
			Reviews struct {
				TotalCount int          `json:"totalCount"`
				Nodes      []reviewNode `json:"nodes"`
			} `json:"reviews"`
			ReviewThreads struct {
				TotalCount int          `json:"totalCount"`
				PageInfo   pageInfo     `json:"pageInfo"`
				Nodes      []threadNode `json:"nodes"`
			} `json:"reviewThreads"`
		} `json:"pullRequest"`
	} `json:"repository"`
}
