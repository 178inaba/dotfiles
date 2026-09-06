package issue

import (
	"context"
	"fmt"
	"net/url"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
)

// TreeKind is what an issue is in the sub-issue hierarchy.
//
// It is derived rather than stored: GitHub knows only that an issue has a
// parent and that it has children, and the four combinations are what the
// skills reading this branch on.
type TreeKind string

const (
	// KindStandalone is an issue with neither a parent nor children.
	KindStandalone TreeKind = "standalone"
	// KindParent is an issue with children and no parent of its own, which is
	// how a release-sized issue looks.
	KindParent TreeKind = "parent"
	// KindSub is an issue with a parent and no children, which is a leaf: one
	// pull request closes it.
	KindSub TreeKind = "sub"
	// KindParentAndSub is both, a middle level of the hierarchy.
	KindParentAndSub TreeKind = "parent_and_sub"
)

// Ref is another issue this one points at: its parent, or one of its blockers.
//
// Both carry the repository because sub-issues and dependencies may cross
// repositories within an owner, and a reference in another one cannot be
// written as a bare #N.
type Ref struct {
	Number   int    `json:"number"`
	Title    string `json:"title"`
	State    string `json:"state"`
	URL      string `json:"url"`
	Repo     string `json:"repo"`
	SameRepo bool   `json:"same_repo"`
}

// PR is a pull request that closes a sub-issue.
type PR struct {
	Number  int           `json:"number"`
	State   ghapi.PRState `json:"state"`
	BaseRef string        `json:"base_ref"`
	Merged  bool          `json:"merged"`
	URL     string        `json:"url"`
}

// SubIssue is one child of the issue being resolved.
type SubIssue struct {
	Number int    `json:"number"`
	Title  string `json:"title"`
	State  string `json:"state"`
	URL    string `json:"url"`
	// Absent unless --with-prs, which costs a round trip per sub-issue.
	PRs *[]PR `json:"prs,omitzero"`
	// Absent unless --with-deps, which costs a round trip per sub-issue that
	// has blockers.
	BlockedBy *[]Ref `json:"blocked_by,omitzero"`
	// Absent unless --with-deps. False whenever the answer is not known, the
	// same safe direction the other closed flags take.
	BlockersClosed *bool `json:"blockers_closed,omitzero"`
}

// Summary is GitHub's own count of an issue's children, which arrives with the
// issue itself and is therefore available even when the list is not.
type Summary struct {
	Total     int `json:"total"`
	Completed int `json:"completed"`
}

// Hierarchy is where one issue sits among its parent, children and blockers.
// The keys are printed in the order below, which is part of the contract.
type Hierarchy struct {
	Repo   string   `json:"repo"`
	Number int      `json:"number"`
	Title  string   `json:"title"`
	State  string   `json:"state"`
	URL    string   `json:"url"`
	Kind   TreeKind `json:"kind"`
	// Null for an issue that is nobody's child. A parent in a repository the
	// token cannot read is answered as no parent by GitHub, which this cannot
	// tell from having none; every other refusal stops the resolution, so a
	// null here is never a lookup that failed.
	Parent    *Ref  `json:"parent"`
	BlockedBy []Ref `json:"blocked_by"`
	// False whenever the answer is not known, the same safe direction
	// all_sub_issues_closed and all_siblings_closed take: all three gate
	// closing an issue.
	BlockersClosed     bool       `json:"blockers_closed"`
	SubIssues          []SubIssue `json:"sub_issues"`
	SubIssuesSummary   Summary    `json:"sub_issues_summary"`
	AllSubIssuesClosed bool       `json:"all_sub_issues_closed"`
	Siblings           []SubIssue `json:"siblings"`
	AllSiblingsClosed  bool       `json:"all_siblings_closed"`
	// Warnings are the degradations that did not stop the answer being useful.
	Warnings []string `json:"warnings"`
}

// TreeOptions are the two annotations a caller can ask for.
type TreeOptions struct {
	// WithPRs attaches the pull requests that close each sub-issue, which is
	// how a parent's caller checks that every child merged into the base
	// branch before closing it.
	WithPRs bool
	// WithDeps attaches each sub-issue's blockers, which is how a stalled
	// parent's caller finds the child that can be started next.
	WithDeps bool
}

// Tree resolves one issue's hierarchy.
//
// A lookup that fails is an error. Each of these fields answers a question
// whose other answers are facts — has no parent, is blocked by nothing, was
// closed by no pull request — and a caller acts on those, so a failed request
// arriving as one of them is worse than no answer at all.
//
// What stays in Warnings is what is true of the data rather than of the run: a
// parent in another repository, whose children this one cannot list, and a
// summary the children or the blockers disagree with. Each of those also
// clears the flag that gates closing an issue, so a partial answer cannot pass
// for a complete one.
func Tree(ctx context.Context, c *ghapi.Client, repo ghapi.Repo, number int, o TreeOptions) (Hierarchy, error) {
	r := &resolver{c: c, repo: repo, opts: o}
	return r.tree(ctx, number)
}

// resolver carries what every step of one resolution shares, including the
// warnings it accumulates.
type resolver struct {
	c        *ghapi.Client
	repo     ghapi.Repo
	opts     TreeOptions
	warnings []string
}

func (r *resolver) warn(format string, a ...any) {
	r.warnings = append(r.warnings, fmt.Sprintf(format, a...))
}

func (r *resolver) tree(ctx context.Context, number int) (Hierarchy, error) {
	base := fmt.Sprintf("repos/%s/issues/%d", r.repo, number)

	var self issueWire
	if err := r.c.Get(ctx, base, &self); err != nil {
		return Hierarchy{}, fmt.Errorf("fetch issue #%d in %s: %w", number, r.repo, err)
	}

	parent, err := r.parent(ctx, number)
	if err != nil {
		return Hierarchy{}, err
	}
	subs, err := r.subIssues(ctx, base, number, self.SubIssuesSummary.Total)
	if err != nil {
		return Hierarchy{}, err
	}
	blockers, err := r.blockers(ctx, base, self.Dependencies.TotalBlockedBy, fmt.Sprintf("#%d", number))
	if err != nil {
		return Hierarchy{}, err
	}
	siblings, siblingsFetched, err := r.siblings(ctx, number, parent)
	if err != nil {
		return Hierarchy{}, err
	}

	out := make([]SubIssue, 0, len(subs))
	for _, s := range subs {
		out = append(out, s.subIssue())
	}
	if r.opts.WithPRs {
		for i, s := range subs {
			prs, err := r.closingPRs(ctx, s)
			if err != nil {
				return Hierarchy{}, err
			}
			out[i].PRs = &prs
		}
	}
	if r.opts.WithDeps {
		for i, s := range subs {
			path, err := issuePath(s.HTMLURL)
			if err != nil {
				// Unreachable through the API, which always returns an html
				// url; returned rather than dropped so it cannot hide.
				return Hierarchy{}, fmt.Errorf("read the url of Sub #%d: %w", s.Number, err)
			}
			b, err := r.blockers(ctx, path, s.Dependencies.TotalBlockedBy, fmt.Sprintf("Sub #%d", s.Number))
			if err != nil {
				return Hierarchy{}, err
			}
			out[i].BlockedBy = &b.list
			out[i].BlockersClosed = &b.closed
		}
	}

	// After the annotations, so that a count that does not match reads as the
	// last thing that went wrong rather than the first. A summary the children
	// disagree with is a fact about the data rather than a failed request, so
	// it stays a warning — and the flag it clears is what keeps a caller from
	// closing a parent over a child that never arrived.
	subsComplete := true
	if self.SubIssuesSummary.Total != len(out) {
		r.warn("sub_issues count mismatch for #%d: summary=%d fetched=%d", number, self.SubIssuesSummary.Total, len(out))
		subsComplete = false
	}

	return Hierarchy{
		Repo:               r.repo.String(),
		Number:             self.Number,
		Title:              self.Title,
		State:              self.State,
		URL:                self.HTMLURL,
		Kind:               kindOf(self.SubIssuesSummary.Total > 0, parent != nil),
		Parent:             parent,
		BlockedBy:          blockers.list,
		BlockersClosed:     blockers.closed,
		SubIssues:          out,
		SubIssuesSummary:   Summary{Total: self.SubIssuesSummary.Total, Completed: self.SubIssuesSummary.Completed},
		AllSubIssuesClosed: subsComplete && len(out) > 0 && allClosed(out),
		Siblings:           siblings,
		AllSiblingsClosed:  parent != nil && siblingsFetched && allClosed(siblings),
		Warnings:           r.warnings,
	}, nil
}

// parent reads the parent, which is null only where the issue has none.
//
// GitHub answers 404 both for an issue that is nobody's child and for a parent
// in a repository this token cannot see, and IssueParent has already turned
// both into no parent. Every other refusal is about the run rather than about
// the issue, and answering it with a null would make "has no parent" also mean
// "could not be asked" — which is the one thing a caller gating on a parent
// must not be told.
func (r *resolver) parent(ctx context.Context, number int) (*Ref, error) {
	parent, err := r.c.IssueParent(ctx, r.repo, number)
	if err != nil {
		return nil, fmt.Errorf("read the parent of #%d in %s%s: %w", number, r.repo, ghapi.SSOHint(err), err)
	}
	if parent == nil {
		return nil, nil
	}
	ref := refOf(*parent, r.repo)
	return &ref, nil
}

// subIssues reads the children.
func (r *resolver) subIssues(ctx context.Context, base string, number, total int) ([]issueWire, error) {
	// The summary comes with the issue, so an issue with no children costs no
	// round trip — and most issues have none, while this runs on every skill
	// startup.
	if total == 0 {
		return nil, nil
	}
	subs, err := ghapi.GetAll[issueWire](ctx, r.c, base+"/sub_issues?per_page=100")
	if err != nil {
		return nil, fmt.Errorf("read the children of #%d in %s%s: %w", number, r.repo, ghapi.SSOHint(err), err)
	}
	return subs, nil
}

// siblings reads the parent's other children, which is how a caller learns
// whether this issue is the last one left.
//
// The bool is whether the answer covers them all. It is false for a parent
// elsewhere, whose children this repository cannot list — a fact about where
// the parent lives rather than a failed request — and that false is what stops
// all_siblings_closed from reading as yes over children nobody checked.
func (r *resolver) siblings(ctx context.Context, number int, parent *Ref) ([]SubIssue, bool, error) {
	if parent == nil {
		return nil, false, nil
	}
	if !parent.SameRepo {
		// Following a parent into another repository would mean listing
		// children this repository's caller cannot act on anyway.
		r.warn("parent #%d is in another repository (%s); siblings unknown", parent.Number, parent.Repo)
		return nil, false, nil
	}

	path := fmt.Sprintf("repos/%s/issues/%d/sub_issues?per_page=100", r.repo, parent.Number)
	subs, err := ghapi.GetAll[issueWire](ctx, r.c, path)
	if err != nil {
		return nil, false, fmt.Errorf("read the children of #%d in %s, the parent of #%d%s: %w",
			parent.Number, r.repo, number, ghapi.SSOHint(err), err)
	}

	out := make([]SubIssue, 0, len(subs))
	for _, s := range subs {
		if s.Number == number {
			continue
		}
		out = append(out, s.subIssue())
	}
	return out, true, nil
}

// blockerResult is the pair of fields a blocker lookup produces, which appear
// side by side both on the issue itself and on each annotated sub-issue.
type blockerResult struct {
	list   []Ref
	closed bool
}

// blockers reads what is blocking an issue.
//
// The gate is total_blocked_by rather than blocked_by, because the latter
// counts only the open ones and would read an issue whose blockers have all
// been closed as having none — which is the opposite of what the caller needs
// to know.
func (r *resolver) blockers(ctx context.Context, base string, total int, label string) (blockerResult, error) {
	if total == 0 {
		return blockerResult{closed: true}, nil
	}

	list, err := ghapi.GetAll[issueWire](ctx, r.c, base+"/dependencies/blocked_by?per_page=100")
	if err != nil {
		return blockerResult{}, fmt.Errorf("read what is blocking %s in %s%s: %w", label, r.repo, ghapi.SSOHint(err), err)
	}

	refs := make([]Ref, 0, len(list))
	for _, b := range list {
		refs = append(refs, b.ref(r.repo))
	}
	if total != len(refs) {
		r.warn("blocked_by count mismatch for %s: summary=%d fetched=%d", label, total, len(refs))
		return blockerResult{list: refs}, nil
	}

	closed := true
	for _, ref := range refs {
		if ref.State != "closed" {
			closed = false
			break
		}
	}
	return blockerResult{list: refs, closed: closed}, nil
}

// closingPRs reads the pull requests that close one sub-issue.
//
// Both lookups go by url rather than by number: a sub-issue may live in another
// repository of the same owner, and so may a pull request closing it, and a
// number resolved against this repository would silently name something else.
func (r *resolver) closingPRs(ctx context.Context, sub issueWire) ([]PR, error) {
	var refs struct {
		Resource *struct {
			ClosedBy struct {
				Nodes []struct {
					URL string `json:"url"`
				} `json:"nodes"`
			} `json:"closedByPullRequestsReferences"`
		} `json:"resource"`
	}
	if err := r.c.GraphQL(ctx, closingPRsQuery, map[string]any{"url": sub.HTMLURL}, &refs); err != nil {
		return nil, fmt.Errorf("read the pull requests closing Sub #%d%s: %w", sub.Number, ghapi.SSOHint(err), err)
	}
	if refs.Resource == nil {
		return nil, fmt.Errorf("read the pull requests closing Sub #%d: %s names no issue", sub.Number, sub.HTMLURL)
	}

	prs := make([]PR, 0, len(refs.Resource.ClosedBy.Nodes))
	for _, n := range refs.Resource.ClosedBy.Nodes {
		var out struct {
			Resource *struct {
				Number      int           `json:"number"`
				State       ghapi.PRState `json:"state"`
				BaseRefName string        `json:"baseRefName"`
				URL         string        `json:"url"`
			} `json:"resource"`
		}
		if err := r.c.GraphQL(ctx, prByURLQuery, map[string]any{"url": n.URL}, &out); err != nil {
			return nil, fmt.Errorf("read %s, which closes Sub #%d%s: %w", n.URL, sub.Number, ghapi.SSOHint(err), err)
		}
		if out.Resource == nil {
			return nil, fmt.Errorf("read %s, which closes Sub #%d: it names no pull request", n.URL, sub.Number)
		}
		p := out.Resource
		prs = append(prs, PR{
			Number:  p.Number,
			State:   p.State,
			BaseRef: p.BaseRefName,
			Merged:  p.State == ghapi.StateMerged,
			URL:     p.URL,
		})
	}
	return prs, nil
}

// closingPRsQuery asks which pull requests close an issue.
//
// includeClosedPrs has to be set: it defaults to false, and without it a
// sub-issue closed by a merged pull request — every finished one — answers with
// an empty list rather than the pull request that closed it.
const closingPRsQuery = `
query($url: URI!) {
  resource(url: $url) {
    ... on Issue {
      closedByPullRequestsReferences(first: 100, includeClosedPrs: true) {
        nodes { url }
      }
    }
  }
}`

// prByURLQuery reads a pull request the way `gh pr view --json` did, so that
// its state stays OPEN, CLOSED or MERGED rather than REST's open and closed.
const prByURLQuery = `
query($url: URI!) {
  resource(url: $url) {
    ... on PullRequest {
      number
      state
      baseRefName
      url
    }
  }
}`

// issueWire is the GitHub issue object, as much of it as this reads. The same
// shape arrives from the issue endpoint, the parent endpoint and both list
// endpoints.
type issueWire struct {
	Number  int    `json:"number"`
	Title   string `json:"title"`
	State   string `json:"state"`
	HTMLURL string `json:"html_url"`
	// RepositoryURL rather than the repository object, because only this one is
	// required by the issue schema.
	RepositoryURL    string `json:"repository_url"`
	SubIssuesSummary struct {
		Total     int `json:"total"`
		Completed int `json:"completed"`
	} `json:"sub_issues_summary"`
	Dependencies struct {
		TotalBlockedBy int `json:"total_blocked_by"`
	} `json:"issue_dependencies_summary"`
}

func (w issueWire) issue() ghapi.Issue {
	// An unparseable url leaves the repository at its zero value; refOf below
	// says what this command makes of that.
	from, _ := ghapi.RepoFromAPIURL(w.RepositoryURL)
	return ghapi.Issue{Number: w.Number, Title: w.Title, State: w.State, URL: w.HTMLURL, Repo: from}
}

func (w issueWire) ref(repo ghapi.Repo) Ref { return refOf(w.issue(), repo) }

// refOf projects an issue into a reference of it.
//
// A repository that could not be read leaves the name empty, which reads as
// "not this one" — the safe direction, since a caller that cannot name the
// repository writes owner/repo#N rather than a bare #N.
func refOf(i ghapi.Issue, repo ghapi.Repo) Ref {
	r := i.Repo.String()
	if i.Repo == (ghapi.Repo{}) {
		r = ""
	}
	return Ref{
		Number:   i.Number,
		Title:    i.Title,
		State:    i.State,
		URL:      i.URL,
		Repo:     r,
		SameRepo: r == repo.String(),
	}
}

func (w issueWire) subIssue() SubIssue {
	return SubIssue{Number: w.Number, Title: w.Title, State: w.State, URL: w.HTMLURL}
}

// issuePath turns an issue's html url into its api path, so that a sub-issue in
// another repository is asked about in its own.
func issuePath(htmlURL string) (string, error) {
	u, err := url.Parse(htmlURL)
	if err != nil {
		return "", fmt.Errorf("parse issue url %q: %w", htmlURL, err)
	}
	parts := strings.Split(strings.Trim(u.Path, "/"), "/")
	if len(parts) != 4 || parts[2] != "issues" {
		return "", fmt.Errorf("not an issue url: %s", htmlURL)
	}
	if _, err := strconv.Atoi(parts[3]); err != nil {
		return "", fmt.Errorf("not an issue url: %s", htmlURL)
	}
	return fmt.Sprintf("repos/%s/%s/issues/%s", parts[0], parts[1], parts[3]), nil
}

func kindOf(isParent, isSub bool) TreeKind {
	switch {
	case isParent && isSub:
		return KindParentAndSub
	case isParent:
		return KindParent
	case isSub:
		return KindSub
	default:
		return KindStandalone
	}
}

func allClosed(subs []SubIssue) bool {
	for _, s := range subs {
		if s.State != "closed" {
			return false
		}
	}
	return true
}
