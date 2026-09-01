package issue

import (
	"context"
	"encoding/json/jsontext"
	"encoding/json/v2"
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

// RefList is a list of references GitHub may have declined to supply.
//
// The contract prints null for the refusal rather than an empty list, because
// "nothing is blocking this" and "what is blocking this could not be read" are
// different answers to the question a caller is asking.
type RefList struct {
	Refs    []Ref
	Unknown bool
}

// MarshalJSONTo implements json.MarshalerTo.
func (l RefList) MarshalJSONTo(enc *jsontext.Encoder) error {
	if l.Unknown {
		return enc.WriteToken(jsontext.Null)
	}
	return json.MarshalEncode(enc, l.Refs)
}

// PRList is the pull requests closing one sub-issue, with the same distinction
// between none and not known that RefList makes.
type PRList struct {
	PRs     []PR
	Unknown bool
}

// MarshalJSONTo implements json.MarshalerTo.
func (l PRList) MarshalJSONTo(enc *jsontext.Encoder) error {
	if l.Unknown {
		return enc.WriteToken(jsontext.Null)
	}
	return json.MarshalEncode(enc, l.PRs)
}

// SubIssue is one child of the issue being resolved.
//
// The three annotated fields are absent unless the flag that fetches them was
// given, because each costs a round trip per sub-issue and most callers want
// neither.
type SubIssue struct {
	Number         int      `json:"number"`
	Title          string   `json:"title"`
	State          string   `json:"state"`
	URL            string   `json:"url"`
	PRs            *PRList  `json:"prs,omitzero"`
	BlockedBy      *RefList `json:"blocked_by,omitzero"`
	BlockersClosed *bool    `json:"blockers_closed,omitzero"`
}

// Summary is GitHub's own count of an issue's children, which arrives with the
// issue itself and is therefore available even when the list is not.
type Summary struct {
	Total     int `json:"total"`
	Completed int `json:"completed"`
}

// Hierarchy is where one issue sits among its parent, children and blockers.
//
// The field order is the output contract: it is the order the keys are
// printed in.
type Hierarchy struct {
	Repo   string   `json:"repo"`
	Number int      `json:"number"`
	Title  string   `json:"title"`
	State  string   `json:"state"`
	URL    string   `json:"url"`
	Kind   TreeKind `json:"kind"`
	// Parent is null for an issue that is nobody's child, and also for one
	// whose parent could not be read — the warning tells those apart.
	Parent    *Ref    `json:"parent"`
	BlockedBy RefList `json:"blocked_by"`
	// BlockersClosed is false whenever the answer is not known, which is the
	// same safe direction AllSubIssuesClosed and AllSiblingsClosed take: these
	// gate closing an issue.
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
// Only failing to read the issue itself is an error. Everything else degrades:
// a lookup that fails leaves its field null or empty and adds a line to
// Warnings, because a skill that has already been given an issue number is
// better served by a partial answer than by nothing.
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

	parent := r.parent(ctx, base, number)
	subs, subsFetched := r.subIssues(ctx, base, number, self.SubIssuesSummary.Total)
	blockers := r.blockers(ctx, base, self.Dependencies.TotalBlockedBy, fmt.Sprintf("#%d", number))
	siblings, siblingsFetched := r.siblings(ctx, number, parent)

	out := make([]SubIssue, 0, len(subs))
	for _, s := range subs {
		out = append(out, s.subIssue())
	}
	if r.opts.WithPRs {
		for i, s := range subs {
			prs := r.closingPRs(ctx, s)
			out[i].PRs = &prs
		}
	}
	if r.opts.WithDeps {
		for i, s := range subs {
			path, err := issuePath(s.HTMLURL)
			if err != nil {
				// Unreachable through the API, which always returns an html
				// url; reported rather than dropped so it cannot hide.
				r.warn("blocked_by lookup failed for Sub #%d", s.Number)
				out[i].BlockedBy = &RefList{Unknown: true}
				out[i].BlockersClosed = new(bool)
				continue
			}
			b := r.blockers(ctx, path, s.Dependencies.TotalBlockedBy, fmt.Sprintf("Sub #%d", s.Number))
			out[i].BlockedBy = &b.list
			out[i].BlockersClosed = &b.closed
		}
	}

	// After the annotations, so that a count that does not match reads as the
	// last thing that went wrong rather than the first.
	if subsFetched && self.SubIssuesSummary.Total != len(out) {
		r.warn("sub_issues count mismatch for #%d: summary=%d fetched=%d", number, self.SubIssuesSummary.Total, len(out))
		subsFetched = false
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
		AllSubIssuesClosed: subsFetched && len(out) > 0 && allClosed(out),
		Siblings:           siblings,
		AllSiblingsClosed:  parent != nil && siblingsFetched && allClosed(siblings),
		Warnings:           r.warnings,
	}, nil
}

// parent reads the parent through its own endpoint, where a 404 is the ordinary
// answer for an issue that has none. Any other failure degrades to no parent,
// which is why the two have to be told apart rather than both treated as "none".
func (r *resolver) parent(ctx context.Context, base string, number int) *Ref {
	var w issueWire
	err := r.c.Get(ctx, base+"/parent", &w)
	if ghapi.IsNotFound(err) {
		return nil
	}
	if err != nil {
		r.warn("parent lookup failed for #%d: %v", number, err)
		return nil
	}
	ref := w.ref(r.repo)
	return &ref
}

// subIssues reads the children, reporting whether the list is complete.
func (r *resolver) subIssues(ctx context.Context, base string, number, total int) ([]issueWire, bool) {
	// The summary comes with the issue, so an issue with no children costs no
	// round trip — and most issues have none, while this runs on every skill
	// startup.
	if total == 0 {
		return nil, true
	}
	subs, err := ghapi.GetAll[issueWire](ctx, r.c, base+"/sub_issues?per_page=100")
	if err != nil {
		r.warn("sub_issues lookup failed for #%d", number)
		return nil, false
	}
	return subs, true
}

// siblings reads the parent's other children, which is how a caller learns
// whether this issue is the last one left.
func (r *resolver) siblings(ctx context.Context, number int, parent *Ref) ([]SubIssue, bool) {
	if parent == nil {
		return nil, false
	}
	if !parent.SameRepo {
		// Following a parent into another repository would mean listing
		// children this repository's caller cannot act on anyway.
		r.warn("parent #%d is in another repository (%s); siblings unknown", parent.Number, parent.Repo)
		return nil, false
	}

	path := fmt.Sprintf("repos/%s/issues/%d/sub_issues?per_page=100", r.repo, parent.Number)
	subs, err := ghapi.GetAll[issueWire](ctx, r.c, path)
	if err != nil {
		r.warn("sub_issues lookup failed for parent #%d (siblings unknown)", parent.Number)
		return nil, false
	}

	out := make([]SubIssue, 0, len(subs))
	for _, s := range subs {
		if s.Number == number {
			continue
		}
		out = append(out, s.subIssue())
	}
	return out, true
}

// blockerResult is the pair of fields a blocker lookup produces, which appear
// side by side both on the issue itself and on each annotated sub-issue.
type blockerResult struct {
	list   RefList
	closed bool
}

// blockers reads what is blocking an issue.
//
// The gate is total_blocked_by rather than blocked_by, because the latter
// counts only the open ones and would read an issue whose blockers have all
// been closed as having none — which is the opposite of what the caller needs
// to know.
func (r *resolver) blockers(ctx context.Context, base string, total int, label string) blockerResult {
	if total == 0 {
		return blockerResult{closed: true}
	}

	list, err := ghapi.GetAll[issueWire](ctx, r.c, base+"/dependencies/blocked_by?per_page=100")
	if err != nil {
		r.warn("blocked_by lookup failed for %s", label)
		return blockerResult{list: RefList{Unknown: true}}
	}

	refs := make([]Ref, 0, len(list))
	for _, b := range list {
		refs = append(refs, b.ref(r.repo))
	}
	if total != len(refs) {
		r.warn("blocked_by count mismatch for %s: summary=%d fetched=%d", label, total, len(refs))
		return blockerResult{list: RefList{Refs: refs}}
	}

	closed := true
	for _, ref := range refs {
		if ref.State != "closed" {
			closed = false
			break
		}
	}
	return blockerResult{list: RefList{Refs: refs}, closed: closed}
}

// closingPRs reads the pull requests that close one sub-issue.
//
// Both lookups go by url rather than by number: a sub-issue may live in another
// repository of the same owner, and so may a pull request closing it, and a
// number resolved against this repository would silently name something else.
func (r *resolver) closingPRs(ctx context.Context, sub issueWire) PRList {
	var refs struct {
		Resource *struct {
			ClosedBy struct {
				Nodes []struct {
					URL string `json:"url"`
				} `json:"nodes"`
			} `json:"closedByPullRequestsReferences"`
		} `json:"resource"`
	}
	if err := r.c.GraphQL(ctx, closingPRsQuery, map[string]any{"url": sub.HTMLURL}, &refs); err != nil || refs.Resource == nil {
		r.warn("closing PR lookup failed for Sub #%d", sub.Number)
		return PRList{Unknown: true}
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
		if err := r.c.GraphQL(ctx, prByURLQuery, map[string]any{"url": n.URL}, &out); err != nil || out.Resource == nil {
			r.warn("pr lookup failed for %s (closing Sub #%d)", n.URL, sub.Number)
			return PRList{Unknown: true}
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
	return PRList{PRs: prs}
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

func (w issueWire) ref(repo ghapi.Repo) Ref {
	// An unparseable url leaves the repository empty, which reads as "not this
	// one" — the safe direction, since a caller that cannot name the
	// repository writes owner/repo#N rather than a bare #N.
	from, _ := ghapi.RepoFromAPIURL(w.RepositoryURL)
	r := from.String()
	if from == (ghapi.Repo{}) {
		r = ""
	}
	return Ref{
		Number:   w.Number,
		Title:    w.Title,
		State:    w.State,
		URL:      w.HTMLURL,
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
