package issue_test

import (
	"encoding/json/v2"
	"fmt"
	"net/http"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
	"github.com/178inaba/dotfiles/go/internal/issue"
)

// fakeIssue is one issue the fake GitHub holds.
type fakeIssue struct {
	Number    int      `json:"number"`
	ID        int64    `json:"id"`
	Title     string   `json:"title"`
	Body      string   `json:"body"`
	State     string   `json:"state"`
	UpdatedAt string   `json:"updated_at"`
	HTMLURL   string   `json:"html_url"`
	Labels    []label  `json:"labels"`
	Assignees []person `json:"assignees"`
	// RepositoryURL is what ghapi reads the repository back out of.
	RepositoryURL string `json:"repository_url"`
}

type label struct {
	Name string `json:"name"`
}

type person struct {
	Login string `json:"login"`
}

// fakeGitHub is as much of the issues API as a run touches, with what it was
// asked to do recorded so a test can assert on the calls rather than only on
// the answer.
type fakeGitHub struct {
	mu sync.Mutex
	// issues by number.
	issues map[int]*fakeIssue
	next   int
	// subIssues and blockedBy record the links made, as "parent<-id".
	subIssues []string
	blockedBy []string
	comments  []string
	// dropLabels and dropAssignees make GitHub store less than it was asked
	// for, which is what it does for a user without push access.
	dropLabels, dropAssignees bool
	// failAfter stops the fake once it has accepted this many writes, which is
	// how an interrupted run is staged.
	failAfter int
	writes    int
	// forbid makes the named path answer 403.
	forbid string
	// storeBody stands in for GitHub storing something other than what it was
	// sent, which is how a missed substitution is staged.
	storeBody func(string) string
}

func newFakeGitHub() *fakeGitHub {
	return &fakeGitHub{issues: map[int]*fakeIssue{}, next: 100, failAfter: -1}
}

// existing puts an issue in place for a run to edit or link to.
func (g *fakeGitHub) existing(number int, id int64, updatedAt string, labels ...string) *fakeIssue {
	f := &fakeIssue{
		Number: number, ID: id, Title: "before", Body: "before", State: "open",
		UpdatedAt: updatedAt, RepositoryURL: "https://api.github.com/repos/owner/repo",
		HTMLURL: fmt.Sprintf("https://github.com/owner/repo/issues/%d", number),
	}
	for _, l := range labels {
		f.Labels = append(f.Labels, label{Name: l})
	}
	g.issues[number] = f
	return f
}

var (
	issuePath    = regexp.MustCompile(`^/repos/owner/repo/issues/(\d+)$`)
	subIssuePath = regexp.MustCompile(`^/repos/owner/repo/issues/(\d+)/sub_issues$`)
	blockedPath  = regexp.MustCompile(`^/repos/owner/repo/issues/(\d+)/dependencies/blocked_by$`)
	commentPath  = regexp.MustCompile(`^/repos/owner/repo/issues/(\d+)/comments$`)
)

func (g *gitHubHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	g.g.mu.Lock()
	defer g.g.mu.Unlock()
	g.g.serve(g.t, w, r)
}

type gitHubHandler struct {
	t *testing.T
	g *fakeGitHub
}

// client points a ghapi client at the fake.
func (g *fakeGitHub) client(t *testing.T) *ghapi.Client {
	t.Helper()

	return ghapitest.New(t, &gitHubHandler{t: t, g: g})
}

func (g *fakeGitHub) serve(t *testing.T, w http.ResponseWriter, r *http.Request) {
	t.Helper()
	w.Header().Set("Content-Type", "application/json")

	if r.URL.Path == g.forbid {
		w.WriteHeader(http.StatusForbidden)
		writeJSON(t, w, map[string]string{"message": "Forbidden"})
		return
	}
	if r.Method != http.MethodGet {
		if g.failAfter >= 0 && g.writes >= g.failAfter {
			w.WriteHeader(http.StatusInternalServerError)
			writeJSON(t, w, map[string]string{"message": "the run was interrupted here"})
			return
		}
		g.writes++
	}

	switch {
	case r.URL.Path == "/user":
		writeJSON(t, w, person{Login: "178inaba"})
	case r.URL.Path == "/repos/owner/repo/issues" && r.Method == http.MethodPost:
		g.create(t, w, r)
	case issuePath.MatchString(r.URL.Path) && r.Method == http.MethodGet:
		g.get(t, w, issuePath, r)
	case issuePath.MatchString(r.URL.Path) && r.Method == http.MethodPatch:
		g.patch(t, w, r)
	case subIssuePath.MatchString(r.URL.Path):
		var req struct {
			SubIssueID int64 `json:"sub_issue_id"`
		}
		decode(t, r, &req)
		parent := subIssuePath.FindStringSubmatch(r.URL.Path)[1]
		g.subIssues = append(g.subIssues, fmt.Sprintf("%s<-%d", parent, req.SubIssueID))
		// GitHub moves the parent when it gains a sub, which is the move a
		// re-run must not mistake for somebody else's edit, and answers with
		// the parent as it now stands.
		g.touch(t, parent)
		writeJSON(t, w, g.issues[atoi(t, parent)])
	case blockedPath.MatchString(r.URL.Path):
		var req struct {
			IssueID int64 `json:"issue_id"`
		}
		decode(t, r, &req)
		blocked := blockedPath.FindStringSubmatch(r.URL.Path)[1]
		g.blockedBy = append(g.blockedBy, fmt.Sprintf("%s<-%d", blocked, req.IssueID))
		// Same shape: the issue in the path moves, and comes back.
		g.touch(t, blocked)
		writeJSON(t, w, g.issues[atoi(t, blocked)])
	case commentPath.MatchString(r.URL.Path):
		var req struct {
			Body string `json:"body"`
		}
		decode(t, r, &req)
		number := commentPath.FindStringSubmatch(r.URL.Path)[1]
		g.comments = append(g.comments, number+": "+req.Body)
		g.touch(t, number)
		writeJSON(t, w, map[string]string{
			"html_url": "https://github.com/owner/repo/issues/" + number + "#issuecomment-1",
		})
	default:
		t.Errorf("unexpected %s %s", r.Method, r.URL.Path)
		w.WriteHeader(http.StatusNotFound)
		writeJSON(t, w, map[string]string{"message": "Not Found"})
	}
}

func (g *fakeGitHub) create(t *testing.T, w http.ResponseWriter, r *http.Request) {
	t.Helper()

	var req struct {
		Title     string   `json:"title"`
		Body      string   `json:"body"`
		Labels    []string `json:"labels"`
		Assignees []string `json:"assignees"`
	}
	decode(t, r, &req)

	g.next++
	f := &fakeIssue{
		Number: g.next, ID: int64(g.next) * 1_000_000_000, Title: req.Title, Body: g.stored(req.Body),
		State: "open", UpdatedAt: "2026-03-01T00:00:00Z",
		RepositoryURL: "https://api.github.com/repos/owner/repo",
		HTMLURL:       fmt.Sprintf("https://github.com/owner/repo/issues/%d", g.next),
	}
	if !g.dropLabels {
		for _, l := range req.Labels {
			f.Labels = append(f.Labels, label{Name: l})
		}
	}
	if !g.dropAssignees {
		for _, a := range req.Assignees {
			f.Assignees = append(f.Assignees, person{Login: a})
		}
	}
	g.issues[f.Number] = f
	writeJSON(t, w, f)
}

func (g *fakeGitHub) patch(t *testing.T, w http.ResponseWriter, r *http.Request) {
	t.Helper()

	var req struct {
		Title  *string   `json:"title"`
		Body   *string   `json:"body"`
		Labels *[]string `json:"labels"`
	}
	decode(t, r, &req)

	f := g.issues[atoi(t, issuePath.FindStringSubmatch(r.URL.Path)[1])]
	if f == nil {
		t.Errorf("a PATCH of %s, which does not exist", r.URL.Path)
		return
	}
	if req.Body != nil {
		f.Body = g.stored(*req.Body)
	}
	if req.Title != nil {
		f.Title = *req.Title
	}
	if req.Labels != nil && !g.dropLabels {
		f.Labels = nil
		for _, l := range *req.Labels {
			f.Labels = append(f.Labels, label{Name: l})
		}
	}
	f.UpdatedAt = bump(f.UpdatedAt)
	writeJSON(t, w, f)
}

func (g *fakeGitHub) get(t *testing.T, w http.ResponseWriter, re *regexp.Regexp, r *http.Request) {
	t.Helper()

	f := g.issues[atoi(t, re.FindStringSubmatch(r.URL.Path)[1])]
	if f == nil {
		w.WriteHeader(http.StatusNotFound)
		writeJSON(t, w, map[string]string{"message": "Not Found"})
		return
	}
	writeJSON(t, w, f)
}

// stored is what the fake keeps for a body it was sent.
func (g *fakeGitHub) stored(body string) string {
	if g.storeBody == nil {
		return body
	}
	return g.storeBody(body)
}

// touch is what GitHub does to an issue that gains a comment or a link.
func (g *fakeGitHub) touch(t *testing.T, number string) {
	t.Helper()

	if f := g.issues[atoi(t, number)]; f != nil {
		f.UpdatedAt = bump(f.UpdatedAt)
	}
}

// bump moves a timestamp on by a day, which is all a freshness comparison
// needs of it.
func bump(at string) string {
	day := atoiSafe(at[8:10]) + 1
	return fmt.Sprintf("%s%02d%s", at[:8], day, at[10:])
}

func atoiSafe(s string) int { n, _ := strconv.Atoi(s); return n }

func atoi(t *testing.T, s string) int {
	t.Helper()

	n, err := strconv.Atoi(s)
	if err != nil {
		t.Fatalf("atoi %q: %v", s, err)
	}
	return n
}

func decode(t *testing.T, r *http.Request, v any) {
	t.Helper()

	if err := json.UnmarshalRead(r.Body, v); err != nil {
		t.Errorf("decode the %s of %s: %v", r.Method, r.URL.Path, err)
	}
}

func writeJSON(t *testing.T, w http.ResponseWriter, v any) {
	t.Helper()

	if err := json.MarshalWrite(w, v); err != nil {
		t.Errorf("write the response: %v", err)
	}
}

// parentAndSubs is the shape the whole feature exists for: a parent whose body
// names subs that do not exist yet, a sub that names another sub, and an
// existing issue joining them.
func parentAndSubs() issue.PublishManifest {
	return issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("PARENT", "parent.md"),
			row("SUB_A", "sub-a.md", withParent("PARENT")),
			row("SUB_B", "sub-b.md", withParent("PARENT")),
		},
		BlockedBy: []issue.PublishManifestBlockedBy{{Blocked: ptr("SUB_B"), By: ptr("SUB_A")}},
	}
}

func parentAndSubFiles() map[string]string {
	return map[string]string{
		// Forward: the parent names both subs before either exists.
		"parent.md": leafDraft + "\nComposed of #{SUB_A} then #{SUB_B}.\n",
		// Forward: a sub names the one created after it.
		"sub-a.md": leafDraft + "\nOut of scope here, see #{SUB_B}.\n",
		// Backward: a sub names the one created before it.
		"sub-b.md": leafDraft + "\nWaits for #{SUB_A}.\n",
	}
}

func TestPublishNumbersEveryReferenceAndLinksTheSet(t *testing.T) {
	t.Parallel()

	m := parentAndSubs()
	file := writeManifest(t, m, parentAndSubFiles())
	g := newFakeGitHub()

	got, err := issue.Publish(t.Context(), g.client(t), m, file)
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}

	want := []issue.PublishedIssue{
		{Key: "PARENT", Number: 101, URL: "https://github.com/owner/repo/issues/101"},
		{Key: "SUB_A", Number: 102, URL: "https://github.com/owner/repo/issues/102"},
		{Key: "SUB_B", Number: 103, URL: "https://github.com/owner/repo/issues/103"},
	}
	if diff := cmp.Diff(want, got.Created); diff != "" {
		t.Errorf("Publish created (-want +got):\n%s", diff)
	}

	// Every body ends up carrying real numbers, forward references included.
	for number, want := range map[int]string{
		101: "Composed of #102 then #103.",
		102: "Out of scope here, see #103.",
		103: "Waits for #102.",
	} {
		if body := g.issues[number].Body; !strings.Contains(body, want) {
			t.Errorf("#%d does not carry %q:\n%s", number, want, body)
		}
	}
	if diff := cmp.Diff([]string{"101<-102000000000", "101<-103000000000"}, g.subIssues); diff != "" {
		t.Errorf("sub-issue links (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff([]string{"103<-102000000000"}, g.blockedBy); diff != "" {
		t.Errorf("blocked_by (-want +got):\n%s", diff)
	}
	if len(got.Degraded) != 0 {
		t.Errorf("Publish reported %v as degraded, want none", got.Degraded)
	}
}

// TestPublishFillsInTheFirstIssuesOfARepository is what a second judgement
// would break: the numbers a run assigns may well be 1, 2 and 3, and a parent
// naming three subs then carries exactly the run of bare references the rule
// refuses. What that rule guards against is numbering somebody typed, so the
// substitution makes no judgement about what it produces.
func TestPublishFillsInTheFirstIssuesOfARepository(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("PARENT", "parent.md"),
			row("SUB_A", "sub-a.md", withParent("PARENT")),
			row("SUB_B", "sub-b.md", withParent("PARENT")),
			row("SUB_C", "sub-c.md", withParent("PARENT")),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"parent.md": leafDraft + "\nComposed of #{SUB_A}, #{SUB_B} and #{SUB_C}.\n",
		"sub-a.md":  leafDraft,
		"sub-b.md":  leafDraft,
		"sub-c.md":  leafDraft,
	})
	g := newFakeGitHub()
	// An empty repository, so that the numbers this run assigns are the low
	// ones the rule is about; the default start is high enough that #100 and
	// up are not bare item numbers at all.
	g.next = 0

	got, err := issue.Publish(t.Context(), g.client(t), m, file)
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}

	want := []issue.PublishedIssue{
		{Key: "PARENT", Number: 1, URL: "https://github.com/owner/repo/issues/1"},
		{Key: "SUB_A", Number: 2, URL: "https://github.com/owner/repo/issues/2"},
		{Key: "SUB_B", Number: 3, URL: "https://github.com/owner/repo/issues/3"},
		{Key: "SUB_C", Number: 4, URL: "https://github.com/owner/repo/issues/4"},
	}
	if diff := cmp.Diff(want, got.Created); diff != "" {
		t.Errorf("Publish created (-want +got):\n%s", diff)
	}
	if body, want := g.issues[1].Body, "Composed of #2, #3 and #4."; !strings.Contains(body, want) {
		t.Errorf("#1 does not carry %q:\n%s", want, body)
	}
}

// TestPublishResumesAfterAnInterruptedCreate is the property the record exists
// for: the same manifest run again finishes the job without making a second
// copy of what already landed.
func TestPublishResumesAfterAnInterruptedCreate(t *testing.T) {
	t.Parallel()

	m := parentAndSubs()
	file := writeManifest(t, m, parentAndSubFiles())
	g := newFakeGitHub()
	g.failAfter = 1 // the parent's create, and nothing after it

	if _, err := issue.Publish(t.Context(), g.client(t), m, file); err == nil {
		t.Fatal("Publish succeeded, want the interruption to be reported")
	}
	if len(g.issues) != 1 {
		t.Fatalf("the interrupted run left %d issues, want 1", len(g.issues))
	}

	g.failAfter = -1
	got, err := issue.Publish(t.Context(), g.client(t), m, file)
	if err != nil {
		t.Fatalf("Publish (resumed): %v", err)
	}

	if len(g.issues) != 3 {
		t.Errorf("the run created %d issues in total, want 3 — the parent must not be made twice", len(g.issues))
	}
	// The resumed run reports only what it did itself.
	want := []issue.PublishedIssue{
		{Key: "SUB_A", Number: 102, URL: "https://github.com/owner/repo/issues/102"},
		{Key: "SUB_B", Number: 103, URL: "https://github.com/owner/repo/issues/103"},
	}
	if diff := cmp.Diff(want, got.Created); diff != "" {
		t.Errorf("the resumed run created (-want +got):\n%s", diff)
	}
	if body := g.issues[101].Body; !strings.Contains(body, "Composed of #102 then #103.") {
		t.Errorf("the parent's forward references were never filled in:\n%s", body)
	}
	if diff := cmp.Diff([]string{"101<-102000000000", "101<-103000000000"}, g.subIssues); diff != "" {
		t.Errorf("sub-issue links (-want +got):\n%s", diff)
	}
}

// TestPublishResumesAfterATargetWasEdited is the freshness case a naive check
// gets wrong: the run's own edit moved the issue, and a re-run must not read
// that as somebody else's change.
func TestPublishResumesAfterATargetWasEdited(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("42", "42.md", withUpdatedAt("2026-01-01T00:00:00Z"), withComment("42-comment.md")),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"42.md":         leafDraft,
		"42-comment.md": "The body was rewritten.\n",
	})
	g := newFakeGitHub()
	g.existing(42, 4200, "2026-01-01T00:00:00Z", "enhancement")
	g.failAfter = 1 // the PATCH, and not the comment after it

	if _, err := issue.Publish(t.Context(), g.client(t), m, file); err == nil {
		t.Fatal("Publish succeeded, want the interruption to be reported")
	}

	g.failAfter = -1
	got, err := issue.Publish(t.Context(), g.client(t), m, file)
	if err != nil {
		t.Fatalf("Publish (resumed) was refused over its own edit: %v", err)
	}
	if len(got.Edited) != 0 {
		t.Errorf("the resumed run edited %+v, want nothing — the edit is already recorded", got.Edited)
	}
	if len(g.comments) != 1 {
		t.Errorf("the run posted %d comments in total, want 1: %v", len(g.comments), g.comments)
	}
}

// TestPublishResumesAfterLinkingToAnExistingParent is the same property one
// step earlier: a link moves the parent, and where the parent is also a target
// the run has to remember where it left it.
func TestPublishResumesAfterLinkingToAnExistingParent(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("42", "42.md", withUpdatedAt("2026-01-01T00:00:00Z"), withKind("parent")),
			row("SUB_A", "sub-a.md", withParent("42")),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"42.md":    parentDraft,
		"sub-a.md": leafDraft,
	})
	g := newFakeGitHub()
	g.existing(42, 4200, "2026-01-01T00:00:00Z", "enhancement")
	// The create and the link land; the PATCH of the parent does not.
	g.failAfter = 2

	if _, err := issue.Publish(t.Context(), g.client(t), m, file); err == nil {
		t.Fatal("Publish succeeded, want the interruption to be reported")
	}
	if g.issues[42].UpdatedAt == "2026-01-01T00:00:00Z" {
		t.Fatal("the fake did not move the parent on the link, so the case under test did not happen")
	}

	g.failAfter = -1
	if _, err := issue.Publish(t.Context(), g.client(t), m, file); err != nil {
		t.Fatalf("Publish (resumed) was refused over the move its own link made: %v", err)
	}
	if diff := cmp.Diff([]string{"42<-101000000000"}, g.subIssues); diff != "" {
		t.Errorf("sub-issue links (-want +got):\n%s", diff)
	}
}

// parentDraft satisfies the section check for a parent, which requires three
// sections a leaf does not.
const parentDraft = `## Background / Purpose

Why.

## Structure (Sub-Issues)

- One.

## Cross-cutting rules

- None.

## Acceptance criteria

- [ ] Done.

## Manual release steps

None (completed by merging all Subs)

## Out of scope

- Everything else.
`

func TestPublishReportsWhatGitHubDropped(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"), Issues: []issue.PublishManifestIssue{row("A", "a.md")},
	}
	file := writeManifest(t, m, map[string]string{"a.md": leafDraft})
	g := newFakeGitHub()
	g.dropLabels, g.dropAssignees = true, true

	got, err := issue.Publish(t.Context(), g.client(t), m, file)
	if err != nil {
		t.Fatalf("Publish failed over a drop, want it reported and exit 0: %v", err)
	}
	want := []string{
		`#101 did not receive the label "enhancement"`,
		"#101 was not assigned to 178inaba",
	}
	if diff := cmp.Diff(want, got.Degraded); diff != "" {
		t.Errorf("Publish degraded (-want +got):\n%s", diff)
	}
}

func TestPublishFailsOnAForbiddenLink(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("PARENT", "parent.md"),
			row("SUB_A", "sub-a.md", withParent("PARENT")),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"parent.md": leafDraft,
		"sub-a.md":  leafDraft,
	})
	g := newFakeGitHub()
	g.forbid = "/repos/owner/repo/issues/101/sub_issues"

	if _, err := issue.Publish(t.Context(), g.client(t), m, file); err == nil {
		t.Fatal("Publish succeeded despite a forbidden link, want a failure")
	} else if !strings.Contains(err.Error(), "link SUB_A to PARENT") {
		t.Errorf("Publish failed with %q, want it to name the link", err)
	}
}

// TestPublishFailsOnAMissedSubstitution is the accident the whole command was
// written for: an issue went out holding #{NAME} whose issue already had a
// number, and nobody noticed for a day.
func TestPublishFailsOnAMissedSubstitution(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("A", "a.md"),
			row("B", "b.md"),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"a.md": leafDraft,
		// B is created second, so A has a number by the time B goes out.
		"b.md": leafDraft + "\nFollows on from #{A}.\n",
	})
	g := newFakeGitHub()
	// GitHub stores a body with the substitution undone, which is what a
	// missed one looks like from here.
	g.storeBody = func(body string) string {
		return strings.ReplaceAll(body, "#101", "#{A}")
	}

	_, err := issue.Publish(t.Context(), g.client(t), m, file)
	if err == nil {
		t.Fatal("Publish succeeded with a placeholder left in a stored body, want a failure")
	}
	for _, want := range []string{"#102", "already numbered", "#{A} on line 21"} {
		if !strings.Contains(err.Error(), want) {
			t.Errorf("Publish failed with %q, want it to mention %q", err, want)
		}
	}
}

// TestPublishFailsWhenTheWriteDidNotFillInAForwardReference is the other route
// into the read-back: a body that went out holding a forward reference on
// purpose, and came back still holding it from the write that was meant to
// fill it in.
//
// It is the same verdict as a missed substitution, because by the write stage
// every issue exists and so every name is numbered. That is why readBack's
// other branch — a name still unnumbered after everything is created — has no
// test: the pre-flight refuses a name no row defines, and the create stage
// numbers every row, so nothing can reach it. It is kept as a guard against a
// bug in those two rather than as a path.
func TestPublishFailsWhenTheWriteDidNotFillInAForwardReference(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("A", "a.md"),
			row("B", "b.md"),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"a.md": leafDraft + "\nSee #{B}.\n",
		"b.md": leafDraft,
	})
	g := newFakeGitHub()
	// The forward reference is sent as a placeholder and comes back as one
	// from the edit that was meant to fill it in.
	g.storeBody = func(body string) string {
		return strings.ReplaceAll(body, "#102", "#{B}")
	}

	_, err := issue.Publish(t.Context(), g.client(t), m, file)
	if err == nil {
		t.Fatal("Publish succeeded with a placeholder left after every issue exists, want a failure")
	}
	if !strings.Contains(err.Error(), "already numbered") {
		t.Errorf("Publish failed with %q, want it to name the missed substitution", err)
	}
}

// TestPublishLinksAnExistingIssueToItsParent covers the row that is edited and
// re-homed in one run.
func TestPublishLinksAnExistingIssueToItsParent(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo: ptr("owner/repo"),
		Issues: []issue.PublishManifestIssue{
			row("PARENT", "parent.md"),
			row("42", "42.md", withParent("PARENT"), withUpdatedAt("2026-01-01T00:00:00Z")),
		},
	}
	file := writeManifest(t, m, map[string]string{
		"parent.md": leafDraft,
		"42.md":     leafDraft,
	})
	g := newFakeGitHub()
	g.existing(42, 4200, "2026-01-01T00:00:00Z", "enhancement")

	got, err := issue.Publish(t.Context(), g.client(t), m, file)
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if diff := cmp.Diff([]string{"101<-4200"}, g.subIssues); diff != "" {
		t.Errorf("sub-issue links (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff([]issue.PlannedLink{{From: "42", To: "PARENT"}}, got.Linked); diff != "" {
		t.Errorf("Publish linked (-want +got):\n%s", diff)
	}
}

// TestPublishRegistersADependencyOnAnIssueWithNoRow is why a bare number needs
// no row: an existing issue can block a new one without this run editing it.
func TestPublishRegistersADependencyOnAnIssueWithNoRow(t *testing.T) {
	t.Parallel()

	m := issue.PublishManifest{
		Repo:      ptr("owner/repo"),
		Issues:    []issue.PublishManifestIssue{row("A", "a.md")},
		BlockedBy: []issue.PublishManifestBlockedBy{{Blocked: ptr("A"), By: ptr("42")}},
	}
	file := writeManifest(t, m, map[string]string{"a.md": leafDraft})
	g := newFakeGitHub()
	before := g.existing(42, 4200, "2026-01-01T00:00:00Z", "enhancement")

	if _, err := issue.Publish(t.Context(), g.client(t), m, file); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if diff := cmp.Diff([]string{"101<-4200"}, g.blockedBy); diff != "" {
		t.Errorf("blocked_by (-want +got):\n%s", diff)
	}
	if before.Body != "before" || before.Title != "before" {
		t.Errorf("#42 was written to though no row names it: %+v", before)
	}
}
