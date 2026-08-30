package prinfo

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	now = int64(1756600000)
	key = "/Users/x/repo:feat"
)

// fakeRunner answers by command name and records what it was asked.
type fakeRunner struct {
	out   map[string]string
	fail  map[string]bool
	calls []string
}

func (f *fakeRunner) Run(_ context.Context, c runner.Command) ([]byte, error) {
	name := c.Name + " " + c.Args[0]
	f.calls = append(f.calls, name)
	if f.fail[name] {
		return nil, &runner.Error{Name: c.Name, Err: os.ErrInvalid}
	}
	return []byte(f.out[name]), nil
}

func TestLookup(t *testing.T) {
	tests := []struct {
		name            string
		record, attempt string
		wantResult      string
		wantRefresh     bool
	}{
		{
			name:       "a fresh record is used and gh is not asked",
			record:     "1756600000\n" + key + "\n123 NONE https://e/1",
			wantResult: "123 NONE https://e/1",
		},
		{
			name:        "no record renders nothing and starts a refresh",
			wantRefresh: true,
		},
		{
			// The badge stays put while the refresh runs, exactly as the
			// exchange rate does.
			name:        "a stale record is still rendered",
			record:      "1\n" + key + "\n123 NONE https://e/1",
			wantResult:  "123 NONE https://e/1",
			wantRefresh: true,
		},
		{
			// "no pull request" is a real answer and is cached like any other,
			// which is what keeps gh from running on every redraw offline.
			name:   "an empty record is a fresh answer",
			record: "1756600000\n" + key + "\n",
		},
		{
			// Deep directories can share a cache file once the name is cut to
			// length; the key inside decides whether the record is ours.
			name:        "a record for another key is discarded",
			record:      "1756600000\n/Users/x/other:main\n999 NONE https://e/9",
			wantRefresh: true,
		},
		{
			name:    "a recent attempt suppresses the refresh",
			attempt: "1756599990\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := filepath.Join(t.TempDir(), "pr-cache")
			if tt.record != "" {
				write(t, p, tt.record)
			}
			if tt.attempt != "" {
				write(t, p+".attempt", tt.attempt)
			}

			result, refresh := Lookup(p, key, now)

			if result != tt.wantResult {
				t.Errorf("result = %q, want %q", result, tt.wantResult)
			}
			if refresh != tt.wantRefresh {
				t.Errorf("refresh = %t, want %t", refresh, tt.wantRefresh)
			}
		})
	}
}

func TestParse(t *testing.T) {
	tests := []struct {
		name   string
		record string
		want   Info
		ok     bool
	}{
		{
			name:   "number, state and link",
			record: "123 NONE https://example.test/pull/123",
			want:   Info{Number: "123", State: StateNoReviewRequested, URL: "https://example.test/pull/123"},
			ok:     true,
		},
		{
			// gh can report a pull request without a URL, and the record then
			// ends in a separator that read discards rather than turning into
			// an empty link.
			name:   "a missing link leaves no trailing space",
			record: "127 APPROVED ",
			want:   Info{Number: "127", State: StateApproved},
			ok:     true,
		},
		{name: "no pull request", record: ""},
		{name: "a non-numeric number is not a pull request", record: "abc NONE https://e/1"},
		{name: "a partial record is not a pull request", record: " NONE"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, ok := Parse(tt.record)
			if ok != tt.ok {
				t.Fatalf("ok = %t, want %t", ok, tt.ok)
			}
			if diff := cmp.Diff(tt.want, got); diff != "" {
				t.Errorf("Info mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestRefresh(t *testing.T) {
	const prView = "gh pr"

	tests := []struct {
		name   string
		branch string
		out    map[string]string
		fail   map[string]bool
		want   string
		wantGH bool
	}{
		{
			name:   "an open pull request awaiting review",
			branch: "feat",
			out: map[string]string{
				"git symbolic-ref": "origin/main\n",
				prView:             `{"number":123,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://e/123"}`,
			},
			want:   "123 " + StateNoReviewRequested + " https://e/123",
			wantGH: true,
		},
		{
			name:   "a draft outranks its review decision",
			branch: "feat",
			out: map[string]string{
				"git symbolic-ref": "origin/main\n",
				prView:             `{"number":126,"reviewDecision":"","state":"OPEN","isDraft":true,"url":"https://e/126"}`,
			},
			want:   "126 " + StateDraft + " https://e/126",
			wantGH: true,
		},
		{
			name:   "an approved pull request",
			branch: "feat",
			out: map[string]string{
				"git symbolic-ref": "origin/main\n",
				prView:             `{"number":124,"reviewDecision":"APPROVED","state":"OPEN","isDraft":false,"url":"https://e/124"}`,
			},
			want:   "124 " + StateApproved + " https://e/124",
			wantGH: true,
		},
		{
			// A merged pull request is history, not the current work.
			name:   "a merged pull request is not shown",
			branch: "feat",
			out: map[string]string{
				"git symbolic-ref": "origin/main\n",
				prView:             `{"number":127,"reviewDecision":"APPROVED","state":"MERGED","isDraft":false,"url":"https://e/127"}`,
			},
			wantGH: true,
		},
		{
			// The default branch may be the head of a release pull request, but
			// it is not a branch-specific context, so gh is never even asked.
			name:   "the default branch is skipped without asking gh",
			branch: "main",
			out:    map[string]string{"git symbolic-ref": "origin/main\n"},
		},
		{
			// A repository whose remote was added by hand has no origin/HEAD,
			// and gh knows the answer instead.
			name:   "gh supplies the default branch when origin/HEAD is missing",
			branch: "main",
			out:    map[string]string{"gh repo": `{"defaultBranchRef":{"name":"main"}}`},
			fail:   map[string]bool{"git symbolic-ref": true},
		},
		{
			// Neither source knows, so the badge is shown rather than hidden:
			// a badge too many beats a badge missing.
			name:   "an unknown default branch still shows the pull request",
			branch: "main",
			out: map[string]string{
				prView: `{"number":133,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://e/133"}`,
			},
			fail:   map[string]bool{"git symbolic-ref": true, "gh repo": true},
			want:   "133 " + StateNoReviewRequested + " https://e/133",
			wantGH: true,
		},
		{
			// gh reports no pull request, no network and no credentials with
			// the same exit status, and all three mean the same thing here.
			name:   "a gh failure is cached as no pull request",
			branch: "feat",
			out:    map[string]string{"git symbolic-ref": "origin/main\n"},
			fail:   map[string]bool{prView: true},
			wantGH: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := filepath.Join(t.TempDir(), "pr-cache")
			r := &fakeRunner{out: tt.out, fail: tt.fail}

			Refresh(t.Context(), r, p, key, tt.branch, now)

			rec, ok := readRecord(t, p)
			if !ok {
				t.Fatal("no record written")
			}
			if rec != tt.want {
				t.Errorf("record = %q, want %q", rec, tt.want)
			}

			asked := false
			for _, c := range r.calls {
				if c == prView {
					asked = true
				}
			}
			if asked != tt.wantGH {
				t.Errorf("gh pr view called = %t, want %t (calls: %v)", asked, tt.wantGH, r.calls)
			}
		})
	}
}

func readRecord(t *testing.T, path string) (string, bool) {
	t.Helper()
	b, err := os.ReadFile(path)
	if err != nil {
		return "", false
	}
	want := "1756600000\n" + key + "\n"
	if len(b) < len(want) || string(b[:len(want)]) != want {
		t.Fatalf("record = %q, want it to start with %q", b, want)
	}
	return string(b[len(want):]), true
}

func write(t *testing.T, name, body string) {
	t.Helper()
	if err := os.WriteFile(name, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", name, err)
	}
}
