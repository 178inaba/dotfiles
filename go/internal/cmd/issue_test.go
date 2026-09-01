package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/178inaba/dotfiles/go/internal/issue"
	"github.com/178inaba/dotfiles/go/internal/selfbuild"
)

const jaLeafDraft = `## 背景・目的

なぜやるか。

## 要件

- ひとつ

## 受け入れ条件

- [ ] 通る

## 影響範囲・関連コード

x

## スコープ外

なし
`

// write puts content in a file under the test's directory and returns its path.
func write(t *testing.T, name, content string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), name)
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("WriteFile(%q): %v", path, err)
	}
	return path
}

// TestIssueSectionsStatus pins the statuses, which are the whole of what the
// check and find subcommands tell a caller that is not reading their output.
func TestIssueSectionsStatus(t *testing.T) {
	t.Parallel()

	clean := write(t, "clean.md", jaLeafDraft)
	missing := write(t, "missing.md", strings.Replace(jaLeafDraft, "## 受け入れ条件\n\n- [ ] 通る\n\n", "", 1))
	unknown := write(t, "unknown.md", jaLeafDraft+"\n## 知らない見出し\n\nx\n")
	mixed := write(t, "mixed.md", strings.Replace(jaLeafDraft, "## 要件", "## Requirements", 1))
	badMapping := write(t, "bad.txt", "depends_on Prerequisites\n")

	tests := []struct {
		name     string
		args     []string
		wantCode int
		// wantStderr are fragments of the reasons, which go to standard error
		// one per line.
		wantStderr []string
		bareStdout bool
	}{
		{
			name: "a clean draft says nothing at all",
			args: []string{"issue", "sections", "check", clean, "--locale", "ja", "--kind", "leaf"},
			// Silence on both streams is the contract: the caller branches on
			// the status, and anything printed would land in whatever it pipes
			// the command into.
			wantCode: 0, bareStdout: true,
		},
		{
			name:     "a missing required section",
			args:     []string{"issue", "sections", "check", missing, "--locale", "ja", "--kind", "leaf"},
			wantCode: 2, wantStderr: []string{"missing required section: acceptance"}, bareStdout: true,
		},
		{
			name:     "an unknown heading",
			args:     []string{"issue", "sections", "check", unknown, "--locale", "ja", "--kind", "leaf"},
			wantCode: 3, wantStderr: []string{"unknown heading"}, bareStdout: true,
		},
		{
			name:     "a machine-consumed key in the mapping",
			args:     []string{"issue", "sections", "check", clean, "--locale", "ja", "--kind", "leaf", "--mapping", badMapping},
			wantCode: 4, wantStderr: []string{"machine-consumed key"}, bareStdout: true,
		},
		{
			name:     "a heading in the other language",
			args:     []string{"issue", "sections", "check", mixed, "--locale", "ja", "--kind", "leaf"},
			wantCode: 5, wantStderr: []string{"heading locale mismatch"}, bareStdout: true,
		},
		{
			// Its own status, so that a caller can tell "this issue has no such
			// section" from "the body could not be read" without parsing text.
			name:     "a section the body does not have",
			args:     []string{"issue", "sections", "find", clean, "composition"},
			wantCode: 6, bareStdout: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stdout, stderr bytes.Buffer
			code := run(t.Context(), tt.args, strings.NewReader(""), &stdout, &stderr, selfbuild.State{})

			if code != tt.wantCode {
				t.Errorf("exit code = %d, want %d (stderr=%q)", code, tt.wantCode, stderr.String())
			}
			if tt.bareStdout && stdout.Len() != 0 {
				t.Errorf("stdout = %q, want it empty", stdout.String())
			}
			for _, want := range tt.wantStderr {
				if !strings.Contains(stderr.String(), want) {
					t.Errorf("stderr = %q, want it to contain %q", stderr.String(), want)
				}
			}
			if len(tt.wantStderr) == 0 && tt.wantCode <= 1 && stderr.Len() != 0 {
				t.Errorf("stderr = %q, want it empty", stderr.String())
			}
		})
	}
}

func TestIssueSectionsJSON(t *testing.T) {
	t.Parallel()

	body := write(t, "body.md", jaLeafDraft)

	tests := []struct {
		name string
		args []string
		want []string
	}{
		{
			name: "schema", args: []string{"issue", "sections", "schema", "depends_on"},
			want: []string{`"key": "depends_on"`, `"ja": "依存"`, `"none_markers"`},
		},
		{
			name: "list", args: []string{"issue", "sections", "list", "--locale", "en", "--kind", "parent"},
			want: []string{`"locale": "en"`, `"kind": "parent"`, `"Structure (Sub-Issues)"`},
		},
		{
			name: "find", args: []string{"issue", "sections", "find", body, "requirements"},
			want: []string{`"key": "requirements"`, `"locale": "ja"`, `"body": "- ひとつ"`},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stdout, stderr bytes.Buffer
			if code := run(t.Context(), tt.args, strings.NewReader(""), &stdout, &stderr, selfbuild.State{}); code != 0 {
				t.Fatalf("exit code = %d, want 0 (stderr=%q)", code, stderr.String())
			}
			if stderr.Len() != 0 {
				t.Errorf("stderr = %q, want it empty", stderr.String())
			}
			for _, want := range tt.want {
				if !strings.Contains(stdout.String(), want) {
					t.Errorf("stdout = %q, want it to contain %q", stdout.String(), want)
				}
			}
			if !strings.HasSuffix(stdout.String(), "}\n") {
				t.Errorf("stdout = %q, want it to end with a newline after the object", stdout.String())
			}
		})
	}
}

// TestIssueSectionsBareParentPrintsHelp records a difference from the shell,
// which exited 1 on a missing subcommand: a parent with no arguments answers
// with its help and a zero status, the same as `ccx` and `ccx hook`. The
// dangerous case, a subcommand misspelled rather than absent, is still an
// error — see newParentCmd.
func TestIssueSectionsBareParentPrintsHelp(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	if code := run(t.Context(), []string{"issue", "sections"}, strings.NewReader(""), &stdout, &stderr, selfbuild.State{}); code != 0 {
		t.Errorf("exit code = %d, want 0", code)
	}
	if !strings.Contains(stdout.String(), "Available Commands:") {
		t.Errorf("stdout = %q, want the subcommand listing", stdout.String())
	}
}

// TestIssueSectionsPreconditions covers the ways a caller can be wrong, all of
// which exit 1 with a message rather than a class-specific status: the statuses
// above mean the draft was read and judged.
func TestIssueSectionsPreconditions(t *testing.T) {
	t.Parallel()

	clean := write(t, "clean.md", jaLeafDraft)
	empty := write(t, "empty.md", "")
	dir := t.TempDir()

	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "an unknown subcommand", args: []string{"issue", "sections", "nope"}, want: "unknown command"},
		{name: "check without a kind", args: []string{"issue", "sections", "check", clean, "--locale", "ja"}, want: `"kind" not set`},
		{name: "an undefined flag", args: []string{"issue", "sections", "schema", "depends_on", "--locale", "ja"}, want: "unknown flag"},
		{name: "find takes no flags", args: []string{"issue", "sections", "find", clean, "requirements", "--kind", "leaf"}, want: "unknown flag"},
		{name: "too few arguments", args: []string{"issue", "sections", "find", clean}, want: "accepts 2 arg"},
		{name: "an unknown key", args: []string{"issue", "sections", "schema", "nope"}, want: "unknown section key"},
		{name: "an unsupported locale", args: []string{"issue", "sections", "list", "--locale", "fr", "--kind", "leaf"}, want: "unsupported locale"},
		{name: "an unsupported kind", args: []string{"issue", "sections", "list", "--locale", "ja", "--kind", "epic"}, want: "unsupported kind"},
		{
			name: "a draft file that is not there",
			args: []string{"issue", "sections", "check", filepath.Join(dir, "nope.md"), "--locale", "ja", "--kind", "leaf"},
			want: "draft file not found",
		},
		{
			// A directory is not a draft either, and saying so beats reading it.
			name: "a directory where a draft was expected",
			args: []string{"issue", "sections", "check", dir, "--locale", "ja", "--kind", "leaf"},
			want: "draft file not found or not a regular file",
		},
		{
			name: "a mapping file that is not there",
			args: []string{"issue", "sections", "check", clean, "--locale", "ja", "--kind", "leaf", "--mapping", filepath.Join(dir, "nope.txt")},
			want: "mapping file not found",
		},
		{
			// An empty body is a failed fetch, not a body without sections, and
			// the message names the file so that the caller knows which read
			// came back empty.
			name: "an empty body names the file",
			args: []string{"issue", "sections", "find", empty, "requirements"},
			want: "input file is empty: " + empty,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var stdout, stderr bytes.Buffer
			code := run(t.Context(), tt.args, strings.NewReader(""), &stdout, &stderr, selfbuild.State{})

			if code != 1 {
				t.Errorf("exit code = %d, want 1 (stdout=%q stderr=%q)", code, stdout.String(), stderr.String())
			}
			if stdout.Len() != 0 {
				t.Errorf("stdout = %q, want it empty", stdout.String())
			}
			if !strings.Contains(stderr.String(), tt.want) {
				t.Errorf("stderr = %q, want it to contain %q", stderr.String(), tt.want)
			}
		})
	}
}

// TestScriptSubcommandReportsABrokenBuildOnStderr keeps the build failure out
// of the standard output these subcommands promise is JSON. The hooks put the
// same news in a systemMessage on their standard output, and copying that here
// would corrupt what the caller pipes to jq.
func TestScriptSubcommandReportsABrokenBuildOnStderr(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	build := selfbuild.State{Failed: true, JustFailed: true, FirstError: "issue/sections.go:1:1: nope"}
	if code := run(t.Context(), []string{"issue", "sections", "schema", "depends_on"}, strings.NewReader(""), &stdout, &stderr, build); code != 0 {
		t.Fatalf("exit code = %d, want 0", code)
	}

	if !strings.Contains(stderr.String(), "does not build") || !strings.Contains(stderr.String(), "nope") {
		t.Errorf("stderr = %q, want it to report the build failure", stderr.String())
	}
	if strings.Contains(stdout.String(), "does not build") {
		t.Errorf("stdout = %q, want the failure kept off it", stdout.String())
	}
	// The answer itself still has to be there: a broken build is reported, not
	// substituted for the result.
	if !strings.Contains(stdout.String(), `"key": "depends_on"`) {
		t.Errorf("stdout = %q, want the schema anyway", stdout.String())
	}
}

// TestRenderIssueTree pins the shape of `ccx issue tree`: the order of the
// keys, and which fields answer with null, with an empty array, or by not
// being there at all. The values themselves are the issue package's tests;
// what a golden can hold that they cannot is the JSON.
//
// Two of them, because the annotated and unannotated sub-issues are different
// objects: prs and blocked_by are absent without their flags, null when the
// lookup failed, and a list otherwise, and only a rendered file shows the
// three apart.
func TestRenderIssueTree(t *testing.T) {
	t.Parallel()

	plain := issue.Hierarchy{
		Repo: "178inaba/dotfiles", Number: 121, Title: "Port the scripts", State: "open",
		URL:  "https://github.com/178inaba/dotfiles/issues/121",
		Kind: issue.KindSub,
		// Null both because the lookup failed and because there is nothing to
		// report; the warning is what tells a reader which.
		Parent:           nil,
		BlockedBy:        issue.RefList{Unknown: true},
		SubIssues:        []issue.SubIssue{},
		SubIssuesSummary: issue.Summary{},
		Siblings:         []issue.SubIssue{},
		Warnings: []string{
			"parent lookup failed for #121: HTTP 500",
			"blocked_by lookup failed for #121",
		},
	}

	closed := true
	prs := issue.PRList{PRs: []issue.PR{
		{Number: 124, State: "MERGED", BaseRef: "main", Merged: true, URL: "https://github.com/178inaba/dotfiles/pull/124"},
	}}
	unknownPRs := issue.PRList{Unknown: true}
	blockers := issue.RefList{Refs: []issue.Ref{
		{Number: 7, Title: "Blocker", State: "closed", URL: "https://github.com/178inaba/other/issues/7", Repo: "178inaba/other", SameRepo: false},
	}}
	annotated := issue.Hierarchy{
		Repo: "178inaba/dotfiles", Number: 119, Title: "Move the extensions to Go", State: "open",
		URL:            "https://github.com/178inaba/dotfiles/issues/119",
		Kind:           issue.KindParentAndSub,
		Parent:         &issue.Ref{Number: 3, Title: "Release", State: "open", URL: "https://github.com/178inaba/other/issues/3", Repo: "178inaba/other", SameRepo: false},
		BlockedBy:      blockers,
		BlockersClosed: true,
		SubIssues: []issue.SubIssue{
			{Number: 123, Title: "Sub 123", State: "closed", URL: "https://github.com/178inaba/dotfiles/issues/123",
				PRs: &prs, BlockedBy: &blockers, BlockersClosed: &closed},
			{Number: 122, Title: "Sub 122", State: "open", URL: "https://github.com/178inaba/dotfiles/issues/122",
				PRs: &unknownPRs, BlockedBy: &issue.RefList{}, BlockersClosed: &closed},
		},
		SubIssuesSummary: issue.Summary{Total: 2, Completed: 1},
		Siblings:         []issue.SubIssue{},
		Warnings:         nil,
	}

	tests := []struct {
		name   string
		tree   issue.Hierarchy
		golden string
	}{
		{name: "plain", tree: plain, golden: "issue-tree.golden"},
		{name: "annotated", tree: annotated, golden: "issue-tree-annotated.golden"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			var got bytes.Buffer
			if err := renderJSON(&got, tt.tree); err != nil {
				t.Fatalf("renderJSON: %v", err)
			}

			path := filepath.Join("testdata", tt.golden)
			if *update {
				if err := os.WriteFile(path, got.Bytes(), 0o644); err != nil {
					t.Fatalf("WriteFile(%q): %v", path, err)
				}
			}
			want, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("ReadFile(%q): %v", path, err)
			}
			if diff := cmp.Diff(string(want), got.String()); diff != "" {
				t.Errorf("renderJSON differs from %s (-want +got):\n%s", tt.golden, diff)
			}
		})
	}
}
