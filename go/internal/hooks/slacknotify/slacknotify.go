// Package slacknotify posts a Claude Code notification to a Slack webhook.
package slacknotify

import (
	"bytes"
	"context"
	"encoding/json/v2"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/178inaba/dotfiles/go/internal/hooks"
	"github.com/178inaba/dotfiles/go/internal/runner"
)

const (
	// webhookEnv is where the incoming webhook lives. Absent means the machine
	// has not set Slack up, which is not a failure.
	webhookEnv = "CLAUDE_SLACK_WEBHOOK"
	// defaultType labels a notification that did not say what kind it is.
	defaultType = "notification"
	// timeout bounds the post. A hook holds up the event that triggered it, so
	// an unreachable Slack must not hold up the session.
	timeout = 5 * time.Second
	// worktrees is where this repository's skills put linked worktrees.
	worktrees = "/.claude/worktrees/"
)

// Deps are the seams. Use Default for the real ones.
type Deps struct {
	Client *http.Client
	Runner runner.Runner
	Getenv func(string) string
}

// Default wires the real implementations.
func Default() Deps {
	return Deps{Client: http.DefaultClient, Runner: runner.Exec{}, Getenv: os.Getenv}
}

// Hook posts the notification.
type Hook struct{ deps Deps }

// New returns the hook.
func New(d Deps) Hook { return Hook{deps: d} }

// Run implements the hook contract.
func (h Hook) Run(ctx context.Context, in hooks.Payload, stderr io.Writer) hooks.Result {
	if err := h.post(ctx, in); err != nil {
		fmt.Fprintf(stderr, "ccx: the Slack notification was not delivered: %v\n", err)
		return hooks.Result{Decision: hooks.Fail}
	}
	return hooks.Result{}
}

// Post sends the payload's message, for idle-notify, which notifies and then
// rings the bell rather than being a hook of its own.
func Post(ctx context.Context, d Deps, in hooks.Payload) error {
	return Hook{deps: d}.post(ctx, in)
}

func (h Hook) post(ctx context.Context, in hooks.Payload) error {
	webhook := h.deps.Getenv(webhookEnv)
	// A payload with no message is every event that is not a notification, so
	// there is nothing here to report.
	if webhook == "" || in.Message == "" {
		return nil
	}

	kind := in.NotificationType
	if kind == "" {
		kind = defaultType
	}
	text := fmt.Sprintf("[%s] (%s) %s", h.project(ctx, in.Dir), kind, in.Message)

	body, err := json.Marshal(struct {
		Text string `json:"text"`
	}{text})
	if err != nil {
		return err
	}

	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, webhook, bytes.NewReader(body))
	if err != nil {
		return err
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := h.deps.Client.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	// curl ran without -f, so a webhook that has been revoked answered 403 and
	// the hook exited 0: every notification since would have gone nowhere with
	// nothing said about it.
	if resp.StatusCode < 200 || resp.StatusCode > 299 {
		return fmt.Errorf("slack answered %s", resp.Status)
	}
	return nil
}

// project asks git where it is and turns the answer into a label.
func (h Hook) project(ctx context.Context, dir string) string {
	if dir == "" {
		return ""
	}
	out, err := h.deps.Runner.Run(ctx, runner.Command{
		Name: "git",
		Args: []string{"-C", dir, "rev-parse", "--path-format=absolute", "--show-toplevel", "--git-common-dir"},
	})
	if err != nil {
		return label(dir, "", "")
	}
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	if len(lines) < 2 {
		return label(dir, "", "")
	}
	return label(dir, lines[0], lines[len(lines)-1])
}

// label names the project a notification came from.
//
// The directory's own name will not do: in a worktree it is the worktree's,
// which says nothing about which project this is. The common directory names
// the repository whichever tree we are standing in, so the label is that name,
// and the worktree's after a colon when we are not in the main tree.
func label(dir, toplevel, common string) string {
	if dir == "" {
		return ""
	}
	if toplevel == "" || common == "" {
		return filepath.Base(dir)
	}

	mainRoot := filepath.Dir(common)
	// Trimming ".git" before taking the base name is what lets one expression
	// cover an ordinary repository ("repo/.git") and a bare one ("repo.git").
	name := filepath.Base(strings.TrimSuffix(common, ".git"))
	if toplevel == mainRoot {
		return name
	}

	worktree := strings.TrimPrefix(toplevel, mainRoot+worktrees)
	if worktree == toplevel {
		// A worktree somewhere else entirely; its own name is all there is.
		worktree = filepath.Base(toplevel)
	}
	return name + ":" + worktree
}
