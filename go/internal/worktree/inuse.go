package worktree

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"github.com/178inaba/dotfiles/go/internal/runner"
)

// Removing a worktree that a process has as its working directory kills that
// process's every later command, and git does not refuse to do it. Both halves
// of /cleanup-merged need the same check — collect to leave such a worktree out
// of the candidates, delete to refuse it at the last moment — so it lives here
// once.
//
// Working directories rather than open files: an editor reads a file and closes
// it again, so `lsof +D` would cost time proportional to the tree for almost no
// extra coverage, while a shell or a session sitting in the directory is
// exactly what dies.

// cwdRow is one process and the directory it is sitting in.
type cwdRow struct {
	pid  string
	comm string
	dir  string
}

// cwdTable is every process's working directory, read once. lsof costs time
// proportional to the number of processes, so a candidate loop reads this table
// rather than asking again per candidate.
type cwdTable []cwdRow

// loadCWDTable asks lsof for every process's working directory.
//
// A failure has to be an error rather than an empty table: reading "no
// processes" as "nothing is in use" would turn the guard off precisely when it
// could not be evaluated, which is the moment it matters.
func loadCWDTable(ctx context.Context, r runner.Runner) (cwdTable, error) {
	// The one place the environment is consulted rather than injected: which
	// lsof runs is a property of the machine.
	if _, err := exec.LookPath("lsof"); err != nil {
		return nil, errors.New("lsof is required")
	}

	// -F pcn is the machine-readable form: p<pid>, c<command>, then fcwd and
	// n<path> for the descriptor itself.
	out, err := r.Run(ctx, runner.Command{Name: "lsof", Args: []string{"-a", "-d", "cwd", "-F", "pcn"}})
	if err != nil {
		return nil, errors.New("lsof failed to enumerate process cwds")
	}

	var table cwdTable
	var pid, comm string
	for line := range strings.SplitSeq(string(out), "\n") {
		if line == "" {
			continue
		}
		value := line[1:]
		switch line[0] {
		case 'p':
			pid = value
		case 'c':
			comm = value
		case 'n':
			table = append(table, cwdRow{pid: pid, comm: comm, dir: value})
		}
	}

	// This process has a working directory, so it must be in the table. Its
	// absence is how a run that produced no usable output — lsof refused, or
	// answered something else entirely — is told from a machine that genuinely
	// has nothing to report.
	self := strconv.Itoa(os.Getpid())
	if !slices.ContainsFunc(table, func(row cwdRow) bool { return row.pid == self }) {
		return nil, errors.New("lsof failed to enumerate process cwds")
	}
	return table, nil
}

// holders names the processes sitting in path or below it, as
// "<command> (PID <n>), ...", and returns empty when there are none.
func (t cwdTable) holders(path string) string {
	// lsof answers with resolved paths, so the query has to be resolved too:
	// on macOS a worktree under /var and the same one under /private/var would
	// otherwise fail to match and the guard would miss it.
	p, err := filepath.EvalSymlinks(path)
	if err != nil {
		p = path
	}

	var found []string
	for _, row := range t {
		// The separator matters: without it /tmp/wt would match /tmp/wt-other,
		// and a sibling's process would read as this one's.
		if row.dir == p || strings.HasPrefix(row.dir, p+string(filepath.Separator)) {
			found = append(found, fmt.Sprintf("%s (PID %s)", row.comm, row.pid))
		}
	}
	return strings.Join(found, ", ")
}
