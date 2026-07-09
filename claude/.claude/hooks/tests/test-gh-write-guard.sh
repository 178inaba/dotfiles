#!/bin/bash

# gh-write-guard.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-gh-write-guard.sh
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../gh-write-guard.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

# ルール3（本文中の素の #N 検出）用の本文ファイル
printf -- '- #1 foo\n- #2 bar\n- #3 baz\n' > "$tmpdir/hash-numbering.md"
printf -- '1. foo\n2. bar\n3. baz\n' > "$tmpdir/ordered-list.md"
printf -- 'see #1 and #2 and #2\n' > "$tmpdir/two-distinct-refs.md"
printf -- '- `#1` foo\n- `#2` bar\n- `#3` baz\n' > "$tmpdir/backtick-refs.md"
printf -- 'before\n```\n#1 #2 #3\n```\nafter\n' > "$tmpdir/fenced-refs.md"
printf -- '- foo/bar#1 x\n- foo/bar#2 y\n- foo/bar#3 z\n' > "$tmpdir/cross-repo-refs.md"
printf -- 'refs #123 #456 #789\n' > "$tmpdir/multi-digit-refs.md"
printf -- 'colors #1a2b3c and #2f4f4f, place #3rd\n' > "$tmpdir/alnum-suffix-refs.md"

pass=0
fail=0

run_test() {
  local name=$1 input=$2 want_exit=$3
  local got_exit
  printf '%s' "$input" | "$HOOK" >/dev/null 2>&1
  got_exit=$?
  if [ "$got_exit" -eq "$want_exit" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got exit %d, want %d)\n' "$name" "$got_exit" "$want_exit"
  fi
}

# 通過すべきケース (exit 0)
run_test 'non-gh command'                '{"tool_name":"Bash","tool_input":{"command":"ls -la"}}' 0
run_test 'gh read: issue list'           '{"tool_name":"Bash","tool_input":{"command":"gh issue list"}}' 0
run_test 'gh read: pr view'              '{"tool_name":"Bash","tool_input":{"command":"gh pr view 123"}}' 0
run_test 'gh read: repo clone'           '{"tool_name":"Bash","tool_input":{"command":"gh repo clone foo/bar"}}' 0
run_test 'gh repo create (excluded)'     '{"tool_name":"Bash","tool_input":{"command":"gh repo create foo/bar --public"}}' 0
run_test 'gh repo fork (excluded)'       '{"tool_name":"Bash","tool_input":{"command":"gh repo fork foo/bar"}}' 0
run_test 'gh write with -R'              '{"tool_name":"Bash","tool_input":{"command":"gh issue create -R foo/bar --title T --body B"}}' 0
run_test 'gh write with --repo'          '{"tool_name":"Bash","tool_input":{"command":"gh pr create --repo foo/bar --title T"}}' 0
run_test 'gh write with --repo='         '{"tool_name":"Bash","tool_input":{"command":"gh pr comment --repo=foo/bar 1 --body x"}}' 0
run_test 'GH_REPO env prefix'            '{"tool_name":"Bash","tool_input":{"command":"GH_REPO=foo/bar gh issue create --title T --body B"}}' 0
run_test 'tool_name=Edit (not Bash)'     '{"tool_name":"Edit","tool_input":{"command":"gh issue create --title T"}}' 0
run_test 'empty command'                 '{"tool_name":"Bash","tool_input":{"command":""}}' 0
run_test 'chained: cd && gh read'        '{"tool_name":"Bash","tool_input":{"command":"cd /tmp/foo && gh issue list"}}' 0
run_test 'gh write with -R, --body-file path'     '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body-file /tmp/body.md"}}' 0
run_test 'gh write with -R, --body-file stdin'    '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body-file - <<EOF\nline1\nline2\nEOF"}}' 0
run_test 'body-file: ordered list numbering'      "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/ordered-list.md\"}}" 0
run_test 'body-file: only 2 distinct #N'          "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/two-distinct-refs.md\"}}" 0
run_test 'body-file: #N in backticks'             "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/backtick-refs.md\"}}" 0
run_test 'body-file: #N in fenced code block'     "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/fenced-refs.md\"}}" 0
run_test 'body-file: OWNER/REPO#N form'           "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/cross-repo-refs.md\"}}" 0
run_test 'body-file: multi-digit #N only'         "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/multi-digit-refs.md\"}}" 0
run_test 'body-file: hex color / ordinal #N'      "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/alnum-suffix-refs.md\"}}" 0
run_test 'body-file: nonexistent path (fail-open)' "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/missing.md\"}}" 0

# ブロックされるべきケース (exit 2)
run_test 'gh issue create (no -R)'                  '{"tool_name":"Bash","tool_input":{"command":"gh issue create --title T --body B"}}' 2
run_test 'gh pr create (no -R)'                     '{"tool_name":"Bash","tool_input":{"command":"gh pr create --title T --body B"}}' 2
run_test 'gh issue comment (no -R)'                 '{"tool_name":"Bash","tool_input":{"command":"gh issue comment 1 --body x"}}' 2
run_test 'gh release create (no -R)'                '{"tool_name":"Bash","tool_input":{"command":"gh release create v1 --title v1"}}' 2
run_test 'gh repo edit (no -R)'                     '{"tool_name":"Bash","tool_input":{"command":"gh repo edit --description x"}}' 2
run_test 'gh label create (no -R)'                  '{"tool_name":"Bash","tool_input":{"command":"gh label create bug --color FF0000"}}' 2
run_test 'gh pr merge (no -R)'                      '{"tool_name":"Bash","tool_input":{"command":"gh pr merge 5 --squash"}}' 2
run_test 'chained: cd && gh issue create (no -R)'   '{"tool_name":"Bash","tool_input":{"command":"cd /tmp/foo && gh issue create --title T"}}' 2
run_test 'multiline --body (with -R)'               '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body \"line1\nline2\""}}' 2
run_test 'multiline --body heredoc (with -R)'       '{"tool_name":"Bash","tool_input":{"command":"gh issue comment -R foo/bar 1 --body \"$(cat <<EOF\nline1\nEOF\n)\""}}' 2
run_test 'multiline -b (with -R)'                   '{"tool_name":"Bash","tool_input":{"command":"gh pr create -R foo/bar --title T -b \"line1\nline2\""}}' 2
run_test 'multiline --body= (with -R)'              '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body=\"line1\nline2\""}}' 2
run_test 'body-file: bare #N numbering'             "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/hash-numbering.md\"}}" 2
run_test 'body-file: quoted path, bare #N'          "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh issue comment -R foo/bar 1 --body-file \\\"$tmpdir/hash-numbering.md\\\"\"}}" 2
run_test 'body-file: -F short flag, bare #N'        "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh issue create -R foo/bar --title T -F $tmpdir/hash-numbering.md\"}}" 2
run_test 'inline --body: bare #N (single line)'     '{"tool_name":"Bash","tool_input":{"command":"gh issue comment -R foo/bar 1 --body \"fix #1, #2, #3\""}}' 2

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
