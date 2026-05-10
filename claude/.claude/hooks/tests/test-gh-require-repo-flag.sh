#!/bin/bash

# gh-require-repo-flag.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-gh-require-repo-flag.sh
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../gh-require-repo-flag.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

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

# ブロックされるべきケース (exit 2)
run_test 'gh issue create (no -R)'                  '{"tool_name":"Bash","tool_input":{"command":"gh issue create --title T --body B"}}' 2
run_test 'gh pr create (no -R)'                     '{"tool_name":"Bash","tool_input":{"command":"gh pr create --title T --body B"}}' 2
run_test 'gh issue comment (no -R)'                 '{"tool_name":"Bash","tool_input":{"command":"gh issue comment 1 --body x"}}' 2
run_test 'gh release create (no -R)'                '{"tool_name":"Bash","tool_input":{"command":"gh release create v1 --title v1"}}' 2
run_test 'gh repo edit (no -R)'                     '{"tool_name":"Bash","tool_input":{"command":"gh repo edit --description x"}}' 2
run_test 'gh label create (no -R)'                  '{"tool_name":"Bash","tool_input":{"command":"gh label create bug --color FF0000"}}' 2
run_test 'gh pr merge (no -R)'                      '{"tool_name":"Bash","tool_input":{"command":"gh pr merge 5 --squash"}}' 2
run_test 'chained: cd && gh issue create (no -R)'   '{"tool_name":"Bash","tool_input":{"command":"cd /tmp/foo && gh issue create --title T"}}' 2

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
