#!/bin/bash

# no-op-wait-guard.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-no-op-wait-guard.sh
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../no-op-wait-guard.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

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

# stderr メッセージの安定部分文字列を検証する。全文一致にしないのは、
# 文言の推敲でテストが壊れないようにするため（要件の3点が残っているかだけを見る）。
run_stderr_test() {
  local name=$1 input=$2 want_substring=$3
  printf '%s' "$input" | "$HOOK" >/dev/null 2>"$tmpdir/stderr.txt"
  if grep -qF -- "$want_substring" "$tmpdir/stderr.txt"; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (stderr lacks %s)\n' "$name" "$want_substring"
  fi
}

# 解析できない入力での fail-open を検証する。exit code の実値は jq に依存する
# ため、「ブロックしない」＝ exit 2 以外であることだけをアサートする。
run_non_block_test() {
  local name=$1 input=$2
  local got_exit
  printf '%s' "$input" | "$HOOK" >/dev/null 2>&1
  got_exit=$?
  if [ "$got_exit" -ne 2 ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (blocked with exit 2, want fail-open)\n' "$name"
  fi
}

# 通過すべきケース (exit 0)
run_test 'tool_name=Edit (not Bash)'      '{"tool_name":"Edit","tool_input":{"command":"echo waiting"}}' 0
run_test 'empty command'                  '{"tool_name":"Bash","tool_input":{"command":""}}' 0
run_test 'polling loop with real command' '{"tool_name":"Bash","tool_input":{"command":"until ! gh pr checks 13 -R o/r 2>&1 | grep -q pending; do sleep 5; done"}}' 0
run_test 'sleep then real command'        '{"tool_name":"Bash","tool_input":{"command":"sleep 5; gh pr checks 13 -R o/r"}}' 0
run_test 'echo with redirection'          '{"tool_name":"Bash","tool_input":{"command":"echo \"$x\" > f"}}' 0
run_test 'echo with two words'            '{"tool_name":"Bash","tool_input":{"command":"echo a b"}}' 0
run_test 'echo piped'                     '{"tool_name":"Bash","tool_input":{"command":"echo ok | cat"}}' 0
run_test 'for loop with echo'             '{"tool_name":"Bash","tool_input":{"command":"for i in $(seq 1 60); do [ -f x ] && exit 0; sleep 1; done; echo TIMEOUT"}}' 0
run_test 'sleep && echo (not a leading ;)' '{"tool_name":"Bash","tool_input":{"command":"sleep 1 && echo w"}}' 0
run_test 'echo inside multi-line script'  '{"tool_name":"Bash","tool_input":{"command":"set -e\nmake build\necho done"}}' 0
run_test 'pwd (out of scope no-op)'       '{"tool_name":"Bash","tool_input":{"command":"pwd"}}' 0
run_test 'git status (out of scope no-op)' '{"tool_name":"Bash","tool_input":{"command":"git status"}}' 0
run_test 'token over the length limit'    '{"tool_name":"Bash","tool_input":{"command":"echo abcdefghijklmnopqrstuvwxy"}}' 0
run_test 'trailing semicolon (other shape)' '{"tool_name":"Bash","tool_input":{"command":"echo idle1;"}}' 0
run_test 'sleep without duration'         '{"tool_name":"Bash","tool_input":{"command":"sleep"}}' 0
run_test 'echo with a flag'               '{"tool_name":"Bash","tool_input":{"command":"echo -n ok"}}' 0

# 解析できない入力 (fail-open)
run_non_block_test 'malformed JSON'       'not json at all'
run_non_block_test 'JSON without tool_input' '{"tool_name":"Bash"}'

# ブロックされるべきケース (exit 2)
run_test 'echo idle12'                    '{"tool_name":"Bash","tool_input":{"command":"echo idle12"}}' 2
run_test 'echo w7'                        '{"tool_name":"Bash","tool_input":{"command":"echo w7"}}' 2
run_test 'echo waiting'                   '{"tool_name":"Bash","tool_input":{"command":"echo waiting"}}' 2
run_test 'echo ok'                        '{"tool_name":"Bash","tool_input":{"command":"echo ok"}}' 2
run_test 'echo waiting-for-plan-agent'    '{"tool_name":"Bash","tool_input":{"command":"echo waiting-for-plan-agent"}}' 2
run_test 'printf ok'                      '{"tool_name":"Bash","tool_input":{"command":"printf ok"}}' 2
run_test 'echo (no argument)'             '{"tool_name":"Bash","tool_input":{"command":"echo"}}' 2
run_test 'true'                           '{"tool_name":"Bash","tool_input":{"command":"true"}}' 2
run_test 'colon'                          '{"tool_name":"Bash","tool_input":{"command":":"}}' 2
run_test 'sleep 2'                        '{"tool_name":"Bash","tool_input":{"command":"sleep 2"}}' 2
run_test 'sleep 5m (suffix form)'         '{"tool_name":"Bash","tool_input":{"command":"sleep 5m"}}' 2
run_test 'sleep 30s (suffix form)'        '{"tool_name":"Bash","tool_input":{"command":"sleep 30s"}}' 2
run_test 'sleep 1; echo waiting'          '{"tool_name":"Bash","tool_input":{"command":"sleep 1; echo waiting"}}' 2
run_test 'sleep 1; echo done'             '{"tool_name":"Bash","tool_input":{"command":"sleep 1; echo done"}}' 2
run_test 'sleep 1 ; true (spaced ;)'      '{"tool_name":"Bash","tool_input":{"command":"sleep 1 ; true"}}' 2
run_test 'sleep 1;echo w (unspaced ;)'    '{"tool_name":"Bash","tool_input":{"command":"sleep 1;echo w"}}' 2
run_test 'sleep 0.5; echo w (decimal)'    '{"tool_name":"Bash","tool_input":{"command":"sleep 0.5; echo w"}}' 2
run_test 'sleep .5; true (bare decimal)'  '{"tool_name":"Bash","tool_input":{"command":"sleep .5; true"}}' 2
run_test 'echo with double-quoted token'  '{"tool_name":"Bash","tool_input":{"command":"echo \"ok\""}}' 2
run_test 'echo with single-quoted token'  "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"echo 'ok'\"}}" 2
run_test 'extra whitespace around tokens' '{"tool_name":"Bash","tool_input":{"command":"  echo\t  idle1  "}}' 2
run_test 'newline-separated sleep; echo'  '{"tool_name":"Bash","tool_input":{"command":"sleep 1;\necho waiting"}}' 2
run_test 'token at the length limit'      '{"tool_name":"Bash","tool_input":{"command":"echo abcdefghijklmnopqrstuvwx"}}' 2

# stderr メッセージが要件の3点を伝えているか（1点につき1つの部分文字列）
block_input='{"tool_name":"Bash","tool_input":{"command":"echo waiting"}}'
run_stderr_test 'message: end the turn'   "$block_input" 'ターンを終えて'
run_stderr_test 'message: no other no-op' "$block_input" '別の no-op コマンドで置き換える'
run_stderr_test 'message: foreground timeout' "$block_input" 'Bash ツールの timeout パラメータ'

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
