#!/bin/bash

# subagent-tracker.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-subagent-tracker.sh
# 失敗したケースがあれば exit 1 で終了する。
#
# マーカーは /tmp/claude-subagents-${session_id}/ に作られるため、
# テスト固有の session_id を使い trap で掃除する（実セッションには触れない）。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../subagent-tracker.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

SID="test-tracker-$$"
DIR="/tmp/claude-subagents-${SID}"
trap 'rm -rf "$DIR"' EXIT

pass=0
fail=0

hook_json() {
  printf '{"session_id":"%s","agent_id":"%s"}' "$1" "$2"
}

run_hook() {
  local mode=$1 input=$2
  printf '%s' "$input" | "$HOOK" "$mode" >/dev/null 2>&1
}

check() {
  local name=$1 got=$2 want=$3
  if [ "$got" = "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got %s, want %s)\n' "$name" "$got" "$want"
  fi
}

exists() {
  [ -e "$1" ] && echo yes || echo no
}

# --start: マーカー作成
run_hook --start "$(hook_json "$SID" agent-1)"
check '--start: exit 0' "$?" 0
check '--start: marker created' "$(exists "$DIR/agent-1")" yes
# 親が claude|node でない（= このテストシェル）ため watch PID は空
check '--start: empty pid without claude parent' "$(cat "$DIR/agent-1" 2>/dev/null)" ""

# --start: TRACKER_WATCH_PID seam で watch PID を記録
printf '%s' "$(hook_json "$SID" agent-2)" | TRACKER_WATCH_PID=12345 "$HOOK" --start >/dev/null 2>&1
check '--start: watch pid recorded via seam' "$(cat "$DIR/agent-2" 2>/dev/null)" 12345

# --start: agent_id なし → 何もしない
rm -rf "$DIR"
run_hook --start "$(hook_json "$SID" "")"
check '--start without agent_id: exit 0' "$?" 0
check '--start without agent_id: no dir' "$(exists "$DIR")" no

# --stop: 対象マーカーだけ削除
run_hook --start "$(hook_json "$SID" agent-1)"
run_hook --start "$(hook_json "$SID" agent-2)"
run_hook --stop "$(hook_json "$SID" agent-1)"
check '--stop: exit 0' "$?" 0
check '--stop: marker removed' "$(exists "$DIR/agent-1")" no
check '--stop: sibling marker kept' "$(exists "$DIR/agent-2")" yes

# --stop: agent_id なし → マーカー・ディレクトリを消さない
run_hook --stop "$(hook_json "$SID" "")"
check '--stop without agent_id: exit 0' "$?" 0
check '--stop without agent_id: markers kept' "$(exists "$DIR/agent-2")" yes

# --session-end: ディレクトリごと削除
run_hook --session-end "$(hook_json "$SID" "")"
check '--session-end: exit 0' "$?" 0
check '--session-end: dir removed' "$(exists "$DIR")" no

# 異常系入力
run_hook --start 'not-json'
check 'malformed json: exit 0' "$?" 0
run_hook '' "$(hook_json "$SID" agent-1)"
check 'unknown mode: exit 0' "$?" 0

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
