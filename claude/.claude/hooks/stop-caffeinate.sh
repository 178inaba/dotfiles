#!/bin/bash

# Stop / Notification(permission_prompt|idle_prompt) / SessionEnd フック:
# start-caffeinate.sh で起動した caffeinate を停止する
#
# 目的: Claude が応答完了・承認待ち（ExitPlanMode 含む）・入力待ち・セッション
#       終了になった瞬間に caffeinate を kill し、macOS のスリープタイマを
#       通常状態に戻す。
#
# 仕様:
#   - 入力: stdin に hook JSON（session_id / hook_event_name を使用）
#   - Remote Control 接続中（CLAUDE_CODE_BRIDGE_SESSION_ID あり）は SessionEnd
#     以外で停止しない。ホストがスリープすると約10分でリモートセッションが
#     タイムアウトするため、スマホからの返信待ちの間も抑止を維持する必要がある
#   - PID file 無し / プロセス死亡時も safe（常に exit 0、フックでブロックしない）
#   - kill 失敗時の残骸を防ぐため PID file は kill 前に削除する

set -euo pipefail

input=$(cat)
session_id=$(printf '%s' "$input" | jq -r '.session_id // empty' | tr -cd 'A-Za-z0-9-')
: "${session_id:=unknown}"

hook_event=$(printf '%s' "$input" | jq -r '.hook_event_name // empty')

if [ "$hook_event" != "SessionEnd" ] && [ -n "${CLAUDE_CODE_BRIDGE_SESSION_ID:-}" ]; then
  exit 0
fi

pid_file="/tmp/claude-caffeinate-${session_id}.pid"

[ -f "$pid_file" ] || exit 0

pid=$(cat "$pid_file" 2>/dev/null || true)
rm -f "$pid_file"

[ -n "$pid" ] || exit 0

kill "$pid" 2>/dev/null || true

exit 0
