#!/bin/bash

# Notification(idle_prompt) フック:
# サブエージェント稼働中の idle_prompt 通知を抑止するガード + 通知実行
#
# 目的: 親セッションがサブエージェントの完了待ちでターンを終えた「一時的な
#       アイドル」は親が完了通知で自動再開するため人間のやることが無い。
#       この間は通知せず、本当に次の入力待ちになった時だけ Ping 音 + Slack
#       通知を出す（「人間の入力が必要な時だけ通知」の実現）。
#
# 仕様:
#   - 入力: stdin に hook JSON（session_id を判定に、全体を slack-notify.sh へ）
#   - /tmp/claude-subagents-${session_id}/ のマーカー（subagent-tracker.sh が
#     管理）を走査し、稼働中サブエージェントが 1 つでもあれば通知せず exit 0
#   - マーカーに PID が記録されていて死亡している場合は無視する（クラッシュ後
#     の resume 等で同一 session_id の残骸を踏んでも沈黙し続けない）。
#     PID 空のマーカーは稼働扱い
#   - 判定不能（JSON 破損等）は通知する側に倒す。「本当に入力が必要な時に
#     鳴らない」事故だけは作らない
#   - 常に exit 0（フックでブロックしない）

set -euo pipefail

input=$(cat)

session_id=$(
  printf '%s' "$input" |
    jq -r '(.session_id // "") | gsub("[^A-Za-z0-9-]"; "")' 2>/dev/null
) || true
: "${session_id:=unknown}"
marker_dir="/tmp/claude-subagents-${session_id}"

for marker in "$marker_dir"/*; do
  [ -f "$marker" ] || continue
  pid=$(cat "$marker" 2>/dev/null || true)
  if [ -z "$pid" ] || kill -0 "$pid" 2>/dev/null; then
    exit 0
  fi
done

afplay /System/Library/Sounds/Ping.aiff 2>/dev/null || true
printf '%s' "$input" | "$(cd "$(dirname "$0")" && pwd)/slack-notify.sh"

exit 0
