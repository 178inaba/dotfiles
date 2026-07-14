#!/bin/bash

# SubagentStart / SubagentStop / SessionEnd フック:
# 稼働中サブエージェントのマーカーを通知ガード（idle-notify.sh）向けに管理する
#
# 目的: 「このセッションでサブエージェントが稼働中か」をマーカーファイルの
#       有無として提供する。caffeinate の per-agent PID file は再利用しない
#       （ライフサイクルの所有者が別で、スリープ抑止都合の変更で通知が壊れる
#       上、残留時に通知が「沈黙」側へ倒れるため、通知専用に独立管理する）。
#
# 仕様:
#   - 入力: stdin に hook JSON（session_id / agent_id を使用）
#   - マーカー: /tmp/claude-subagents-${session_id}/${agent_id}
#     中身は Claude 本体の PID（親プロセスの comm が claude|node のときのみ。
#     idle-notify.sh がクラッシュ残骸の検出に使う。判定不能時は空 = 生存確認
#     なしの稼働扱い）
#   - --start（SubagentStart）: agent_id が空なら何もしない。マーカー作成
#   - --stop（SubagentStop）: agent_id が空なら何もしない。マーカー削除
#   - --session-end（SessionEnd）: セッションディレクトリごと削除
#   - TRACKER_WATCH_PID 環境変数で記録 PID を差し替え可（テスト用）
#   - 常に exit 0（フックでブロックしない）

set -euo pipefail

input=$(cat)
mode="${1:-}"

IFS=$'\t' read -r session_id agent_id < <(
  printf '%s' "$input" |
    jq -r '[.session_id // "", .agent_id // ""] | map(gsub("[^A-Za-z0-9-]"; "")) | @tsv'
) || true
: "${session_id:=unknown}"
: "${agent_id:=}"
marker_dir="/tmp/claude-subagents-${session_id}"

case "$mode" in
  --start)
    [ -n "$agent_id" ] || exit 0
    watch_pid="${TRACKER_WATCH_PID:-}"
    if [ -z "$watch_pid" ]; then
      case "$(ps -o comm= -p "$PPID" 2>/dev/null | tr -d ' ' || true)" in
        claude|node) watch_pid=$PPID ;;
      esac
    fi
    mkdir -p "$marker_dir"
    printf '%s\n' "$watch_pid" > "${marker_dir}/${agent_id}"
    ;;
  --stop)
    [ -n "$agent_id" ] || exit 0
    rm -f "${marker_dir}/${agent_id}"
    ;;
  --session-end)
    rm -rf "$marker_dir"
    ;;
esac

exit 0
