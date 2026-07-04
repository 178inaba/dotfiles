#!/bin/bash

# UserPromptSubmit / PreToolUse フック: Claude 作業中の macOS スリープを抑止する
#
# 目的: Claude が能動的にタスクを処理している間のみ caffeinate -di を起動し、
#       応答待ち・終了後は通常のスリープタイマに戻す。idempotent なので
#       PreToolUse で毎ツール呼び出し時に呼ばれても多重起動しない。
#
# 仕様:
#   - 入力: stdin に hook JSON（session_id を使用）
#   - PID file: /tmp/claude-caffeinate-${session_id}.pid
#   - 既存プロセスが生存していれば no-op、stale なら respawn
#   - 親プロセスが Claude 本体のとき caffeinate -w で寿命を連動させ、クラッシュ・
#     SIGKILL 等フックを経由しない終了でも caffeinate が残留しないようにする。
#     親を Claude 本体と確認できない場合に -w を使うと、短命な親の終了と同時に
#     caffeinate が消えて抑止自体が壊れるため、その場合は -w なしで起動する
#   - CAFFEINATE_BIN / CAFFEINATE_WATCH_PID 環境変数で差し替え可（テスト用）
#   - 常に exit 0（フックでブロックしない）

set -euo pipefail

input=$(cat)
session_id=$(printf '%s' "$input" | jq -r '.session_id // empty' | tr -cd 'A-Za-z0-9-')
: "${session_id:=unknown}"

pid_file="/tmp/claude-caffeinate-${session_id}.pid"
caffeinate_bin="${CAFFEINATE_BIN:-/usr/bin/caffeinate}"

if [ -f "$pid_file" ]; then
  existing_pid=$(cat "$pid_file" 2>/dev/null || true)
  if [ -n "$existing_pid" ] && kill -0 "$existing_pid" 2>/dev/null; then
    exit 0
  fi
fi

watch_pid="${CAFFEINATE_WATCH_PID:-}"
if [ -z "$watch_pid" ]; then
  case "$(ps -o comm= -p "$PPID" 2>/dev/null | tr -d ' ' || true)" in
    claude|node) watch_pid=$PPID ;;
  esac
fi

if [ -n "$watch_pid" ]; then
  nohup "$caffeinate_bin" -di -w "$watch_pid" </dev/null >/dev/null 2>&1 &
else
  nohup "$caffeinate_bin" -di </dev/null >/dev/null 2>&1 &
fi
echo $! > "$pid_file"

exit 0
