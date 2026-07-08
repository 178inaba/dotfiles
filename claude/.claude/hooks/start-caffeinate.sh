#!/bin/bash

# UserPromptSubmit / PreToolUse / SubagentStart フック:
# Claude 作業中の macOS スリープを抑止する
#
# 目的: Claude（メインセッション・サブエージェント）が能動的にタスクを処理して
#       いる間のみ caffeinate -di を起動し、応答待ち・終了後は通常のスリープ
#       タイマに戻す。
#
# リース設計: caffeinate は -t（既定30分）付きで起動し、呼び出しのたびに
#   kill → 再起動でリースを更新する。Esc 中断・API エラー（StopFailure）等、
#   Stop フックを経ない終了では stop-caffeinate.sh が呼ばれないため、
#   「更新が止まったら自己失効する」ことで caffeinate の残留（スリープ抑止
#   しっぱなし）を防ぐ。30分は単一ツール呼び出しの上限（10分）+ 推論時間を
#   十分に上回るため、活動中に失効することは実質ない。
#
# 仕様:
#   - 入力: stdin に hook JSON（session_id / agent_id を使用）
#   - PID file: セッションは /tmp/claude-caffeinate-${session_id}.pid、
#     サブエージェント（agent_id あり）は
#     /tmp/claude-caffeinate-${session_id}-agent-${agent_id}.pid
#     （サブエージェントはバックグラウンド実行され、親ターン終了の Stop で
#     セッションの caffeinate が消えても稼働し続けるため、独立した
#     per-agent caffeinate で抑止を維持する）
#   - Remote Control 接続中（CLAUDE_CODE_BRIDGE_SESSION_ID あり）のセッション
#     caffeinate は -t なし（返信待ちの間もホストを起こし続ける契約。
#     stop-caffeinate.sh の停止例外と対）。per-agent はセッション側が
#     カバーするため常に -t 付き
#   - 親プロセスが Claude 本体のとき caffeinate -w で寿命を連動させ、クラッシュ・
#     SIGKILL 等フックを経由しない終了でも caffeinate が残留しないようにする。
#     親を Claude 本体と確認できない場合に -w を使うと、短命な親の終了と同時に
#     caffeinate が消えて抑止自体が壊れるため、その場合は -w なしで起動する
#   - CAFFEINATE_BIN / CAFFEINATE_WATCH_PID / CAFFEINATE_LEASE_SECONDS 環境変数で
#     差し替え可（テスト用）
#   - 常に exit 0（フックでブロックしない）

set -euo pipefail

input=$(cat)
IFS=$'\t' read -r session_id agent_id < <(
  printf '%s' "$input" |
    jq -r '[.session_id // "", .agent_id // ""] | map(gsub("[^A-Za-z0-9-]"; "")) | @tsv'
)
: "${session_id:=unknown}"

caffeinate_bin="${CAFFEINATE_BIN:-/usr/bin/caffeinate}"
lease_seconds="${CAFFEINATE_LEASE_SECONDS:-1800}"

if [ -n "$agent_id" ]; then
  pid_file="/tmp/claude-caffeinate-${session_id}-agent-${agent_id}.pid"
else
  pid_file="/tmp/claude-caffeinate-${session_id}.pid"
fi

# リース更新: 旧プロセスを殺し、残り時間をリセットした新プロセスに置き換える。
# -t 自己失効後は stale な PID file が残るのが正常系のため、PID が別プロセスに
# 再利用されている場合の誤 kill を防ぐべく、コマンドラインを確認してから殺す
if [ -f "$pid_file" ]; then
  existing_pid=$(cat "$pid_file" 2>/dev/null || true)
  existing_cmd=$(ps -o command= -p "${existing_pid:-0}" 2>/dev/null || true)
  case "$existing_cmd" in
    *"$caffeinate_bin"*) kill "$existing_pid" 2>/dev/null || true ;;
  esac
fi

args=(-di)

watch_pid="${CAFFEINATE_WATCH_PID:-}"
if [ -z "$watch_pid" ]; then
  case "$(ps -o comm= -p "$PPID" 2>/dev/null | tr -d ' ' || true)" in
    claude|node) watch_pid=$PPID ;;
  esac
fi
if [ -n "$watch_pid" ]; then
  args+=(-w "$watch_pid")
fi

if [ -n "$agent_id" ] || [ -z "${CLAUDE_CODE_BRIDGE_SESSION_ID:-}" ]; then
  args+=(-t "$lease_seconds")
fi

nohup "$caffeinate_bin" "${args[@]}" </dev/null >/dev/null 2>&1 &
echo $! > "$pid_file"

exit 0
