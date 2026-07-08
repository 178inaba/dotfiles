#!/bin/bash

# Stop / Notification(permission_prompt|idle_prompt) / SessionEnd / SubagentStop フック:
# start-caffeinate.sh で起動した caffeinate を停止・回収する
#
# 目的: Claude が応答完了・承認待ち（ExitPlanMode 含む）・入力待ち・セッション
#       終了になった瞬間に caffeinate を kill し、macOS のスリープタイマを
#       通常状態に戻す。リース（-t）による自己失効はフェイルセーフであり、
#       イベントが取れる経路ではここで即時解放する。
#
# 仕様:
#   - 入力: stdin に hook JSON（session_id / agent_id を使用）
#   - --agent-done（SubagentStop 用）: per-agent PID file を .pid → .done に
#     リネームするだけで kill しない。完了直後は親が結果を読んで要約している
#     間もスリープ抑止を維持する必要があるため、回収は親の次の Stop に委ねる
#   - デフォルト（Stop / Notification）: セッションの caffeinate を kill し、
#     .done になった per-agent caffeinate を回収する。稼働中（.pid）の
#     per-agent は殺さない（バックグラウンドのサブエージェントは親ターン
#     終了後も動き続けており、抑止が切れるとスリープで凍結するため）
#   - Remote Control 接続中（CLAUDE_CODE_BRIDGE_SESSION_ID あり）はセッションの
#     caffeinate を停止しない。ホストがスリープすると約10分でリモートセッションが
#     タイムアウトするため、スマホからの返信待ちの間も抑止を維持する必要がある
#     （.done の回収は完了済みエージェントの解放であり抑止を妨げないため行う）
#   - --force はこの例外を無視し、per-agent（.pid/.done とも）を含めて
#     無条件に停止する（settings.json の SessionEnd 登録で指定。セッション
#     終了時の残留 caffeinate を防ぐ）
#   - PID file 無し / プロセス死亡時も safe（常に exit 0、フックでブロックしない）
#   - kill 失敗時の残骸を防ぐため PID file は kill 前に削除する
#   - -t 自己失効後の stale PID が別プロセスに再利用されている場合の誤 kill を
#     防ぐため、コマンドラインに caffeinate のパスを含むことを確認してから殺す

set -euo pipefail

input=$(cat)
mode="${1:-}"

IFS=$'\t' read -r session_id agent_id < <(
  printf '%s' "$input" |
    jq -r '[.session_id // "", .agent_id // ""] | map(gsub("[^A-Za-z0-9-]"; "")) | @tsv'
)
: "${session_id:=unknown}"
agent_prefix="/tmp/claude-caffeinate-${session_id}-agent-"
caffeinate_bin="${CAFFEINATE_BIN:-/usr/bin/caffeinate}"

kill_pid_file() {
  local f=$1 pid cmd
  [ -f "$f" ] || return 0
  pid=$(cat "$f" 2>/dev/null || true)
  rm -f "$f"
  [ -n "$pid" ] || return 0
  cmd=$(ps -o command= -p "$pid" 2>/dev/null || true)
  case "$cmd" in
    *"$caffeinate_bin"*) kill "$pid" 2>/dev/null || true ;;
  esac
}

if [ "$mode" = "--agent-done" ]; then
  [ -n "$agent_id" ] || exit 0
  agent_pid_file="${agent_prefix}${agent_id}.pid"
  if [ -f "$agent_pid_file" ]; then
    mv -f "$agent_pid_file" "${agent_pid_file%.pid}.done"
  fi
  exit 0
fi

if [ "$mode" = "--force" ] || [ -z "${CLAUDE_CODE_BRIDGE_SESSION_ID:-}" ]; then
  kill_pid_file "/tmp/claude-caffeinate-${session_id}.pid"
fi

for f in "${agent_prefix}"*.done; do
  kill_pid_file "$f"
done

if [ "$mode" = "--force" ]; then
  for f in "${agent_prefix}"*.pid; do
    kill_pid_file "$f"
  done
fi

exit 0
