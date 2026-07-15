#!/bin/bash

# idle-notify.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-idle-notify.sh
# 失敗したケースがあれば exit 1 で終了する。
#
# afplay / curl は PATH 先頭のスタブに差し替えて呼び出しをファイルに記録する
# （実音・実 webhook・ネットワークには触れない）。slack-notify.sh /
# terminal-bell.sh は実物を通す統合形（stdout をファイルに捕捉し、通知時は
# terminalSequence JSON のみが出ることを検証する）。マーカーはテスト固有の
# session_id 配下に直接作成し trap で掃除する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../idle-notify.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

TMP_BASE=$(mktemp -d)
SID="test-idle-$$"
DIR="/tmp/claude-subagents-${SID}"
trap 'rm -rf "$TMP_BASE" "$DIR"' EXIT

AFPLAY_LOG="$TMP_BASE/afplay.log"
CURL_LOG="$TMP_BASE/curl.log"
STDOUT_LOG="$TMP_BASE/stdout.log"
BELL=$(printf '\a')
mkdir -p "$TMP_BASE/bin" "$TMP_BASE/proj"
cat > "$TMP_BASE/bin/afplay" <<EOF
#!/bin/bash
printf '%s\n' "\$*" >> "$AFPLAY_LOG"
EOF
cat > "$TMP_BASE/bin/curl" <<EOF
#!/bin/bash
printf '%s\n' "\$*" >> "$CURL_LOG"
EOF
chmod +x "$TMP_BASE/bin/afplay" "$TMP_BASE/bin/curl"
export PATH="$TMP_BASE/bin:$PATH"

INPUT="$(printf '{"message":"hello","notification_type":"idle_prompt","cwd":"%s","session_id":"%s"}' "$TMP_BASE/proj" "$SID")"

pass=0
fail=0

run_hook() {
  : > "$AFPLAY_LOG"
  : > "$CURL_LOG"
  printf '%s' "$1" | CLAUDE_SLACK_WEBHOOK="https://example.invalid/webhook" "$HOOK" >"$STDOUT_LOG" 2>/dev/null
}

# stdout 全体が terminalSequence JSON（値は BEL 1文字）であること
bell_emitted() {
  jq -e . "$STDOUT_LOG" >/dev/null 2>&1 &&
    [ "$(jq -r '.terminalSequence' "$STDOUT_LOG" 2>/dev/null)" = "$BELL" ]
}

# 期待: 通知する（afplay + slack + 端末ベルが発火）
check_notified() {
  local name=$1 rc=$2
  if [ "$rc" -eq 0 ] && [ -s "$AFPLAY_LOG" ] && grep -qF '(idle_prompt) hello' "$CURL_LOG" && bell_emitted; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (exit %d, afplay: %s, curl: %s, stdout: %s)\n' "$name" "$rc" "$(cat "$AFPLAY_LOG")" "$(cat "$CURL_LOG")" "$(cat "$STDOUT_LOG")"
  fi
}

# 期待: 沈黙（afplay も slack も端末ベルも発火しない）
check_silent() {
  local name=$1 rc=$2
  if [ "$rc" -eq 0 ] && [ ! -s "$AFPLAY_LOG" ] && [ ! -s "$CURL_LOG" ] && [ ! -s "$STDOUT_LOG" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (exit %d, afplay: %s, curl: %s, stdout: %s)\n' "$name" "$rc" "$(cat "$AFPLAY_LOG")" "$(cat "$CURL_LOG")" "$(cat "$STDOUT_LOG")"
  fi
}

marker() {
  mkdir -p "$DIR"
  printf '%s\n' "$2" > "$DIR/$1"
}

sleep 0.1 &
DEAD_PID=$!
wait "$DEAD_PID"

# マーカーなし → 通知
run_hook "$INPUT"
check_notified 'no markers: notify' "$?"

# Slack 本文にプロジェクトラベルが載ること（slack-notify.sh との統合確認）
if grep -qF '[proj] (idle_prompt) hello' "$CURL_LOG"; then
  pass=$((pass + 1)); printf 'PASS  %s\n' 'no markers: slack text has project label'
else
  fail=$((fail + 1)); printf 'FAIL  %s (curl: %s)\n' 'no markers: slack text has project label' "$(cat "$CURL_LOG")"
fi

# 生きた PID のマーカー → 沈黙
marker agent-1 "$$"
run_hook "$INPUT"
check_silent 'live-pid marker: silent' "$?"
rm -rf "$DIR"

# 死んだ PID のマーカーのみ → 通知（クラッシュ残骸で沈黙し続けない）
marker agent-1 "$DEAD_PID"
run_hook "$INPUT"
check_notified 'dead-pid marker only: notify' "$?"
rm -rf "$DIR"

# PID 空のマーカー → 沈黙（生存確認不能時は稼働扱い）
marker agent-1 ""
run_hook "$INPUT"
check_silent 'empty-pid marker: silent' "$?"
rm -rf "$DIR"

# 複数マーカーで 1 つでも生存 → 沈黙
marker agent-1 "$DEAD_PID"
marker agent-2 "$$"
run_hook "$INPUT"
check_silent 'mixed markers with one live: silent' "$?"
rm -rf "$DIR"

# 壊れた JSON → exit 0 で通知側に倒れる（音とベルは鳴る。Slack は message 空のため飛ばない）
run_hook 'not-json'
rc=$?
if [ "$rc" -eq 0 ] && [ -s "$AFPLAY_LOG" ] && [ ! -s "$CURL_LOG" ] && bell_emitted; then
  pass=$((pass + 1)); printf 'PASS  %s\n' 'malformed json: fail-open to notify'
else
  fail=$((fail + 1)); printf 'FAIL  %s (exit %d, afplay: %s, curl: %s, stdout: %s)\n' 'malformed json: fail-open to notify' "$rc" "$(cat "$AFPLAY_LOG")" "$(cat "$CURL_LOG")" "$(cat "$STDOUT_LOG")"
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
