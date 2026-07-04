#!/bin/bash

# start-caffeinate.sh / stop-caffeinate.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-caffeinate.sh
#
# 設計判断:
#   - 実機の caffeinate を起動するとマシン全体のスリープ動作に副作用が出るため、
#     CAFFEINATE_BIN 環境変数経由でスタブ（sleep するだけのプロセス）に差し替える
#   - session_id をテストケースごとにユニークにして並列実行・前回残骸の干渉を避ける

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
START_HOOK="$SCRIPT_DIR/../start-caffeinate.sh"
STOP_HOOK="$SCRIPT_DIR/../stop-caffeinate.sh"

for h in "$START_HOOK" "$STOP_HOOK"; do
  if [ ! -x "$h" ]; then
    printf 'ERROR: hook not executable: %s\n' "$h" >&2
    exit 1
  fi
done

STUB=$(mktemp -t caffeinate-stub.XXXXXX)
cat >"$STUB" <<'EOF'
#!/bin/bash
exec sleep 9999
EOF
chmod +x "$STUB"
export CAFFEINATE_BIN="$STUB"

# Remote Control 接続中の環境でテストを実行しても既存ケースが影響を受けないよう、
# 継承された可能性のある値を落とす（スキップ判定のケースでは明示的に付与する）
unset CLAUDE_CODE_BRIDGE_SESSION_ID

cleanup() {
  pkill -f "$STUB" 2>/dev/null || true
  rm -f "$STUB"
  rm -f /tmp/claude-caffeinate-test-*.pid /tmp/claude-caffeinate-unknown.pid
}
trap cleanup EXIT

pass=0
fail=0

assert() {
  local name=$1 cond=$2
  if eval "$cond"; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (cond: %s)\n' "$name" "$cond"
  fi
}

call_start() {
  local sid=$1
  printf '{"session_id":"%s"}' "$sid" | "$START_HOOK"
}

call_stop() {
  local sid=$1
  printf '{"session_id":"%s"}' "$sid" | "$STOP_HOOK"
}

call_stop_event() {
  local sid=$1 event=$2
  printf '{"session_id":"%s","hook_event_name":"%s"}' "$sid" "$event" | "$STOP_HOOK"
}

pid_file_for() {
  printf '/tmp/claude-caffeinate-%s.pid' "$1"
}

# Case 1: start with no PID file → spawns process & writes file
SID="test-case1-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
assert 'case1: PID file created'        "[ -f '$PF' ]"
PID=$(cat "$PF")
assert 'case1: process alive'           "kill -0 $PID 2>/dev/null"

# Case 2: start when PID file exists & process alive → no-op (PID unchanged)
SID="test-case2-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
PID1=$(cat "$PF")
call_start "$SID"
PID2=$(cat "$PF")
assert 'case2: PID unchanged'           "[ '$PID1' = '$PID2' ]"
assert 'case2: process still alive'     "kill -0 $PID2 2>/dev/null"

# Case 3: start with stale PID file (process dead) → respawns
SID="test-case3-$$"
PF=$(pid_file_for "$SID")
echo 99999 > "$PF"
call_start "$SID"
NEW_PID=$(cat "$PF")
assert 'case3: PID changed from stale'  "[ '$NEW_PID' != '99999' ]"
assert 'case3: new process alive'       "kill -0 $NEW_PID 2>/dev/null"

# Case 4: stop with PID file & alive → kills process and removes file
SID="test-case4-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
PID=$(cat "$PF")
call_stop "$SID"
assert 'case4: PID file removed'        "[ ! -f '$PF' ]"
sleep 0.2
assert 'case4: process killed'          "! kill -0 $PID 2>/dev/null"

# Case 5: stop with no PID file → exit 0
SID="test-case5-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
if call_stop "$SID"; then
  pass=$((pass + 1)); printf 'PASS  %s\n' 'case5: stop no-file exits 0'
else
  fail=$((fail + 1)); printf 'FAIL  %s (exit %d)\n' 'case5: stop no-file' $?
fi

# Case 6: stop with stale PID file → cleanup & exit 0
SID="test-case6-$$"
PF=$(pid_file_for "$SID")
echo 99999 > "$PF"
if call_stop "$SID"; then
  cleanup_ok=1
else
  cleanup_ok=0
fi
assert 'case6: stop stale exits 0'      "[ $cleanup_ok -eq 1 ]"
assert 'case6: PID file removed'        "[ ! -f '$PF' ]"

# Case 7: empty session_id → falls back to "unknown", no crash
PF=$(pid_file_for "unknown")
rm -f "$PF"
printf '{}' | "$START_HOOK"
assert 'case7: unknown PID file created' "[ -f '$PF' ]"
printf '{}' | "$STOP_HOOK"
assert 'case7: unknown PID file removed' "[ ! -f '$PF' ]"

# Case 8: Stop event while Remote Control connected → skip (process stays alive)
SID="test-case8-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
PID=$(cat "$PF")
CLAUDE_CODE_BRIDGE_SESSION_ID="rc-$$" call_stop_event "$SID" "Stop"
assert 'case8: PID file kept'           "[ -f '$PF' ]"
assert 'case8: process still alive'     "kill -0 $PID 2>/dev/null"
call_stop "$SID"

# Case 9: SessionEnd while Remote Control connected → stops unconditionally
SID="test-case9-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
PID=$(cat "$PF")
CLAUDE_CODE_BRIDGE_SESSION_ID="rc-$$" call_stop_event "$SID" "SessionEnd"
assert 'case9: PID file removed'        "[ ! -f '$PF' ]"
sleep 0.2
assert 'case9: process killed'          "! kill -0 $PID 2>/dev/null"

# Case 10: Stop event without Remote Control connection → stops as before
SID="test-case10-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
PID=$(cat "$PF")
call_stop_event "$SID" "Stop"
assert 'case10: PID file removed'       "[ ! -f '$PF' ]"
sleep 0.2
assert 'case10: process killed'         "! kill -0 $PID 2>/dev/null"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -gt 0 ] && exit 1
exit 0
