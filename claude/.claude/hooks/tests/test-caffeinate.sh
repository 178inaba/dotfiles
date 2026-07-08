#!/bin/bash

# start-caffeinate.sh / stop-caffeinate.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-caffeinate.sh
#
# 設計判断:
#   - 実機の caffeinate を起動するとマシン全体のスリープ動作に副作用が出るため、
#     CAFFEINATE_BIN 環境変数経由でスタブ（sleep するだけのプロセス）に差し替える
#   - session_id をテストケースごとにユニークにして並列実行・前回残骸の干渉を避ける
#   - スタブは -t を解釈しない（リースの自己失効は引数検証のみで担保し、
#     実 caffeinate の -t 動作は OS 保証とする）

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
if [ -n "${STUB_ARGS_FILE:-}" ]; then
  printf '%s\n' "$*" > "$STUB_ARGS_FILE"
fi
# exec sleep に置き換えると argv から stub パスが消え、フック側の kill 前
# コマンドライン確認（PID 再利用の誤 kill 防止）に一致しなくなるため常駐で待つ
trap 'exit 0' TERM
while :; do sleep 0.05; done
EOF
chmod +x "$STUB"
export CAFFEINATE_BIN="$STUB"

# 実行環境から継承された値が既存ケースの分岐を変えないよう落とす
# （該当ケースでは明示的に付与する）
unset CLAUDE_CODE_BRIDGE_SESSION_ID CAFFEINATE_WATCH_PID CAFFEINATE_LEASE_SECONDS

cleanup() {
  pkill -f "$STUB" 2>/dev/null || true
  rm -f "$STUB"
  rm -f /tmp/claude-caffeinate-test-*.pid /tmp/claude-caffeinate-test-*.done
  rm -f /tmp/claude-caffeinate-unknown.pid
  rm -f /tmp/claude-caffeinate-test-args-*
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

call_start_agent() {
  local sid=$1 aid=$2
  printf '{"session_id":"%s","agent_id":"%s"}' "$sid" "$aid" | "$START_HOOK"
}

call_stop() {
  local sid=$1
  shift
  printf '{"session_id":"%s"}' "$sid" | "$STOP_HOOK" "$@"
}

call_agent_done() {
  local sid=$1 aid=$2
  printf '{"session_id":"%s","agent_id":"%s"}' "$sid" "$aid" | "$STOP_HOOK" --agent-done
}

pid_file_for() {
  printf '/tmp/claude-caffeinate-%s.pid' "$1"
}

agent_pid_file_for() {
  printf '/tmp/claude-caffeinate-%s-agent-%s.pid' "$1" "$2"
}

# case16/17 共有フィクスチャ: セッション + 稼働中エージェント + 完了済み（.done）
# エージェントを起動し、PF/APF_LIVE/APF_DONE/SPID/LIVE_PID/DONE_PID を設定する
setup_reap_fixture() {
  local sid=$1
  PF=$(pid_file_for "$sid")
  APF_LIVE=$(agent_pid_file_for "$sid" "alive")
  APF_DONE=$(agent_pid_file_for "$sid" "finished")
  rm -f "$PF" "$APF_LIVE" "$APF_DONE" "${APF_DONE%.pid}.done"
  call_start "$sid"
  call_start_agent "$sid" "alive"
  call_start_agent "$sid" "finished"
  SPID=$(cat "$PF")
  LIVE_PID=$(cat "$APF_LIVE")
  DONE_PID=$(cat "$APF_DONE")
  call_agent_done "$sid" "finished"
}

# 固定 sleep での消滅待ちはテスト実行時間を無駄に延ばすため、ポーリングで待つ
# （SIGTERM の消滅は通常数 ms。上限 200ms 待っても生きていれば失敗を返す）
wait_dead() {
  local pid=$1 i
  for i in $(seq 1 20); do
    kill -0 "$pid" 2>/dev/null || return 0
    sleep 0.01
  done
  return 1
}

# スタブは nohup でデタッチ起動されるため、引数記録の書き込み完了をポーリングで待つ
wait_file() {
  local f=$1 i
  for i in $(seq 1 20); do
    [ -s "$f" ] && return 0
    sleep 0.01
  done
  return 1
}

# Case 1: start with no PID file → spawns process & writes file
SID="test-case1-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
assert 'case1: PID file created'        "[ -f '$PF' ]"
PID=$(cat "$PF")
assert 'case1: process alive'           "kill -0 $PID 2>/dev/null"

# Case 2: start when process alive → lease renewal (old killed, new spawned)
SID="test-case2-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
PID1=$(cat "$PF")
call_start "$SID"
PID2=$(cat "$PF")
assert 'case2: PID renewed'             "[ '$PID1' != '$PID2' ]"
assert 'case2: old process killed'      "wait_dead $PID1"
assert 'case2: new process alive'       "kill -0 $PID2 2>/dev/null"

# Case 3: start with stale PID file (process dead) → respawns
# （999999 は macOS の PID 上限 99999 を超え、実プロセスと衝突しない stale 値）
SID="test-case3-$$"
PF=$(pid_file_for "$SID")
echo 999999 > "$PF"
call_start "$SID"
NEW_PID=$(cat "$PF")
assert 'case3: PID changed from stale'  "[ '$NEW_PID' != '999999' ]"
assert 'case3: new process alive'       "kill -0 $NEW_PID 2>/dev/null"

# Case 4: stop with PID file & alive → kills process and removes file
SID="test-case4-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
PID=$(cat "$PF")
call_stop "$SID"
assert 'case4: PID file removed'        "[ ! -f '$PF' ]"
assert 'case4: process killed'          "wait_dead $PID"

# Case 5: stop with no PID file → exit 0
SID="test-case5-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
if call_stop "$SID"; then
  pass=$((pass + 1)); printf 'PASS  %s\n' 'case5: stop no-file exits 0'
else
  rc=$?; fail=$((fail + 1)); printf 'FAIL  %s (exit %d)\n' 'case5: stop no-file' "$rc"
fi

# Case 6: stop with stale PID file → cleanup & exit 0
SID="test-case6-$$"
PF=$(pid_file_for "$SID")
echo 999999 > "$PF"
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

# Case 8: stop while Remote Control connected → session kept, .done agents still reaped
SID="test-case8-$$"
PF=$(pid_file_for "$SID")
APF_DONE=$(agent_pid_file_for "$SID" "agent8done")
rm -f "$PF" "$APF_DONE" "${APF_DONE%.pid}.done"
call_start "$SID"
call_start_agent "$SID" "agent8done"
PID=$(cat "$PF")
DONE_PID=$(cat "$APF_DONE")
call_agent_done "$SID" "agent8done"
CLAUDE_CODE_BRIDGE_SESSION_ID="rc-$$" call_stop "$SID"
assert 'case8: PID file kept'           "[ -f '$PF' ]"
assert 'case8: process still alive'     "kill -0 $PID 2>/dev/null"
assert 'case8: done agent reaped'       "wait_dead $DONE_PID"
assert 'case8: done file removed'       "[ ! -f '${APF_DONE%.pid}.done' ]"

# Case 9: stop --force (SessionEnd) while Remote Control connected → stops unconditionally
SID="test-case9-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
call_start "$SID"
PID=$(cat "$PF")
CLAUDE_CODE_BRIDGE_SESSION_ID="rc-$$" call_stop "$SID" --force
assert 'case9: PID file removed'        "[ ! -f '$PF' ]"
assert 'case9: process killed'          "wait_dead $PID"

# Case 10: watch pid resolved → caffeinate lifetime tied via -w, with lease
SID="test-case10-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
ARGS_FILE="/tmp/claude-caffeinate-test-args-watch-$$"
STUB_ARGS_FILE="$ARGS_FILE" CAFFEINATE_WATCH_PID=$$ call_start "$SID"
assert 'case10: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case10: -w with watch pid + -t' "grep -qx -- '-di -w $$ -t 1800' '$ARGS_FILE'"

# Case 11: parent is not the claude process (bash in tests) → no -w, lease only
SID="test-case11-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
ARGS_FILE="/tmp/claude-caffeinate-test-args-plain-$$"
STUB_ARGS_FILE="$ARGS_FILE" call_start "$SID"
assert 'case11: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case11: -di with -t 1800'       "grep -qx -- '-di -t 1800' '$ARGS_FILE'"

# Case 12: session start while Remote Control connected → no -t (indefinite)
SID="test-case12-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
ARGS_FILE="/tmp/claude-caffeinate-test-args-bridge-$$"
STUB_ARGS_FILE="$ARGS_FILE" CLAUDE_CODE_BRIDGE_SESSION_ID="rc-$$" call_start "$SID"
assert 'case12: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case12: no -t while bridge'     "grep -qx -- '-di' '$ARGS_FILE'"

# Case 13: agent_id input → per-agent PID file, lease always applied (even with bridge)
SID="test-case13-$$"
AID="agent13a"
APF=$(agent_pid_file_for "$SID" "$AID")
rm -f "$APF"
ARGS_FILE="/tmp/claude-caffeinate-test-args-agent-$$"
STUB_ARGS_FILE="$ARGS_FILE" CLAUDE_CODE_BRIDGE_SESSION_ID="rc-$$" call_start_agent "$SID" "$AID"
assert 'case13: agent PID file created' "[ -f '$APF' ]"
APID=$(cat "$APF")
assert 'case13: agent process alive'    "kill -0 $APID 2>/dev/null"
assert 'case13: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case13: -t even while bridge'   "grep -qx -- '-di -t 1800' '$ARGS_FILE'"

# Case 14: CAFFEINATE_LEASE_SECONDS override is reflected in args
SID="test-case14-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
ARGS_FILE="/tmp/claude-caffeinate-test-args-lease-$$"
STUB_ARGS_FILE="$ARGS_FILE" CAFFEINATE_LEASE_SECONDS=5 call_start "$SID"
assert 'case14: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case14: -t override'            "grep -qx -- '-di -t 5' '$ARGS_FILE'"

# Case 15: --agent-done renames .pid → .done without killing
SID="test-case15-$$"
AID="agent15a"
APF=$(agent_pid_file_for "$SID" "$AID")
rm -f "$APF" "${APF%.pid}.done"
call_start_agent "$SID" "$AID"
APID=$(cat "$APF")
call_agent_done "$SID" "$AID"
assert 'case15: .pid removed'           "[ ! -f '$APF' ]"
assert 'case15: .done created'          "[ -f '${APF%.pid}.done' ]"
assert 'case15: process still alive'    "kill -0 $APID 2>/dev/null"
if call_agent_done "$SID" "no-such-agent"; then
  pass=$((pass + 1)); printf 'PASS  %s\n' 'case15: agent-done no-file exits 0'
else
  rc=$?; fail=$((fail + 1)); printf 'FAIL  %s (exit %d)\n' 'case15: agent-done no-file' "$rc"
fi

# Case 16: default stop reaps session + .done agents, leaves running agents alone
SID="test-case16-$$"
setup_reap_fixture "$SID"
call_stop "$SID"
assert 'case16: session killed'         "wait_dead $SPID"
assert 'case16: session file removed'   "[ ! -f '$PF' ]"
assert 'case16: done agent reaped'      "wait_dead $DONE_PID"
assert 'case16: done file removed'      "[ ! -f '${APF_DONE%.pid}.done' ]"
assert 'case16: live agent survives'    "kill -0 $LIVE_PID 2>/dev/null"
assert 'case16: live file kept'         "[ -f '$APF_LIVE' ]"

# Case 17: stop --force kills everything including running agents
SID="test-case17-$$"
setup_reap_fixture "$SID"
call_stop "$SID" --force
assert 'case17: session killed'         "wait_dead $SPID"
assert 'case17: live agent killed'      "wait_dead $LIVE_PID"
assert 'case17: done agent killed'      "wait_dead $DONE_PID"
assert 'case17: all files removed'      "[ ! -f '$PF' ] && [ ! -f '$APF_LIVE' ] && [ ! -f '${APF_DONE%.pid}.done' ]"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -gt 0 ] && exit 1
exit 0
