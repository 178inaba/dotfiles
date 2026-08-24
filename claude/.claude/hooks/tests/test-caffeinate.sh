#!/bin/bash

# start-caffeinate.sh / stop-caffeinate.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-caffeinate.sh
#
# 設計判断:
#   - 実機の caffeinate を起動するとマシン全体のスリープ動作に副作用が出るため、
#     CAFFEINATE_BIN 環境変数経由でスタブ（sleep するだけのプロセス）に差し替える
#   - session_id をテストケースごとにユニークにし、フィクスチャ（PID file・引数記録）は
#     run ごとの作業ディレクトリに隔離して、並列実行・前回残骸の干渉を避ける。
#     PID file の置き場所は CAFFEINATE_PID_DIR で差し替える（空 session_id の
#     フォールバック名はフック側で固定されており、テスト側では分離できないため）
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

WORK_DIR=$(mktemp -d -t claude-caffeinate-test.XXXXXX)
export CAFFEINATE_PID_DIR="$WORK_DIR"

STUB="$WORK_DIR/caffeinate-stub"
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

DEFAULT_DIR_SID="test-defaultdir-$$"
DEFAULT_DIR_PF="/tmp/claude-caffeinate-${DEFAULT_DIR_SID}.pid"

# フィクスチャは WORK_DIR に隔離されているため、ディレクトリごと消せば足りる
# （グロブでの一括削除は他プロセスの実行中フィクスチャまで巻き込む）。
# 既定パスを使う case21 の 1 本だけは WORK_DIR の外にあるので個別に消す
cleanup() {
  pkill -f "$STUB" 2>/dev/null || true
  # ゾンビの親を殺すと、ゾンビは init に reparent されて回収される。
  # :- 付きなのは、ケース到達前の異常終了でも EXIT trap が走るため（set -u）
  [ -n "${ZOMBIE_PARENT:-}" ] && kill "$ZOMBIE_PARENT" 2>/dev/null
  rm -rf "$WORK_DIR"
  rm -f "$DEFAULT_DIR_PF"
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
  printf '%s/claude-caffeinate-%s.pid' "$CAFFEINATE_PID_DIR" "$1"
}

agent_pid_file_for() {
  printf '%s/claude-caffeinate-%s-agent-%s.pid' "$CAFFEINATE_PID_DIR" "$1" "$2"
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

# プロセスの状態を答える唯一のヘルパー。「消えた」「生きている」の両方をこれで表現し、
# スイート内で同じ問いに 2 つの答えが出ないようにする。
# kill -0 は「PID が存在するか」しか答えず、終了済み・未回収（state Z）にも成功する。
# スタブは nohup でデタッチ起動され、回収するのは init / subreaper でテストの制御外の
# ため、正しく終了したスタブが任意の時間 Z に留まりうる。
# 空出力 = プロセス不在。macOS の ps は state に修飾子を付ける（Z+ 等）ので前方一致で見る
proc_state() {
  local s
  s=$(ps -o state= -p "$1" 2>/dev/null)
  printf '%s' "${s// /}"
}

proc_gone() {
  case "$(proc_state "$1")" in
    '' | Z*) return 0 ;;
  esac
  return 1
}

proc_alive() {
  ! proc_gone "$1"
}

# ゾンビ（終了済み・未回収）のフィクスチャを作り、ZOMBIE_PID に設定する。
# bash は SIGCHLD で背景ジョブを自動回収するため、親を exec で sleep に置き換えて
# wait() を呼ばない親にする（perl 等の追加依存を避けるための pure bash 実装）。
# 親は生存し続けるので init への reparent も起きず、子は kill 後 Z に留まる
ZOMBIE_PARENT=''
ZOMBIE_PID=''
make_zombie() {
  local pid_file="$WORK_DIR/zombie.pid" i
  rm -f "$pid_file"
  bash -c 'sleep 100 & printf "%s" "$!" > "$1"; exec sleep 30' _ "$pid_file" &
  ZOMBIE_PARENT=$!
  wait_file "$pid_file" || return 1
  ZOMBIE_PID=$(cat "$pid_file")
  kill -TERM "$ZOMBIE_PID" 2>/dev/null || return 1
  for i in $(seq 1 50); do
    case "$(proc_state "$ZOMBIE_PID")" in
      Z*) return 0 ;;
    esac
    sleep 0.01
  done
  return 1
}

# 固定 sleep での消滅待ちはテスト実行時間を無駄に延ばすため、ポーリングで待つ
# （SIGTERM の消滅は通常数 ms。上限 200ms 待っても生きていれば失敗を返す）
wait_dead() {
  local pid=$1 i
  for i in $(seq 1 20); do
    proc_gone "$pid" && return 0
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
assert 'case1: process alive'           "proc_alive $PID"

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
assert 'case2: new process alive'       "proc_alive $PID2"

# Case 3: start with stale PID file (process dead) → respawns
# （999999 は macOS の PID 上限 99999 を超え、実プロセスと衝突しない stale 値）
SID="test-case3-$$"
PF=$(pid_file_for "$SID")
echo 999999 > "$PF"
call_start "$SID"
NEW_PID=$(cat "$PF")
assert 'case3: PID changed from stale'  "[ '$NEW_PID' != '999999' ]"
assert 'case3: new process alive'       "proc_alive $NEW_PID"

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
assert 'case8: process still alive'     "proc_alive $PID"
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
ARGS_FILE="$WORK_DIR/args-watch"
STUB_ARGS_FILE="$ARGS_FILE" CAFFEINATE_WATCH_PID=$$ call_start "$SID"
assert 'case10: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case10: -w with watch pid + -t' "grep -qx -- '-di -w $$ -t 1800' '$ARGS_FILE'"

# Case 11: parent is not the claude process (bash in tests) → no -w, lease only
SID="test-case11-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
ARGS_FILE="$WORK_DIR/args-plain"
STUB_ARGS_FILE="$ARGS_FILE" call_start "$SID"
assert 'case11: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case11: -di with -t 1800'       "grep -qx -- '-di -t 1800' '$ARGS_FILE'"

# Case 12: session start while Remote Control connected → no -t (indefinite)
SID="test-case12-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
ARGS_FILE="$WORK_DIR/args-bridge"
STUB_ARGS_FILE="$ARGS_FILE" CLAUDE_CODE_BRIDGE_SESSION_ID="rc-$$" call_start "$SID"
assert 'case12: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case12: no -t while bridge'     "grep -qx -- '-di' '$ARGS_FILE'"

# Case 13: agent_id input → per-agent PID file, lease always applied (even with bridge)
SID="test-case13-$$"
AID="agent13a"
APF=$(agent_pid_file_for "$SID" "$AID")
rm -f "$APF"
ARGS_FILE="$WORK_DIR/args-agent"
STUB_ARGS_FILE="$ARGS_FILE" CLAUDE_CODE_BRIDGE_SESSION_ID="rc-$$" call_start_agent "$SID" "$AID"
assert 'case13: agent PID file created' "[ -f '$APF' ]"
APID=$(cat "$APF")
assert 'case13: agent process alive'    "proc_alive $APID"
assert 'case13: args recorded'          "wait_file '$ARGS_FILE'"
assert 'case13: -t even while bridge'   "grep -qx -- '-di -t 1800' '$ARGS_FILE'"

# Case 14: CAFFEINATE_LEASE_SECONDS override is reflected in args
SID="test-case14-$$"
PF=$(pid_file_for "$SID")
rm -f "$PF"
ARGS_FILE="$WORK_DIR/args-lease"
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
assert 'case15: process still alive'    "proc_alive $APID"
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
assert 'case16: live agent survives'    "proc_alive $LIVE_PID"
assert 'case16: live file kept'         "[ -f '$APF_LIVE' ]"

# Case 17: stop --force kills everything including running agents
SID="test-case17-$$"
setup_reap_fixture "$SID"
call_stop "$SID" --force
assert 'case17: session killed'         "wait_dead $SPID"
assert 'case17: live agent killed'      "wait_dead $LIVE_PID"
assert 'case17: done agent killed'      "wait_dead $DONE_PID"
assert 'case17: all files removed'      "[ ! -f '$PF' ] && [ ! -f '$APF_LIVE' ] && [ ! -f '${APF_DONE%.pid}.done' ]"

# Case 18: a terminated-but-unreaped process (state Z) counts as gone, not alive
if make_zombie; then
  # kill -0 がゾンビに成功することの確認。これは liveness 判定ではなく、
  # フィクスチャが本当に「kill -0 が誤答する窓」を再現できている健全性確認
  assert 'case18: kill -0 still succeeds on the zombie' "kill -0 $ZOMBIE_PID 2>/dev/null"
  assert 'case18: wait_dead treats the zombie as gone'  "wait_dead $ZOMBIE_PID"
  assert 'case18: proc_alive rejects the zombie'        "! proc_alive $ZOMBIE_PID"
else
  fail=$((fail + 1))
  printf 'FAIL  %s\n' 'case18: zombie fixture did not reach state Z'
fi

# Case 21: CAFFEINATE_PID_DIR unset → hooks still use their default location
# （テスト外の挙動が変わっていないことの担保。この 1 ケースだけ WORK_DIR の外へ
#   書くため session_id を $$ スコープにして並行 run と衝突させない）
rm -f "$DEFAULT_DIR_PF"
( unset CAFFEINATE_PID_DIR; call_start "$DEFAULT_DIR_SID" )
assert 'case21: default dir PID file created' "[ -f '$DEFAULT_DIR_PF' ]"
DEFAULT_DIR_PID=$(cat "$DEFAULT_DIR_PF" 2>/dev/null || echo 0)
( unset CAFFEINATE_PID_DIR; call_stop "$DEFAULT_DIR_SID" )
assert 'case21: default dir PID file removed' "[ ! -f '$DEFAULT_DIR_PF' ]"
assert 'case21: default dir process killed'   "wait_dead $DEFAULT_DIR_PID"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -gt 0 ] && exit 1
exit 0
