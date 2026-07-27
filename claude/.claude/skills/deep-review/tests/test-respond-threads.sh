#!/bin/bash

# respond-threads.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/deep-review/tests/test-respond-threads.sh
# gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/respond-threads.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# --- gh スタブ ---
# mutation の種別は query 文字列で判別する（引数位置に依存させない）。
# 失敗の注入は env ではなくファイルで行う: `VAR=x shell_function` の assignment は
# 関数内で spawn する子プロセス（このスタブ）に export されないため、env 注入だと
# 「失敗させたつもりが素通り」で silent に無効化される。
# 呼び出しは CALL_LOG に 1 行 1 呼び出しで記録し、「検証違反時に mutation を
# 1 度も打たない」性質をテストから確認できるようにする
mkdir -p "$TMP/stub"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
# 成功時にも stderr へ出す（gh の deprecation 警告等）。スクリプトが 2>&1 で stdout に
# 混ぜていると JSON パースが壊れるため、全ケースがこの分離を検証することになる
printf 'gh: warning noise on stderr (must not reach stdout)\n' >&2
if [ "$1" != "api" ] || [ "$2" != "graphql" ]; then exit 1; fi
query=''
thread_id=''
body=''
for a in "$@"; do
  case "$a" in
    query=*) query=${a#query=} ;;
    threadId=*) thread_id=${a#threadId=} ;;
    body=*) body=${a#body=} ;;
  esac
done
fails_for() { [ -f "$1" ] && grep -qxF "$thread_id" "$1"; }
case "$query" in
  *addPullRequestReviewThreadReply*)
    # 改行は行指向のログに載せるため RS(0x1e) へ置換して記録する
    printf 'reply\t%s\t%s\n' "$thread_id" "$(printf '%s' "$body" | tr '\n' '\036')" >> "$GH_STUB_CALL_LOG"
    if fails_for "$GH_STUB_DIR/reply-fail"; then
      printf 'HTTP 403: reply forbidden\n' >&2
      exit 1
    fi
    printf '{"data":{"addPullRequestReviewThreadReply":{"comment":{"url":"https://example.com/reply-%s"}}}}\n' "$thread_id"
    ;;
  *resolveReviewThread*)
    printf 'resolve\t%s\n' "$thread_id" >> "$GH_STUB_CALL_LOG"
    if fails_for "$GH_STUB_DIR/resolve-fail"; then
      printf 'HTTP 403: Resource not accessible by integration\n' >&2
      exit 1
    fi
    printf '{"data":{"resolveReviewThread":{"thread":{"isResolved":true}}}}\n'
    ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_DIR="$TMP"
export GH_STUB_CALL_LOG="$TMP/calls.log"

# --- フィクスチャ ---
# PRRT_A / PRRT_B: 対象適格（awaiting_my_confirmation true）
# PRRT_X: 未解決だが起点が他人 / PRRT_Y: 解決済み → どちらも不適格
cat > "$TMP/context.json" <<'EOF'
{
  "repo": "owner/repo",
  "current_user": "testuser",
  "is_own_pr": false,
  "pr": {"number": 5, "head_oid": "abc123"},
  "review_threads": [
    {"id": "PRRT_A", "is_resolved": false, "path": "a.go", "line": 1, "awaiting_my_confirmation": true},
    {"id": "PRRT_B", "is_resolved": false, "path": "b.go", "line": 2, "awaiting_my_confirmation": true},
    {"id": "PRRT_X", "is_resolved": false, "path": "x.go", "line": 3, "awaiting_my_confirmation": false},
    {"id": "PRRT_Y", "is_resolved": true, "path": "y.go", "line": 4, "awaiting_my_confirmation": false}
  ]
}
EOF

pass=0
fail=0

assert() {
  local name=$1 json=$2 expr=$3
  if printf '%s' "$json" | jq -e "$expr" >/dev/null 2>&1; then
    pass=$((pass + 1)); printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1)); printf 'FAIL  %s\n' "$name"
  fi
}

assert_exit() {
  local name=$1 got=$2 want=$3
  if [ "$got" -eq "$want" ]; then
    pass=$((pass + 1)); printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1)); printf 'FAIL  %s (got exit %d, want %d)\n' "$name" "$got" "$want"
  fi
}

# 条件をそのままコマンドとして渡す（`[ ... ]` / grep 等）
assert_ok() {
  local name=$1
  shift
  if "$@"; then
    pass=$((pass + 1)); printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1)); printf 'FAIL  %s\n' "$name"
  fi
}

assert_fails() {
  local name=$1
  shift
  if "$@"; then
    fail=$((fail + 1)); printf 'FAIL  %s\n' "$name"
  else
    pass=$((pass + 1)); printf 'PASS  %s\n' "$name"
  fi
}

run() {
  : > "$GH_STUB_CALL_LOG"
  bash "$SCRIPT" "$TMP/context.json" "$1" 2>"$TMP/err.txt"
}
calls() { cat "$GH_STUB_CALL_LOG"; }
inject_fail() { printf '%s\n' "$2" > "$TMP/$1-fail"; }
clear_fail() { rm -f "$TMP/reply-fail" "$TMP/resolve-fail"; }
no_mutation() { [ ! -s "$GH_STUB_CALL_LOG" ]; }

write_input() { printf '%s' "$2" > "$TMP/$1.json"; printf '%s' "$TMP/$1.json"; }

# --- 正常系: 返信 + resolve ---
in_ok=$(write_input ok '{"threads": [{"id": "PRRT_A", "body": "対応を確認しました", "resolve": true}]}')
out=$(run "$in_ok")
assert_exit 'reply+resolve: exit 0' $? 0
assert 'reply+resolve: replied/resolved reported' "$out" \
  '.replied == [{"id": "PRRT_A", "url": "https://example.com/reply-PRRT_A"}]
   and .resolved == ["PRRT_A"] and .resolve_failed == [] and .warnings == []'
# 順序が逆だと「解決済みスレッドに返信」になり通知が埋もれる
assert_ok 'reply+resolve: reply precedes resolve for the same thread' \
  [ "$(calls)" = "$(printf 'reply\tPRRT_A\t対応を確認しました\nresolve\tPRRT_A')" ]

# --- resolve: false は返信のみ（resolve mutation を打たない） ---
in_nores=$(write_input noresolve '{"threads": [{"id": "PRRT_A", "body": "まだ解消していません", "resolve": false}]}')
out=$(run "$in_nores")
assert_exit 'reply only: exit 0' $? 0
assert 'reply only: replied but nothing resolved' "$out" \
  '(.replied | length) == 1 and .resolved == []'
assert_fails 'reply only: no resolveReviewThread call' grep -q '^resolve' "$GH_STUB_CALL_LOG"

# --- 複数スレッドを順に処理 ---
in_multi=$(write_input multi '{"threads": [{"id": "PRRT_A", "body": "確認", "resolve": true}, {"id": "PRRT_B", "body": "未解消", "resolve": false}]}')
out=$(run "$in_multi")
assert_exit 'multiple threads: exit 0' $? 0
assert 'multiple threads: both replied, only one resolved' "$out" \
  '[.replied[].id] == ["PRRT_A", "PRRT_B"] and .resolved == ["PRRT_A"]'

# --- 改行を含む本文がそのまま渡ること（返信本文が壊れると議論が読めなくなる） ---
in_ml=$(write_input multiline '{"threads": [{"id": "PRRT_A", "body": "1行目\n\n2行目", "resolve": false}]}')
run "$in_ml" >/dev/null
assert_ok 'multiline body passed through intact' \
  grep -qF "$(printf '1行目\036\0362行目')" "$GH_STUB_CALL_LOG"

# --- 検証違反: 副作用ゼロで停止すること ---
# 不適格スレッド（起点が他人）を resolve すると越権になるため、スクリプト側で構造的に止める
in_inelig=$(write_input inelig '{"threads": [{"id": "PRRT_X", "body": "x", "resolve": true}]}')
run "$in_inelig" >/dev/null
assert_exit 'ineligible thread (opened by someone else): exit 1' $? 1
assert_ok 'ineligible thread: no mutation issued' no_mutation
assert_ok 'ineligible thread: stderr names the id' grep -q 'PRRT_X' "$TMP/err.txt"

in_resolved=$(write_input already '{"threads": [{"id": "PRRT_Y", "body": "y", "resolve": true}]}')
run "$in_resolved" >/dev/null
assert_exit 'already resolved thread: exit 1' $? 1
assert_ok 'already resolved thread: no mutation issued' no_mutation

in_unknown=$(write_input unknown '{"threads": [{"id": "PRRT_ZZZ", "body": "z", "resolve": false}]}')
run "$in_unknown" >/dev/null
assert_exit 'unknown thread id: exit 1' $? 1
assert_ok 'unknown thread id: no mutation issued' no_mutation

# 重複 id は二重返信になるため事前に弾く
in_dup=$(write_input dup '{"threads": [{"id": "PRRT_A", "body": "1", "resolve": false}, {"id": "PRRT_A", "body": "2", "resolve": true}]}')
run "$in_dup" >/dev/null
assert_exit 'duplicate thread id: exit 1' $? 1
assert_ok 'duplicate thread id: no mutation issued' no_mutation

# 適格・不適格の混在でも、適格分だけ先に投稿する部分適用にしない
in_mixed=$(write_input mixed '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}, {"id": "PRRT_X", "body": "x", "resolve": true}]}')
run "$in_mixed" >/dev/null
assert_exit 'mixed eligible/ineligible: exit 1' $? 1
assert_ok 'mixed eligible/ineligible: no partial posting' no_mutation

# --- 入力契約違反 ---
for bad_name in nobody badresolve notarray badid nothreads; do
  case "$bad_name" in
    nobody)     bad='{"threads": [{"id": "PRRT_A", "resolve": true}]}' ;;
    badresolve) bad='{"threads": [{"id": "PRRT_A", "body": "a", "resolve": "yes"}]}' ;;
    notarray)   bad='{"threads": {"id": "PRRT_A"}}' ;;
    badid)      bad='{"threads": [{"id": 5, "body": "a", "resolve": true}]}' ;;
    nothreads)  bad='{}' ;;
  esac
  in_bad=$(write_input "bad-$bad_name" "$bad")
  run "$in_bad" >/dev/null
  assert_exit "malformed input ($bad_name): exit 1" $? 1
  assert_ok "malformed input ($bad_name): no mutation issued" no_mutation
done

# 空本文は「返信したつもりで中身が無い」状態になるため弾く
in_blank=$(write_input blankbody '{"threads": [{"id": "PRRT_A", "body": "   ", "resolve": true}]}')
run "$in_blank" >/dev/null
assert_exit 'blank body rejected' $? 1
assert_ok 'blank body: no mutation issued' no_mutation

# --- 空配列は正常な no-op ---
in_none=$(write_input none '{"threads": []}')
out=$(run "$in_none")
assert_exit 'empty threads: exit 0' $? 0
assert 'empty threads: empty result arrays' "$out" \
  '.replied == [] and .resolved == [] and .resolve_failed == [] and .warnings == []'
assert_ok 'empty threads: no mutation issued' no_mutation

# --- resolve 失敗は縮退（返信は残し、warning で報告して exit 0） ---
# fork PR・write 権限なしで resolveReviewThread だけ失敗するケース。
# ここで exit 1 にするとレビュー運用全体が止まるため、返信済みの事実を出力に残して続行する
inject_fail resolve PRRT_A
out=$(run "$in_multi")
assert_exit 'resolve failure: exit 0 (degraded)' $? 0
assert 'resolve failure: reply kept, resolve reported as failed' "$out" \
  '[.replied[].id] == ["PRRT_A", "PRRT_B"] and .resolved == []
   and [.resolve_failed[].id] == ["PRRT_A"] and (.warnings | length) > 0'
assert 'resolve failure: error text carried in output' "$out" \
  '.resolve_failed[0].error | test("403")'

# resolve 失敗後も後続スレッドの処理を止めない
in_multi2=$(write_input multi2 '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}, {"id": "PRRT_B", "body": "b", "resolve": true}]}')
out=$(run "$in_multi2")
assert_exit 'resolve failure: subsequent threads still processed' $? 0
assert 'resolve failure: later thread resolved normally' "$out" \
  '.resolved == ["PRRT_B"] and [.resolve_failed[].id] == ["PRRT_A"]'
clear_fail

# --- 返信失敗は停止し、投稿済み/未処理を stderr で切り分けられること ---
# （再実行時に二重返信しないため、どこまで進んだかの報告が必須）
inject_fail reply PRRT_A
in_multi3=$(write_input multi3 '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}, {"id": "PRRT_B", "body": "b", "resolve": true}]}')
run "$in_multi3" >/dev/null
assert_exit 'reply failure: exit 1' $? 1
assert_ok 'reply failure: stderr names the failed thread' grep -q 'PRRT_A' "$TMP/err.txt"
assert_ok 'reply failure: stderr names the unprocessed thread' grep -q 'PRRT_B' "$TMP/err.txt"
assert_fails 'reply failure: does not proceed to reply the rest' \
  grep -q "$(printf 'reply\tPRRT_B')" "$GH_STUB_CALL_LOG"

# 途中失敗では投稿済みスレッドを stderr で切り分けられること
inject_fail reply PRRT_B
run "$in_multi3" >/dev/null
assert_exit 'mid-run reply failure: exit 1' $? 1
assert_ok 'mid-run reply failure: already-replied thread reported' \
  grep -q 'PRRT_A' "$TMP/err.txt"
clear_fail

# --- 引数・ファイル不備 ---
bash "$SCRIPT" >/dev/null 2>&1
assert_exit 'missing args: exit 1' $? 1
bash "$SCRIPT" "$TMP/context.json" >/dev/null 2>&1
assert_exit 'missing threads file: exit 1' $? 1
bash "$SCRIPT" "$TMP/no-such.json" "$in_ok" >/dev/null 2>&1
assert_exit 'nonexistent context file: exit 1' $? 1
bash "$SCRIPT" "$TMP/context.json" "$TMP/no-such.json" >/dev/null 2>&1
assert_exit 'nonexistent threads file: exit 1' $? 1

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
