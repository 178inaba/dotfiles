#!/bin/bash

# check-pr-freshness.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/worktree-resolution/tests/test-check-pr-freshness.sh
# 使い捨ての bare リポジトリを origin 代わりに使う。入力の pr-context.json は
# fetch-pr-context.sh の出力契約（pr.head_oid / pr.head_ref / pr.base_ref / is_own_pr）を模す。
# 実 gh・実リポジトリには触れない。失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/check-pr-freshness.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

HEAD_REF="feature/7-work"

# --- origin として使う bare リポジトリを組む（main + PR head branch） ---
git init -q --bare -b main "$TMP/origin.git"
git clone -q "$TMP/origin.git" "$TMP/seed" 2>/dev/null
(
  cd "$TMP/seed"
  git config user.email test@example.com
  git config user.name test
  git commit -q --allow-empty -m "initial"
  git push -q origin main
  git switch -qc "$HEAD_REF"
  git commit -q --allow-empty -m "pr work"
  git push -q origin "$HEAD_REF"
  git switch -q main
)

# 他の経路（対象リポジトリからの push 等）で origin が進んでいても壊れないよう origin に揃えてから積む
push_origin_commit() {
  local msg=$1
  (
    cd "$TMP/seed"
    git fetch -q origin "$HEAD_REF"
    git switch -q "$HEAD_REF"
    git reset -q --hard "origin/$HEAD_REF"
    git commit -q --allow-empty -m "$msg"
    git push -q origin "$HEAD_REF"
    git switch -q main
  )
}

# 対象リポジトリ（PR head branch を checkout 済み）を作る。stdout にパスを返す
setup_repo() {
  local name=$1
  git clone -q "$TMP/origin.git" "$TMP/$name" 2>/dev/null
  git -C "$TMP/$name" config user.email test@example.com
  git -C "$TMP/$name" config user.name test
  git -C "$TMP/$name" switch -q "$HEAD_REF"
  printf '%s' "$TMP/$name"
}

# pr-context.json を組む: write_context <path> <head_oid> <is_own_pr> [head_ref]
write_context() {
  local path=$1 head_oid=$2 is_own=$3 head_ref=${4:-$HEAD_REF}
  jq -n --arg oid "$head_oid" --arg ref "$head_ref" --argjson own "$is_own" \
    '{pr: {head_oid: $oid, head_ref: $ref, base_ref: "main"}, is_own_pr: $own}' > "$path"
}

origin_head() {
  git -C "$TMP/seed" rev-parse "$HEAD_REF"
}

pass=0
fail=0

assert() {
  local name=$1 cond=$2 detail=${3:-}
  if eval "$cond"; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s %s\n' "$name" "$detail"
  fi
}

assert_json() {
  local name=$1 json=$2 expr=$3
  if printf '%s' "$json" | jq -e "$expr" >/dev/null 2>&1; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (json: %s)\n' "$name" "$json"
  fi
}

assert_exit() {
  local name=$1 got=$2 want=$3
  if [ "$got" -eq "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got exit %d, want %d)\n' "$name" "$got" "$want"
  fi
}

# --- ケース1: HEAD == head_oid → ok ---
repo=$(setup_repo case1)
write_context "$TMP/ctx1.json" "$(origin_head)" false
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx1.json")
assert_exit 'in-sync: exit 0' $? 0
assert_json 'in-sync: status ok' "$out" '.status == "ok"'

# --- ケース2: 自分の PR でローカルが ahead（未 push commit）→ ahead_own で続行可 ---
repo=$(setup_repo case2)
write_context "$TMP/ctx2.json" "$(origin_head)" true
git -C "$repo" commit -q --allow-empty -m "unpushed local work"
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx2.json")
assert_exit 'own-pr ahead: exit 0' $? 0
assert_json 'own-pr ahead: status ahead_own' "$out" '.status == "ahead_own"'

# --- ケース3: 他人の PR でローカルが ahead → diverged で停止 ---
repo=$(setup_repo case3)
write_context "$TMP/ctx3.json" "$(origin_head)" false
git -C "$repo" commit -q --allow-empty -m "local commit on someone else pr"
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx3.json")
assert_exit 'other-pr ahead: exit 0' $? 0
assert_json 'other-pr ahead: status diverged' "$out" '.status == "diverged"'

# --- ケース4: behind・clean → ff 同期して synced ---
repo=$(setup_repo case4)
push_origin_commit "case4 remote advance"
write_context "$TMP/ctx4.json" "$(origin_head)" false
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx4.json")
assert_exit 'behind-clean: exit 0' $? 0
assert_json 'behind-clean: status synced' "$out" '.status == "synced"'
local_head=$(git -C "$repo" rev-parse HEAD)
assert 'behind-clean: fast-forwarded to head_oid' "[ '$local_head' = '$(origin_head)' ]"

# --- ケース5: behind・dirty → behind_dirty で停止（作業は破棄されない） ---
repo=$(setup_repo case5)
echo tracked > "$repo/file.txt"
git -C "$repo" add file.txt
git -C "$repo" commit -qm "add file"
git -C "$repo" push -q origin "$HEAD_REF"
push_origin_commit "case5 remote advance"
echo dirty >> "$repo/file.txt"
before=$(git -C "$repo" rev-parse HEAD)
write_context "$TMP/ctx5.json" "$(origin_head)" false
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx5.json")
assert_exit 'behind-dirty: exit 0' $? 0
assert_json 'behind-dirty: status behind_dirty' "$out" '.status == "behind_dirty"'
after=$(git -C "$repo" rev-parse HEAD)
assert 'behind-dirty: HEAD untouched' "[ '$before' = '$after' ]"
assert 'behind-dirty: dirty change intact' "grep -q dirty '$repo/file.txt'"

# --- ケース6: untracked のみの変更は dirty 扱いしない → ff 同期 ---
repo=$(setup_repo case6)
push_origin_commit "case6 remote advance"
echo untracked > "$repo/untracked.txt"
write_context "$TMP/ctx6.json" "$(origin_head)" false
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx6.json")
assert_exit 'behind untracked-only: exit 0' $? 0
assert_json 'behind untracked-only: status synced' "$out" '.status == "synced"'

# --- ケース7: diverged（双方に独自 commit）→ diverged で停止 ---
repo=$(setup_repo case7)
git -C "$repo" commit -q --allow-empty -m "local side"
push_origin_commit "case7 remote side"
before=$(git -C "$repo" rev-parse HEAD)
write_context "$TMP/ctx7.json" "$(origin_head)" false
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx7.json")
assert_exit 'diverged: exit 0' $? 0
assert_json 'diverged: status diverged' "$out" '.status == "diverged"'
after=$(git -C "$repo" rev-parse HEAD)
assert 'diverged: HEAD untouched' "[ '$before' = '$after' ]"

# --- ケース8: カレント branch が head_ref と不一致 → branch_mismatch で停止 ---
repo=$(setup_repo case8)
git -C "$repo" switch -q main
write_context "$TMP/ctx8.json" "$(origin_head)" false
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx8.json")
assert_exit 'branch mismatch: exit 0' $? 0
assert_json 'branch mismatch: status branch_mismatch' "$out" '.status == "branch_mismatch"'
main_head_before=$(git -C "$repo" rev-parse HEAD)
assert 'branch mismatch: main not fast-forwarded' "[ '$main_head_before' = '$(git -C "$repo" rev-parse main)' ]"

# --- ケース9: detached HEAD → branch_mismatch で停止 ---
repo=$(setup_repo case9)
git -C "$repo" switch -q --detach HEAD
write_context "$TMP/ctx9.json" "$(origin_head)" false
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx9.json")
assert_exit 'detached head: exit 0' $? 0
assert_json 'detached head: status branch_mismatch' "$out" '.status == "branch_mismatch"'

# --- ケース10: fetch 失敗（head branch が origin に無い = fork PR 相当）→ fetch_failed ---
repo=$(setup_repo case10)
write_context "$TMP/ctx10.json" "$(origin_head)" false "fork-user/branch-not-on-origin"
out=$(cd "$repo" && bash "$SCRIPT" "$TMP/ctx10.json" 2>/dev/null)
assert_exit 'fetch failure: exit 0' $? 0
assert_json 'fetch failure: status fetch_failed' "$out" '.status == "fetch_failed"'

# --- ケース11: 入力不正 → 非ゼロ exit + stderr ---
repo=$(setup_repo case11)
(cd "$repo" && bash "$SCRIPT" 2>"$TMP/err11a.txt")
assert_exit 'missing arg: non-zero exit' $? 1
assert 'missing arg: stderr present' "[ -s '$TMP/err11a.txt' ]"

(cd "$repo" && bash "$SCRIPT" "$TMP/does-not-exist.json" 2>"$TMP/err11b.txt")
assert_exit 'missing file: non-zero exit' $? 1
assert 'missing file: stderr present' "[ -s '$TMP/err11b.txt' ]"

printf '{"pr": {}}' > "$TMP/ctx11.json"
(cd "$repo" && bash "$SCRIPT" "$TMP/ctx11.json" 2>"$TMP/err11c.txt")
assert_exit 'missing fields: non-zero exit' $? 1
assert 'missing fields: stderr present' "[ -s '$TMP/err11c.txt' ]"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
