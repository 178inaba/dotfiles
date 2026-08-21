#!/bin/bash

# delete-candidates.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/cleanup-merged/tests/test-delete-candidates.sh
# 使い捨ての git リポジトリで完結し、実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/delete-candidates.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# --- リポジトリフィクスチャ ---
git init -q -b main "$TMP/repo"
cd "$TMP/repo" || exit 1
git config user.email test@example.com
git config user.name test
git commit -q --allow-empty -m init

commit_file() {
  printf '%s\n' "$2" > "$1"
  git add "$1"
  git commit -q -m "add $1"
}

# wt-del: worktree 付き・main にマージ済み → worktree と branch の両方が消える
git worktree add -q "$TMP/wt-del" -b wt-del main
(cd "$TMP/wt-del" && commit_file a.txt a)
git merge -q wt-del

# closed-br: 未マージ（verdict: pr_closed）→ -D でのみ削除できる
git switch -qc closed-br
commit_file b.txt b
git switch -q main

# fake-merged: 未マージなのに verdict: merged_no_pr → -d が拒否して failure、処理は継続
git switch -qc fake-merged
commit_file c.txt c
git switch -q main

# live-br: 別 worktree でチェックアウト中 → -d が拒否して failure（自然ガード）
git worktree add -q "$TMP/wt-live" -b live-br main

# merged-br: マージ済み branch（worktree なし）→ -d で削除
git switch -qc merged-br
commit_file d.txt d
git switch -q main
git merge -q merged-br

# wt-self: cwd をこの worktree にして実行 → 事前検査で failure、他候補は削除継続
git worktree add -q "$TMP/wt-self" -b wt-self main
git merge -q wt-self 2>/dev/null || true

wt_json() {
  jq -nc --arg p "$1" --arg b "$2" --arg v "$3" '{path: $p, branch: $b, verdict: $v, detail: ""}'
}
br_json() {
  jq -nc --arg b "$1" --arg v "$2" '{branch: $b, verdict: $v, detail: ""}'
}

input_main=$(jq -nc \
  --argjson wts "[$(wt_json "$TMP/wt-del" wt-del merged_no_pr)]" \
  --argjson brs "[$(br_json fake-merged merged_no_pr), $(br_json closed-br pr_closed), $(br_json live-br merged_no_pr), $(br_json merged-br merged_no_pr)]" \
  '{candidates: {worktrees: $wts, branches: $brs}}')

out_main=$(printf '%s' "$input_main" | bash "$SCRIPT")
main_exit=$?

pass=0
fail=0

assert() {
  local name=$1 json=$2 expr=$3
  if printf '%s' "$json" | jq -e "$expr" >/dev/null 2>&1; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s\n' "$name"
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

assert_state() {
  local name=$1 cmd=$2 want=$3
  local got=false
  if eval "$cmd" >/dev/null 2>&1; then got=true; fi
  if [ "$got" = "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s\n' "$name"
  fi
}

assert_exit 'exit 0 despite individual failures' "$main_exit" 0

# worktree 削除
assert 'worktree removed and reported' "$out_main" \
  '.removed.worktrees == ["'"$TMP"'/wt-del"]'
assert 'worktree branch removed too' "$out_main" \
  '.removed.branches | index("wt-del")'
assert_state 'wt-del directory gone' "[ -d '$TMP/wt-del' ]" false
assert_state 'wt-del branch gone' "git rev-parse --verify --quiet wt-del" false

# branch 削除の -d/-D 分岐
assert 'pr_closed branch deleted (proves -D)' "$out_main" \
  '.removed.branches | index("closed-br")'
assert_state 'closed-br branch gone' "git rev-parse --verify --quiet closed-br" false
assert 'merged branch deleted with -d' "$out_main" \
  '.removed.branches | index("merged-br")'

# 失敗の記録と継続
assert 'unmerged branch with merged verdict fails via -d' "$out_main" \
  'any(.failures[]; .type == "branch" and .target == "fake-merged")'
assert_state 'fake-merged branch survives' "git rev-parse --verify --quiet fake-merged" true
assert 'checked-out branch fails (natural guard)' "$out_main" \
  'any(.failures[]; .type == "branch" and .target == "live-br" and (.error | length > 0))'
assert 'later candidates processed after failure' "$out_main" \
  '.removed.branches | index("merged-br")'

# カレント worktree の事前検査
input_self=$(jq -nc \
  --argjson wts "[$(wt_json "$TMP/wt-self" wt-self merged_no_pr)]" \
  --argjson brs "[$(br_json merged-br-2 merged_no_pr)]" \
  '{candidates: {worktrees: $wts, branches: $brs}}')
git switch -qc merged-br-2
git switch -q main
git merge -q merged-br-2
out_self=$(cd "$TMP/wt-self" && printf '%s' "$input_self" | bash "$SCRIPT")
self_exit=$?

assert_exit 'exit 0 with current-worktree failure' "$self_exit" 0
assert 'current worktree refused with guidance' "$out_self" \
  'any(.failures[]; .type == "worktree" and .target == "'"$TMP"'/wt-self" and (.error | contains("ExitWorktree")))'
assert_state 'wt-self directory survives' "[ -d '$TMP/wt-self' ]" true
assert 'other candidates still processed' "$out_self" \
  '.removed.branches == ["merged-br-2"]'

# 異常系
printf 'not json' | bash "$SCRIPT" >/dev/null 2>"$TMP/err1"
assert_exit 'invalid JSON rejected' "$?" 1
grep -qi 'json' "$TMP/err1" && pass=$((pass + 1)) && printf 'PASS  invalid JSON error on stderr\n' || {
  fail=$((fail + 1))
  printf 'FAIL  invalid JSON error on stderr\n'
}

printf '{}' | bash "$SCRIPT" >/dev/null 2>"$TMP/err2"
assert_exit 'missing candidates rejected' "$?" 1

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
