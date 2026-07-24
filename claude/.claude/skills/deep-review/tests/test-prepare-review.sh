#!/bin/bash

# prepare-review.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/deep-review/tests/test-prepare-review.sh
# gh・fetch-pr-context.sh・check-pr-freshness.sh はすべて env でスタブ化し、
# prepare-review.sh 自身のロジック（フラグ解析・PR 存在プローブ・truncation 再実行・
# モード決定表・ベースブランチ判定・出力集約）だけを検証する。
# 実 gh・実リポジトリには触れない。失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/prepare-review.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(cd "$(mktemp -d)" && pwd -P)
trap 'rm -rf "$TMP"' EXIT

# --- 対象リポジトリ（ベースブランチ判定・branch 一致確認に使う） ---
git init -q --bare -b main "$TMP/origin.git"
git clone -q "$TMP/origin.git" "$TMP/repo" 2>/dev/null
(
  cd "$TMP/repo"
  git config user.email test@example.com
  git config user.name test
  git commit -q --allow-empty -m "initial"
  git push -q origin main
  git switch -qc feature/9-work
)
REPO="$TMP/repo"

mkdir -p "$TMP/scratch"

# --- gh スタブ（PR 存在プローブ: pr view --json number,headRefName） ---
# GH_STUB_PR_JSON が空なら失敗（PR なし / 指定 PR 不在）
mkdir -p "$TMP/stub"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
printf '%s\n' "$*" >> "$GH_STUB_LOG"
case "$*" in
  *"number,headRefName"*)
    [ -n "${GH_STUB_PR_JSON:-}" ] || exit 1
    printf '%s\n' "$GH_STUB_PR_JSON"
    ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$TMP/stub/gh"

# --- fetch-pr-context.sh スタブ ---
# FETCH_STUB_CONTEXT の JSON を out-dir に書き、{"path": ...} を返す。
# MAX_COMMENTS 付き再実行の検証のため、呼び出し時の MAX_COMMENTS を記録し、
# MAX_COMMENTS が設定されていれば FETCH_STUB_CONTEXT_FULL の方を書く
cat > "$TMP/stub/fetch-pr-context.sh" <<'EOF'
#!/bin/bash
printf 'MAX_COMMENTS=%s\n' "${MAX_COMMENTS:-}" >> "$FETCH_STUB_LOG"
[ "${FETCH_STUB_FAIL:-}" != "1" ] || { printf 'stub fetch failure\n' >&2; exit 1; }
out_dir=$1
path="$out_dir/pr-context-stub.json"
if [ -n "${MAX_COMMENTS:-}" ] && [ -n "${FETCH_STUB_CONTEXT_FULL:-}" ]; then
  printf '%s' "$FETCH_STUB_CONTEXT_FULL" > "$path"
else
  printf '%s' "$FETCH_STUB_CONTEXT" > "$path"
fi
printf '{"path": "%s"}\n' "$path"
EOF
chmod +x "$TMP/stub/fetch-pr-context.sh"

# --- check-pr-freshness.sh スタブ ---
cat > "$TMP/stub/check-pr-freshness.sh" <<'EOF'
#!/bin/bash
printf 'ctx=%s\n' "$1" >> "$FRESHNESS_STUB_LOG"
json=${FRESHNESS_STUB_JSON:-}
[ -n "$json" ] || json='{"status": "ok", "warnings": []}'
printf '%s\n' "$json"
EOF
chmod +x "$TMP/stub/check-pr-freshness.sh"

export GH_BIN="$TMP/stub/gh"
export GH_STUB_LOG="$TMP/gh-log"
export FETCH_PR_CONTEXT_BIN="$TMP/stub/fetch-pr-context.sh"
export FETCH_STUB_LOG="$TMP/fetch-log"
export CHECK_PR_FRESHNESS_BIN="$TMP/stub/check-pr-freshness.sh"
export FRESHNESS_STUB_LOG="$TMP/freshness-log"

# 標準の PR コンテキスト（他人の PR・truncation なし）
context_json() {
  local own=$1
  jq -n --argjson own "$own" '{
    repo: "acme/foo",
    is_own_pr: $own,
    pr: {number: 9, head_ref: "feature/9-work", base_ref: "main", head_oid: "abc123"},
    linked_issues: [{repo: null, number: 77}],
    comments_total_count: 2,
    comments_truncated: false
  }'
}
export FETCH_STUB_CONTEXT=$(context_json false)
export GH_STUB_PR_JSON='{"number": 9, "headRefName": "feature/9-work"}'

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

# --- ケース1: 他人の PR（番号明示・カレント branch 一致）→ コメントON・個人ルールOFF・自動対応OFF ---
: > "$FETCH_STUB_LOG"
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/scratch" 9)
assert_exit 'other pr: exit 0' $? 0
assert_json 'other pr: status ok' "$out" '.status == "ok"'
assert_json 'other pr: pr_exists' "$out" '.pr_exists == true'
assert_json 'other pr: flags.pr_number' "$out" '.flags.pr_number == 9'
assert_json 'other pr: comment mode ON' "$out" '.modes.comment == true'
assert_json 'other pr: personal rules OFF' "$out" '.modes.personal_rules == false'
assert_json 'other pr: autofix OFF' "$out" '.modes.autofix == false'
assert_json 'other pr: base_branch from pr' "$out" '.base_branch == "origin/main"'
assert_json 'other pr: context_path set' "$out" '.context_path | type == "string"'
assert_json 'other pr: linked issues surfaced' "$out" '.issues == [{repo: null, number: 77}]'
assert_json 'other pr: freshness ok' "$out" '.freshness.status == "ok"'
assert_json 'other pr: not degraded' "$out" '.degraded == false'
assert 'other pr: freshness received context path' "grep -q 'pr-context-stub.json' '$FRESHNESS_STUB_LOG'"

# --- ケース2: 自分の PR → コメントOFF・個人ルールON・自動対応ON ---
out=$(cd "$REPO" && FETCH_STUB_CONTEXT="$(context_json true)" bash "$SCRIPT" "$TMP/scratch" 9)
assert_exit 'own pr: exit 0' $? 0
assert_json 'own pr: comment OFF' "$out" '.modes.comment == false'
assert_json 'own pr: personal rules ON' "$out" '.modes.personal_rules == true'
assert_json 'own pr: autofix ON' "$out" '.modes.autofix == true'

# --- ケース3: 自分の PR + --no-autofix → 自動対応のみ強制OFF ---
out=$(cd "$REPO" && FETCH_STUB_CONTEXT="$(context_json true)" bash "$SCRIPT" "$TMP/scratch" 9 --no-autofix)
assert_exit 'own pr no-autofix: exit 0' $? 0
assert_json 'own pr no-autofix: autofix OFF' "$out" '.modes.autofix == false'
assert_json 'own pr no-autofix: personal rules unaffected' "$out" '.modes.personal_rules == true'

# --- ケース4: 他人の PR + --local-only → コメントのみ強制OFF ---
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/scratch" 9 --local-only)
assert_exit 'other pr local-only: exit 0' $? 0
assert_json 'other pr local-only: comment OFF' "$out" '.modes.comment == false'
assert_json 'other pr local-only: autofix unaffected (OFF)' "$out" '.modes.autofix == false'
assert_json 'other pr local-only: freshness still checked' "$out" '.freshness.status == "ok"'

# --- ケース5: --issue 明示 → linked_issues ではなく指定 Issue を使う ---
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/scratch" 9 --issue 123)
assert_exit 'explicit issue: exit 0' $? 0
assert_json 'explicit issue: issues from flag' "$out" '.issues == [{repo: null, number: 123}]'
assert_json 'explicit issue: flag recorded' "$out" '.flags.issue == 123'

# --- ケース6: PR なし（番号省略・推論失敗）→ 縮退（ローカルレビュー） ---
out=$(cd "$REPO" && GH_STUB_PR_JSON= bash "$SCRIPT" "$TMP/scratch")
assert_exit 'no pr: exit 0' $? 0
assert_json 'no pr: degraded' "$out" '.degraded == true'
assert_json 'no pr: pr_exists false' "$out" '.pr_exists == false'
assert_json 'no pr: context null' "$out" '.context_path == null'
assert_json 'no pr: freshness null' "$out" '.freshness == null'
assert_json 'no pr: comment OFF' "$out" '.modes.comment == false'
assert_json 'no pr: personal rules ON' "$out" '.modes.personal_rules == true'
assert_json 'no pr: autofix ON' "$out" '.modes.autofix == true'
assert_json 'no pr: base_branch from default branch' "$out" '.base_branch == "origin/main"'
assert_json 'no pr: issues empty' "$out" '.issues == []'

# --- ケース7: PR 番号明示だが PR が見つからない → 非ゼロ exit（縮退と混同しない） ---
(cd "$REPO" && GH_STUB_PR_JSON= bash "$SCRIPT" "$TMP/scratch" 9 2>"$TMP/err7.txt")
assert_exit 'explicit pr not found: non-zero exit' $? 1
assert 'explicit pr not found: stderr present' "[ -s '$TMP/err7.txt' ]"

# --- ケース8: PR ありで fetch-pr-context 失敗 → 縮退せず非ゼロ exit ---
(cd "$REPO" && FETCH_STUB_FAIL=1 bash "$SCRIPT" "$TMP/scratch" 9 2>"$TMP/err8.txt")
assert_exit 'context fetch failure: non-zero exit' $? 1
assert 'context fetch failure: stderr present' "[ -s '$TMP/err8.txt' ]"

# --- ケース9: comments_truncated → MAX_COMMENTS を総数まで引き上げて自動再実行 ---
truncated=$(context_json false | jq '.comments_total_count = 700 | .comments_truncated = true')
full=$(context_json false | jq '.comments_total_count = 700')
: > "$FETCH_STUB_LOG"
out=$(cd "$REPO" && FETCH_STUB_CONTEXT="$truncated" FETCH_STUB_CONTEXT_FULL="$full" bash "$SCRIPT" "$TMP/scratch" 9)
assert_exit 'truncation rerun: exit 0' $? 0
assert 'truncation rerun: first call without MAX_COMMENTS' "grep -q '^MAX_COMMENTS=$' '$FETCH_STUB_LOG'"
assert 'truncation rerun: second call raised MAX_COMMENTS' "grep -q '^MAX_COMMENTS=700$' '$FETCH_STUB_LOG'"
assert_json 'truncation rerun: status ok' "$out" '.status == "ok"'

# --- ケース10: 再実行しても truncated のまま → warnings で通知して続行 ---
: > "$FETCH_STUB_LOG"
out=$(cd "$REPO" && FETCH_STUB_CONTEXT="$truncated" FETCH_STUB_CONTEXT_FULL="$truncated" bash "$SCRIPT" "$TMP/scratch" 9)
assert_exit 'still truncated: exit 0' $? 0
assert_json 'still truncated: warning recorded' "$out" '.warnings | length >= 1'

# --- ケース11: <pr-number> 指定・--worktree なし・branch 不一致 → branch_mismatch で停止 ---
(cd "$REPO" && git switch -q main)
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/scratch" 9)
assert_exit 'branch mismatch: exit 0' $? 0
assert_json 'branch mismatch: status branch_mismatch' "$out" '.status == "branch_mismatch"'
assert_json 'branch mismatch: head_ref provided for guidance' "$out" '.head_ref == "feature/9-work"'

# --- ケース12: 同条件で --worktree あり → branch 一致確認はスキップ（worktree 解決済み前提） ---
# 注: --worktree 時は worktree 解決後に prepare が呼ばれる想定のため、ここでは
# フラグ解釈（1.6 スキップ）だけを検証する。branch は main のままでも通る
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/scratch" 9 --worktree 2>/dev/null)
rc=$?
if [ "$rc" -eq 0 ]; then
  assert_json 'worktree flag: 1.6 check skipped' "$out" '.flags.worktree == true'
else
  # freshness スタブは ok を返すので、--worktree で branch チェックが残っていれば失敗する
  assert 'worktree flag: 1.6 check skipped' "false" "(exit $rc)"
fi
(cd "$REPO" && git switch -q feature/9-work)

# --- ケース13: 未定義フラグ → 非ゼロ exit + stderr に定義済みフラグ一覧 ---
(cd "$REPO" && bash "$SCRIPT" "$TMP/scratch" --unknown-flag 2>"$TMP/err13.txt")
assert_exit 'unknown flag: non-zero exit' $? 1
assert 'unknown flag: stderr lists defined flags' "grep -q -- '--local-only' '$TMP/err13.txt'"

# --- ケース14: 引数不足・不正 → 非ゼロ exit ---
bash "$SCRIPT" 2>"$TMP/err14a.txt"
assert_exit 'missing scratchpad dir: non-zero exit' $? 1

(cd "$REPO" && bash "$SCRIPT" "$TMP/does-not-exist" 9 2>"$TMP/err14b.txt")
assert_exit 'nonexistent scratchpad dir: non-zero exit' $? 1

(cd "$REPO" && bash "$SCRIPT" "$TMP/scratch" 9 12 2>"$TMP/err14c.txt")
assert_exit 'two positional numbers: non-zero exit' $? 1

# --- ケース15: --issue の値欠落 → 非ゼロ exit ---
(cd "$REPO" && bash "$SCRIPT" "$TMP/scratch" 9 --issue 2>"$TMP/err15.txt")
assert_exit 'issue flag without value: non-zero exit' $? 1

# --- ケース16: freshness の停止 status がそのまま top-level status になる ---
out=$(cd "$REPO" && FRESHNESS_STUB_JSON='{"status": "diverged", "warnings": []}' bash "$SCRIPT" "$TMP/scratch" 9)
assert_exit 'freshness diverged: exit 0' $? 0
assert_json 'freshness diverged: top-level status' "$out" '.status == "diverged"'
assert_json 'freshness diverged: freshness detail kept' "$out" '.freshness.status == "diverged"'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
