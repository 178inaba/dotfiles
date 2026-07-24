#!/bin/bash

# post-review.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/deep-review/tests/test-post-review.sh
# 使い捨てリポジトリで実 diff を作り、行番号の投稿前検証を実データで確かめる。
# gh は GH_BIN スタブで差し替え、投稿 payload をファイルに記録して中身を検証する。
# 実 gh・実 GitHub には触れない。失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/post-review.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(cd "$(mktemp -d)" && pwd -P)
trap 'rm -rf "$TMP"' EXIT

# --- 対象リポジトリ: main（10行のファイル）→ feature で5行目を変更 + 新規ファイル追加 ---
# added.txt の "++ tricky" は diff 上 "+++ tricky" と描画される追加行。ファイルヘッダと
# 衝突しても行番号追跡が壊れないこと（後続行の検証）のリグレッション用フィクスチャ
git init -q --bare -b main "$TMP/origin.git"
git clone -q "$TMP/origin.git" "$TMP/repo" 2>/dev/null
REPO="$TMP/repo"
(
  cd "$REPO"
  git config user.email test@example.com
  git config user.name test
  seq 1 10 | sed 's/^/line /' > stable.txt
  git add stable.txt
  git commit -qm "initial"
  git push -q origin main
  git switch -qc feature/5-change
  { seq 1 4 | sed 's/^/line /'; printf 'line 5 changed\n'; seq 6 10 | sed 's/^/line /'; } > stable.txt
  printf 'new a\n++ tricky\nnew b\n' > added.txt
  git add stable.txt added.txt
  git commit -qm "change line 5 and add file"
)
HEAD_OID=$(git -C "$REPO" rev-parse HEAD)

# --- gh スタブ: 投稿 payload を記録して html_url を返す ---
mkdir -p "$TMP/stub"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
printf '%s\n' "$*" >> "$GH_STUB_LOG"
[ "${GH_STUB_FAIL:-}" != "1" ] || { printf 'stub api failure\n' >&2; exit 1; }
cat > "$GH_STUB_PAYLOAD"
printf '{"html_url": "https://github.com/acme/foo/pull/9#pullrequestreview-1"}\n'
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_LOG="$TMP/gh-log"
export GH_STUB_PAYLOAD="$TMP/payload.json"

# --- 入力ファイルの組み立て ---
write_context() {
  local path=$1 head_oid=${2:-$HEAD_OID}
  jq -n --arg oid "$head_oid" \
    '{repo: "acme/foo", pr: {number: 9, head_ref: "feature/5-change", base_ref: "main", head_oid: $oid}}' > "$path"
}

write_review() {
  local path=$1 assessment=$2 comments=$3
  jq -n --arg a "$assessment" --argjson c "$comments" \
    '{assessment: $a, body: "## レビュー結果\n\n本文", comments: $c}' > "$path"
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

payload() {
  cat "$GH_STUB_PAYLOAD"
}

write_context "$TMP/ctx.json"

# --- ケース1: Approve可能 + 行コメント（変更行・context 行・"+++ " 描画行の直後）→ 投稿成功 ---
: > "$GH_STUB_LOG"
write_review "$TMP/rev1.json" "Approve可能" \
  '[{"path": "stable.txt", "line": 5, "body": "変更行への指摘"},
    {"path": "stable.txt", "line": 3, "body": "context 行への指摘"},
    {"path": "added.txt", "line": 3, "body": "++ 行の直後への指摘（ヘッダ誤認リグレッション検知）"}]'
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/ctx.json" "$TMP/rev1.json")
assert_exit 'approve: exit 0' $? 0
assert_json 'approve: url returned' "$out" '.url | test("pullrequestreview")'
assert_json 'approve: event APPROVE' "$(payload)" '.event == "APPROVE"'
assert_json 'approve: commit_id is head_oid' "$(payload)" ".commit_id == \"$HEAD_OID\""
assert_json 'approve: body passed through' "$(payload)" '.body | test("レビュー結果")'
assert_json 'approve: 3 comments passed' "$(payload)" '.comments | length == 3'
assert 'approve: posted to pulls/9/reviews' "grep -q 'repos/acme/foo/pulls/9/reviews' '$GH_STUB_LOG'"

# --- ケース2: 修正が必要 → REQUEST_CHANGES ---
write_review "$TMP/rev2.json" "修正が必要" '[]'
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/ctx.json" "$TMP/rev2.json")
assert_exit 'request changes: exit 0' $? 0
assert_json 'request changes: event' "$(payload)" '.event == "REQUEST_CHANGES"'
assert_json 'request changes: empty comments array' "$(payload)" '.comments == []'

# --- ケース3: 要議論 → COMMENT ---
write_review "$TMP/rev3.json" "要議論" '[]'
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/ctx.json" "$TMP/rev3.json")
assert_exit 'discussion: exit 0' $? 0
assert_json 'discussion: event COMMENT' "$(payload)" '.event == "COMMENT"'

# --- ケース4: diff 外の行番号 → 投稿せず非ゼロ exit + stderr に違反エントリ ---
: > "$GH_STUB_LOG"
write_review "$TMP/rev4.json" "Approve可能" \
  '[{"path": "stable.txt", "line": 10, "body": "hunk 外の行"}]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/ctx.json" "$TMP/rev4.json" 2>"$TMP/err4.txt")
assert_exit 'line outside diff: non-zero exit' $? 1
assert 'line outside diff: not posted' "! grep -q 'pulls/9/reviews' '$GH_STUB_LOG'"
assert 'line outside diff: offending entry in stderr' "grep -q 'stable.txt:10' '$TMP/err4.txt'"

# --- ケース5: diff に無いファイルへのコメント → 投稿せず非ゼロ exit ---
: > "$GH_STUB_LOG"
write_review "$TMP/rev5.json" "Approve可能" \
  '[{"path": "not-in-diff.txt", "line": 1, "body": "diff 外ファイル"}]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/ctx.json" "$TMP/rev5.json" 2>"$TMP/err5.txt")
assert_exit 'file outside diff: non-zero exit' $? 1
assert 'file outside diff: not posted' "! grep -q 'pulls/9/reviews' '$GH_STUB_LOG'"

# --- ケース6: HEAD != head_oid（stale）→ 投稿せず非ゼロ exit ---
: > "$GH_STUB_LOG"
write_context "$TMP/ctx-stale.json" "0000000000000000000000000000000000000000"
write_review "$TMP/rev6.json" "Approve可能" '[]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/ctx-stale.json" "$TMP/rev6.json" 2>"$TMP/err6.txt")
assert_exit 'stale head: non-zero exit' $? 1
assert 'stale head: not posted' "! grep -q 'pulls/9/reviews' '$GH_STUB_LOG'"
assert 'stale head: stderr present' "[ -s '$TMP/err6.txt' ]"

# --- ケース7: 不正な assessment → 非ゼロ exit ---
write_review "$TMP/rev7.json" "たぶん大丈夫" '[]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/ctx.json" "$TMP/rev7.json" 2>"$TMP/err7.txt")
assert_exit 'invalid assessment: non-zero exit' $? 1
assert 'invalid assessment: stderr present' "[ -s '$TMP/err7.txt' ]"

# --- ケース8: 行コメントのフィールド欠落 → 非ゼロ exit ---
printf '{"assessment": "Approve可能", "body": "b", "comments": [{"path": "stable.txt"}]}' > "$TMP/rev8.json"
(cd "$REPO" && bash "$SCRIPT" "$TMP/ctx.json" "$TMP/rev8.json" 2>"$TMP/err8.txt")
assert_exit 'malformed comment: non-zero exit' $? 1

# --- ケース9: gh api 失敗 → 非ゼロ exit + stderr ---
write_review "$TMP/rev9.json" "Approve可能" '[]'
(cd "$REPO" && GH_STUB_FAIL=1 bash "$SCRIPT" "$TMP/ctx.json" "$TMP/rev9.json" 2>"$TMP/err9.txt")
assert_exit 'api failure: non-zero exit' $? 1
assert 'api failure: stderr present' "[ -s '$TMP/err9.txt' ]"

# --- ケース10: 引数不足 → 非ゼロ exit ---
bash "$SCRIPT" 2>"$TMP/err10a.txt"
assert_exit 'no args: non-zero exit' $? 1
bash "$SCRIPT" "$TMP/ctx.json" 2>"$TMP/err10b.txt"
assert_exit 'missing review json: non-zero exit' $? 1

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
