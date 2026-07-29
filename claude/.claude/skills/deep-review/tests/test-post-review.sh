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

write_context "$TMP/pr-context-acme@foo-9.json"

# 入力ファイルの置き場所は context と対になる作業ディレクトリ（prepare-review.sh の work_dir）。
# フィクスチャを scratchpad 直下に置くと検証の対象そのものがテストされない
WORK="$TMP/deep-review-acme@foo-9"
mkdir -p "$WORK"

# --- ケース1: Approve可能 + 行コメント（変更行・context 行・"+++ " 描画行の直後）→ 投稿成功 ---
: > "$GH_STUB_LOG"
write_review "$WORK/rev1.json" "Approve可能" \
  '[{"path": "stable.txt", "line": 5, "body": "変更行への指摘"},
    {"path": "stable.txt", "line": 3, "body": "context 行への指摘"},
    {"path": "added.txt", "line": 3, "body": "++ 行の直後への指摘（ヘッダ誤認リグレッション検知）"}]'
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev1.json")
assert_exit 'approve: exit 0' $? 0
assert_json 'approve: url returned' "$out" '.url | test("pullrequestreview")'
assert_json 'approve: event APPROVE' "$(payload)" '.event == "APPROVE"'
assert_json 'approve: commit_id is head_oid' "$(payload)" ".commit_id == \"$HEAD_OID\""
assert_json 'approve: body passed through' "$(payload)" '.body | test("レビュー結果")'
assert_json 'approve: 3 comments passed' "$(payload)" '.comments | length == 3'
assert 'approve: posted to pulls/9/reviews' "grep -q 'repos/acme/foo/pulls/9/reviews' '$GH_STUB_LOG'"

# --- ケース2: 修正が必要 → REQUEST_CHANGES ---
write_review "$WORK/rev2.json" "修正が必要" '[]'
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev2.json")
assert_exit 'request changes: exit 0' $? 0
assert_json 'request changes: event' "$(payload)" '.event == "REQUEST_CHANGES"'
assert_json 'request changes: empty comments array' "$(payload)" '.comments == []'

# --- ケース3: 要議論 → COMMENT ---
write_review "$WORK/rev3.json" "要議論" '[]'
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev3.json")
assert_exit 'discussion: exit 0' $? 0
assert_json 'discussion: event COMMENT' "$(payload)" '.event == "COMMENT"'

# --- ケース4: diff 外の行番号 → 投稿せず非ゼロ exit + stderr に違反エントリ ---
: > "$GH_STUB_LOG"
write_review "$WORK/rev4.json" "Approve可能" \
  '[{"path": "stable.txt", "line": 10, "body": "hunk 外の行"}]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev4.json" 2>"$TMP/err4.txt")
assert_exit 'line outside diff: non-zero exit' $? 1
assert 'line outside diff: not posted' "! grep -q 'pulls/9/reviews' '$GH_STUB_LOG'"
assert 'line outside diff: offending entry in stderr' "grep -q 'stable.txt:10' '$TMP/err4.txt'"

# --- ケース5: diff に無いファイルへのコメント → 投稿せず非ゼロ exit ---
: > "$GH_STUB_LOG"
write_review "$WORK/rev5.json" "Approve可能" \
  '[{"path": "not-in-diff.txt", "line": 1, "body": "diff 外ファイル"}]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev5.json" 2>"$TMP/err5.txt")
assert_exit 'file outside diff: non-zero exit' $? 1
assert 'file outside diff: not posted' "! grep -q 'pulls/9/reviews' '$GH_STUB_LOG'"

# --- ケース6: HEAD != head_oid（stale）→ 投稿せず非ゼロ exit ---
: > "$GH_STUB_LOG"
mkdir -p "$TMP/stale"
write_context "$TMP/stale/pr-context-acme@foo-9.json" "0000000000000000000000000000000000000000"
mkdir -p "$TMP/stale/deep-review-acme@foo-9"
write_review "$TMP/stale/deep-review-acme@foo-9/rev6.json" "Approve可能" '[]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/stale/pr-context-acme@foo-9.json" \
  "$TMP/stale/deep-review-acme@foo-9/rev6.json" 2>"$TMP/err6.txt")
assert_exit 'stale head: non-zero exit' $? 1
assert 'stale head: not posted' "! grep -q 'pulls/9/reviews' '$GH_STUB_LOG'"
assert 'stale head: stderr present' "[ -s '$TMP/err6.txt' ]"

# --- ケース7: 不正な assessment → 非ゼロ exit ---
write_review "$WORK/rev7.json" "たぶん大丈夫" '[]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev7.json" 2>"$TMP/err7.txt")
assert_exit 'invalid assessment: non-zero exit' $? 1
assert 'invalid assessment: stderr present' "[ -s '$TMP/err7.txt' ]"

# --- ケース8: 行コメントのフィールド欠落 → 非ゼロ exit ---
printf '{"assessment": "Approve可能", "body": "b", "comments": [{"path": "stable.txt"}]}' > "$WORK/rev8.json"
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev8.json" 2>"$TMP/err8.txt")
assert_exit 'malformed comment: non-zero exit' $? 1

# --- ケース9: gh api 失敗 → 非ゼロ exit + stderr ---
write_review "$WORK/rev9.json" "Approve可能" '[]'
(cd "$REPO" && GH_STUB_FAIL=1 bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev9.json" 2>"$TMP/err9.txt")
assert_exit 'api failure: non-zero exit' $? 1
assert 'api failure: stderr present' "[ -s '$TMP/err9.txt' ]"

# --- ケース10: 引数不足 → 非ゼロ exit ---
bash "$SCRIPT" 2>"$TMP/err10a.txt"
assert_exit 'no args: non-zero exit' $? 1
bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" 2>"$TMP/err10b.txt"
assert_exit 'missing review json: non-zero exit' $? 1

# --- ケース11: レビューファイルの作業ディレクトリ束縛 ---
# 並列サブエージェントは同一セッションの scratchpad を共有するため、共有直下の固定名ファイルは
# 別 PR のレビュー内容に上書きされる。comments[] が空のレビューでは path/line 検証が
# 取り違えを検出できず、他 PR の本文をそのまま投稿してしまうため、置き場所の側で構造的に止める
: > "$GH_STUB_LOG"
write_review "$TMP/review.json" "Approve可能" '[]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$TMP/review.json" 2>"$TMP/err11a.txt")
assert_exit 'review file outside the work dir: non-zero exit' $? 1
assert 'review file outside the work dir: nothing posted' "[ ! -s '$GH_STUB_LOG' ]"
assert 'review file outside the work dir: stderr points at review_path' "grep -q 'review_path' '$TMP/err11a.txt'"

# 別 PR の作業ディレクトリに置かれたファイル（取り違え）も拒否する
mkdir -p "$TMP/deep-review-acme@foo-99"
write_review "$TMP/deep-review-acme@foo-99/review.json" "Approve可能" '[]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" \
  "$TMP/deep-review-acme@foo-99/review.json" 2>"$TMP/err11b.txt")
assert_exit 'review file in another PR work dir: non-zero exit' $? 1
assert 'review file in another PR work dir: nothing posted' "[ ! -s '$GH_STUB_LOG' ]"

# work_dir が存在しない場合は「取り違え」と区別して復旧手順を案内する（prepare-review.sh の未実行・
# scratchpad の消失で起きる。同じエラーに畳むと置き場所を直そうとして原因を見失う）
: > "$GH_STUB_LOG"
mkdir -p "$TMP/nowork"
write_context "$TMP/nowork/pr-context-acme@foo-9.json"
(cd "$REPO" && bash "$SCRIPT" "$TMP/nowork/pr-context-acme@foo-9.json" "$TMP/review.json" 2>"$TMP/err11c.txt")
assert_exit 'missing work dir: non-zero exit' $? 1
assert 'missing work dir: nothing posted' "[ ! -s '$GH_STUB_LOG' ]"
assert 'missing work dir: stderr names the missing dir' \
  "grep -q 'work dir not found' '$TMP/err11c.txt'"
assert 'missing work dir: stderr gives the recovery step' \
  "grep -q 'prepare-review.sh' '$TMP/err11c.txt'"

# --- ケース12: JSON 構文エラー → parse error として報告（フィールド欠落と誤報告しない） ---
# 長文 body の手書きエスケープ落ちで JSON 全体が無効になった際、jq の stderr を捨てて
# "assessment missing" と報告すると原因特定を誤誘導する（2026-07 の実障害）
: > "$GH_STUB_LOG"
printf '{"assessment": "Approve可能", "body": "a "b" c", "comments": []}' > "$WORK/rev12.json"
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev12.json" 2>"$TMP/err12.txt")
assert_exit 'broken review JSON: non-zero exit' $? 1
assert 'broken review JSON: not posted' "[ ! -s '$GH_STUB_LOG' ]"
assert 'broken review JSON: reported as invalid JSON' "grep -q 'invalid JSON' '$TMP/err12.txt'"
assert 'broken review JSON: jq parse error passed through' "grep -qi 'parse error' '$TMP/err12.txt'"
assert 'broken review JSON: not misreported as a missing field' "! grep -q 'assessment missing' '$TMP/err12.txt'"

# context ファイルが壊れている場合も同様（"repo missing" と誤報告しない）
: > "$GH_STUB_LOG"
mkdir -p "$TMP/brokenctx/deep-review-acme@foo-9"
printf '{"repo": ' > "$TMP/brokenctx/pr-context-acme@foo-9.json"
write_review "$TMP/brokenctx/deep-review-acme@foo-9/rev12b.json" "Approve可能" '[]'
(cd "$REPO" && bash "$SCRIPT" "$TMP/brokenctx/pr-context-acme@foo-9.json" \
  "$TMP/brokenctx/deep-review-acme@foo-9/rev12b.json" 2>"$TMP/err12b.txt")
assert_exit 'broken context JSON: non-zero exit' $? 1
assert 'broken context JSON: not posted' "[ ! -s '$GH_STUB_LOG' ]"
assert 'broken context JSON: reported as invalid JSON' "grep -q 'invalid JSON' '$TMP/err12b.txt'"
assert 'broken context JSON: not misreported as a missing field' "! grep -q 'repo missing' '$TMP/err12b.txt'"

# --- ケース13: body_file / comments[].body_file の解決（長文プロースの手書きエスケープ回避経路） ---
printf '## レビュー結果\n\n"エスケープ不要" の本文\n' > "$WORK/body13.md"
printf '**推奨修正**: `data-testid="x"` を付ける\n' > "$WORK/c13.md"
jq -n '{assessment: "Approve可能", body_file: "body13.md",
        comments: [{path: "stable.txt", line: 5, body_file: "c13.md"}]}' > "$WORK/rev13.json"
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev13.json")
assert_exit 'body_file: exit 0' $? 0
assert_json 'body_file: body read from the referenced file' "$(payload)" '.body | test("エスケープ不要")'
assert_json 'body_file: comment body read from the referenced file' "$(payload)" '.comments[0].body | test("推奨修正")'
assert_json 'body_file: body_file key not leaked into the payload' "$(payload)" '(has("body_file") | not) and (.comments[0] | has("body_file") | not)'

# インラインと body_file の混在も可（コメントごとに選べる）
printf 'file 側の本文\n' > "$WORK/c13b.md"
jq -n '{assessment: "Approve可能", body: "インライン本文",
        comments: [{path: "stable.txt", line: 5, body: "インライン指摘"},
                   {path: "stable.txt", line: 3, body_file: "c13b.md"}]}' > "$WORK/rev13b.json"
out=$(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev13b.json")
assert_exit 'mixed inline/body_file: exit 0' $? 0
assert_json 'mixed: inline comment passed through' "$(payload)" '.comments[0].body == "インライン指摘"'
assert_json 'mixed: file comment resolved' "$(payload)" '.comments[1].body | test("file 側の本文")'

# --- ケース14: body_file の契約違反 → 投稿せず非ゼロ exit ---
# body と body_file の両方指定（どちらを投稿すべきか決められない）
: > "$GH_STUB_LOG"
jq -n '{assessment: "Approve可能", body: "a", body_file: "body13.md", comments: []}' > "$WORK/rev14a.json"
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev14a.json" 2>"$TMP/err14a.txt")
assert_exit 'both body and body_file: non-zero exit' $? 1
assert 'both body and body_file: not posted' "[ ! -s '$GH_STUB_LOG' ]"
assert 'both body and body_file: stderr states the exclusivity' "grep -q 'exactly one of' '$TMP/err14a.txt'"

# どちらも無し
jq -n '{assessment: "Approve可能", comments: []}' > "$WORK/rev14b.json"
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev14b.json" 2>"$TMP/err14b.txt")
assert_exit 'neither body nor body_file: non-zero exit' $? 1
assert 'neither body nor body_file: stderr states the exclusivity' "grep -q 'exactly one of' '$TMP/err14b.txt'"

# comments[] 側の両方指定
jq -n '{assessment: "Approve可能", body: "a",
        comments: [{path: "stable.txt", line: 5, body: "x", body_file: "c13.md"}]}' > "$WORK/rev14c.json"
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev14c.json" 2>"$TMP/err14c.txt")
assert_exit 'comment with both body and body_file: non-zero exit' $? 1
assert 'comment with both body and body_file: stderr names the comments contract' "grep -q 'comments must be' '$TMP/err14c.txt'"

# パス区切りを含む参照（work_dir 束縛を file 参照で迂回させない）
: > "$GH_STUB_LOG"
jq -n '{assessment: "Approve可能", body_file: "../outside.md", comments: []}' > "$WORK/rev14d.json"
printf 'outside\n' > "$TMP/outside.md"
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev14d.json" 2>"$TMP/err14d.txt")
assert_exit 'body_file with a path separator: non-zero exit' $? 1
assert 'body_file with a path separator: not posted' "[ ! -s '$GH_STUB_LOG' ]"
assert 'body_file with a path separator: stderr names body_file' "grep -q 'body_file' '$TMP/err14d.txt'"

# 参照先ファイルが存在しない
jq -n '{assessment: "Approve可能", body_file: "no-such.md", comments: []}' > "$WORK/rev14e.json"
(cd "$REPO" && bash "$SCRIPT" "$TMP/pr-context-acme@foo-9.json" "$WORK/rev14e.json" 2>"$TMP/err14e.txt")
assert_exit 'body_file not found: non-zero exit' $? 1
assert 'body_file not found: stderr names the missing file' "grep -q 'no-such.md' '$TMP/err14e.txt'"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
