#!/bin/bash

# fetch-pr-context.sh のリグレッションテスト
#
# 実行: bash claude/.claude/tests/test-fetch-pr-context.sh
# gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/fetch-pr-context.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# --- gh スタブ ---
mkdir -p "$TMP/stub" "$TMP/data"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
case "$1" in
  repo) printf 'owner/repo\n' ;;
  pr)
    # 引数に PR 番号（純数値）があれば番号指定の meta 取得、無ければカレント branch 推論。
    # 位置ではなく内容で判別する（スクリプト側の引数順変更でテストが壊れないように）
    has_number=0
    for a in "$@"; do
      case "$a" in
        '' | *[!0-9]*) ;;
        *) has_number=1 ;;
      esac
    done
    if [ "$has_number" = "0" ] && [ "${GH_STUB_NO_PR:-0}" = "1" ]; then exit 1; fi
    cat "$GH_STUB_DATA/pr-meta.json"
    ;;
  api)
    case "$2" in
      graphql)
        # ページネーションのテスト用に、呼び出し回数 n に応じて graphql-<n>.json を返す。
        # graphql-<n>.fail があれば失敗を模擬し、どちらも無ければ graphql.json へフォールバック
        n=$(cat "$GH_STUB_DATA/.graphql-call-count" 2>/dev/null || printf '0')
        n=$((n + 1))
        printf '%s' "$n" > "$GH_STUB_DATA/.graphql-call-count"
        if [ -f "$GH_STUB_DATA/graphql-$n.fail" ]; then
          exit 1
        fi
        if [ -f "$GH_STUB_DATA/graphql-$n.json" ]; then
          cat "$GH_STUB_DATA/graphql-$n.json"
        else
          cat "$GH_STUB_DATA/graphql.json"
        fi
        ;;
      *) exit 1 ;;
    esac
    ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_DATA="$TMP/data"

# --- フィクスチャ ---
# body は closing keyword 検出のテストコーパス:
#   マッチすべき: Closes #10 / FIXES: #11 / Resolves other/repo#12 / 重複の fix #10
#   マッチすべきでない: 素の #99 / URL 形式（キーワード有無both）
cat > "$TMP/data/pr-meta.json" <<'EOF'
{
  "number": 5,
  "title": "Test PR",
  "body": "Closes #10\nFIXES: #11\nResolves other/repo#12\nfix #10\nSee #99\nSee https://github.com/owner/repo/issues/13\nFixes https://github.com/owner/repo/issues/14",
  "url": "https://github.com/owner/repo/pull/5",
  "state": "OPEN",
  "author": {"login": "testuser"},
  "headRefName": "feature/x",
  "baseRefName": "main",
  "headRefOid": "abc123"
}
EOF

cat > "$TMP/data/graphql.json" <<'EOF'
{
  "data": {
    "viewer": {"login": "testuser"},
    "repository": {
      "pullRequest": {
        "comments": {
          "totalCount": 4,
          "pageInfo": {"hasNextPage": false, "endCursor": "cur-1"},
          "nodes": [
            {"author": {"login": "reviewer1", "__typename": "User"}, "body": "普通のコメント", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/c1"},
            {"author": {"login": "testuser", "__typename": "User"}, "body": "<!-- review-response -->\n対応しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/c2"},
            {"author": {"login": "reviewer1", "__typename": "User"}, "body": "> <!-- review-response -->\n引用返信", "createdAt": "2026-01-03T00:00:00Z", "url": "https://example.com/c3"},
            {"author": {"login": "github-actions", "__typename": "Bot"}, "body": "CI 通知", "createdAt": "2026-01-04T00:00:00Z", "url": "https://example.com/c4"}
          ]
        },
        "reviews": {
          "totalCount": 1,
          "nodes": [
            {"author": {"login": "reviewer1"}, "state": "CHANGES_REQUESTED", "body": "優先度1: テスト不足", "url": "https://example.com/r1", "submittedAt": "2026-01-01T00:00:00Z"}
          ]
        },
        "reviewThreads": {
          "nodes": [
            {
              "id": "PRRT_1", "isResolved": false, "isOutdated": false, "path": "src/main.go", "line": 30,
              "resolvedBy": null,
              "comments": {"nodes": [{"author": {"login": "reviewer1"}, "body": "ここ直して", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t1"}]}
            },
            {
              "id": "PRRT_2", "isResolved": true, "isOutdated": true, "path": "src/util.go", "line": 10,
              "resolvedBy": {"login": "testuser"},
              "comments": {"nodes": [{"author": {"login": "reviewer2"}, "body": "解決済み", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t2"}]}
            }
          ]
        }
      }
    }
  }
}
EOF

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

# 番号明示指定
out=$(bash "$SCRIPT" 5)
assert_exit 'explicit pr number: exit 0' $? 0

assert 'pr meta mapped' "$out" \
  '.pr.number == 5 and .pr.author == "testuser" and .pr.head_ref == "feature/x" and .pr.base_ref == "main" and .pr.head_oid == "abc123"'
assert 'is_own_pr true for own PR' "$out" '.is_own_pr == true and .current_user == "testuser"'
assert 'linked issues: same-repo forms detected (dedup)' "$out" \
  '(.linked_issues | length) == 3 and any(.linked_issues[]; .repo == null and .number == 10) and any(.linked_issues[]; .repo == null and .number == 11)'
assert 'linked issues: cross-repo form detected' "$out" \
  'any(.linked_issues[]; .repo == "other/repo" and .number == 12)'
assert 'linked issues: bare #N and URL forms excluded' "$out" \
  '[.linked_issues[].number] | (index(99) or index(13) or index(14)) | not'
assert 'skill comment flagged by prefix match' "$out" \
  '(.comments | map(select(.is_skill_comment)) | length) == 1 and (.comments[] | select(.is_skill_comment) | .url) == "https://example.com/c2"'
assert 'quoted marker not flagged' "$out" \
  '.comments[] | select(.url == "https://example.com/c3") | .is_skill_comment == false'
assert 'author_type mapped from __typename' "$out" \
  '(.comments[] | select(.url == "https://example.com/c1") | .author_type == "User")
   and (.comments[] | select(.url == "https://example.com/c4") | .author_type == "Bot")'
assert 'comments_total_count exposed, not truncated' "$out" \
  '.comments_total_count == 4 and (.comments | length) == 4 and .comments_truncated == false'
assert 'reviews_total_count exposed, not truncated' "$out" \
  '.reviews_total_count == 1 and .reviews_truncated == false'
assert 'reviews mapped' "$out" \
  '.reviews == [{"author": "reviewer1", "state": "CHANGES_REQUESTED", "body": "優先度1: テスト不足", "url": "https://example.com/r1", "submitted_at": "2026-01-01T00:00:00Z"}]'
assert 'threads mapped with resolution state' "$out" \
  '(.review_threads | length) == 2
   and (.review_threads[0] | .id == "PRRT_1" and .is_resolved == false and .path == "src/main.go" and .line == 30 and .resolved_by == null)
   and (.review_threads[1] | .is_resolved == true and .is_outdated == true and .resolved_by == "testuser")'

# 番号省略（カレント branch から推論）
out_infer=$(bash "$SCRIPT")
assert_exit 'inferred pr number: exit 0' $? 0
assert 'inferred pr number used' "$out_infer" '.pr.number == 5'

# 他人の PR → is_own_pr false
sed 's/"login": "testuser"/"login": "othercoder"/' "$TMP/data/pr-meta.json" > "$TMP/data/pr-meta.json.tmp" \
  && mv "$TMP/data/pr-meta.json.tmp" "$TMP/data/pr-meta.json"
out_other=$(bash "$SCRIPT" 5)
assert 'is_own_pr false for others PR' "$out_other" '.is_own_pr == false and .pr.author == "othercoder"'

# エラー系
GH_STUB_NO_PR=1 bash "$SCRIPT" >/dev/null 2>"$TMP/err.txt"
assert_exit 'inference failure: non-zero exit' $? 1
if [ -s "$TMP/err.txt" ]; then
  pass=$((pass + 1)); printf 'PASS  inference failure: stderr message present\n'
else
  fail=$((fail + 1)); printf 'FAIL  inference failure: stderr message present\n'
fi

bash "$SCRIPT" abc >/dev/null 2>/dev/null
assert_exit 'non-numeric pr number rejected' $? 1

# ページネーション: 2 ページ分を順序どおり結合して全量取得
# （reviews.totalCount を窓より大きくし、reviews_truncated の true 経路も同時に検証する）
rm -f "$TMP/data/.graphql-call-count"
jq '.data.repository.pullRequest.comments = {
      totalCount: 3,
      pageInfo: {hasNextPage: true, endCursor: "p1"},
      nodes: [
        {author: {login: "reviewer1", __typename: "User"}, body: "page1-a", createdAt: "2026-01-01T00:00:00Z", url: "https://example.com/p1a"},
        {author: {login: "ci-bot", __typename: "Bot"}, body: "page1-b", createdAt: "2026-01-01T00:01:00Z", url: "https://example.com/p1b"}
      ]
    }
    | .data.repository.pullRequest.reviews.totalCount = 60' "$TMP/data/graphql.json" > "$TMP/data/graphql-1.json"
jq -n '{data: {repository: {pullRequest: {comments: {
      pageInfo: {hasNextPage: false, endCursor: "p2"},
      nodes: [{author: {login: "reviewer1", __typename: "User"}, body: "page2-a", createdAt: "2026-01-02T00:00:00Z", url: "https://example.com/p2a"}]
    }}}}}' > "$TMP/data/graphql-2.json"
out_page=$(bash "$SCRIPT" 5)
assert_exit 'pagination: exit 0' $? 0
assert 'pagination: pages merged in order' "$out_page" \
  '[.comments[].body] == ["page1-a", "page1-b", "page2-a"] and .comments_total_count == 3 and .comments_truncated == false'
assert 'reviews window eviction flagged' "$out_page" \
  '.reviews_total_count == 60 and .reviews_truncated == true'
rm -f "$TMP/data/graphql-1.json" "$TMP/data/graphql-2.json" "$TMP/data/.graphql-call-count"

# ページネーション: MAX_COMMENTS(既定500) で打ち切り、comments_truncated で消費側が検知できる。
# スタブは呼び出し回数でページを返しカーソル値を検証しないため、後続ページは同一内容で良い
nodes100=$(jq -n '[range(100)] | map({author: {login: "ci-bot", __typename: "Bot"}, body: "noise", createdAt: "2026-01-01T00:00:00Z", url: "https://example.com/n"})')
jq --argjson nodes "$nodes100" '.data.repository.pullRequest.comments = {
      totalCount: 600,
      pageInfo: {hasNextPage: true, endCursor: "cN"},
      nodes: $nodes
    }' "$TMP/data/graphql.json" > "$TMP/data/graphql-1.json"
jq -n --argjson nodes "$nodes100" '{data: {repository: {pullRequest: {comments: {
      pageInfo: {hasNextPage: true, endCursor: "cN"},
      nodes: $nodes
    }}}}}' > "$TMP/data/graphql-2.json"
for i in 3 4 5; do cp "$TMP/data/graphql-2.json" "$TMP/data/graphql-$i.json"; done
out_cap=$(bash "$SCRIPT" 5)
assert_exit 'comment cap: exit 0' $? 0
assert 'comment cap: stops at MAX_COMMENTS with truncation flag' "$out_cap" \
  '(.comments | length) == 500 and .comments_total_count == 600 and .comments_truncated == true'

# MAX_COMMENTS の環境変数上書き（cap フィクスチャを再利用。100件/ページ × 上限250 → 300件で停止）
rm -f "$TMP/data/.graphql-call-count"
out_override=$(MAX_COMMENTS=250 bash "$SCRIPT" 5)
assert_exit 'cap override: exit 0' $? 0
assert 'cap override: MAX_COMMENTS env changes the cap' "$out_override" \
  '(.comments | length) == 300 and .comments_truncated == true'
rm -f "$TMP/data"/graphql-[1-5].json "$TMP/data/.graphql-call-count"

# ページ取得途中の GraphQL 失敗: 部分取得のまま正常出力せず exit 1 で停止する
rm -f "$TMP/data/.graphql-call-count"
jq '.data.repository.pullRequest.comments.pageInfo = {hasNextPage: true, endCursor: "p1"}' \
  "$TMP/data/graphql.json" > "$TMP/data/graphql-1.json"
: > "$TMP/data/graphql-2.fail"
bash "$SCRIPT" 5 >"$TMP/page-out.txt" 2>"$TMP/page-err.txt"
assert_exit 'page fetch failure: non-zero exit' $? 1
if grep -q 'failed to fetch PR comments page' "$TMP/page-err.txt" && [ ! -s "$TMP/page-out.txt" ]; then
  pass=$((pass + 1)); printf 'PASS  page fetch failure: stderr message, no partial stdout\n'
else
  fail=$((fail + 1)); printf 'FAIL  page fetch failure: stderr message, no partial stdout\n'
fi
rm -f "$TMP/data/graphql-1.json" "$TMP/data/graphql-2.fail" "$TMP/data/.graphql-call-count"

# MAX_COMMENTS の非数値は exit 1（書式ミスのまま「上限を上げて再実行」が空回りする事故を防ぐ）
MAX_COMMENTS=abc bash "$SCRIPT" 5 >/dev/null 2>/dev/null
assert_exit 'non-numeric MAX_COMMENTS rejected' $? 1

# マーカー文字列の双方向契約: review-response SKILL.md（書く側）とスクリプトの startswith（検出側）の一致
# 片側だけ変更されると is_skill_comment が silent に false 化し、自分の過去投稿を新規指摘として再対応する退行が起きる
MARKER_SKILL="$SCRIPT_DIR/../skills/review-response/SKILL.md"
if grep -q 'startswith("<!-- review-response -->")' "$SCRIPT" && grep -q '<!-- review-response -->' "$MARKER_SKILL"; then
  pass=$((pass + 1)); printf 'PASS  skill comment marker in sync between writer skill and detector script\n'
else
  fail=$((fail + 1)); printf 'FAIL  skill comment marker in sync between writer skill and detector script\n'
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
