#!/bin/bash

# fetch-pr-context.sh のリグレッションテスト
#
# 実行: bash claude/.claude/scripts/tests/test-fetch-pr-context.sh
# gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../fetch-pr-context.sh"

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
  repo) printf '%s\n' "${GH_STUB_REPO:-owner/repo}" ;;
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
        # 継続クエリに渡った変数（threadId 等）を呼び出し番号ごとに記録する。
        # スタブはレスポンスを呼び出し回数だけで選ぶため、これが無いと
        # 誤った threadId を渡すバグ（スレッド間のページ取り違え）を検出できない
        printf '%s\n' "$@" > "$GH_STUB_DATA/.graphql-args-$n"
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

OUT_DIR="$TMP/out"
mkdir -p "$OUT_DIR"

fetch() { bash "$SCRIPT" "$OUT_DIR" "$@"; }
ctx_path() { printf '%s' "$1" | jq -r .path; }
ctx_of() { cat "$(ctx_path "$1")"; }

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
          "totalCount": 3,
          "pageInfo": {"hasNextPage": false, "endCursor": "tc-1"},
          "nodes": [
            {
              "id": "PRRT_1", "isResolved": false, "isOutdated": false, "path": "src/main.go", "line": 30,
              "resolvedBy": null,
              "comments": {
                "totalCount": 1,
                "pageInfo": {"hasNextPage": false, "endCursor": "t1"},
                "nodes": [{"author": {"login": "reviewer1"}, "body": "ここ直して", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t1"}]
              },
              "tail": {"nodes": [{"author": {"login": "reviewer1"}, "body": "ここ直して", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t1"}]}
            },
            {
              "id": "PRRT_2", "isResolved": true, "isOutdated": true, "path": "src/util.go", "line": 10,
              "resolvedBy": {"login": "testuser"},
              "comments": {
                "totalCount": 1,
                "pageInfo": {"hasNextPage": false, "endCursor": "t2"},
                "nodes": [{"author": {"login": "reviewer2"}, "body": "解決済み", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t2"}]
              },
              "tail": {"nodes": [{"author": {"login": "reviewer2"}, "body": "解決済み", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t2"}]}
            },
            {
              "id": "PRRT_3", "isResolved": false, "isOutdated": false, "path": "src/api.go", "line": 7,
              "resolvedBy": null,
              "comments": {
                "totalCount": 2,
                "pageInfo": {"hasNextPage": false, "endCursor": "t3"},
                "nodes": [
                  {"author": {"login": "reviewer1"}, "body": "ここも直して", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t3a"},
                  {"author": {"login": "testuser"}, "body": "修正しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t3b"}
                ]
              },
              "tail": {"nodes": [{"author": {"login": "testuser"}, "body": "修正しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t3b"}]}
            },
            {
              "id": "PRRT_4", "isResolved": true, "isOutdated": false, "path": "src/db.go", "line": 42,
              "resolvedBy": {"login": "testuser"},
              "comments": {
                "totalCount": 2,
                "pageInfo": {"hasNextPage": false, "endCursor": "t4"},
                "nodes": [
                  {"author": {"login": "reviewer2"}, "body": "ここ確認", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/t4a"},
                  {"author": {"login": "testuser"}, "body": "対応済みです", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t4b"}
                ]
              },
              "tail": {"nodes": [{"author": {"login": "testuser"}, "body": "対応済みです", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/t4b"}]}
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

assert_eq() {
  local name=$1 got=$2 want=$3
  if [ "$got" = "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got %s, want %s)\n' "$name" "$got" "$want"
  fi
}

# 番号明示指定
out=$(fetch 5)
assert_exit 'explicit pr number: exit 0' $? 0
assert 'stdout is path-only JSON' "$out" 'keys == ["path"]'
assert_eq 'context file named pr-context-<owner>@<repo>-<number>.json' \
  "$(ctx_path "$out")" "$OUT_DIR/pr-context-owner@repo-5.json"
ctx=$(ctx_of "$out")

assert 'pr meta mapped' "$ctx" \
  '.pr.number == 5 and .pr.author == "testuser" and .pr.head_ref == "feature/x" and .pr.base_ref == "main" and .pr.head_oid == "abc123"'
assert 'is_own_pr true for own PR' "$ctx" '.is_own_pr == true and .current_user == "testuser"'
assert 'linked issues: same-repo forms detected (dedup)' "$ctx" \
  '(.linked_issues | length) == 3 and any(.linked_issues[]; .repo == null and .number == 10) and any(.linked_issues[]; .repo == null and .number == 11)'
assert 'linked issues: cross-repo form detected' "$ctx" \
  'any(.linked_issues[]; .repo == "other/repo" and .number == 12)'
assert 'linked issues: bare #N and URL forms excluded' "$ctx" \
  '[.linked_issues[].number] | (index(99) or index(13) or index(14)) | not'
assert 'skill comment flagged by prefix match' "$ctx" \
  '(.comments | map(select(.is_skill_comment)) | length) == 1 and (.comments[] | select(.is_skill_comment) | .url) == "https://example.com/c2"'
assert 'quoted marker not flagged' "$ctx" \
  '.comments[] | select(.url == "https://example.com/c3") | .is_skill_comment == false'
assert 'author_type mapped from __typename' "$ctx" \
  '(.comments[] | select(.url == "https://example.com/c1") | .author_type == "User")
   and (.comments[] | select(.url == "https://example.com/c4") | .author_type == "Bot")'
assert 'comments_total_count exposed, not truncated' "$ctx" \
  '.comments_total_count == 4 and (.comments | length) == 4 and .comments_truncated == false'
assert 'reviews_total_count exposed, not truncated' "$ctx" \
  '.reviews_total_count == 1 and .reviews_truncated == false'
assert 'reviews mapped' "$ctx" \
  '.reviews == [{"author": "reviewer1", "state": "CHANGES_REQUESTED", "body": "優先度1: テスト不足", "url": "https://example.com/r1", "submitted_at": "2026-01-01T00:00:00Z"}]'
assert 'threads mapped with resolution state' "$ctx" \
  '(.review_threads | length) == 4
   and (.review_threads[0] | .id == "PRRT_1" and .is_resolved == false and .path == "src/main.go" and .line == 30 and .resolved_by == null)
   and (.review_threads[1] | .is_resolved == true and .is_outdated == true and .resolved_by == "testuser")'
assert 'threads not truncated' "$ctx" \
  '.threads_truncated == false and all(.review_threads[]; .comments_truncated == false)'
# last_comment は反応待ち分類（review-response）の入力。末尾が自分／他人の両方向を張る
assert 'last_comment points at the newest comment' "$ctx" \
  '(.review_threads[0].last_comment | .author == "reviewer1" and .url == "https://example.com/t1")
   and (.review_threads[2].last_comment | .author == "testuser" and .body == "修正しました" and .url == "https://example.com/t3b")'
# waiting_for_response: 未解決 かつ 末尾が自分 かつ 自分の PR の3条件をスクリプト側で確定させる
# （モデルが手順文から毎回導出すると is_own_pr ガードの見落としで分類が反転しうる）。
# フィクスチャは3条件を独立に固定する: PRRT_1 未解決+末尾が他人 / PRRT_3 未解決+末尾が自分 /
# PRRT_4 解決済み+末尾が自分（PRRT_4 が無いと isResolved ガードを外しても全テストが通る）
assert 'waiting_for_response set only for unresolved threads we replied to last' "$ctx" \
  '[.review_threads[].waiting_for_response] == [false, false, true, false]'

# ファイル名一意化: 別リポジトリなら同じ out-dir でも別ファイルになる
# （並列サブエージェントが共有 scratchpad を out-dir に使っても衝突しない性質の担保）
out_x=$(GH_STUB_REPO=other/proj fetch 5)
assert_exit 'cross-repo: exit 0' $? 0
assert_eq 'cross-repo: repo name embedded in filename' \
  "$(ctx_path "$out_x")" "$OUT_DIR/pr-context-other@proj-5.json"

# 番号省略（カレント branch から推論）
out_infer=$(fetch)
assert_exit 'inferred pr number: exit 0' $? 0
assert_eq 'inferred number embedded in filename' \
  "$(ctx_path "$out_infer")" "$OUT_DIR/pr-context-owner@repo-5.json"
assert 'inferred pr number used' "$(ctx_of "$out_infer")" '.pr.number == 5'

# 他人の PR → is_own_pr false
sed 's/"login": "testuser"/"login": "othercoder"/' "$TMP/data/pr-meta.json" > "$TMP/data/pr-meta.json.tmp" \
  && mv "$TMP/data/pr-meta.json.tmp" "$TMP/data/pr-meta.json"
out_other=$(fetch 5)
assert 'is_own_pr false for others PR' "$(ctx_of "$out_other")" \
  '.is_own_pr == false and .pr.author == "othercoder"'
# レビュアー側の PR では「末尾が自分」の意味が反転する（相手の応答待ち）ため分類しない
assert 'waiting_for_response never set on someone elses PR' "$(ctx_of "$out_other")" \
  'all(.review_threads[]; .waiting_for_response == false)'

# エラー系
GH_STUB_NO_PR=1 fetch >/dev/null 2>"$TMP/err.txt"
assert_exit 'inference failure: non-zero exit' $? 1
if [ -s "$TMP/err.txt" ]; then
  pass=$((pass + 1)); printf 'PASS  inference failure: stderr message present\n'
else
  fail=$((fail + 1)); printf 'FAIL  inference failure: stderr message present\n'
fi

fetch abc >/dev/null 2>/dev/null
assert_exit 'non-numeric pr number rejected' $? 1

bash "$SCRIPT" >/dev/null 2>/dev/null
assert_exit 'missing out-dir rejected' $? 1

bash "$SCRIPT" "$TMP/no-such-dir" 5 >/dev/null 2>/dev/null
assert_exit 'nonexistent out-dir rejected' $? 1

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
out_page=$(fetch 5)
assert_exit 'pagination: exit 0' $? 0
ctx_page=$(ctx_of "$out_page")
assert 'pagination: pages merged in order' "$ctx_page" \
  '[.comments[].body] == ["page1-a", "page1-b", "page2-a"] and .comments_total_count == 3 and .comments_truncated == false'
assert 'reviews window eviction flagged' "$ctx_page" \
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
out_cap=$(fetch 5)
assert_exit 'comment cap: exit 0' $? 0
assert 'comment cap: stops at MAX_COMMENTS with truncation flag' "$(ctx_of "$out_cap")" \
  '(.comments | length) == 500 and .comments_total_count == 600 and .comments_truncated == true'

# MAX_COMMENTS の環境変数上書き（cap フィクスチャを再利用。100件/ページ × 上限250 → 300件で停止）
rm -f "$TMP/data/.graphql-call-count"
out_override=$(MAX_COMMENTS=250 fetch 5)
assert_exit 'cap override: exit 0' $? 0
assert 'cap override: MAX_COMMENTS env changes the cap' "$(ctx_of "$out_override")" \
  '(.comments | length) == 300 and .comments_truncated == true'
rm -f "$TMP/data"/graphql-[1-5].json "$TMP/data/.graphql-call-count"

# ページ取得途中の GraphQL 失敗: 部分取得のまま正常出力せず exit 1 で停止し、
# 出力ファイルも作らない（atomic 書き込み — 部分 JSON を後段の jq 参照が読む事故を防ぐ）
rm -f "$TMP/data/.graphql-call-count"
jq '.data.repository.pullRequest.comments.pageInfo = {hasNextPage: true, endCursor: "p1"}' \
  "$TMP/data/graphql.json" > "$TMP/data/graphql-1.json"
: > "$TMP/data/graphql-2.fail"
FAIL_DIR="$TMP/out-fail"
mkdir -p "$FAIL_DIR"
bash "$SCRIPT" "$FAIL_DIR" 5 >"$TMP/page-out.txt" 2>"$TMP/page-err.txt"
assert_exit 'page fetch failure: non-zero exit' $? 1
if grep -q 'failed to fetch PR comments page' "$TMP/page-err.txt" && [ ! -s "$TMP/page-out.txt" ] && [ -z "$(ls -A "$FAIL_DIR")" ]; then
  pass=$((pass + 1)); printf 'PASS  page fetch failure: stderr message, no stdout, no partial file\n'
else
  fail=$((fail + 1)); printf 'FAIL  page fetch failure: stderr message, no stdout, no partial file\n'
fi
rm -f "$TMP/data/graphql-1.json" "$TMP/data/graphql-2.fail" "$TMP/data/.graphql-call-count"

# MAX_COMMENTS の非数値は exit 1（書式ミスのまま「上限を上げて再実行」が空回りする事故を防ぐ）
MAX_COMMENTS=abc fetch 5 >/dev/null 2>/dev/null
assert_exit 'non-numeric MAX_COMMENTS rejected' $? 1

reset_stub() { rm -f "$TMP/data"/graphql-[0-9]*.json "$TMP/data"/graphql-[0-9]*.fail \
  "$TMP/data/.graphql-call-count" "$TMP/data"/.graphql-args-*; }

# スレッドコメントのページネーション: スレッド ID で keyed にマージし、別スレッドのページが混線しないこと。
# 継続クエリに渡した threadId も検証する（スタブは呼び出し回数でレスポンスを選ぶため、
# 引数を見ないと PRRT_1 の続きを PRRT_3 に積むようなバグが素通りする）
reset_stub
jq '.data.repository.pullRequest.reviewThreads.nodes[0].comments += {totalCount: 2, pageInfo: {hasNextPage: true, endCursor: "t1p1"}}
    | .data.repository.pullRequest.reviewThreads.nodes[2].comments += {totalCount: 3, pageInfo: {hasNextPage: true, endCursor: "t3p1"}}' \
  "$TMP/data/graphql.json" > "$TMP/data/graphql-1.json"
jq -n '{data: {node: {comments: {
      pageInfo: {hasNextPage: false, endCursor: "t1p2"},
      nodes: [{author: {login: "reviewer1"}, body: "t1-page2", createdAt: "2026-01-03T00:00:00Z", url: "https://example.com/t1p2"}]
    }}}}' > "$TMP/data/graphql-2.json"
jq -n '{data: {node: {comments: {
      pageInfo: {hasNextPage: false, endCursor: "t3p2"},
      nodes: [{author: {login: "reviewer1"}, body: "t3-page2", createdAt: "2026-01-03T00:00:00Z", url: "https://example.com/t3p2"}]
    }}}}' > "$TMP/data/graphql-3.json"
out_tp=$(fetch 5)
assert_exit 'thread comment pagination: exit 0' $? 0
assert 'thread comment pagination: pages merged per thread without cross-talk' "$(ctx_of "$out_tp")" \
  '(.review_threads[0] | [.comments[].body] == ["ここ直して", "t1-page2"])
   and (.review_threads[1] | [.comments[].body] == ["解決済み"])
   and (.review_threads[2] | [.comments[].body] == ["ここも直して", "修正しました", "t3-page2"])'
if grep -q 'threadId=PRRT_1' "$TMP/data/.graphql-args-2" \
  && grep -q 'threadId=PRRT_3' "$TMP/data/.graphql-args-3"; then
  pass=$((pass + 1)); printf 'PASS  thread comment pagination: threadId passed per thread\n'
else
  fail=$((fail + 1)); printf 'FAIL  thread comment pagination: threadId passed per thread\n'
fi

# reviewThreads 自体のページネーションと MAX_THREADS 打ち切り。
# cap は overshoot する（初回ページは cap に関わらず 100 件取る）ため 250 指定 → 300 件で停止
reset_stub
threads100=$(jq -n '[range(100)] | map({
      id: ("PRRT_P_" + tostring), isResolved: false, isOutdated: false, path: "noise.go", line: 1,
      resolvedBy: null,
      comments: {totalCount: 0, pageInfo: {hasNextPage: false, endCursor: null}, nodes: []},
      tail: {nodes: []}
    })')
jq --argjson nodes "$threads100" '.data.repository.pullRequest.reviewThreads = {
      totalCount: 600,
      pageInfo: {hasNextPage: true, endCursor: "tN"},
      nodes: $nodes
    }' "$TMP/data/graphql.json" > "$TMP/data/graphql-1.json"
jq -n --argjson nodes "$threads100" '{data: {repository: {pullRequest: {reviewThreads: {
      pageInfo: {hasNextPage: true, endCursor: "tN"},
      nodes: $nodes
    }}}}}' > "$TMP/data/graphql-2.json"
cp "$TMP/data/graphql-2.json" "$TMP/data/graphql-3.json"
out_tcap=$(MAX_THREADS=250 fetch 5)
assert_exit 'thread cap: exit 0' $? 0
assert 'thread cap: stops at MAX_THREADS with truncation flag' "$(ctx_of "$out_tcap")" \
  '(.review_threads | length) == 300 and .threads_truncated == true'
assert 'threads with no comments get null last_comment' "$(ctx_of "$out_tcap")" \
  '.review_threads[0].last_comment == null'

# MAX_THREAD_COMMENTS の per-thread 打ち切り。打ち切っても last_comment は tail 由来で真の末尾を指す
# （comments[] に含まれない要素になる — 分類が silent に誤らないための要の保証）
reset_stub
tcomments100=$(jq -n '[range(100)] | map({author: {login: "reviewer1"}, body: "noise", createdAt: "2026-01-01T00:00:00Z", url: "https://example.com/tn"})')
jq --argjson nodes "$tcomments100" '.data.repository.pullRequest.reviewThreads = {
      totalCount: 1,
      pageInfo: {hasNextPage: false, endCursor: "tc-1"},
      nodes: [{
        id: "PRRT_BIG", isResolved: false, isOutdated: false, path: "big.go", line: 1,
        resolvedBy: null,
        comments: {totalCount: 600, pageInfo: {hasNextPage: true, endCursor: "bc"}, nodes: $nodes},
        tail: {nodes: [{author: {login: "testuser"}, body: "最後の返信", createdAt: "2026-02-01T00:00:00Z", url: "https://example.com/newest"}]}
      }]
    }' "$TMP/data/graphql.json" > "$TMP/data/graphql-1.json"
jq -n --argjson nodes "$tcomments100" '{data: {node: {comments: {
      pageInfo: {hasNextPage: true, endCursor: "bc"},
      nodes: $nodes
    }}}}' > "$TMP/data/graphql-2.json"
for i in 3 4 5; do cp "$TMP/data/graphql-2.json" "$TMP/data/graphql-$i.json"; done
out_tccap=$(fetch 5)
assert_exit 'thread comment cap: exit 0' $? 0
assert 'thread comment cap: stops at MAX_THREAD_COMMENTS with per-thread truncation flag' "$(ctx_of "$out_tccap")" \
  '(.review_threads[0].comments | length) == 200 and .review_threads[0].comments_truncated == true'
assert 'last_comment stays accurate under truncation' "$(ctx_of "$out_tccap")" \
  '.review_threads[0].last_comment.url == "https://example.com/newest"
   and (.review_threads[0].last_comment.author == "testuser")
   and ([.review_threads[0].comments[].url] | index("https://example.com/newest") | not)'

# MAX_THREAD_COMMENTS の環境変数上書き（同じフィクスチャで上限 50 → 初回 100 件のまま追撃しない）
rm -f "$TMP/data/.graphql-call-count" "$TMP/data"/.graphql-args-*
out_tcoverride=$(MAX_THREAD_COMMENTS=50 fetch 5)
assert_exit 'thread comment cap override: exit 0' $? 0
assert 'thread comment cap override: MAX_THREAD_COMMENTS env changes the cap' "$(ctx_of "$out_tcoverride")" \
  '(.review_threads[0].comments | length) == 100 and .review_threads[0].comments_truncated == true'

# スレッド継続ページの取得失敗も部分出力せず停止する
reset_stub
jq '.data.repository.pullRequest.reviewThreads.nodes[0].comments += {totalCount: 2, pageInfo: {hasNextPage: true, endCursor: "t1p1"}}' \
  "$TMP/data/graphql.json" > "$TMP/data/graphql-1.json"
: > "$TMP/data/graphql-2.fail"
TFAIL_DIR="$TMP/out-thread-fail"
mkdir -p "$TFAIL_DIR"
bash "$SCRIPT" "$TFAIL_DIR" 5 >"$TMP/tpage-out.txt" 2>"$TMP/tpage-err.txt"
assert_exit 'thread page fetch failure: non-zero exit' $? 1
if grep -q 'failed to fetch review thread comments page' "$TMP/tpage-err.txt" \
  && [ ! -s "$TMP/tpage-out.txt" ] && [ -z "$(ls -A "$TFAIL_DIR")" ]; then
  pass=$((pass + 1)); printf 'PASS  thread page fetch failure: stderr message, no stdout, no partial file\n'
else
  fail=$((fail + 1)); printf 'FAIL  thread page fetch failure: stderr message, no stdout, no partial file\n'
fi
reset_stub

# 新 cap の非数値も MAX_COMMENTS と同様に exit 1
MAX_THREADS=abc fetch 5 >/dev/null 2>/dev/null
assert_exit 'non-numeric MAX_THREADS rejected' $? 1
MAX_THREAD_COMMENTS=abc fetch 5 >/dev/null 2>/dev/null
assert_exit 'non-numeric MAX_THREAD_COMMENTS rejected' $? 1

# マーカー文字列の双方向契約: review-response SKILL.md（書く側）とスクリプトの startswith（検出側）の一致
# 片側だけ変更されると is_skill_comment が silent に false 化し、自分の過去投稿を新規指摘として再対応する退行が起きる
MARKER_SKILL="$SCRIPT_DIR/../../skills/review-response/SKILL.md"
grep -q 'startswith("<!-- review-response -->")' "$SCRIPT" && grep -q '<!-- review-response -->' "$MARKER_SKILL"
assert_exit 'skill comment marker in sync between writer skill and detector script' $? 0

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
