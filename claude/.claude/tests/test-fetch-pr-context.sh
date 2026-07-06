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
    # $3 が --json ならカレント branch の PR 推論、番号なら meta 取得
    if [ "$3" = "--json" ]; then
      if [ "${GH_STUB_NO_PR:-0}" = "1" ]; then exit 1; fi
      printf '5\n'
    else
      cat "$GH_STUB_DATA/pr-meta.json"
    fi
    ;;
  api)
    case "$2" in
      user) printf 'testuser\n' ;;
      graphql) cat "$GH_STUB_DATA/graphql.json" ;;
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
    "repository": {
      "pullRequest": {
        "comments": {
          "nodes": [
            {"author": {"login": "reviewer1"}, "body": "普通のコメント", "createdAt": "2026-01-01T00:00:00Z", "url": "https://example.com/c1"},
            {"author": {"login": "testuser"}, "body": "<!-- review-response -->\n対応しました", "createdAt": "2026-01-02T00:00:00Z", "url": "https://example.com/c2"},
            {"author": {"login": "reviewer1"}, "body": "> <!-- review-response -->\n引用返信", "createdAt": "2026-01-03T00:00:00Z", "url": "https://example.com/c3"}
          ]
        },
        "reviews": {
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

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
