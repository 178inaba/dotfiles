#!/bin/bash

# list-pending-reviews.sh のリグレッションテスト
#
# 実行: bash claude/.claude/tests/test-list-pending-reviews.sh
# gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../skills/review-assigned-prs/scripts/list-pending-reviews.sh"

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
printf '%s\n' "$*" >> "$GH_STUB_LOG"
if [ "${GH_STUB_FAIL_SEARCH:-0}" = "1" ] && [ "$1" = "search" ]; then exit 1; fi
case "$1" in
  search)
    cat "$GH_STUB_DATA/search.json"
    ;;
  api)
    # スクリプトは常に `gh api --paginate repos/...` の順で呼ぶ（$3 が path）
    path=$3
    key=$(printf '%s' "$path" | sed -E 's|^repos/([^/]+)/([^/]+)/pulls/([0-9]+)/reviews$|\1-\2-\3|')
    if [ "${GH_STUB_FAIL_FOR:-}" = "$key" ]; then
      exit 1
    fi
    f="$GH_STUB_DATA/reviews-$key.json"
    if [ -f "$f" ]; then
      cat "$f"
    else
      printf '[]\n'
    fi
    ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_DATA="$TMP/data"
export GH_STUB_LOG="$TMP/gh-log"

# --- フィクスチャ ---
# 4 PR: no reviews / Bot のみ / 人間あり / Bot+人間 混在
cat > "$TMP/data/search.json" <<'EOF'
[
  {"number": 100, "url": "https://github.com/acme/foo/pull/100", "repository": {"nameWithOwner": "acme/foo"}},
  {"number": 101, "url": "https://github.com/acme/bar/pull/101", "repository": {"nameWithOwner": "acme/bar"}},
  {"number": 102, "url": "https://github.com/acme/baz/pull/102", "repository": {"nameWithOwner": "acme/baz"}},
  {"number": 103, "url": "https://github.com/acme/qux/pull/103", "repository": {"nameWithOwner": "acme/qux"}}
]
EOF

# acme/foo#100: reviews-* ファイル無し → stub は [] を返す（レビューゼロ）→ 候補
# acme/bar#101: Copilot のみ → 候補
cat > "$TMP/data/reviews-acme-bar-101.json" <<'EOF'
[{"user": {"type": "Bot", "login": "copilot"}, "state": "COMMENTED"},
 {"user": {"type": "Bot", "login": "copilot"}, "state": "COMMENTED"}]
EOF

# acme/baz#102: 人間レビューあり → 除外
cat > "$TMP/data/reviews-acme-baz-102.json" <<'EOF'
[{"user": {"type": "User", "login": "someone"}, "state": "APPROVED"}]
EOF

# acme/qux#103: Bot + 人間 混在 → 除外
cat > "$TMP/data/reviews-acme-qux-103.json" <<'EOF'
[{"user": {"type": "Bot", "login": "copilot"}, "state": "COMMENTED"},
 {"user": {"type": "User", "login": "someone"}, "state": "COMMENTED"}]
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

# --- 通常ケース ---
: > "$GH_STUB_LOG"
out=$(bash "$SCRIPT")
assert_exit 'normal run: exit 0' $? 0

# gh search prs 呼び出しに --draft=false が含まれる（サーバー側で Draft 除外）
if grep -q -- '--draft=false' "$GH_STUB_LOG"; then
  pass=$((pass + 1)); printf 'PASS  search invocation passes --draft=false to exclude drafts\n'
else
  fail=$((fail + 1)); printf 'FAIL  search invocation passes --draft=false to exclude drafts\n'
fi

assert 'no-review PR kept as candidate' "$out" \
  'any(.prs[]; .owner == "acme" and .repo == "foo" and .number == 100 and .url == "https://github.com/acme/foo/pull/100")'
assert 'bot-only PR kept as candidate' "$out" \
  'any(.prs[]; .owner == "acme" and .repo == "bar" and .number == 101)'
assert 'human-reviewed PR excluded' "$out" \
  '[.prs[].number] | index(102) | not'
assert 'mixed (bot+human) PR excluded' "$out" \
  '[.prs[].number] | index(103) | not'
assert 'candidate count is 2' "$out" \
  '.prs | length == 2'
assert 'normal run: degraded false, no warnings' "$out" \
  '.degraded == false and (.warnings | length == 0)'

# --- 候補ゼロ ---
printf '[]\n' > "$TMP/data/search.json"
out_empty=$(bash "$SCRIPT")
assert_exit 'empty search: exit 0' $? 0
assert 'empty search yields empty prs' "$out_empty" \
  '.prs == [] and .degraded == false'

# --- search 失敗 → 非ゼロ exit + stderr ---
GH_STUB_FAIL_SEARCH=1 bash "$SCRIPT" >/dev/null 2>"$TMP/err.txt"
assert_exit 'search failure: non-zero exit' $? 1
if [ -s "$TMP/err.txt" ]; then
  pass=$((pass + 1)); printf 'PASS  search failure: stderr message present\n'
else
  fail=$((fail + 1)); printf 'FAIL  search failure: stderr message present\n'
fi

# --- 個別 API 失敗 → warnings に蓄積・degraded: true ---
cat > "$TMP/data/search.json" <<'EOF'
[
  {"number": 100, "url": "https://github.com/acme/foo/pull/100", "repository": {"nameWithOwner": "acme/foo"}},
  {"number": 200, "url": "https://github.com/acme/dead/pull/200", "repository": {"nameWithOwner": "acme/dead"}}
]
EOF
out_deg=$(GH_STUB_FAIL_FOR=acme-dead-200 bash "$SCRIPT")
assert_exit 'per-pr api failure: exit 0' $? 0
assert 'degraded flag on per-pr failure' "$out_deg" \
  '.degraded == true and (.warnings | length > 0) and (.warnings[0] | contains("acme/dead#200"))'
assert 'failed PR excluded from candidates' "$out_deg" \
  '[.prs[].number] | index(200) | not'
assert 'other PRs still processed on partial failure' "$out_deg" \
  'any(.prs[]; .number == 100)'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
