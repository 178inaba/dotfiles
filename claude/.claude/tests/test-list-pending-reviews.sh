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
if [ "${GH_STUB_FAIL_USER:-0}" = "1" ] && [ "$1" = "api" ] && [ "$2" = "user" ]; then exit 1; fi
case "$1" in
  search)
    cat "$GH_STUB_DATA/search.json"
    ;;
  api)
    # gh api user --jq .login — 認証ユーザーの login を返す
    if [ "$2" = "user" ]; then
      printf '%s\n' "${GH_STUB_ME:-me}"
      exit 0
    fi
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
export GH_STUB_ME="me"

# --- フィクスチャ ---
# 10 PR（すべて author == "author1"、reviewer の "someone"・"me" とは別）:
#   100 レビュー無し                    → 候補（初回依頼）
#   101 Bot のみ                        → 候補（初回依頼）
#   102 他人（人間）のみ                → 除外（他人が先に対応）
#   103 Bot + 他人（人間）              → 除外（他人が先に対応）
#   104 Bot のみ複数ページ              → 候補（初回依頼）
#   105 自分のみ                        → 候補（再レビュー依頼）
#   106 自分 + 他人（人間）             → 候補（再レビュー依頼で明示的に戻された）
#   107 Bot + 作者自身の COMMENTED       → 候補（作者は「他人」から除外）
#   108 Bot + 自分                       → 候補（Copilot 指摘への再レビュー依頼）
#   109 Bot + 作者 + 他人                → 除外（作者除外があっても真の他人は弾く）
cat > "$TMP/data/search.json" <<'EOF'
[
  {"number": 100, "url": "https://github.com/acme/foo/pull/100", "repository": {"nameWithOwner": "acme/foo"}, "author": {"login": "author1"}},
  {"number": 101, "url": "https://github.com/acme/bar/pull/101", "repository": {"nameWithOwner": "acme/bar"}, "author": {"login": "author1"}},
  {"number": 102, "url": "https://github.com/acme/baz/pull/102", "repository": {"nameWithOwner": "acme/baz"}, "author": {"login": "author1"}},
  {"number": 103, "url": "https://github.com/acme/qux/pull/103", "repository": {"nameWithOwner": "acme/qux"}, "author": {"login": "author1"}},
  {"number": 104, "url": "https://github.com/acme/page/pull/104", "repository": {"nameWithOwner": "acme/page"}, "author": {"login": "author1"}},
  {"number": 105, "url": "https://github.com/acme/self/pull/105", "repository": {"nameWithOwner": "acme/self"}, "author": {"login": "author1"}},
  {"number": 106, "url": "https://github.com/acme/both/pull/106", "repository": {"nameWithOwner": "acme/both"}, "author": {"login": "author1"}},
  {"number": 107, "url": "https://github.com/acme/author/pull/107", "repository": {"nameWithOwner": "acme/author"}, "author": {"login": "author1"}},
  {"number": 108, "url": "https://github.com/acme/botself/pull/108", "repository": {"nameWithOwner": "acme/botself"}, "author": {"login": "author1"}},
  {"number": 109, "url": "https://github.com/acme/triple/pull/109", "repository": {"nameWithOwner": "acme/triple"}, "author": {"login": "author1"}}
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

# acme/page#104: Bot のみが複数ページ（--paginate のページ連結出力）→ 候補
cat > "$TMP/data/reviews-acme-page-104.json" <<'EOF'
[{"user": {"type": "Bot", "login": "copilot"}, "state": "COMMENTED"}]
[{"user": {"type": "Bot", "login": "coderabbit"}, "state": "COMMENTED"}]
EOF

# acme/self#105: 自分のみが過去にレビュー済み → 候補（再レビュー依頼）
cat > "$TMP/data/reviews-acme-self-105.json" <<'EOF'
[{"user": {"type": "User", "login": "me"}, "state": "CHANGES_REQUESTED"}]
EOF

# acme/both#106: 自分の過去レビュー + 他人のレビュー → 候補（明示的な再指名を優先）
cat > "$TMP/data/reviews-acme-both-106.json" <<'EOF'
[{"user": {"type": "User", "login": "me"}, "state": "CHANGES_REQUESTED"},
 {"user": {"type": "User", "login": "someone"}, "state": "APPROVED"}]
EOF

# acme/author#107: Bot + 作者自身の COMMENTED（Reply スレッドが review として記録された想定）
# → 候補（作者は「他人」から除外されるため、他人レビュー 0 件として扱う）
cat > "$TMP/data/reviews-acme-author-107.json" <<'EOF'
[{"user": {"type": "Bot", "login": "copilot"}, "state": "COMMENTED"},
 {"user": {"type": "User", "login": "author1"}, "state": "COMMENTED"},
 {"user": {"type": "User", "login": "author1"}, "state": "COMMENTED"}]
EOF

# acme/botself#108: Bot + 自分の過去レビュー → 候補（Copilot 指摘への再レビュー依頼想定）
# 「Self ありの分岐でも Bot が正しくスキップされる」ことを直接担保する
cat > "$TMP/data/reviews-acme-botself-108.json" <<'EOF'
[{"user": {"type": "Bot", "login": "copilot"}, "state": "COMMENTED"},
 {"user": {"type": "User", "login": "me"}, "state": "CHANGES_REQUESTED"}]
EOF

# acme/triple#109: Bot + 作者 + 真の他人 → 除外
# 「author 除外が有効な状態でも、真の他人（Bot でも自分でも作者でもない人間）は
# 正しく検出されて exclude される」ことを直接担保する
cat > "$TMP/data/reviews-acme-triple-109.json" <<'EOF'
[{"user": {"type": "Bot", "login": "copilot"}, "state": "COMMENTED"},
 {"user": {"type": "User", "login": "author1"}, "state": "COMMENTED"},
 {"user": {"type": "User", "login": "someone"}, "state": "APPROVED"}]
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
assert 'human-reviewed PR excluded (other only)' "$out" \
  '[.prs[].number] | index(102) | not'
assert 'mixed (bot+other-human) PR excluded' "$out" \
  '[.prs[].number] | index(103) | not'
assert 'bot-only multi-page PR kept as candidate' "$out" \
  'any(.prs[]; .owner == "acme" and .repo == "page" and .number == 104)'
assert 'self-only PR kept as candidate (re-review request)' "$out" \
  'any(.prs[]; .owner == "acme" and .repo == "self" and .number == 105)'
assert 'self+other PR kept as candidate (re-review after others reviewed)' "$out" \
  'any(.prs[]; .owner == "acme" and .repo == "both" and .number == 106)'
assert 'author-only comments PR kept as candidate (author excluded from others count)' "$out" \
  'any(.prs[]; .owner == "acme" and .repo == "author" and .number == 107)'
assert 'bot+self PR kept as candidate (bot ignored in re-review branch)' "$out" \
  'any(.prs[]; .owner == "acme" and .repo == "botself" and .number == 108)'
assert 'bot+author+other PR excluded (author exclusion does not weaken other detection)' "$out" \
  '[.prs[].number] | index(109) | not'
assert 'candidate count is 7' "$out" \
  '.prs | length == 7'
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

# --- gh api user 失敗 → 非ゼロ exit + stderr ---
GH_STUB_FAIL_USER=1 bash "$SCRIPT" >/dev/null 2>"$TMP/err-user.txt"
assert_exit 'user fetch failure: non-zero exit' $? 1
if [ -s "$TMP/err-user.txt" ]; then
  pass=$((pass + 1)); printf 'PASS  user fetch failure: stderr message present\n'
else
  fail=$((fail + 1)); printf 'FAIL  user fetch failure: stderr message present\n'
fi

# --- 個別 API 失敗 → warnings に蓄積・degraded: true ---
cat > "$TMP/data/search.json" <<'EOF'
[
  {"number": 100, "url": "https://github.com/acme/foo/pull/100", "repository": {"nameWithOwner": "acme/foo"}, "author": {"login": "author1"}},
  {"number": 200, "url": "https://github.com/acme/dead/pull/200", "repository": {"nameWithOwner": "acme/dead"}, "author": {"login": "author1"}}
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

# --- author 欠損 → warnings に蓄積・degraded: true・候補から除外 ---
# 削除ユーザー等で .author が null になるケース。author 除外が黙って無効化される
# のを防ぐため degraded 経路に落として可視化する（$me 空チェックと対称）。
cat > "$TMP/data/search.json" <<'EOF'
[
  {"number": 100, "url": "https://github.com/acme/foo/pull/100", "repository": {"nameWithOwner": "acme/foo"}, "author": {"login": "author1"}},
  {"number": 300, "url": "https://github.com/acme/ghost/pull/300", "repository": {"nameWithOwner": "acme/ghost"}, "author": null}
]
EOF
out_ghost=$(bash "$SCRIPT")
assert_exit 'missing author: exit 0' $? 0
assert 'degraded flag on missing author' "$out_ghost" \
  '.degraded == true and (.warnings | any(. | contains("acme/ghost#300")))'
assert 'author-missing PR excluded from candidates' "$out_ghost" \
  '[.prs[].number] | index(300) | not'
assert 'other PRs still processed when author missing' "$out_ghost" \
  'any(.prs[]; .number == 100)'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
