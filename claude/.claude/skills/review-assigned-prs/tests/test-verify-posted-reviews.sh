#!/bin/bash

# verify-posted-reviews.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/review-assigned-prs/tests/test-verify-posted-reviews.sh
# gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/verify-posted-reviews.sh"

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
if [ "$1" = "api" ] && [ "$2" = "user" ]; then
  if [ "${GH_STUB_FAIL_USER:-0}" = "1" ]; then exit 1; fi
  printf 'me\n'
  exit 0
fi
case "$1" in
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
# acme/foo#100: 自分のレビューあり → posted: true
cat > "$TMP/data/reviews-acme-foo-100.json" <<'EOF'
[{"user": {"login": "someone", "type": "User"}, "state": "APPROVED"},
 {"user": {"login": "me", "type": "User"}, "state": "COMMENTED"}]
EOF

# acme/bar#101: 他人のレビューのみ → posted: false
cat > "$TMP/data/reviews-acme-bar-101.json" <<'EOF'
[{"user": {"login": "someone", "type": "User"}, "state": "APPROVED"},
 {"user": {"login": "copilot", "type": "Bot"}, "state": "COMMENTED"}]
EOF

# acme/baz#102: reviews-* ファイル無し → stub は [] を返す（レビューゼロ）→ posted: false

# acme/multi#300: --paginate のページ連結出力（自分のレビューが2ページ目）→ posted: true
cat > "$TMP/data/reviews-acme-multi-300.json" <<'EOF'
[{"user": {"login": "someone", "type": "User"}, "state": "APPROVED"}]
[{"user": {"login": "me", "type": "User"}, "state": "COMMENTED"}]
EOF

# acme/pending#400: 自分のレビューが未提出 draft（PENDING）のみ → posted: false
cat > "$TMP/data/reviews-acme-pending-400.json" <<'EOF'
[{"user": {"login": "me", "type": "User"}, "state": "PENDING"}]
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
out=$(bash "$SCRIPT" 'acme/foo#100' 'acme/bar#101' 'acme/baz#102')
assert_exit 'normal run: exit 0' $? 0

# reviews 取得に --paginate が含まれる（30件超のレビューで自分のレビューを取りこぼさないため）
if grep -q -- '--paginate' "$GH_STUB_LOG"; then
  pass=$((pass + 1)); printf 'PASS  reviews fetch passes --paginate\n'
else
  fail=$((fail + 1)); printf 'FAIL  reviews fetch passes --paginate\n'
fi

assert 'own review present -> posted true' "$out" \
  'any(.results[]; .owner == "acme" and .repo == "foo" and .number == 100 and .posted == true)'
assert 'others-only reviews -> posted false' "$out" \
  'any(.results[]; .owner == "acme" and .repo == "bar" and .number == 101 and .posted == false)'
assert 'zero reviews -> posted false' "$out" \
  'any(.results[]; .owner == "acme" and .repo == "baz" and .number == 102 and .posted == false)'
assert 'result count matches input count' "$out" \
  '.results | length == 3'
assert 'normal run: degraded false, no warnings' "$out" \
  '.degraded == false and (.warnings | length == 0)'

# --- --paginate のページ連結出力を1配列として集計 ---
out_multi=$(bash "$SCRIPT" 'acme/multi#300')
assert_exit 'paginated run: exit 0' $? 0
assert 'own review on later page -> posted true' "$out_multi" \
  'any(.results[]; .number == 300 and .posted == true)'

# --- 自分の PENDING（未提出 draft）は投稿済みとして数えない ---
out_pending=$(bash "$SCRIPT" 'acme/pending#400')
assert_exit 'pending-only run: exit 0' $? 0
assert 'own pending draft -> posted false' "$out_pending" \
  'any(.results[]; .number == 400 and .posted == false)'

# --- 個別 API 失敗 → warnings に蓄積・degraded: true・他 PR は処理継続 ---
out_deg=$(GH_STUB_FAIL_FOR=acme-dead-200 bash "$SCRIPT" 'acme/foo#100' 'acme/dead#200')
assert_exit 'per-pr api failure: exit 0' $? 0
assert 'degraded flag on per-pr failure' "$out_deg" \
  '.degraded == true and (.warnings | length > 0) and (.warnings[0] | contains("acme/dead#200"))'
assert 'failed PR excluded from results' "$out_deg" \
  '[.results[].number] | index(200) | not'
assert 'other PRs still verified on partial failure' "$out_deg" \
  'any(.results[]; .number == 100 and .posted == true)'

# --- gh api user 失敗 → 非ゼロ exit + stderr ---
GH_STUB_FAIL_USER=1 bash "$SCRIPT" 'acme/foo#100' >/dev/null 2>"$TMP/err.txt"
assert_exit 'user lookup failure: non-zero exit' $? 1
if [ -s "$TMP/err.txt" ]; then
  pass=$((pass + 1)); printf 'PASS  user lookup failure: stderr message present\n'
else
  fail=$((fail + 1)); printf 'FAIL  user lookup failure: stderr message present\n'
fi

# --- 引数なし → 非ゼロ exit + stderr ---
bash "$SCRIPT" >/dev/null 2>"$TMP/err-noargs.txt"
assert_exit 'no args: non-zero exit' $? 1
if [ -s "$TMP/err-noargs.txt" ]; then
  pass=$((pass + 1)); printf 'PASS  no args: stderr message present\n'
else
  fail=$((fail + 1)); printf 'FAIL  no args: stderr message present\n'
fi

# --- 引数形式不正 → 非ゼロ exit + stderr ---
bash "$SCRIPT" 'acme/foo' >/dev/null 2>"$TMP/err-badspec.txt"
assert_exit 'invalid spec: non-zero exit' $? 1
if [ -s "$TMP/err-badspec.txt" ]; then
  pass=$((pass + 1)); printf 'PASS  invalid spec: stderr message present\n'
else
  fail=$((fail + 1)); printf 'FAIL  invalid spec: stderr message present\n'
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
