#!/bin/bash

# slack-notify.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-slack-notify.sh
# 失敗したケースがあれば exit 1 で終了する。
#
# curl は PATH 先頭のスタブに差し替えて呼び出し引数をファイルに記録する
# （実 webhook・ネットワークには触れない）。git 状態に依存するラベル導出は
# mktemp -d の使い捨てリポジトリ・worktree をフィクスチャとして検証する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../slack-notify.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

TMP_BASE=$(mktemp -d)
trap 'rm -rf "$TMP_BASE"' EXIT

CURL_LOG="$TMP_BASE/curl-args.log"
mkdir -p "$TMP_BASE/bin"
cat > "$TMP_BASE/bin/curl" <<EOF
#!/bin/bash
printf '%s\n' "\$*" >> "$CURL_LOG"
EOF
chmod +x "$TMP_BASE/bin/curl"
export PATH="$TMP_BASE/bin:$PATH"

OUTSIDE="$TMP_BASE/outside"
mkdir -p "$OUTSIDE"

MAIN="$TMP_BASE/myrepo"
mkdir -p "$MAIN/api"
git -C "$MAIN" init -q
git -C "$MAIN" config user.email test@example.com
git -C "$MAIN" config user.name test
printf 'x\n' > "$MAIN/file.txt"
printf 'y\n' > "$MAIN/api/handler.go"
git -C "$MAIN" add -A
git -C "$MAIN" commit -qm init

git -C "$MAIN" worktree add -q "$MAIN/.claude/worktrees/feature-x" -b feature-x
git -C "$MAIN" worktree add -q "$MAIN/.claude/worktrees/feat/nested" -b feat-nested
git -C "$MAIN" worktree add -q "$TMP_BASE/manual-wt" -b manual-wt

BARE="$TMP_BASE/bare.git"
git clone --bare -q "$MAIN" "$BARE"
git -C "$BARE" worktree add -q "$TMP_BASE/bare-wt" -b bare-wt

pass=0
fail=0

notif_json() {
  printf '{"message":"%s","notification_type":"%s","cwd":"%s"}' "$1" "$2" "$3"
}

# フック実行後、curl に渡った本文が期待文字列を含むことを確認する
run_text_test() {
  local name=$1 input=$2 want_substr=$3
  : > "$CURL_LOG"
  printf '%s' "$input" | CLAUDE_SLACK_WEBHOOK="https://example.invalid/webhook" "$HOOK" >/dev/null 2>&1
  if grep -qF "$want_substr" "$CURL_LOG"; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (curl args lack %s; got: %s)\n' "$name" "$want_substr" "$(cat "$CURL_LOG")"
  fi
}

# フック実行後、curl が呼ばれないことを確認する
run_no_curl_test() {
  local name=$1 input=$2 webhook=$3
  : > "$CURL_LOG"
  printf '%s' "$input" | CLAUDE_SLACK_WEBHOOK="$webhook" "$HOOK" >/dev/null 2>&1
  local got_exit=$?
  if [ "$got_exit" -eq 0 ] && [ ! -s "$CURL_LOG" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (exit %d, curl log: %s)\n' "$name" "$got_exit" "$(cat "$CURL_LOG")"
  fi
}

# 通知を送らないケース
run_no_curl_test 'webhook unset: exit 0 without curl' \
  "$(notif_json 'hello' idle_prompt "$MAIN")" ''
run_no_curl_test 'empty message: no curl' \
  "$(notif_json '' idle_prompt "$MAIN")" 'https://example.invalid/webhook'
run_no_curl_test 'malformed json: no curl' \
  'not-json' 'https://example.invalid/webhook'

# ラベル導出
run_text_test 'non-git cwd: basename fallback' \
  "$(notif_json 'hello' idle_prompt "$OUTSIDE")" \
  '[outside] (idle_prompt) hello'
run_text_test 'main tree root: repo name' \
  "$(notif_json 'hello' idle_prompt "$MAIN")" \
  '[myrepo] (idle_prompt) hello'
run_text_test 'main tree subdir: repo name, not subdir' \
  "$(notif_json 'hello' idle_prompt "$MAIN/api")" \
  '[myrepo] (idle_prompt) hello'
run_text_test 'standard worktree: repo:worktree' \
  "$(notif_json 'hello' idle_prompt "$MAIN/.claude/worktrees/feature-x")" \
  '[myrepo:feature-x] (idle_prompt) hello'
run_text_test 'worktree subdir: repo:worktree' \
  "$(notif_json 'hello' idle_prompt "$MAIN/.claude/worktrees/feature-x/api")" \
  '[myrepo:feature-x] (idle_prompt) hello'
run_text_test 'slash-named worktree: name kept intact' \
  "$(notif_json 'hello' idle_prompt "$MAIN/.claude/worktrees/feat/nested")" \
  '[myrepo:feat/nested] (idle_prompt) hello'
run_text_test 'non-standard worktree path: repo:basename' \
  "$(notif_json 'hello' idle_prompt "$TMP_BASE/manual-wt")" \
  '[myrepo:manual-wt] (idle_prompt) hello'
run_text_test 'bare repo worktree: strips .git suffix' \
  "$(notif_json 'hello' idle_prompt "$TMP_BASE/bare-wt")" \
  '[bare:bare-wt] (idle_prompt) hello'

# 既存動作の維持
run_text_test 'missing notification_type: default label' \
  "$(printf '{"message":"hello","cwd":"%s"}' "$OUTSIDE")" \
  '[outside] (notification) hello'

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
