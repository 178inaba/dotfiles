#!/bin/bash

# statusline.sh のリグレッションテスト
#
# 実行: bash claude/.claude/tests/test-statusline.sh
# 失敗したケースがあれば exit 1 で終了する。
# キャッシュ（GIT_CACHE_BASE / USD_JPY_CACHE_FILE / PR_CACHE_BASE）を一時ディレクトリに
# 差し替えるため /tmp の実キャッシュには触れない。curl / gh も env 差し替えでスタブ化する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
STATUSLINE="$SCRIPT_DIR/../statusline.sh"

if [ ! -f "$STATUSLINE" ]; then
  printf 'ERROR: statusline script not found: %s\n' "$STATUSLINE" >&2
  exit 1
fi

TEST_TMPDIR=$(mktemp -d)
trap 'rm -rf "$TEST_TMPDIR"' EXIT
export GIT_CACHE_BASE="$TEST_TMPDIR/git-cache"
export USD_JPY_CACHE_FILE="$TEST_TMPDIR/usd-jpy-cache"

# curl スタブ: 応答ファイルがあればその内容を返し、無ければ失敗。呼び出しはログに記録
export CURL_STUB_LOG="$TEST_TMPDIR/curl-calls.log"
export CURL_STUB_RESPONSE="$TEST_TMPDIR/curl-response"
cat > "$TEST_TMPDIR/curl-stub" <<'EOF'
#!/bin/bash
echo "called" >> "$CURL_STUB_LOG"
[ -f "$CURL_STUB_RESPONSE" ] || exit 1
cat "$CURL_STUB_RESPONSE"
EOF
chmod +x "$TEST_TMPDIR/curl-stub"
export CURL_BIN="$TEST_TMPDIR/curl-stub"

# gh スタブ: サブコマンドに応じた応答ファイルがあればその内容を返し、無ければ失敗。
# 呼び出しはログに記録。GH_STUB_DELAY で応答遅延を再現できる（再スポーン抑止テスト用）
export GH_STUB_LOG="$TEST_TMPDIR/gh-calls.log"
export GH_STUB_RESPONSE="$TEST_TMPDIR/gh-response"
export GH_STUB_REPO_RESPONSE="$TEST_TMPDIR/gh-repo-response"
cat > "$TEST_TMPDIR/gh-stub" <<'EOF'
#!/bin/bash
echo "called $1" >> "$GH_STUB_LOG"
[ -n "${GH_STUB_DELAY:-}" ] && sleep "$GH_STUB_DELAY"
if [ "$1" = "repo" ]; then
  [ -f "$GH_STUB_REPO_RESPONSE" ] || exit 1
  cat "$GH_STUB_REPO_RESPONSE"
  exit 0
fi
[ -f "$GH_STUB_RESPONSE" ] || exit 1
cat "$GH_STUB_RESPONSE"
EOF
chmod +x "$TEST_TMPDIR/gh-stub"
export GH_BIN="$TEST_TMPDIR/gh-stub"
export PR_CACHE_BASE="$TEST_TMPDIR/pr-cache"

pass=0
fail=0

# 入力を流して出力に期待文字列がすべて含まれるか検証（want_substr は | 区切り）
run_test() {
  local name=$1 input=$2 want_substrs=$3
  local output got_exit substr ok=1
  output=$(printf '%s' "$input" | bash "$STATUSLINE" 2>/dev/null)
  got_exit=$?
  if [ "$got_exit" -ne 0 ]; then
    ok=0
  else
    while IFS= read -r substr; do
      case "$output" in
        *"$substr"*) ;;
        *) ok=0 ;;
      esac
    done <<< "$(printf '%s' "$want_substrs" | tr '|' '\n')"
  fi
  if [ "$ok" -eq 1 ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s\n  exit=%d output=%s\n' "$name" "$got_exit" "$output"
  fi
}

# 構文チェック
if bash -n "$STATUSLINE" 2>/dev/null; then
  pass=$((pass + 1))
  printf 'PASS  syntax check (bash -n)\n'
else
  fail=$((fail + 1))
  printf 'FAIL  syntax check (bash -n)\n'
fi

FULL_INPUT='{"session_id":"b257201c-ca28-4ad3-83a0-fa3f531eb808","workspace":{"current_dir":"'$PWD'","project_dir":"'$PWD'"},"model":{"display_name":"Opus"},"cost":{"total_cost_usd":1.23,"total_duration_ms":5400000},"context_window":{"used_percentage":42.5},"rate_limits":{"five_hour":{"used_percentage":35,"resets_at":9999999999},"seven_day":{"used_percentage":73,"resets_at":9999999999}}}'

# 全フィールドあり: モデル・コンテキストバー・レートリミット・コスト・経過時間・セッションIDが出る
run_test 'full fields' "$FULL_INPUT" '[Opus]|42%|5h:35%|7d:73%|$1.23|1h30m|b257201c-ca28-4ad3-83a0-fa3f531eb808'

# 対象フィールドなし: ディレクトリ表示のみでもエラーにならない
run_test 'workspace only' '{"workspace":{"current_dir":"'$PWD'","project_dir":"'$PWD'"}}' "$PWD"

# 空 stdin: フォールバックで PWD を表示
run_test 'empty stdin' '' "$PWD"

# キャッシュ: 1回目の実行でディレクトリキーのキャッシュファイルが作られ、キーが一致する
cache_file=$(ls "$TEST_TMPDIR"/git-cache-* 2>/dev/null | head -1)
if [ -n "$cache_file" ] && [ "$(sed -n 2p "$cache_file")" = "$PWD" ]; then
  pass=$((pass + 1))
  printf 'PASS  cache file keyed by directory\n'
else
  fail=$((fail + 1))
  printf 'FAIL  cache file keyed by directory (file=%s)\n' "${cache_file:-none}"
fi

# --- git 状態別のブランチ行検証 ---

# 指定ディレクトリで実行し、ANSI除去済みの2行目（ブランチ行）が期待値と完全一致するか検証
run_git_test() {
  local name=$1 dir=$2 want=$3
  local output line2
  rm -f "$GIT_CACHE_BASE"-*
  output=$(cd "$dir" && printf '{"workspace":{"current_dir":"%s","project_dir":"%s"}}' "$dir" "$dir" | bash "$STATUSLINE" 2>/dev/null)
  line2=$(printf '%s' "$output" | sed -n 2p | sed $'s/\x1b\\[[0-9;]*m//g')
  if [ "$line2" = "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got %s, want %s)\n' "$name" "$line2" "$want"
  fi
}

GIT="git -c user.email=test@example.com -c user.name=test -c commit.gpgsign=false"

ORIGIN="$TEST_TMPDIR/origin.git"
# -b main: init.defaultBranch 未設定の環境（stow 前・CI）でも clone を成立させる
git init -q --bare -b main "$ORIGIN"

# repo_a: staged +1 / modified ~1 / ahead ↑1（未追跡はカウント対象外）
REPO_A="$TEST_TMPDIR/repo-a"
git init -q -b main "$REPO_A"
(
  cd "$REPO_A"
  echo a > tracked.txt
  git add tracked.txt
  $GIT commit -qm c1
  git remote add origin "$ORIGIN"
  git push -qu origin main
  $GIT commit -q --allow-empty -m c2
  echo b >> tracked.txt
  echo s > staged.txt
  git add staged.txt
  echo u > untracked.txt
)
run_git_test 'git: staged/modified/ahead (untracked excluded)' "$REPO_A" '(main +1 ~1 ↑1)'

# repo_b: behind ↓1（push 後に1コミット戻す）
REPO_B="$TEST_TMPDIR/repo-b"
git clone -q "$ORIGIN" "$REPO_B"
(
  cd "$REPO_B"
  $GIT commit -q --allow-empty -m c3
  git push -q origin main
  git reset -q --hard HEAD~1
)
run_git_test 'git: behind' "$REPO_B" '(main ↓1)'

# detached HEAD: ブランチ名なしの () 表示
(cd "$REPO_B" && git switch -q --detach HEAD)
run_git_test 'git: detached HEAD' "$REPO_B" '()'

# repo_c: コンフリクト（unmerged）は staged/modified の両方に数える（v1 の UU 両カウントと等価）
REPO_C="$TEST_TMPDIR/repo-c"
git init -q -b main "$REPO_C"
(
  cd "$REPO_C"
  echo base > conflict.txt
  git add conflict.txt
  $GIT commit -qm c1
  git switch -qc side
  echo side > conflict.txt
  $GIT commit -qam side
  git switch -q main
  echo main > conflict.txt
  $GIT commit -qam main
  git merge -q side >/dev/null 2>&1 || true
)
run_git_test 'git: unmerged conflict' "$REPO_C" '(main +1 ~1)'

# 非gitディレクトリ: ブランチ行が出ない（2行目はセッション情報行 = 空表示）
PLAIN="$TEST_TMPDIR/plain"
mkdir -p "$PLAIN"
run_git_test 'non-git dir' "$PLAIN" ''

# --- セッションID表示（2行目） ---

SESSION_ID='b257201c-ca28-4ad3-83a0-fa3f531eb808'

# 指定ディレクトリ + session_id 付き入力で実行し、ANSI除去済みの2行目が期待値と完全一致するか検証
run_session_test() {
  local name=$1 dir=$2 want=$3
  local output line2
  rm -f "$GIT_CACHE_BASE"-*
  output=$(cd "$dir" && printf '{"session_id":"%s","workspace":{"current_dir":"%s","project_dir":"%s"}}' "$SESSION_ID" "$dir" "$dir" | bash "$STATUSLINE" 2>/dev/null)
  line2=$(printf '%s' "$output" | sed -n 2p | sed $'s/\x1b\\[[0-9;]*m//g')
  if [ "$line2" = "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got %s, want %s)\n' "$name" "$line2" "$want"
  fi
}

run_session_test 'session id: appended to branch line' "$REPO_C" "(main +1 ~1) $SESSION_ID"
run_session_test 'session id: shown alone in non-git dir' "$PLAIN" "$SESSION_ID"

# --- USD/JPY 円表示 ---

YEN_INPUT='{"workspace":{"current_dir":"'$PWD'","project_dir":"'$PWD'"},"model":{"display_name":"Opus"},"cost":{"total_cost_usd":1.23}}'
FRANKFURTER_RESPONSE='{"amount":1.0,"base":"USD","date":"2026-07-14","rates":{"JPY":162.22}}'

yen_reset() {
  rm -f "$USD_JPY_CACHE_FILE" "$USD_JPY_CACHE_FILE".* "$CURL_STUB_RESPONSE" "$CURL_STUB_LOG"
}

# バックグラウンド更新の完了待ち（条件成立まで最大2秒ポーリング）
wait_for() {
  local i
  for i in $(seq 1 40); do
    eval "$1" && return 0
    sleep 0.05
  done
  return 1
}

check() {
  local name=$1 cond=$2
  if eval "$cond"; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s\n' "$name"
  fi
}

# フレッシュなキャッシュ: 円換算で表示（1.23 * 160.00 = 196.8 → ¥197）
yen_reset
printf '%s\n%s\n' "$(date +%s)" "160.00" > "$USD_JPY_CACHE_FILE"
run_test 'yen: fresh cache converts to JPY' "$YEN_INPUT" '¥197'

# キャッシュなし: USD フォールバックしつつバックグラウンドでキャッシュ生成
yen_reset
printf '%s' "$FRANKFURTER_RESPONSE" > "$CURL_STUB_RESPONSE"
run_test 'yen: no cache falls back to USD' "$YEN_INPUT" '$1.23'
check 'yen: background fetch fills cache' \
  'wait_for "[ -f \"\$USD_JPY_CACHE_FILE\" ] && [ \"\$(sed -n 2p \"\$USD_JPY_CACHE_FILE\")\" = 162.22 ]"'

# 直後の描画は生成されたキャッシュから円表示（1.23 * 162.22 = 199.53 → ¥200）
run_test 'yen: next render uses fetched cache' "$YEN_INPUT" '¥200'

# 期限切れキャッシュ: 手元の値で表示継続（stale-while-revalidate）+ バックグラウンド更新
yen_reset
printf '%s' "$FRANKFURTER_RESPONSE" > "$CURL_STUB_RESPONSE"
printf '1\n100.00\n' > "$USD_JPY_CACHE_FILE"
run_test 'yen: stale cache shown while revalidating' "$YEN_INPUT" '¥123'
check 'yen: stale cache refreshed in background' \
  'wait_for "[ \"\$(sed -n 2p \"\$USD_JPY_CACHE_FILE\")\" = 162.22 ]"'

# 不正なレート値: USD フォールバック
yen_reset
printf '%s\ngarbage\n' "$(date +%s)" > "$USD_JPY_CACHE_FILE"
run_test 'yen: invalid cached rate falls back to USD' "$YEN_INPUT" '$1.23'

# 再試行スロットリング: 直近に試行済みなら curl を呼ばない
yen_reset
date +%s > "$USD_JPY_CACHE_FILE.attempt"
run_test 'yen: recent attempt suppresses refetch' "$YEN_INPUT" '$1.23'
sleep 0.3
check 'yen: no curl call while throttled' '[ ! -f "$CURL_STUB_LOG" ]'

# 取得失敗（curl 異常終了）: キャッシュを書かず USD フォールバック維持
yen_reset
run_test 'yen: fetch failure keeps USD fallback' "$YEN_INPUT" '$1.23'
sleep 0.3
check 'yen: failed fetch writes no cache' '[ ! -f "$USD_JPY_CACHE_FILE" ]'

# --- PR 表示（2行目） ---

# upstream 同期済みのクリーンな repo。clone で origin/HEAD（デフォルトブランチ = main）が
# 設定される。デフォルトブランチでは PR を表示しないため、PR テストは feature ブランチで行う
REPO_PR="$TEST_TMPDIR/repo-pr"
git clone -q "$ORIGIN" "$REPO_PR"
(cd "$REPO_PR" && git switch -qc feat)

ESC_GREEN=$(printf '\033[0;32m')
ESC_RED=$(printf '\033[0;31m')
ESC_PR_YELLOW=$(printf '\033[38;5;220m')
ESC_NC=$(printf '\033[0m')
ESC_UL=$(printf '\033[4m')
ESC_UL_OFF=$(printf '\033[24m')
OSC8=$(printf '\033]8;;')
BEL=$(printf '\a')

pr_reset() {
  rm -f "$PR_CACHE_BASE"-* "$GH_STUB_RESPONSE" "$GH_STUB_REPO_RESPONSE" "$GH_STUB_LOG"
}

# 指定ディレクトリで描画し、2行目（ブランチ行）を返す。_raw はエスケープシーケンス付き
render_line2_raw() {
  local dir=$1
  (cd "$dir" && printf '{"workspace":{"current_dir":"%s","project_dir":"%s"}}' "$dir" "$dir" | bash "$STATUSLINE" 2>/dev/null) | sed -n 2p
}
render_line2() {
  render_line2_raw "$1" | sed $'s/\x1b\\[[0-9;]*m//g; s/\x1b]8;;[^\x07]*\x07//g'
}
line2_is() { [ "$(render_line2 "$1")" = "$2" ]; }
pr_line2_is() { line2_is "$REPO_PR" "$1"; }

# バックグラウンド更新で書かれた本キャッシュ（attempt を除く）の存在確認
pr_cache_written() { ls "$PR_CACHE_BASE"-* 2>/dev/null | grep -v attempt | grep -q .; }

# OPEN PR: 初回描画は表示なし（非同期フェッチ）、更新完了後の描画で PR 番号が出る
pr_reset
printf '{"number":123,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://example.test/pull/123"}' > "$GH_STUB_RESPONSE"
check 'pr: first render shows nothing before fetch' 'pr_line2_is "(feat)"'
check 'pr: open PR shown after background fetch' 'wait_for "pr_line2_is \"(feat) PR #123\""'

# フッターバッジと同じ見た目: 下線付きの番号部分のみ OSC 8 ハイパーリンク（"PR " は非リンク）
check 'pr: only underlined number wrapped in OSC 8 hyperlink' \
  'render_line2_raw "$REPO_PR" | grep -qF "PR ${ESC_PR_YELLOW}${OSC8}https://example.test/pull/123${BEL}${ESC_UL}#123${ESC_UL_OFF}${OSC8}${BEL}"'

# 表示順: ブランチ情報 → PR → セッションID
pr_session_line2=$(cd "$REPO_PR" && printf '{"session_id":"%s","workspace":{"current_dir":"%s","project_dir":"%s"}}' "$SESSION_ID" "$REPO_PR" "$REPO_PR" | bash "$STATUSLINE" 2>/dev/null | sed -n 2p | sed $'s/\x1b\\[[0-9;]*m//g; s/\x1b]8;;[^\x07]*\x07//g')
check 'pr: ordered between branch info and session id' '[ "$pr_session_line2" = "(feat) PR #123 $SESSION_ID" ]'

# reviewDecision の色分け（本家フッターバッジと同じマッピング）:
# "PR " は無色（テーマのデフォルト前景色 = 直前が branch 部の NC）、番号部分が
# APPROVED=緑 / CHANGES_REQUESTED=赤 / レビュー待ち=固定黄 / draft=無色
pr_reset
printf '{"number":124,"reviewDecision":"APPROVED","state":"OPEN","isDraft":false,"url":"https://example.test/pull/124"}' > "$GH_STUB_RESPONSE"
check 'pr: approved shown' 'wait_for "pr_line2_is \"(feat) PR #124\""'
check 'pr: approved colored green' \
  'render_line2_raw "$REPO_PR" | grep -qF "${ESC_NC} PR ${ESC_GREEN}${OSC8}https://example.test/pull/124${BEL}${ESC_UL}#124"'

pr_reset
printf '{"number":125,"reviewDecision":"CHANGES_REQUESTED","state":"OPEN","isDraft":false,"url":"https://example.test/pull/125"}' > "$GH_STUB_RESPONSE"
check 'pr: changes_requested shown' 'wait_for "pr_line2_is \"(feat) PR #125\""'
check 'pr: changes_requested colored red' \
  'render_line2_raw "$REPO_PR" | grep -qF "${ESC_NC} PR ${ESC_RED}${OSC8}https://example.test/pull/125${BEL}${ESC_UL}#125"'

pr_reset
printf '{"number":126,"reviewDecision":"","state":"OPEN","isDraft":true,"url":"https://example.test/pull/126"}' > "$GH_STUB_RESPONSE"
check 'pr: draft shown' 'wait_for "pr_line2_is \"(feat) PR #126\""'
check 'pr: draft uncolored (default fg)' \
  'render_line2_raw "$REPO_PR" | grep -qF "${ESC_NC} PR ${OSC8}https://example.test/pull/126${BEL}${ESC_UL}#126"'

# レビュー待ちは reviewDecision が REVIEW_REQUIRED（必須レビュー設定あり）と
# 空文字（設定なし）の2形態がある。どちらも黄
pr_reset
printf '{"number":130,"reviewDecision":"REVIEW_REQUIRED","state":"OPEN","isDraft":false,"url":"https://example.test/pull/130"}' > "$GH_STUB_RESPONSE"
check 'pr: review_required shown' 'wait_for "pr_line2_is \"(feat) PR #130\""'
check 'pr: review_required colored yellow' \
  'render_line2_raw "$REPO_PR" | grep -qF "${ESC_NC} PR ${ESC_PR_YELLOW}${OSC8}https://example.test/pull/130${BEL}${ESC_UL}#130"'

pr_reset
printf '{"number":134,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://example.test/pull/134"}' > "$GH_STUB_RESPONSE"
check 'pr: empty decision shown' 'wait_for "pr_line2_is \"(feat) PR #134\""'
check 'pr: empty decision colored yellow' \
  'render_line2_raw "$REPO_PR" | grep -qF "${ESC_NC} PR ${ESC_PR_YELLOW}${OSC8}https://example.test/pull/134${BEL}${ESC_UL}#134"'

# OPEN 以外（マージ済み等）は表示しない
pr_reset
printf '{"number":127,"reviewDecision":"APPROVED","state":"MERGED","isDraft":false}' > "$GH_STUB_RESPONSE"
render_line2 "$REPO_PR" >/dev/null
check 'pr: merged PR hidden' 'wait_for "pr_cache_written" && pr_line2_is "(feat)"'

# gh 失敗（PR 無し・オフライン等）: 「無し」としてキャッシュし、新鮮な間は再フェッチしない
pr_reset
render_line2 "$REPO_PR" >/dev/null
check 'pr: gh failure cached as none' 'wait_for "pr_cache_written" && pr_line2_is "(feat)"'
rm -f "$GH_STUB_LOG"
render_line2 "$REPO_PR" >/dev/null
sleep 0.3
check 'pr: fresh none-cache suppresses refetch' '[ ! -f "$GH_STUB_LOG" ]'

# フェッチ完了前の再描画では gh を多重起動しない（attempt ファイルによる抑止）
pr_reset
printf '{"number":128,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://example.test/pull/128"}' > "$GH_STUB_RESPONSE"
export GH_STUB_DELAY=0.5
render_line2 "$REPO_PR" >/dev/null
render_line2 "$REPO_PR" >/dev/null
unset GH_STUB_DELAY
check 'pr: in-flight fetch not re-spawned' \
  'wait_for "pr_line2_is \"(feat) PR #128\"" && [ "$(grep -c called "$GH_STUB_LOG")" -eq 1 ]'

# ブランチ切替でキャッシュが即無効化される（キーが dir+branch のため）
pr_reset
printf '{"number":129,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://example.test/pull/129"}' > "$GH_STUB_RESPONSE"
check 'pr: cached on feat' 'wait_for "pr_line2_is \"(feat) PR #129\""'
(cd "$REPO_PR" && git switch -qc feature)
rm -f "$GIT_CACHE_BASE"-*
check 'pr: branch switch invalidates cache' 'pr_line2_is "(feature)"'
# 新ブランチのキーで再フェッチされる（完了待ちを兼ね、次テストのログ削除とのレースを防ぐ）
check 'pr: new branch fetches its own PR' 'wait_for "pr_line2_is \"(feature) PR #129\""'

# detached HEAD・非 git ディレクトリでは gh を起動しない
pr_reset
(cd "$REPO_B" && git switch -q --detach HEAD)
rm -f "$GIT_CACHE_BASE"-*
render_line2 "$REPO_B" >/dev/null
render_line2 "$PLAIN" >/dev/null
sleep 0.3
check 'pr: detached HEAD / non-git dir do not invoke gh' '[ ! -f "$GH_STUB_LOG" ]'

# --- デフォルトブランチでは PR を表示しない ---

repo_a_line2_is() { line2_is "$REPO_A" "$1"; }

# origin/HEAD（clone 時に設定済み）から判定し、PR があっても表示せず gh 起動もしない
pr_reset
printf '{"number":131,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://example.test/pull/131"}' > "$GH_STUB_RESPONSE"
(cd "$REPO_PR" && git switch -q main)
rm -f "$GIT_CACHE_BASE"-*
render_line2 "$REPO_PR" >/dev/null
check 'pr: default branch hides PR' 'wait_for "pr_cache_written" && pr_line2_is "(main)"'
check 'pr: default branch does not invoke gh' '[ ! -f "$GH_STUB_LOG" ]'

# origin/HEAD 未設定の repo（REPO_A: remote add のみで clone ではない）は gh repo view で補完
pr_reset
printf '{"number":132,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://example.test/pull/132"}' > "$GH_STUB_RESPONSE"
printf 'main\n' > "$GH_STUB_REPO_RESPONSE"
render_line2 "$REPO_A" >/dev/null
check 'pr: default branch via gh repo view fallback hides PR' \
  'wait_for "pr_cache_written" && repo_a_line2_is "(main +1 ~1 ↑1)"'

# デフォルトブランチが判定不能（origin/HEAD 未設定 + gh repo view 失敗）なら表示する側に倒す
pr_reset
printf '{"number":133,"reviewDecision":"","state":"OPEN","isDraft":false,"url":"https://example.test/pull/133"}' > "$GH_STUB_RESPONSE"
render_line2 "$REPO_A" >/dev/null
check 'pr: unknown default branch still shows PR' \
  'wait_for "repo_a_line2_is \"(main +1 ~1 ↑1) PR #133\""'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
