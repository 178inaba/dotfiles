#!/bin/bash

# statusline.sh のリグレッションテスト
#
# 実行: bash claude/.claude/tests/test-statusline.sh
# 失敗したケースがあれば exit 1 で終了する。
# GIT_CACHE_BASE を一時ディレクトリに差し替えるため /tmp の実キャッシュには触れない。

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

FULL_INPUT='{"session_id":"test","workspace":{"current_dir":"'$PWD'","project_dir":"'$PWD'"},"model":{"display_name":"Opus"},"cost":{"total_cost_usd":1.23,"total_duration_ms":5400000},"context_window":{"used_percentage":42.5},"rate_limits":{"five_hour":{"used_percentage":35,"resets_at":9999999999},"seven_day":{"used_percentage":73,"resets_at":9999999999}}}'

# 全フィールドあり: モデル・コンテキストバー・レートリミット・コスト・経過時間が出る
run_test 'full fields' "$FULL_INPUT" '[Opus]|42%|5h:35%|7d:73%|$1.23|1h30m'

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

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
