#!/bin/bash

# worktree-edit-guard.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-worktree-edit-guard.sh
# 失敗したケースがあれば exit 1 で終了する。
#
# git 状態に依存するフックのため、mktemp -d の使い捨てリポジトリと
# linked worktree をフィクスチャとして構築する（実リポジトリには触れない）。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../worktree-edit-guard.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

TMP_BASE=$(mktemp -d)
trap 'rm -rf "$TMP_BASE"' EXIT

MAIN="$TMP_BASE/repo"
OUTSIDE="$TMP_BASE/outside"
mkdir -p "$MAIN" "$OUTSIDE"
printf 'note\n' > "$OUTSIDE/notes.md"

git -C "$MAIN" init -q
git -C "$MAIN" config user.email test@example.com
git -C "$MAIN" config user.name test
mkdir -p "$MAIN/api"
printf 'x\n' > "$MAIN/file.txt"
printf 'y\n' > "$MAIN/api/handler.go"
git -C "$MAIN" add -A
git -C "$MAIN" commit -qm init

git -C "$MAIN" worktree add -q "$MAIN/.claude/worktrees/feature-test" -b worktree-feature-test
git -C "$MAIN" worktree add -q "$MAIN/.claude/worktrees/other-wt" -b worktree-other-wt
WT="$MAIN/.claude/worktrees/feature-test"
OTHER="$MAIN/.claude/worktrees/other-wt"

# 命名規約外の配置の worktree（リポジトリ内・リポジトリ外）
git -C "$MAIN" worktree add -q "$MAIN/wt-manual" -b wt-manual
git -C "$MAIN" worktree add -q "$TMP_BASE/ext-wt" -b ext-wt

# bare リポジトリ + worktree 構成（メインツリーが存在しないレイアウト）
BARE="$TMP_BASE/bare.git"
git clone --bare -q "$MAIN" "$BARE"
git -C "$BARE" worktree add -q "$TMP_BASE/bare-wt1" -b bare-wt1
git -C "$BARE" worktree add -q "$TMP_BASE/bare-wt2" -b bare-wt2
printf 'z\n' > "$TMP_BASE/unrelated.txt"

# 正規化確認用: worktree への symlink 経由パス
ln -s "$WT" "$TMP_BASE/link-wt"

pass=0
fail=0

run_test() {
  local name=$1 input=$2 want_exit=$3
  local got_exit
  printf '%s' "$input" | "$HOOK" >/dev/null 2>&1
  got_exit=$?
  if [ "$got_exit" -eq "$want_exit" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got exit %d, want %d)\n' "$name" "$got_exit" "$want_exit"
  fi
}

# ブロックメッセージが worktree 側の対応パスを提示することの確認
run_message_test() {
  local name=$1 input=$2 want_substr=$3
  local msg
  msg=$(printf '%s' "$input" | "$HOOK" 2>&1 >/dev/null || true)
  if printf '%s' "$msg" | grep -qF "$want_substr"; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (message lacks %s)\n' "$name" "$want_substr"
  fi
}

edit_json() {
  printf '{"tool_name":"%s","tool_input":{"file_path":"%s"},"cwd":"%s"}' "$1" "$2" "$3"
}

# 通過すべきケース (exit 0)
run_test 'tool_name=Bash (not a target tool)'      '{"tool_name":"Bash","tool_input":{"command":"ls"},"cwd":"'"$WT"'"}' 0
run_test 'Edit without file_path'                  '{"tool_name":"Edit","tool_input":{},"cwd":"'"$WT"'"}' 0
run_test 'Edit without cwd'                        "$(printf '{"tool_name":"Edit","tool_input":{"file_path":"%s"}}' "$MAIN/file.txt")" 0
run_test 'Edit with relative file_path'            "$(edit_json Edit "api/handler.go" "$WT")" 0
run_test 'cwd outside any git repo'                "$(edit_json Edit "$MAIN/file.txt" "$OUTSIDE")" 0
run_test 'cwd in main worktree, edit wt file'      "$(edit_json Edit "$WT/file.txt" "$MAIN")" 0
run_test 'cwd in wt, edit file inside wt'          "$(edit_json Edit "$WT/api/handler.go" "$WT")" 0
run_test 'cwd in wt, write new file inside wt'     "$(edit_json Write "$WT/new/dir/new.txt" "$WT")" 0
run_test 'cwd in wt, edit file outside repo'       "$(edit_json Edit "$OUTSIDE/notes.md" "$WT")" 0
run_test 'cwd in wt, path prefix collision'        "$(edit_json Edit "$TMP_BASE/repo-extra/x.txt" "$WT")" 0
run_test 'cwd in wt, edit wt file via symlink'     "$(edit_json Edit "$TMP_BASE/link-wt/file.txt" "$WT")" 0
run_test 'bare layout: edit file outside any wt'   "$(edit_json Edit "$TMP_BASE/unrelated.txt" "$TMP_BASE/bare-wt1")" 0

# ブロックされるべきケース (exit 2)
run_test 'cwd in wt, edit main tree file'          "$(edit_json Edit "$MAIN/api/handler.go" "$WT")" 2
run_test 'cwd in wt subdir, edit main tree file'   "$(edit_json Edit "$MAIN/api/handler.go" "$WT/api")" 2
run_test 'cwd in wt, write new file in main tree'  "$(edit_json Write "$MAIN/api/new.go" "$WT")" 2
run_test 'cwd in wt, edit sibling worktree file'   "$(edit_json Edit "$OTHER/file.txt" "$WT")" 2
run_test 'cwd in wt, edit non-standard sibling wt' "$(edit_json Edit "$MAIN/wt-manual/file.txt" "$WT")" 2
run_test 'cwd in wt, edit external sibling wt'     "$(edit_json Edit "$TMP_BASE/ext-wt/file.txt" "$WT")" 2
run_test 'bare layout: edit sibling wt file'       "$(edit_json Edit "$TMP_BASE/bare-wt2/file.txt" "$TMP_BASE/bare-wt1")" 2
run_test 'cwd in wt, NotebookEdit main tree file'  '{"tool_name":"NotebookEdit","tool_input":{"notebook_path":"'"$MAIN/nb.ipynb"'"},"cwd":"'"$WT"'"}' 2

# 提示パス: メインツリー相対の付け替え
run_message_test 'block message suggests worktree path' \
  "$(edit_json Edit "$MAIN/api/handler.go" "$WT")" \
  "/.claude/worktrees/feature-test/api/handler.go"
# 提示パス: メインツリー配下の sibling worktree は最長 prefix（sibling 側）起点で付け替える
run_message_test 'block message maps sibling wt by longest prefix' \
  "$(edit_json Edit "$MAIN/wt-manual/file.txt" "$WT")" \
  "/.claude/worktrees/feature-test/file.txt"

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
