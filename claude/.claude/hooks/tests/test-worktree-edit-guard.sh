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

edit_json() {
  printf '{"tool_name":"%s","tool_input":{"file_path":"%s"},"cwd":"%s"}' "$1" "$2" "$3"
}

# 通過すべきケース (exit 0)
run_test 'tool_name=Bash (not a target tool)'      '{"tool_name":"Bash","tool_input":{"command":"ls"},"cwd":"'"$WT"'"}' 0
run_test 'Edit without file_path'                  '{"tool_name":"Edit","tool_input":{},"cwd":"'"$WT"'"}' 0
run_test 'Edit without cwd'                        "$(printf '{"tool_name":"Edit","tool_input":{"file_path":"%s"}}' "$MAIN/file.txt")" 0
run_test 'cwd outside any git repo'                "$(edit_json Edit "$MAIN/file.txt" "$OUTSIDE")" 0
run_test 'cwd in main worktree, edit main file'    "$(edit_json Edit "$MAIN/file.txt" "$MAIN")" 0
run_test 'cwd in main worktree, edit wt file'      "$(edit_json Edit "$WT/file.txt" "$MAIN")" 0
run_test 'cwd in wt, edit file inside wt'          "$(edit_json Edit "$WT/api/handler.go" "$WT")" 0
run_test 'cwd in wt, write new file inside wt'     "$(edit_json Write "$WT/new/dir/new.txt" "$WT")" 0
run_test 'cwd in wt, edit file outside repo'       "$(edit_json Edit "$OUTSIDE/notes.md" "$WT")" 0
run_test 'cwd in wt, path prefix collision'        "$(edit_json Edit "$TMP_BASE/repo-extra/x.txt" "$WT")" 0
run_test 'cwd in wt, edit wt file via symlink'     "$(edit_json Edit "$TMP_BASE/link-wt/file.txt" "$WT")" 0

# ブロックされるべきケース (exit 2)
run_test 'cwd in wt, edit main tree file'          "$(edit_json Edit "$MAIN/api/handler.go" "$WT")" 2
run_test 'cwd in wt subdir, edit main tree file'   "$(edit_json Edit "$MAIN/api/handler.go" "$WT/api")" 2
run_test 'cwd in wt, write new file in main tree'  "$(edit_json Write "$MAIN/api/new.go" "$WT")" 2
run_test 'cwd in wt, edit sibling worktree file'   "$(edit_json Edit "$OTHER/file.txt" "$WT")" 2
run_test 'cwd in wt, NotebookEdit main tree file'  '{"tool_name":"NotebookEdit","tool_input":{"notebook_path":"'"$MAIN/nb.ipynb"'"},"cwd":"'"$WT"'"}' 2

# ブロックメッセージが worktree 側の対応パスを提示することの確認
msg=$(printf '%s' "$(edit_json Edit "$MAIN/api/handler.go" "$WT")" | "$HOOK" 2>&1 >/dev/null || true)
if printf '%s' "$msg" | grep -qF "/.claude/worktrees/feature-test/api/handler.go"; then
  pass=$((pass + 1))
  printf 'PASS  block message suggests worktree path\n'
else
  fail=$((fail + 1))
  printf 'FAIL  block message suggests worktree path\n'
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
