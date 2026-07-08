#!/bin/bash

# PreToolUse フック: worktree セッションでのメインツリー誤編集ガード
#
# EnterWorktree で session cwd が linked worktree に切り替わっても、
# Edit/Write/NotebookEdit が受け取る絶対パスは自動変換されない。
# 切替前の調査で Read したメインツリー絶対パスを Edit にそのまま流用すると、
# worktree ではなくメインツリーを修正してしまう事故が起きる
# （Read はどちらのツリーでも同じ内容が返るため直前まで気付けない）。
#
# 仕様:
#   - 入力: stdin に PreToolUse の JSON
#   - 対象: tool_name が Edit / Write / NotebookEdit
#   - session cwd が linked worktree 内なのに、対象パスが同一リポジトリの
#     worktree 外（メインツリー本体・別 worktree）を指す場合 exit 2
#   - リポジトリ外のパス（~/.claude/・scratchpad 等）や、cwd がメイン
#     worktree / リポジトリ外の場合は exit 0 で素通り（fail open）

set -euo pipefail

input=$(cat)

tool_name=$(printf '%s' "$input" | jq -r '.tool_name // empty')
case "$tool_name" in
  Edit | Write | NotebookEdit) ;;
  *) exit 0 ;;
esac

target=$(printf '%s' "$input" | jq -r '.tool_input.file_path // .tool_input.notebook_path // empty')
[ -n "$target" ] || exit 0

cwd=$(printf '%s' "$input" | jq -r '.cwd // empty')
[ -n "$cwd" ] || exit 0

worktree_root=$(git -C "$cwd" rev-parse --show-toplevel 2>/dev/null) || exit 0
git_dir=$(git -C "$cwd" rev-parse --path-format=absolute --git-dir 2>/dev/null) || exit 0
common_dir=$(git -C "$cwd" rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || exit 0

# git-dir と git-common-dir が一致 = メイン worktree（ガード対象外）
[ "$git_dir" != "$common_dir" ] || exit 0

main_root=$(dirname "$common_dir")

# 物理パス化（macOS の /tmp -> /private/tmp 等の symlink 差異を吸収して
# prefix 比較を成立させる）。ディレクトリ以外・未存在パスは、存在する
# 最近接の祖先ディレクトリを物理化して残りを連結する。
physical_path() {
  local path=$1 suffix=""
  case "$path" in
    /*) ;;
    *) path="$cwd/$path" ;;
  esac
  while [ ! -d "$path" ]; do
    suffix="/$(basename "$path")$suffix"
    path=$(dirname "$path")
  done
  printf '%s%s' "$(cd "$path" && pwd -P)" "$suffix"
}

worktree_phys=$(physical_path "$worktree_root")
main_root_phys=$(physical_path "$main_root")
target_phys=$(physical_path "$target")

case "$target_phys" in
  "$worktree_phys" | "$worktree_phys"/*) exit 0 ;;
esac

case "$target_phys" in
  "$main_root_phys"/*) ;;
  *) exit 0 ;;
esac

rel=${target_phys#"$main_root_phys"/}
target_location="the main tree"
case "$rel" in
  .claude/worktrees/*/*)
    rel=${rel#.claude/worktrees/*/}
    target_location="another worktree"
    ;;
esac
suggested="$worktree_phys/$rel"

cat >&2 <<EOF
Blocked: this $tool_name targets a path outside the current worktree.

  session cwd (worktree): $worktree_phys
  target path:            $target
  main repository root:   $main_root_phys

This session works in a linked git worktree, but the target path points at
$target_location of the same repository. This usually happens when an
absolute path obtained before the worktree switch (e.g. Read during
investigation) is reused for $tool_name, silently modifying the wrong tree.

Fix: re-run $tool_name with the corresponding path inside the worktree:
  $suggested

If you really intend to modify the main tree, do it explicitly via Bash
(e.g. git -C "$main_root_phys" ...) or confirm with the user first.
EOF

exit 2
