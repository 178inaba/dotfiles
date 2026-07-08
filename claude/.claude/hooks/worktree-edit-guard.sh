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
#     別ツリー（メインツリー・別 worktree）に属する場合 exit 2
#   - 対象パスの所属ツリーは `git worktree list --porcelain` で分類する
#     （ディレクトリ配置の推測に依存しないため、bare リポジトリ + worktree
#     構成やリポジトリディレクトリ外に作られた worktree でも誤判定しない）
#   - どのツリーにも属さないパス（~/.claude/・scratchpad 等）や、cwd が
#     メイン worktree / リポジトリ外の場合は exit 0 で素通り（fail open）

set -euo pipefail

input=$(cat)

tool_name="" target="" cwd=""
eval "$(printf '%s' "$input" | jq -r '@sh "tool_name=\(.tool_name // "") target=\(.tool_input.file_path // .tool_input.notebook_path // "") cwd=\(.cwd // "")"')"

case "$tool_name" in
  Edit | Write | NotebookEdit) ;;
  *) exit 0 ;;
esac
[ -n "$target" ] || exit 0
[ -n "$cwd" ] || exit 0

# Edit/Write/NotebookEdit は絶対パス契約。相対パスは判定不能として素通り
case "$target" in
  /*) ;;
  *) exit 0 ;;
esac

info=$(git -C "$cwd" rev-parse --path-format=absolute --show-toplevel --git-dir --git-common-dir 2>/dev/null) || exit 0
{
  read -r worktree_root
  read -r git_dir
  read -r common_dir
} <<<"$info"

# git-dir と git-common-dir が一致 = メイン worktree（ガード対象外）
[ "$git_dir" != "$common_dir" ] || exit 0

# 物理パス化（macOS の /tmp -> /private/tmp 等の symlink 差異を吸収して
# prefix 比較を成立させる）。未存在パス（Write の新規ファイル）は、存在する
# 最近接の祖先ディレクトリを物理化して残りを連結する。
physical_path() {
  local path=$1 suffix=""
  while [ ! -d "$path" ]; do
    suffix="/$(basename "$path")$suffix"
    path=$(dirname "$path")
  done
  printf '%s%s' "$(cd "$path" && pwd -P)" "$suffix"
}

worktree_phys=$(physical_path "$worktree_root")
target_phys=$(physical_path "$target")

case "$target_phys" in
  "$worktree_phys" | "$worktree_phys"/*) exit 0 ;;
esac

# コールドパス: 対象が現 worktree の外。所属ツリーを worktree 一覧から分類する
wt_list=$(git -C "$cwd" worktree list --porcelain 2>/dev/null) || exit 0

paths=() bares=()
while IFS= read -r line; do
  case "$line" in
    "worktree "*)
      paths+=("${line#worktree }")
      bares+=(0)
      ;;
    bare)
      bares[${#bares[@]} - 1]=1
      ;;
  esac
done <<<"$wt_list"

# 一覧の先頭はメイン worktree（bare の場合はメインツリーなし）。
# worktree はメインツリー配下に置かれることがあるため、最長 prefix の
# ツリーを所属先として採用する
owner_root="" owner_label=""
for i in "${!paths[@]}"; do
  [ "${bares[i]}" -eq 0 ] || continue
  p_phys=$(physical_path "${paths[i]}")
  [ "$p_phys" != "$worktree_phys" ] || continue
  case "$target_phys" in
    "$p_phys" | "$p_phys"/*)
      if [ "${#p_phys}" -gt "${#owner_root}" ]; then
        owner_root=$p_phys
        if [ "$i" -eq 0 ]; then owner_label="the main tree"; else owner_label="another worktree"; fi
      fi
      ;;
  esac
done

[ -n "$owner_root" ] || exit 0

rel=${target_phys#"$owner_root"/}
suggested="$worktree_phys/$rel"

cat >&2 <<EOF
Blocked: this $tool_name targets a path outside the current worktree.

  session cwd (worktree): $worktree_phys
  target path:            $target
  owning tree:            $owner_root ($owner_label)

This session works in a linked git worktree, but the target path belongs to
$owner_label of the same repository. This usually happens when an
absolute path obtained before the worktree switch (e.g. Read during
investigation) is reused for $tool_name, silently modifying the wrong tree.

Fix: re-run $tool_name with the corresponding path inside the worktree:
  $suggested

If you really intend to modify that tree, do it explicitly via Bash
(e.g. git -C "$owner_root" ...) or confirm with the user first.
EOF

exit 2
