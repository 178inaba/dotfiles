# EnterWorktree バイパス経路が共有する .worktreeinclude コピー（source して使う。単体実行しない）
#
# Claude Code は EnterWorktree で worktree を作る際、committed な .worktreeinclude の
# パターンに一致する gitignored ファイル（.env 等）をコピーする。git worktree add で直接
# 作る経路（issue-handle の create-worktree.sh、worktree-resolution の create-fallback）は
# このネイティブ挙動を素通りするため、ここで再現する。片方だけが実装を持つとネイティブ挙動の
# 変更時にドリフトするので、経路ごとではなく「バイパス作成」の性質としてここに一本化する。
# テストは scripts/tests/test-worktreeinclude-lib.sh。
#
# 呼び出し元が定義済みであることを前提とする関数（warning・JSON の配管は持たない）:
#   add_warning <msg>  非致命の注意を蓄積する
#   fatal <msg>        前提不成立で非ゼロ exit する
#
# copy_worktreeinclude <source-root> <worktree-path>
#   <worktree-path>/.worktreeinclude（= 起点/checkout 中の commit に含まれるもの）の
#   パターンに一致し、かつ <source-root> で gitignored のファイルを worktree へコピーする。
#   tracked・単なる untracked は対象外。.worktreeinclude が無い、または symlink なら何もしない。
#   コピー元が symlink、コピー先が committed symlink 経由で worktree 外へ出る場合はスキップ
#   （いずれも add_warning）。.claude/worktrees/ 配下はコピー元にしない。
#
#   コピー数はグローバル WORKTREEINCLUDE_COPIED に返す。stdout を戻り値にしないのは、
#   command substitution で呼ぶと add_warning の蓄積がサブシェルに閉じて warning が消えるため。
copy_worktreeinclude() {
  local src_root=$1 worktree_path=$2
  # set -u の呼び出し元が早期 return 経路でも参照できるよう無条件に初期化する
  WORKTREEINCLUDE_COPIED=0

  local include_file="$worktree_path/.worktreeinclude"
  { [ -f "$include_file" ] && [ ! -L "$include_file" ]; } || return 0

  local worktree_phys
  worktree_phys=$(cd "$worktree_path" && pwd -P) || fatal "failed to resolve: $worktree_path"

  # パターン一致の untracked を列挙 → 実際に gitignored のものへ絞る（tracked は --others が除外）
  local file dest_dir resolved_dir
  while IFS= read -r -d '' file; do
    case "$file" in
      .claude/worktrees/*) continue ;;  # 他 worktree 内のファイルはコピー元にしない
    esac
    if [ -L "$src_root/$file" ]; then
      add_warning "skipped symlink in .worktreeinclude: $file"
      continue
    fi
    dest_dir="$worktree_path/$(dirname "$file")"
    mkdir -p "$dest_dir" || fatal "failed to create directory: $dest_dir"
    # コピー先が committed symlink 経由で worktree 外へ出ていないか
    resolved_dir=$(cd "$dest_dir" && pwd -P) || fatal "failed to resolve: $dest_dir"
    case "$resolved_dir/" in
      "$worktree_phys/"*) ;;
      *)
        add_warning "skipped .worktreeinclude entry (destination escapes worktree): $file"
        continue
        ;;
    esac
    cp -p "$src_root/$file" "$worktree_path/$file" || fatal "failed to copy: $file"
    WORKTREEINCLUDE_COPIED=$((WORKTREEINCLUDE_COPIED + 1))
  done < <(git -C "$src_root" ls-files -z --others --ignored --exclude-from="$include_file" \
             | git -C "$src_root" check-ignore -z --stdin)
}
