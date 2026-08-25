#!/bin/bash

# PostToolUse フック: SKILL.md の frontmatter 検査
#
# 防ぐ事故:
#   SKILL.md の frontmatter は Claude Code のパーサーが寛容なため、標準の YAML として
#   壊れていてもスキル自体は読み込めてしまう。壊れに気付ける契機が「編集中に誰かが
#   目視で気付く」か「検査コマンドを覚えている人が手で走らせる」しかなく、実際に
#   壊れた argument-hint が 2 ファイル分 main に居座った。保存の時点で違反を
#   モデルの目の前に置き、覚えていることへの依存を外す。
#
# PostToolUse を選ぶ理由（PreToolUse ではなく）:
#   PreToolUse は編集の適用前に発火し、編集後のファイル内容をペイロードから得る
#   公式手段が無い（hooks ガイド・リファレンスのいずれにも無い）。PreToolUse 版は
#   old_string/new_string を自前で再適用することになり、ハーネス挙動の再実装になる
#   （worktree-edit-guard.sh が git worktree list での分類に倒して避けているのと同種の
#   推測）。PostToolUse は書き終わったファイルをそのまま読める。
#   代償として**書き込みは取り消せない**（リファレンスも PostToolUse は blocking
#   しないと明記）。exit 2 は書き込みを巻き戻さず、同じターン内でモデルに問題を
#   提示するだけ。
#
# 仕様:
#   - 入力: stdin に PostToolUse の JSON
#   - 対象: tool_name が Edit / Write / NotebookEdit で、対象パスの basename が SKILL.md
#   - 検出ロジックは持たず check-skill-frontmatter.sh を呼ぶ（親 Issue の
#     one-implementation 規約。契約が同じものを 2 つ持つとドリフトする）
#   - 違反あり → exit 2 + stderr に 1 違反 1 行。パスはスクリプトの relative な
#     violations[].file ではなく**ペイロードの元パス**を出す（前者は <skill>/SKILL.md
#     形式で、リポジトリ上のどこの話か分からない）
#   - 違反なし・対象外 → exit 0 で無出力
#   - 検査スクリプトの非ゼロ exit（yq 欠如・jq 欠如・対象が読めない等）は原因を
#     問わず exit 2 + 「検査できなかった」旨 + スクリプトの stderr。warnings-lib.sh の
#     fatal は常に exit 1 なので exit code から原因を区別できず、区別しようとしない
#   - 入力の解析に失敗した場合は fail-open（exit 0）
#
# 単一ファイル検査では検査スクリプトの warnings[] は常に空（積むのは
# ディレクトリ検査で SKILL.md を持たないサブディレクトリのみ）なので描画しない。
#
# 本フックは ~/.claude/hooks/ に置かれるため、このリポジトリ外の SKILL.md 編集でも
# 発火する。4 種の違反は Claude Code の規約であってリポジトリ固有ではなく、exit 2 は
# ブロックしないので、他所での誤検出のコストはメッセージ 1 通に留まる。

set -euo pipefail

input=$(cat)

# jq を起動する前の足切り。対象パスが SKILL.md なら JSON のどこかに必ずこの部分文字列が
# 現れるので偽陰性は無い（本文に SKILL.md を含むだけの編集は素通りして jq へ進むだけ）。
# 本フックは編集系ツールの呼び出しごとに起動されるため、大多数を占める非 SKILL.md の
# 呼び出しで jq を起動しない形にしておく
case "$input" in
  *SKILL.md*) ;;
  *) exit 0 ;;
esac

tool_name="" target="" cwd=""
eval "$(printf '%s' "$input" | jq -r '@sh "tool_name=\(.tool_name // "") target=\(.tool_input.file_path // .tool_input.notebook_path // "") cwd=\(.cwd // "")"')"

case "$tool_name" in
  Edit | Write | NotebookEdit) ;;
  *) exit 0 ;;
esac
[ -n "$target" ] || exit 0
[ "$(basename "$target")" = "SKILL.md" ] || exit 0

# Edit/Write/NotebookEdit は絶対パス契約だが、相対パスのまま検査スクリプトへ渡すと
# フック自身の cwd 次第で「対象不在」の誤報になる
case "$target" in
  /*) ;;
  *)
    [ -n "$cwd" ] || exit 0
    target="$cwd/$target"
    ;;
esac

# ~ 決め打ちにせず相対トラバースで解決する（check-skill-refs.sh と同じ理由）。
# hooks/ と skills/ の相対位置は stow symlink のどちら側でも同一なので、
# リポジトリから走らせたテストはリポジトリ側のスクリプトを読む
hook_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
check_dir="$hook_dir/../skills/skill-authoring/scripts"
# 実行と再検査の案内の両方に使うので `..` を畳んでおく（未存在なら論理パスのまま進み、
# 下の起動が失敗して「検査できなかった」経路に落ちる）
check_dir=$( (cd "$check_dir" && pwd) 2>/dev/null || printf '%s' "$check_dir")
check_script="$check_dir/check-skill-frontmatter.sh"

# 案内するコマンドは実際に呼んだものと同じパスで出す（`~` を直書きすると、
# check-skill-frontmatter.sh をリポジトリ側で編集中に stow 済みの古いコピーを案内する）
recheck_hint() {
  # %q は空白・引用符を含むパスでもそのまま実行できる形に整える（リポジトリ外の
  # SKILL.md が対象になりうるため。通常のパスでは引用符は付かない）
  printf '\nRe-check with:\n  bash %q %q\n' "$check_script" "$target"
}

tmp_err=$(mktemp)
trap 'rm -f "$tmp_err"' EXIT

# bash 経由で起動する（check-skill-frontmatter.sh は実行ビットを持たないため直接実行は
# exit 126 になる。リポジトリの全スクリプトが bash <path> 起動で揃えられている）
if ! out=$(bash "$check_script" "$target" 2>"$tmp_err"); then
  {
    printf 'The frontmatter of %s was not checked.\n\n' "$target"
    printf 'check-skill-frontmatter.sh failed before it could inspect the file:\n'
    sed 's/^/  /' "$tmp_err"
    printf '\nFix the reported prerequisite.\n'
    recheck_hint
  } >&2
  exit 2
fi

violations=$(printf '%s' "$out" | jq -r --arg file "$target" '
  .violations[]
  | "  \($file): \(.type) — " + (
      if .type == "invalid_yaml" then .message
      elif .type == "missing_field" then "`\(.field)` is missing or empty"
      elif .type == "name_mismatch" then "expected `\(.expected)`, actual `\(.actual)`"
      elif .type == "unquoted_flow" then "line \(.line): the value of `\(.key)` starts with an unquoted `[` or `{`, so YAML reads it as a sequence or mapping instead of a string — quote it"
      else tostring
      end)')

[ -n "$violations" ] || exit 0

{
  printf 'This SKILL.md has invalid frontmatter:\n\n'
  printf '%s\n' "$violations"
  recheck_hint
} >&2

exit 2
