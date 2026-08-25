#!/bin/bash

# SKILL.md frontmatter の妥当性チェック（skill-authoring スキル所有。SKILL.md 編集時に使う）
#
# frontmatter は Claude Code のパーサーが寛容なため、標準の YAML として壊れていても
# スキル自体は読み込めてしまう（`argument-hint: [<pr-number>] [--dry-run]` は1行に複数の
# flow sequence が並んだ状態で、標準パーサーは解析を拒否する）。壊れていることに気付ける
# 契機が「編集中に誰かが目視で気付く」しかなく、実際 3 ファイルのうち 1 つはそうやって
# 直り、残る 2 つは放置された。それを決定的な検査に置き換える。
#
# 使用方法: check-skill-frontmatter.sh [target]
#   [target] 検査対象。ディレクトリなら直下の */SKILL.md を、単一ファイルならそれだけを
#            検査する（2 形式を受けるのは、後で hook がファイル単位で呼ぶため。同じ契約に
#            実装を 2 つ持たせない）。省略時はこのスクリプトが属する skills/
#            （リポジトリ側・stow 済み ~/.claude 側のどちらから実行しても同じ相対位置）
#
# 検出する違反（violations[].type）:
#   invalid_yaml   frontmatter ブロックが YAML として解析できない。
#                  対処: エラー文の指す箇所を直す（値の引用符忘れが典型）
#   missing_field  name / description が無いか空。frontmatter ブロックそのものが無い
#                  ファイルもここに落ちる（両フィールドが無いので 2 件出る）
#   name_mismatch  name がスキルのディレクトリ名と一致しない
#   unquoted_flow  値が引用符なしの `[` / `{` で始まり、文字列ではなく flow sequence /
#                  flow mapping として読まれる。対処: 値を引用符で囲む
#
# invalid_yaml のファイルは他 3 種を報告しない（解析できないファイルを他の基準で
# 判定しようがないため）。unquoted_flow は生テキスト由来で解析成否に依存しないが、
# この排他が優先する。
#
# stdout は JSON のみ（契約の正はここ）:
#   target      検査した対象の絶対パス（ディレクトリ・単一ファイルとも）。~/.claude/skills は
#               リポジトリへの symlink なので、どちらのコピーをどちらの形式で読んだかを
#               出力だけで判別できるようにする
#   violations  上記の配列。各要素は type / file と、type ごとの詳細:
#                 invalid_yaml   message  yq のエラー文。yq は --front-matter=extract で
#                                         対象を temp ファイルへ展開してから読むため、生の
#                                         エラーは temp パスを名指しする。それを剥がした
#                                         残り（元のパスは同じレコードの file が持つ）。
#                                         **message 中の行番号は抽出した frontmatter
#                                         ブロック基準**でファイルの行番号ではない
#                 missing_field  field    欠けているフィールド名
#                 name_mismatch  expected / actual
#                 unquoted_flow  key / line（line はファイル先頭からの絶対行番号）
#               file はディレクトリ検査なら target からの相対パス、単一ファイル検査なら
#               <skill>/SKILL.md（素の basename にしない — 出力だけで場所に辿り着けるように）。
#               違反なしなら空配列
#   warnings    非致命の注意。ディレクトリ検査で SKILL.md を持たないサブディレクトリを
#               1 件ずつ積む（違反ではないが、黙って飛ばすと未検査だったことが隠れる）
#
# 違反の有無は exit code に反映しない（検査を完了できたら 0。判定は呼び出し側が
# violations で行う）。前提不成立（依存欠如・対象不在・ディレクトリ配下に */SKILL.md
# なし）は非ゼロ exit + 英語 stderr。
#
# 環境変数:
#   YQ_BIN  yq の実行パス（既定 yq）。テストが「yq 欠如」を再現するための差し替え口

set -u

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# skills/<skill>/scripts/ → .claude/scripts/ の相対深さは、リポジトリ側と stow 済みの
# ~/.claude 側で同一なので相対トラバースで解決する
. "$script_dir/../../../scripts/warnings-lib.sh"

command -v jq >/dev/null 2>&1 || fatal 'jq is required'
yq_bin=${YQ_BIN:-yq}
command -v "$yq_bin" >/dev/null 2>&1 || fatal 'yq is required'

target=${1:-"$script_dir/../.."}
[ -e "$target" ] || fatal "target not found: $target"

tmp_err=$(mktemp)
trap 'rm -f "$tmp_err"' EXIT

records=""

emit() {
  records="${records}$(jq -cn "$@")
"
}

# 1 ファイル分の違反を records へ積む。rel は出力に載せる <skill>/SKILL.md 形式のパス
check_file() {
  local file=$1 rel=$2
  local skill=${rel%%/*}

  # frontmatter ブロックの有無は生テキストで決める。yq の exit status では代用できない —
  # fence が無いと --front-matter=extract は body を解析するので、結果が body の内容次第で
  # 変わる（実 SKILL.md の body は 1、内容の薄い body は 0）
  local fm_end
  fm_end=$(awk 'NR == 1 { if ($0 != "---") exit 0; next } $0 == "---" { print NR; exit 0 }' "$file")

  if [ -z "$fm_end" ]; then
    emit --arg file "$rel" '{type: "missing_field", file: $file, field: "name"}'
    emit --arg file "$rel" '{type: "missing_field", file: $file, field: "description"}'
    return
  fi

  local parsed
  if ! parsed=$("$yq_bin" --front-matter=extract -o=json '.' "$file" 2>"$tmp_err"); then
    local message
    # 生の形は `Error: bad file '<temp>': yaml: line N: ...`。temp パスを載せると読者が
    # 元のファイルに辿り着けないので前置きごと剥がす
    message=$(tr '\n' ' ' <"$tmp_err" \
      | sed -E "s/^Error: bad file '[^']*': //" \
      | sed -E 's/[[:space:]]+$//')
    emit --arg file "$rel" --arg message "$message" \
      '{type: "invalid_yaml", file: $file, message: $message}'
    return
  fi

  local fields name description
  # frontmatter がシーケンスやスカラーでも解析は成功するので、mapping 以外は空扱いに正規化する
  fields=$(printf '%s' "$parsed" | jq -c '
    (if type == "object" then . else {} end)
    | {name: ((.name // "") | if type == "string" then . else tostring end),
       description: ((.description // "") | if type == "string" then . else tostring end)}')
  name=$(printf '%s' "$fields" | jq -r .name)
  description=$(printf '%s' "$fields" | jq -r .description)

  if [ -z "$name" ]; then
    emit --arg file "$rel" '{type: "missing_field", file: $file, field: "name"}'
  elif [ "$name" != "$skill" ]; then
    emit --arg file "$rel" --arg expected "$skill" --arg actual "$name" \
      '{type: "name_mismatch", file: $file, expected: $expected, actual: $actual}'
  fi
  [ -n "$description" ] || emit --arg file "$rel" \
    '{type: "missing_field", file: $file, field: "description"}'

  # unquoted_flow は生テキストからしか判定できない。解析後の値では区別が消えるため
  # （`[--yes]` は seq、`"[--yes]"` は str になり、`[a] [b]` はそもそも解析できない）。
  # キー名で特別扱いしないのは、argument-hint だけを見ると次のキーが同じ事故を繰り返せるから。
  # 非インデント行に限るので、ブロックスカラーの中身は自然に外れる
  local flows
  flows=$(awk -v last="$((fm_end - 1))" '
    NR >= 2 && NR <= last && match($0, /^[A-Za-z0-9_][A-Za-z0-9_.-]*:[ \t]*[[{]/) {
      key = $0
      sub(/:.*/, "", key)
      printf "%s\t%d\n", key, NR
    }
  ' "$file")
  local key line
  while IFS=$'\t' read -r key line; do
    [ -n "$key" ] || continue
    emit --arg file "$rel" --arg key "$key" --argjson line "$line" \
      '{type: "unquoted_flow", file: $file, key: $key, line: $line}'
  done <<EOF
$flows
EOF
}

if [ -d "$target" ]; then
  target=$(cd "$target" && pwd)
  scan_list=$(cd "$target" && ls -1 -- */SKILL.md 2>/dev/null)
  [ -n "$scan_list" ] || fatal "no */SKILL.md found under $target"

  for dir in "$target"/*/; do
    [ -d "$dir" ] || continue
    [ -f "$dir/SKILL.md" ] && continue
    add_warning "no SKILL.md in $(basename "$dir")/"
  done

  while IFS= read -r rel; do
    [ -n "$rel" ] || continue
    check_file "$target/$rel" "$rel"
  done <<EOF
$scan_list
EOF
else
  file_dir=$(cd "$(dirname "$target")" && pwd)
  base=$(basename "$target")
  target="$file_dir/$base"
  check_file "$target" "$(basename "$file_dir")/$base"
fi

printf '%s' "$records" | jq -s \
  --arg target "$target" \
  --argjson warnings "$(warnings_json)" '
  {
    target: $target,
    violations: sort_by(.file, (.line // 0), .type),
    warnings: $warnings
  }'
