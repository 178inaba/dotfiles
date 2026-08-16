#!/bin/bash

# スキル間 `@` 参照の整合チェック（skill-authoring スキル所有。SKILL.md 編集時に使う）
#
# SKILL.md 本文中の `@~/.claude/skills/<skill>/<file>` は、そのスキルが起動されたとき
# （slash 起動・Skill ツール起動とも）に参照先ファイルを 1 段だけ添付する @-mention であり、
# 添付されたファイルの中の `@` は再走査されない。この「1 段のみ」を知らずに参照を入れ子に
# すると、2 段目以降のファイルが silent に落ちる（例: issue-handle → deep-plan-review →
# fresh-reader-verification で、収束プロトコルが未読のまま検証が回った事故）。
# その構造上の危険を、規約（skill-authoring「スキル間参照」）どおりかを決定的に検査して防ぐ。
#
# 使用方法: check-skill-refs.sh [skills-dir]
#   [skills-dir] 検査対象の skills ディレクトリ。省略時はこのスクリプトが属する skills/
#                （リポジトリ側・stow 済み ~/.claude 側のどちらから実行しても同じ相対位置）
#
# 検査対象は skills-dir 直下の */SKILL.md と、そこから `@` 参照される既存ファイル。
# 検出する違反（violations[].type）:
#   uncovered_nested  file が `@` 参照する ref の中に、file 自身が直接 `@` 参照していない
#                     `@` 参照 nested がある。file を起動しても nested は添付されない。
#                     対処: 手順として実行する相手なら `@` をやめ Skill ツール起動に変え、
#                     知識として取り込みたいなら file 側でも nested を直接 `@` 参照する
#   missing_target    `@` 参照先ファイルが存在しない（typo・移動漏れ）
#   ref_in_code       バッククォート／フェンス内の `@` 参照。言及用途に `@` を使っている
#                     疑い。対処: 言及は `@` を付けず `` `<skill>` `` 等で書く
#
# stdout は JSON のみ（契約の正はここ）:
#   skills_dir   検査した skills ディレクトリの絶対パス
#   violations   上記の配列。各要素は type / file / line / ref と、uncovered_nested のみ
#                nested（file が直接参照していない 2 段目のパス）を持つ。file / ref / nested
#                は skills-dir からの相対パス。違反なしなら空配列
#   warnings     非致命の注意（現状は常に空。契約の形を他スクリプトと揃えるために持つ）
#
# 違反の有無は exit code に反映しない（検査を完了できたら 0。判定は呼び出し側が
# violations で行う）。前提不成立（ディレクトリ不在・jq 欠如）は非ゼロ exit + 英語 stderr。

set -u

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# skills/<skill>/scripts/ → .claude/scripts/ の相対深さは、リポジトリ側と stow 済みの
# ~/.claude 側で同一なので相対トラバースで解決する
. "$script_dir/../../../scripts/warnings-lib.sh"

command -v jq >/dev/null 2>&1 || fatal 'jq is required'

skills_dir=${1:-"$script_dir/../.."}
[ -d "$skills_dir" ] || fatal "skills directory not found: $skills_dir"
skills_dir=$(cd "$skills_dir" && pwd)

# 1 ファイル分の `@~/.claude/skills/...` 参照を TSV（file, line, ref, in_code）で出す。
# in_code は行内バッククォートの奇偶（インラインコード）か ``` フェンス内かで判定する。
# 参照パスの文字集合は英数・. _ / - に限定し、日本語の括弧・句読点やバッククォートで止める
extract_refs() {
  local file=$1 rel=$2
  awk -v rel="$rel" '
    BEGIN { fence = 0 }
    {
      line = $0
      if (line ~ /^[ \t]*```/) { fence = !fence; next }
      pos = 1
      while (match(substr(line, pos), /@~\/\.claude\/skills\/[A-Za-z0-9._\/-]+/)) {
        start = pos + RSTART - 1
        ref = substr(line, start + length("@~/.claude/skills/"), RLENGTH - length("@~/.claude/skills/"))
        before = substr(line, 1, start - 1)
        ticks = gsub(/`/, "`", before)
        in_code = (fence || ticks % 2 == 1) ? 1 : 0
        printf "%s\t%d\t%s\t%d\n", rel, NR, ref, in_code
        pos = start + RLENGTH
      }
    }
  ' "$file"
}

# 走査対象: */SKILL.md と、そこから参照される既存ファイル（後者は入れ子検査のために読む。
# 添付されるのは参照されたファイルそのものなので、そのファイルの `@` が 2 段目になる）
scan_list=$(cd "$skills_dir" && ls -1 */SKILL.md 2>/dev/null)
[ -n "$scan_list" ] || fatal "no */SKILL.md found under $skills_dir"

records=""
scanned=""
queue=$scan_list
while [ -n "$queue" ]; do
  next_queue=""
  while IFS= read -r rel; do
    [ -n "$rel" ] || continue
    case "$scanned" in *"|$rel|"*) continue ;; esac
    scanned="${scanned}|$rel|"
    [ -f "$skills_dir/$rel" ] || continue
    out=$(extract_refs "$skills_dir/$rel" "$rel")
    [ -n "$out" ] || continue
    records="${records}${out}
"
    # 参照先のうち未走査のものを次の周回へ
    next_queue="${next_queue}$(printf '%s\n' "$out" | awk -F'\t' '$4 == 0 { print $3 }')
"
  done <<EOF
$queue
EOF
  queue=$next_queue
done

# TSV → NDJSON（exists を付与）→ jq で違反を組み立てる
ndjson=""
while IFS=$'\t' read -r file line ref in_code; do
  [ -n "$file" ] || continue
  if [ -f "$skills_dir/$ref" ]; then exists=true; else exists=false; fi
  ndjson="${ndjson}$(jq -cn --arg file "$file" --argjson line "$line" --arg ref "$ref" \
    --argjson in_code "$in_code" --argjson exists "$exists" \
    '{file: $file, line: $line, ref: $ref, in_code: ($in_code == 1), exists: $exists}')
"
done <<EOF
$records
EOF

printf '%s' "$ndjson" | jq -s \
  --arg skills_dir "$skills_dir" \
  --argjson warnings "$(warnings_json)" '
  . as $r
  # 添付として効く参照だけを依存とみなす（コード内・存在しない参照先は除く）
  | ($r | map(select((.in_code | not) and .exists))
        | group_by(.file)
        | map({key: .[0].file, value: (map(.ref) | unique)})
        | from_entries) as $deps
  | {
      skills_dir: $skills_dir,
      violations: (
        ($r | map(select(.in_code)
                  | {type: "ref_in_code", file, line, ref}))
        + ($r | map(select(.exists | not)
                    | {type: "missing_target", file, line, ref}))
        + [ $r[]
            | select((.in_code | not) and .exists)
            | . as $edge
            | ($deps[$edge.ref] // [])[] as $nested
            | select($nested != $edge.file
                     and (any(($deps[$edge.file] // [])[]; . == $nested) | not))
            | {type: "uncovered_nested", file: $edge.file, line: $edge.line,
               ref: $edge.ref, nested: $nested} ]
      ) | sort_by(.file, .line, .type),
      warnings: $warnings
    }'
