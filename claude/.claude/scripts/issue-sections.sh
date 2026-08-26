#!/bin/bash

# Issue 本文の節スキーマ（意味キー ↔ 見出し文字列）を持つ唯一の実行可能な置き場
# （issue-draft・issue-handle・github-sub-issues 共有）
#
# Issue 本文の `## ` 見出しは「読者向けの散文」と「他スキルが機械的に探すアンカー」を兼ねており、
# 見出し文字列に直接依存すると英語リポジトリで両方向に壊れる（起案側は見出しの言語を選べず、
# 消費側は翻訳された見出しを見つけられない）。そこで節を言語非依存の意味キーで識別し、
# キー → 見出しの表をここに 1 つだけ置く。**SKILL.md へ表を複製しない**（複製はドリフトの元）。
# 表の出典は 178inaba/dotfiles#85「Cross-cutting rules」。テストは scripts/tests/test-issue-sections.sh。
#
# ネットワーク・gh を一切呼ばない。引数と入力ファイルに対する純粋なテキスト処理。
#
# 使用方法:
#   issue-sections.sh schema <key>
#   issue-sections.sh list --locale <ja|en> --kind <leaf|sub|parent>
#   issue-sections.sh check <draft-file> --locale <ja|en> --kind <leaf|sub|parent> [--mapping <file>]
#   issue-sections.sh find <file> <key>
#
# stdout は JSON のみ。契約（正はここ。各 SKILL.md には自スキルが使うフィールドの解釈のみ書く）:
#
#   schema → {key, headings: {ロケール: 見出し}, required_on, template_mappable, none_markers}
#     1 キー分の表の行をそのまま返す。**ロケールを取らない**のは消費側向けの口だからで、
#     #85 注記 4 のとおり消費側は両ロケールの見出し・マーカーを受け付ける（どちらで書かれた
#     Issue かを知らないまま判定する）。none_markers は list と同じ形。
#
#   list   → {locale, kind, sections: [{key, heading, required, required_on, template_mappable, none_markers}]}
#     issue-draft が節をレンダリングするための一覧。**kind で行を絞らない** — #85 注記 1 のとおり
#     「その kind に必須でないキーが本文に現れてもよい」ため、絞ると起案側の選択肢を落とす。
#     required     指定 kind でその節が必須か
#     required_on  表の "required on" 列そのまま（[] は「どの kind でも任意」）。required だけだと
#                  「parent 専用の任意キー」と「全 kind で任意のキー」が区別できず、消費側が
#                  表を複製することになるため生の列も出す
#     none_markers {ja, en} または null。固定の「なし」マーカーを持つキーだけ非 null。
#                  **全ロケールを出す**のは #85 注記 4「消費側は両言語を受け付ける」に対応するため
#                  （レンダリングはロケール固定だが、消費はロケール非依存）
#     sections の並びは下の表の行順（#85 の表の順序であり、kind ごとのレンダリング順ではない）
#
#   check  → stdout は空。**違反理由を 1 行 1 件 stderr に出し、違反クラス別の exit code で返す**。
#     skill-authoring の「stdout には JSON のみ」に対する唯一の意図的な例外で、#85 の Sub である
#     178inaba/dotfiles#86 要件 3.2 がこの形（reason per line on stderr）を指定している。
#     消費側（issue-draft）が要るのは pass/fail だけで、理由は人間とモデルが読むため。
#     検査規則:
#       1. kind の必須キーがすべて存在する
#       2. draft の全 `## ` 見出しが既知（いずれかのロケールの canonical 見出し、または mapping にある見出し）
#       3. template-mappable = no のキーが mapping に現れない
#       4. mapping に無い見出し（= canonical にレンダリングされた見出し）の字種がロケールと一致する
#     規則 2 の「既知」を**いずれかのロケール**の canonical と読むのは意図的。ロケール厳密に読むと
#     mapping に無い見出しは当該ロケールの canonical と完全一致するしかなく、字種は構成上必ず一致して
#     規則 4 が到達不能になる。緩めることで、規則 2 が「表の外の見出し」を、規則 4 が「他ロケールの
#     canonical 見出し」を担当する分担になる（後者が「見出しが混在したドラフト」の検出そのもの）。
#     mapping にある見出しは規則 4 の対象外 — #85「Repository issue templates」のとおり、
#     mappable なキーではテンプレート側の言語が勝つため。
#     見出し → キーの解決順は mapping → 表の行順 × ロケール順（mapping が勝つ）。規則 1 の
#     「存在」もこの解決で判定するので、他ロケールの見出しは規則 4 の 1 件だけで落ち、規則 1 の
#     理由が重複して出ることはない。
#
#   find   → {key, locale, heading, body}
#     全ロケールの canonical 見出しを受け付ける（消費側はロケールを知らないため --locale を取らない）。
#     locale はマッチした見出しのロケール。body は**見出し行を含まない**節本文（次の `## ` 見出しの
#     手前まで、前後の空行を除去し、行末 CR も除去）。節が空なら body は空文字列。
#     節が無い場合は stdout に何も出さず exit 6（消費側はこれで分岐する）。
#
# 解析規則（check・find 共通）:
#   - 節見出しは行頭 `## ` のみ（`### ` 以下は節境界にしない）。見出しは前後の空白を除去して比較する
#     （除去対象に \r を含むので CRLF のドラフトも同じに扱える。find の body からも CR を落とす）
#   - バッククォート 3 個以上で開くフェンス内の行は見出しとして扱わない（本文テンプレートを例示する
#     ドラフトで誤検出しないため）。`~~~` フェンスとインデントコードブロックは対象外
#   - 同じ見出しが 2 回現れた場合・同一キーの複数ロケールの見出しが両方現れた場合、find は最初の 1 件を
#     返し、check は既知の見出しとして規則 2 を通す（重複それ自体は検査しない）
#   - 字種判定は文字クラスのみ（辞書を使わない）: ひらがな・カタカナ・漢字のいずれかを含むか
#
# mapping ファイル（check の --mapping）の形式:
#   1 行 1 件で `<key> <テンプレートの見出し>`（key の後は空白 1 個以上、残りが見出し）。
#   空行と `#` で始まる行は無視する。リポジトリの issue テンプレートに従うとき、起案モデルが
#   「スキーマのキー → テンプレートの見出し」の対応を手で書くファイルなので、区切りをタブに
#   限定せず最小の形にしてある。形式不正・未知のキー・キーの重複・同じ見出しへの複数キーは
#   前提不成立（exit 1）— いずれも起案モデルの書き間違いで、黙って片方を採用すると
#   「必須キーが見つからない」等の無関係な理由で落ちるため。
#
# ロケールを増やすときに触る場所（表の行を足すだけでは済まないので、ここに列挙しておく）:
#   1. LOCALES に 1 語足す（ロケール集合の正で、表の見出し列の並びでもある）
#   2. SECTION_TABLE と NONE_MARKER_TABLE の各行に、その位置の見出し列を 1 つ足す
#   3. CHECK_JQ の script_matches に、そのロケールの字種判定を足す（規則 4 で使う。字種は言語固有で
#      表には持てないため、ここだけは列の追加では済まない）
#
# exit code:
#   0  成功
#   1  前提不成立（usage・未対応 locale / kind・未知のキー・ファイル不在・mapping 不正）
#   2  check: 必須キーの欠落（規則 1）
#   3  check: 未知の見出し（規則 2）
#   4  check: machine-consumed キーが mapping にある（規則 3）
#   5  check: 見出しの字種がロケールと不一致（規則 4）
#   6  find: 節が見つからない
#   複数の違反クラスが同時に成立した場合は全理由を stderr に出し、exit は 2 → 3 → 4 → 5 の
#   評価順で最初に成立したクラス（理由の出力順も同じ）。
#
# 前提不成立は英語 stderr + 非ゼロ exit。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
. "$SCRIPT_DIR/warnings-lib.sh"

USAGE='usage: issue-sections.sh schema <key>
       issue-sections.sh list --locale <ja|en> --kind <leaf|sub|parent>
       issue-sections.sh check <draft-file> --locale <ja|en> --kind <leaf|sub|parent> [--mapping <file>]
       issue-sections.sh find <file> <key>'

command -v jq >/dev/null 2>&1 || fatal 'jq is required'

# ロケール集合の正。並びは下の 2 つの表の見出し列の並びでもある
LOCALES='ja en'
# Issue の種別。表の "required on" 列に現れうる値の全体だが、表からは導出しない
# （必須キーを 1 つも持たない kind があり得るため、集合はここで宣言する）
KINDS='leaf sub parent'

# 節スキーマの表（この 11 行が唯一の正）。列は `|` 区切りで
#   key | <LOCALES の順の見出し...> | required on | template-mappable
# "required on" の `-` は「どの kind でも任意」。template-mappable = no は machine-consumed キー
# （他スキルが機械的に探す節）で、リポジトリの issue テンプレートに従うときも canonical 見出しのまま置く
SECTION_TABLE='background|背景・目的|Background / Purpose|leaf,sub,parent|yes
depends_on|依存|Depends on|sub|no
requirements|要件|Requirements|leaf,sub|yes
acceptance|受け入れ条件|Acceptance criteria|leaf,sub,parent|yes
affected_code|影響範囲・関連コード|Affected code|leaf,sub|yes
impl_notes|実装方針の示唆|Implementation notes (suggestions)|-|yes
deferred|実装時判断に委ねる事項|Deferred to implementer judgment|-|yes
out_of_scope|スコープ外|Out of scope|leaf,sub,parent|yes
composition|構成（Sub-Issues）|Structure (Sub-Issues)|parent|no
cross_cutting|横断ルール|Cross-cutting rules|parent|yes
release_manual_steps|リリース時の手動作業|Manual release steps|parent|no'

# 節に固有の「なし」マーカー（key | <LOCALES の順のマーカー...>）。表と一緒に持ち回るのは
# #85 注記 4 の要求で、消費側はこの文字列で「依存なし」「手動作業なし」を判定する
NONE_MARKER_TABLE='depends_on|なし|None
release_manual_steps|なし（全 Sub のマージで完了）|None (completed by merging all Subs)'

LOCALES_JSON=$(jq -nc '$ARGS.positional' --args $LOCALES)
SUPPORTED_LOCALES=$(printf '%s' "$LOCALES" | sed 's/ /, /g')
SUPPORTED_KINDS=$(printf '%s' "$KINDS" | sed 's/ /, /g')

# 表 → JSON。以降のキー引きは全て jq 側で行う（連想配列は bash 4 以降で、macOS の /bin/bash は 3.2）。
# 見出し列は LOCALES と zip して {ロケール: 見出し} にするので、ロケールを増やしても
# 引く側（schema・list・find・CHECK_JQ）は書き換え不要
TABLE_JSON=$(printf '%s\n' "$SECTION_TABLE" | jq -Rs --argjson locales "$LOCALES_JSON" '
  ($locales | length) as $n
  | split("\n") | map(select(length > 0)) | map(split("|")) | map({
      key: .[0],
      headings: ([$locales, .[1:1 + $n]] | transpose | map({key: .[0], value: .[1]}) | from_entries),
      required_on: (if .[1 + $n] == "-" then [] else (.[1 + $n] | split(",")) end),
      template_mappable: (.[2 + $n] == "yes")
    })')

require_locale() {
  local candidate
  for candidate in $LOCALES; do
    [ "$1" = "$candidate" ] && return 0
  done
  fatal "unsupported locale: $1 (supported: $SUPPORTED_LOCALES)
add a locale by extending the section table in this script (issue-sections.sh); the header lists every place to touch"
}

require_kind() {
  local candidate
  for candidate in $KINDS; do
    [ "$1" = "$candidate" ] && return 0
  done
  fatal "unsupported kind: $1 (supported: $SUPPORTED_KINDS)"
}

require_key() {
  printf '%s' "$TABLE_JSON" | jq -e --arg k "$1" 'any(.key == $k)' >/dev/null \
    || fatal "unknown section key: $1 (see the section table in this script)"
}

require_file() {
  # require_file <path> <役割>
  [ -f "$1" ] || fatal "$2 not found or not a regular file: $1"
}

scan_headings() {
  # scan_headings <file> → 1 行 1 見出しで `<行番号>\t<見出しテキスト>`。
  # フェンス内の行は見出しとして扱わない
  awk '
    /^[ \t]*```/ { fence = !fence; next }
    fence { next }
    /^## / {
      h = substr($0, 4)
      gsub(/^[ \t\r]+/, "", h)
      gsub(/[ \t\r]+$/, "", h)
      printf "%d\t%s\n", NR, h
    }
  ' "$1"
}

normalize_body() {
  # stdin の前後の空行（空白のみの行を含む）を落とし、各行の行末 CR を除去する。
  # CR を落とすのは、GitHub の Web UI で書かれた本文が API 経由で CRLF になるため
  # （消費側は body を "なし" 等の固定マーカーと突き合わせるので、\r が残ると一致しない）
  awk '
    { sub(/\r$/, ""); lines[NR] = $0 }
    END {
      first = 0
      last = 0
      for (i = 1; i <= NR; i++) {
        if (lines[i] ~ /[^ \t]/) {
          if (first == 0) first = i
          last = i
        }
      }
      if (first == 0) exit
      for (i = first; i <= last; i++) print lines[i]
    }
  '
}

# 共通フラグの解析。値はグローバルの locale / kind / mapping_file に、位置引数は positional に置く。
# サブコマンドごとにループを持つと、フラグの綴りと検証の文言が 4 箇所に複製されて片方だけ直る
# 事故になるため、文法はここに 1 つだけ置き、各 cmd_* は解析後の検証だけを行う
locale=''
kind=''
mapping_file=''
positional=()
parse_flags() {
  locale=''
  kind=''
  mapping_file=''
  positional=()
  while [ $# -gt 0 ]; do
    case "$1" in
      --locale|--kind|--mapping)
        [ -n "${2:-}" ] || fatal "$1 requires a value
$USAGE"
        case "$1" in
          --locale) locale=$2 ;;
          --kind) kind=$2 ;;
          --mapping) mapping_file=$2 ;;
        esac
        shift 2
        ;;
      -*) fatal "unknown flag: $1
$USAGE" ;;
      *)
        positional+=("$1")
        shift
        ;;
    esac
  done
}

MAPPING_JSON='[]'
parse_mapping() {
  # parse_mapping <file> → [{key, heading}] を MAPPING_JSON に置く。形式不正・未知のキーは fatal。
  # 戻り値を $( ) で受けないのは、コマンド置換のサブシェルで fatal を呼んでもサブシェルしか終わらず、
  # 親は空の mapping で検査を続けてしまうため（issue-hierarchy.sh の resolve_blockers と同じ理由）。
  # 行の形の判定を 1 本の jq に寄せているのは、grep(POSIX ERE) と jq(Oniguruma) に分けて書くと
  # 受け付ける形を変えたときに片方だけ直り、両者が食い違うため
  local file=$1 parsed bad unknown duplicate
  parsed=$(jq -Rs --argjson t "$TABLE_JSON" '
    split("\n")
    | map(select(test("^[[:space:]]*(#|$)") | not))
    | map({raw: ., entry: (capture("^[[:space:]]*(?<key>[^[:space:]]+)[[:space:]]+(?<heading>.*[^[:space:]])") // null)})
    | [.[] | select(.entry != null) | .entry] as $entries
    | {
        bad: ([.[] | select(.entry == null) | .raw] | first),
        unknown: ([$entries[].key | select(. as $k | ($t | any(.key == $k)) | not)] | first),
        duplicate_key: ([$entries[].key] | group_by(.) | map(select(length > 1) | .[0]) | first),
        duplicate_heading: ([$entries[].heading] | group_by(.) | map(select(length > 1) | .[0]) | first),
        entries: $entries
      }' < "$file")

  bad=$(printf '%s' "$parsed" | jq -r '.bad // empty')
  [ -z "$bad" ] || fatal "malformed mapping line (expected: <key> <template heading>): $bad"
  unknown=$(printf '%s' "$parsed" | jq -r '.unknown // empty')
  [ -z "$unknown" ] || fatal "unknown section key in the mapping: $unknown (see the section table in this script)"
  duplicate=$(printf '%s' "$parsed" | jq -r '.duplicate_key // empty')
  [ -z "$duplicate" ] || fatal "section key mapped more than once: $duplicate"
  duplicate=$(printf '%s' "$parsed" | jq -r '.duplicate_heading // empty')
  [ -z "$duplicate" ] || fatal "template heading mapped from more than one key: $duplicate"
  MAPPING_JSON=$(printf '%s' "$parsed" | jq -c '.entries')
}

# check の判定は 1 本の jq プログラムに寄せる。字種判定が jq の Oniguruma
# （\p{Hiragana} 等。BSD grep に \p{} は無い）でしか素直に書けず、見出しごとに外部コマンドを
# 呼ぶより解決規則を 1 箇所にまとめた方が読めるため。ロケール固有なのは script_matches だけで、
# 残りは $table を汎用的に走査する
CHECK_JQ='
def script_matches($heading; $loc):
  ($heading | test("\\p{Hiragana}|\\p{Katakana}|\\p{Han}")) as $has_ja
  | if $loc == "ja" then $has_ja else ($has_ja | not) end;

($mapping | map({key: .heading, value: .key}) | from_entries) as $key_of_mapped
| ($mapping | map({key: .key, value: .heading}) | from_entries) as $heading_of_mapped
| ($table | map({key: .key, value: .}) | from_entries) as $row_of
| [ $headings[] as $h
    | { heading: $h,
        mapped: ($key_of_mapped | has($h)),
        key: (if $key_of_mapped | has($h) then $key_of_mapped[$h]
              else first($table[] | select(.headings | to_entries | any(.value == $h)) | .key) // null
              end) } ] as $hs
| ($hs | map(.key) | map(select(. != null))) as $present
| ( [ $table[] | select(.required_on | index($kind)) | .key ]
    | map(select(. as $k | ($present | index($k)) == null))
    | map(. as $k
          | { code: 2,
              message: ("missing required section: " + $k + " (expected heading: \""
                + ($heading_of_mapped[$k] // $row_of[$k].headings[$locale]) + "\")") }) ) as $r1
| ( $hs | map(select(.key == null))
    | map({code: 3, message: ("unknown heading: \"" + .heading + "\"")}) ) as $r2
| ( $mapping | map(select(.key as $k | $row_of[$k].template_mappable | not))
    | map({code: 4,
           message: ("machine-consumed key must keep its canonical heading: " + .key
             + " (mapped to \"" + .heading + "\")")}) ) as $r3
| ( $hs | map(select(.key != null and (.mapped | not)))
    | map(select(script_matches(.heading; $locale) | not))
    | map({code: 5,
           message: ("heading locale mismatch: \"" + .heading + "\" is not " + $locale
             + " (canonical " + $locale + " heading for " + .key + ": \""
             + $row_of[.key].headings[$locale] + "\")")}) ) as $r4
| $r1 + $r2 + $r3 + $r4
'

markers_json() {
  # マーカー表 → {key: {ロケール: マーカー}}。list と schema の両方が使う
  printf '%s\n' "$NONE_MARKER_TABLE" | jq -Rs --argjson locales "$LOCALES_JSON" '
    split("\n") | map(select(length > 0)) | map(split("|"))
    | map({key: .[0], value: ([$locales, .[1:]] | transpose | map({key: .[0], value: .[1]}) | from_entries)})
    | from_entries'
}

cmd_schema() {
  local key
  parse_flags "$@"
  [ "${#positional[@]}" -eq 1 ] || fatal "$USAGE"
  [ -z "$locale" ] && [ -z "$kind" ] && [ -z "$mapping_file" ] || fatal "schema takes no flags
$USAGE"
  key=${positional[0]}
  require_key "$key"

  printf '%s' "$TABLE_JSON" | jq --arg k "$key" --argjson markers "$(markers_json)" '
    .[] | select(.key == $k)
    | {key, headings, required_on, template_mappable, none_markers: ($markers[$k] // null)}'
}

cmd_list() {
  parse_flags "$@"
  [ "${#positional[@]}" -eq 0 ] && [ -n "$locale" ] && [ -n "$kind" ] || fatal "$USAGE"
  [ -z "$mapping_file" ] || fatal "list takes no --mapping
$USAGE"
  require_locale "$locale"
  require_kind "$kind"

  jq -n --argjson table "$TABLE_JSON" --argjson markers "$(markers_json)" \
    --arg locale "$locale" --arg kind "$kind" '
    {locale: $locale, kind: $kind,
     sections: ($table | map({
       key: .key,
       heading: .headings[$locale],
       required: ((.required_on | index($kind)) != null),
       required_on: .required_on,
       template_mappable: .template_mappable,
       none_markers: ($markers[.key] // null)
     }))}'
}

cmd_check() {
  local file headings_json reasons
  parse_flags "$@"
  [ "${#positional[@]}" -eq 1 ] && [ -n "$locale" ] && [ -n "$kind" ] || fatal "$USAGE"
  file=${positional[0]}
  require_locale "$locale"
  require_kind "$kind"
  require_file "$file" 'draft file'
  if [ -n "$mapping_file" ]; then
    require_file "$mapping_file" 'mapping file'
    parse_mapping "$mapping_file"
  fi

  headings_json=$(to_string_array "$(scan_headings "$file" | cut -f2-)")

  reasons=$(jq -nc --argjson table "$TABLE_JSON" --argjson mapping "$MAPPING_JSON" \
    --argjson headings "$headings_json" --arg locale "$locale" --arg kind "$kind" "$CHECK_JQ")

  [ "$reasons" = '[]' ] && return 0

  printf '%s' "$reasons" | jq -r '.[].message' >&2
  exit "$(printf '%s' "$reasons" | jq 'map(.code) | min')"
}

lookup_locale() {
  # lookup_locale <見出し> <`<見出し>\t<ロケール>` の行リスト> → 一致したロケールを stdout。無ければ非ゼロ
  local text=$1 candidate candidate_locale
  while IFS=$'\t' read -r candidate candidate_locale; do
    if [ "$text" = "$candidate" ]; then
      printf '%s' "$candidate_locale"
      return 0
    fi
  done <<CANDIDATES
$2
CANDIDATES
  return 1
}

cmd_find() {
  local file key key_headings scan start end matched matched_locale body lineno text
  parse_flags "$@"
  [ "${#positional[@]}" -eq 2 ] || fatal "$USAGE"
  [ -z "$locale" ] && [ -z "$kind" ] && [ -z "$mapping_file" ] || fatal "find takes no flags
$USAGE"
  file=${positional[0]}
  key=${positional[1]}
  require_key "$key"
  require_file "$file" 'input file'

  key_headings=$(printf '%s' "$TABLE_JSON" | jq -r --arg k "$key" \
    '.[] | select(.key == $k) | .headings | to_entries[] | "\(.value)\t\(.key)"')
  scan=$(scan_headings "$file")

  # 見出しの一致判定を awk ではなく bash で行うのは、macOS の awk（one-true-awk 20200816）が
  # UTF-8 ロケールで多バイト文字列の == を誤判定するため（`LC_ALL=C` なら正しいが、
  # 呼び出し側のロケールに依存させない）。awk には ASCII 正規表現の行選別だけを任せる
  start=''
  matched=''
  matched_locale=''
  end=''
  while IFS=$'\t' read -r lineno text; do
    [ -n "$lineno" ] || continue
    if [ -z "$start" ]; then
      if matched_locale=$(lookup_locale "$text" "$key_headings"); then
        start=$lineno
        matched=$text
      fi
    else
      end=$lineno
      break
    fi
  done <<SCAN
$scan
SCAN
  [ -n "$start" ] || exit 6

  # 節が空（見出しの直後に次の見出しが来る）ときに sed を通さないのは、第 2 アドレスが
  # 第 1 以下だと POSIX sed が第 1 アドレスの行だけを選び、次節の見出し行が body に漏れるため
  if [ -n "$end" ] && [ "$((end - start))" -le 1 ]; then
    body=''
  elif [ -n "$end" ]; then
    body=$(sed -n "$((start + 1)),$((end - 1))p" "$file" | normalize_body)
  else
    body=$(sed -n "$((start + 1)),\$p" "$file" | normalize_body)
  fi

  jq -n --arg key "$key" --arg locale "$matched_locale" --arg heading "$matched" --arg body "$body" \
    '{key: $key, locale: $locale, heading: $heading, body: $body}'
}

[ $# -gt 0 ] || fatal "$USAGE"
subcommand=$1
shift
case "$subcommand" in
  schema) cmd_schema "$@" ;;
  list) cmd_list "$@" ;;
  check) cmd_check "$@" ;;
  find) cmd_find "$@" ;;
  *) fatal "unknown subcommand: $subcommand
$USAGE" ;;
esac
