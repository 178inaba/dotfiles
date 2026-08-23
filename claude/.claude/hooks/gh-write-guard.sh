#!/bin/bash

# PreToolUse フック: gh の書き込み系サブコマンドの事故防止ガード
#
# ルール1: 対象リポジトリがコマンド上で明示されていることを必須化する
#   別リポジトリへ調査目的で cd した状態で、cwd の git remote が
#   暗黙参照されることによる「意図しないリポジトリへの Issue/PR 作成」
#   事故を防ぐ。守るべき不変条件は「対象リポジトリが明示されていること」で、
#   -R はその十分条件の1つに過ぎない。gh が受け付けるリポジトリ指定の形は
#   noun ごとに異なるため、判定も noun ごとに分ける:
#     - gh repo edit/delete/archive/unarchive/sync: verb 直後の位置引数が
#       OWNER/REPO・HOST/OWNER/REPO・リポジトリ URL であること。これらは -R を
#       持たず、-R を持たないサブコマンドでは GH_REPO も効かない（gh は GH_REPO を
#       -R の既定値として解決するため）。bare な REPO は gh が認証ユーザーを
#       補完するので、cwd 解決と同じく暗黙とみなして許可しない
#     - gh repo rename: 位置引数は新しいリポジトリ名なので -R または GH_REPO=
#     - gh issue/pr の selector を取る verb: -R / GH_REPO= に加え、位置引数が
#       Issue/PR の完全 URL でもよい。ブランチ名にもスラッシュが含まれるため
#       「スラッシュを含むか」ではなく URL の形そのものを判定する
#     - gh issue create / gh pr create / gh release / gh label: -R または GH_REPO=
#   verb 直後が --help / -h の出現は write ではないとみなして免除する（免除は
#   その出現のみで、同じコマンド行の後続の write は個別に判定する）。
#   -R / GH_REPO= の有無だけは全文字列を1回だけ判定して全出現で共有する（出現ごとの
#   引数解析はしない）。このため 1 行に write が複数並ぶと、どれか1つに -R があれば
#   他の出現も明示済みとみなされる（見逃し側の既知の限界。位置引数だけで判定する
#   gh repo edit/delete/archive/unarchive/sync は影響を受けないが、-R を参照する
#   gh repo rename は影響を受ける）。
#   既知の限界（いずれもブロック過剰側）: 出現走査は引用符を解析しないため、
#   inline --body 内のリテラル「gh <noun> <verb>」も出現として数える。
#   verb 直後の1トークンしか見ないため、gh が受理するフラグ先行形
#   （gh repo delete --yes owner/repo 等）もブロックされる（フラグを読み飛ばすと
#   --homepage https://github.com/o/r のようなフラグ値をリポジトリ指定と
#   誤認するため、読み飛ばさないことを選んだ意図的なトレードオフ）。
#
# ルール2: 複数行の本文を --body/-b で渡すことを禁止する（--body-file へ誘導）
#   --body "$(cat <<'EOF' ... EOF)" のような引用符レイヤの重なりが
#   誤エスケープを誘発し、本文にリテラルの \ が残る事故を防ぐ。
#
# ルール3: 本文中の項番目的とみられる素の #N を検出してブロックする
#   素の #数字 は GitHub で Issue/PR への自動リンクになるため、項目の
#   番号付け（指摘 #1, #2, ...）に使うと無関係な Issue/PR へ参照通知が
#   飛ぶ事故が起きる。#1〜#9 が3種類以上あれば項番とみなす。
#   GitHub がリンク化しない形は除外する: コードスパン・fenced code block 内、
#   数字の直後に英数字が続く形（#12 等の複数桁は実参照の可能性が高く、
#   #1a2b3c 等の hex カラー・#1st 等の序数はそもそも参照でない）。
#   OWNER/REPO#N 形式（直前が英数字）も意図的な参照とみなして除外する。
#   本文が取得できない場合（--body-file が stdin・読取不可等）は fail-open。
#   既知の限界: インライン --body は閉じ引用符を解析せず後続の引数も
#   走査対象になる（ブロック過剰側）。未クローズの fence 以降は検出されない
#   （fail-open 側）。
#
# ルール4: PR 本文中のバッククォート付き closing keyword を検出してブロックする
#   コードスパン・fenced code block 内の Closes/Fixes/Resolves #N は GitHub が
#   closing keyword として解釈せず、PR をマージしても Issue が自動 close されない
#   （open のまま残る事故）。対象は gh pr create / gh pr edit のみ
#   （closing keyword が効くのは PR 本文だけで、Issue 本文・コメントでは
#   単なるリンクにしかならないため）。キーワードは大文字小文字不問、
#   OWNER/REPO#N 形式も対象。番号なしのプレースホルダ（`Closes #N` 等）は
#   引用・文書化用途として許容する。本文取得不可時の fail-open と
#   既知の限界はルール3と同じ。
#
# 仕様:
#   - 入力: stdin に PreToolUse の JSON
#   - 対象: tool_name == "Bash" かつ command が gh の write サブコマンド
#   - いずれかのルールに違反していれば exit 2 (Claude にエラー返却)
#   - 対象外コマンドは exit 0 で素通り

set -euo pipefail

input=$(cat)

tool_name=$(printf '%s' "$input" | jq -r '.tool_name // empty')
[ "$tool_name" = "Bash" ] || exit 0

command=$(printf '%s' "$input" | jq -r '.tool_input.command // empty')
[ -n "$command" ] || exit 0

# 書き込み系とみなす verb の一覧（noun ごと）。リポジトリ指定が文脈上意味を持たない
# もの（gh repo create / fork / set-default、gh repo clone 等）は対象外とする。
issue_write_verbs='create|comment|edit|close|reopen|delete|develop|lock|unlock|pin|unpin|transfer'
pr_write_verbs='create|comment|edit|close|reopen|lock|unlock|merge|ready|revert|review|update-branch|checkout'
release_write_verbs='create|edit|delete|upload|delete-asset'
repo_write_verbs='edit|delete|archive|unarchive|rename|sync'
label_write_verbs='create|edit|delete|clone'

# コマンド文字列をそのまま受け取るため、値には引用符が付いたまま現れる。
# サブシェルを挟まないよう戻り値ではなく unquoted へ格納する（毎回の Bash 呼び出しで
# 走るフックなので fork を増やさない）。
unquoted=''
strip_outer_quotes() {
  unquoted=$1
  if [[ $unquoted == \"*\" || $unquoted == \'*\' ]]; then
    unquoted=${unquoted:1:${#unquoted}-2}
  fi
}

# 「gh <noun> <verb> [直後の1トークン]」を出現ごとに取り出す。位置引数の判定に
# 使うため verb 直後のトークンまで拾い、フラグは読み飛ばさない（ヘッダーのルール1）。
# トークンからシェル区切り（; & |）を除くのは、密着した区切り（gh pr view 1;gh pr
# merge 5）でトークンが後続の gh ごと飲み込み、その write が走査されないまま
# 素通りするのを防ぐため。区切りが残れば次の周回で先頭境界として機能する。
occurrence_pattern='(^|[^A-Za-z0-9_])gh[[:space:]]+(issue|pr|release|repo|label)[[:space:]]+([A-Za-z][A-Za-z-]*)([[:space:]]+([^[:space:];&|]+))?'

occ_nouns=()
occ_verbs=()
occ_tokens=()
rest=$command
while [[ $rest =~ $occurrence_pattern ]]; do
  matched=${BASH_REMATCH[0]}
  noun=${BASH_REMATCH[2]}
  verb=${BASH_REMATCH[3]}
  token=${BASH_REMATCH[5]:-}
  rest=${rest#*"$matched"}

  case $noun in
    issue) allowed_verbs=$issue_write_verbs ;;
    pr) allowed_verbs=$pr_write_verbs ;;
    release) allowed_verbs=$release_write_verbs ;;
    repo) allowed_verbs=$repo_write_verbs ;;
    label) allowed_verbs=$label_write_verbs ;;
  esac
  [[ $verb =~ ^($allowed_verbs)$ ]] || continue

  # gh repo edit "owner/repo" を弾かないため引用符を剥ぐ。
  strip_outer_quotes "$token"
  token=$unquoted

  # verb 直後の --help / -h は write ではない。この出現のみ免除する。
  if [ "$token" = "--help" ] || [ "$token" = "-h" ]; then
    continue
  fi

  occ_nouns+=("$noun")
  occ_verbs+=("$verb")
  occ_tokens+=("$token")
done

if [ ${#occ_nouns[@]} -eq 0 ]; then
  exit 0
fi

# --body/-b のインライン本文を抽出する（ルール2・ルール3 で共用）。
# --body-file は「--body」の直後が「-」のためパターンに一致せず、対象外となる。
body_flag_pattern='(^|[[:space:]])(--body|-b)([[:space:]=])'
inline_body=''
if [[ $command =~ $body_flag_pattern ]]; then
  inline_body=${command#*"${BASH_REMATCH[0]}"}
fi

# 複数行の本文を --body/-b で渡すことを禁止する。
if [[ $inline_body == *$'\n'* ]]; then
  cat >&2 <<EOF
gh の書き込み系サブコマンドで複数行の本文を --body/-b で渡すことは禁止しています。

実行しようとしたコマンド:
  $command

理由:
  --body "\$(...)" と HEREDOC を組み合わせると引用符レイヤが重なり、
  誤ったエスケープ（バッククォート前の不要なバックスラッシュ等）が
  そのまま本文に残る事故が起きます。

対処:
  1. 本文を一時ファイル（scratchpad 等）に Write で書き出す
  2. --body の代わりに --body-file <path> を指定して再実行する
       例: gh pr edit -R owner/repo 123 --body-file /path/to/body.md
EOF
  exit 2
fi

# ルール3: 本文中の項番目的とみられる素の #N を検出する。
# GitHub がリンク化しない箇所（fenced code block・インラインコード）を
# 除去した上で、単語頭の #1〜#9 の異なり数を数える。
count_bare_hash_refs() {
  awk '
    /^[[:space:]]*(```|~~~)/ { fence = !fence; next }
    fence { next }
    {
      gsub(/`[^`]*`/, "")
      for (i = 1; i <= NF; i++) {
        t = $i
        if (t !~ /^[^[:alnum:]#]*#[1-9]([^[:alnum:]]|$)/) continue
        sub(/^[^#]*#/, "", t)
        d = substr(t, 1, 1)
        if (!(d in seen)) { seen[d] = 1; n++ }
      }
    }
    END { print n + 0 }
  '
}

# 本文の取得はルール3・ルール4で共用する。
distinct_refs=0
body_source=''
body_content=''
body_file_pattern='(^|[[:space:]])(--body-file|-F)([[:space:]]+|=)("[^"]*"|'\''[^'\'']*'\''|[^[:space:]]+)'
if [[ $command =~ $body_file_pattern ]]; then
  strip_outer_quotes "${BASH_REMATCH[4]}"
  body_path=$unquoted
  if [ "$body_path" != "-" ] && [ -r "$body_path" ] && [ -f "$body_path" ]; then
    body_content=$(cat "$body_path")
    body_source="--body-file $body_path"
  fi
elif [ -n "$inline_body" ]; then
  body_content=$inline_body
  body_source='--body/-b の本文'
fi

if [ -n "$body_source" ]; then
  distinct_refs=$(printf '%s\n' "$body_content" | count_bare_hash_refs)
fi

if [ "$distinct_refs" -ge 3 ]; then
  cat >&2 <<EOF
gh の書き込み系サブコマンドの本文に、項番とみられる素の #N を検出しました
（#1〜#9 のうち ${distinct_refs} 種類）。

検出元: $body_source

理由:
  素の #数字 は GitHub で Issue/PR への自動リンクになるため、項目の
  番号付け（指摘 #1, #2, ...）に使うと、無関係な Issue/PR に参照通知
  （mentioned 表示）が飛ぶ事故が起きます。通知は後から取り消せません。

対処:
  1. 項番が目的の場合: 順序リスト（1. 2. ...）等、# を使わない形式に書き換える
  2. 実際に Issue/PR を参照する意図の場合: OWNER/REPO#N 形式で明示する
       例: 178inaba/dotfiles#3
     （リンクは維持され、このガードにも掛かりません）
EOF
  exit 2
fi

# ルール4: PR 本文中のバッククォート付き closing keyword を検出する。
# fenced code block 内は行全体を、それ以外の行はコードスパン内のみを走査する。
# GitHub は「keyword（+ 任意の :）+ 空白 + 参照」の直接隣接のみ解釈するため、
# 検出も同じ隣接形に限定する。番号なし（#N 等のプレースホルダ）は対象外。
has_quoted_closing_keyword() {
  awk '
    function has_kw(s) {
      return s ~ /(^|[^[:alnum:]])(close[sd]?|fix(e[sd])?|resolve[sd]?):?[[:space:]]+([[:alnum:]_.-]+\/[[:alnum:]_.-]+)?#[0-9]+/
    }
    {
      line = tolower($0)
      if (line ~ /^[[:space:]]*(```|~~~)/) { fence = !fence; next }
      if (fence) { if (has_kw(line)) found = 1; next }
      while (match(line, /`[^`]*`/)) {
        if (has_kw(substr(line, RSTART, RLENGTH))) found = 1
        line = substr(line, RSTART + RLENGTH)
      }
    }
    END { exit !found }
  '
}

pr_body_pattern='(^|[^A-Za-z0-9_])gh[[:space:]]+pr[[:space:]]+(create|edit)([[:space:]]|$)'
if [ -n "$body_source" ] && printf '%s' "$command" | grep -qE "$pr_body_pattern" \
  && printf '%s\n' "$body_content" | has_quoted_closing_keyword; then
  cat >&2 <<EOF
PR 本文中に、バッククォート（コードスパン/コードブロック）で囲まれた
GitHub closing keyword（Closes/Fixes/Resolves #N）を検出しました。

検出元: $body_source

理由:
  バッククォートで囲むと GitHub が closing keyword として解釈せず、
  PR をマージしても対象 Issue が自動 close されません（open のまま残る事故）。

対処:
  1. Issue を自動 close する意図の場合: バッククォートを外して生のまま書く
       例: Closes #656
  2. closing keyword を引用・文書化する意図の場合: 実番号を避けて
     プレースホルダに置き換える（例: \`Closes #N\`。番号がなければ検出されません）
EOF
  exit 2
fi

# ルール1: 対象リポジトリの明示を判定する。
# -R <value> / --repo <value> / --repo=<value> と GH_REPO= のコマンド前置は、gh が
# GH_REPO を -R の既定値として解決するため通過条件としては同じもの。1つの真偽値に
# まとめ、全文字列で1回だけ求める（採否は出現ごとに noun で決める）。
repo_flag_or_env_pattern='(^|[[:space:]])((-R|--repo)[[:space:]=]|GH_REPO=)'
has_repo_flag_or_env=0
if [[ $command =~ $repo_flag_or_env_pattern ]]; then
  has_repo_flag_or_env=1
fi

# 位置引数がリポジトリを指しているか。OWNER/REPO・HOST/OWNER/REPO はスラッシュの
# 個数（1〜2）で bare REPO と区別する。
repo_positional_pattern='^(https?://[^/[:space:]]+/[^/[:space:]]+/[^/[:space:]]+(\.git)?/?|[A-Za-z0-9_.][A-Za-z0-9_.-]*/[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)?)$'
issue_url_pattern='^https?://[^/[:space:]]+/[^/[:space:]]+/[^/[:space:]]+/(issues|pull)/[0-9]+/?$'

occurrence_is_explicit() {
  local noun=$1 verb=$2 token=$3
  case "$noun:$verb" in
    repo:rename)
      # 位置引数は新しいリポジトリ名なのでリポジトリの明示には使えない。
      [ "$has_repo_flag_or_env" = 1 ] ;;
    repo:*)
      # -R を持たないため、位置引数だけがリポジトリ明示の手段になる。
      [[ $token =~ $repo_positional_pattern ]] ;;
    issue:create|pr:create)
      # selector を取らないため URL で示す余地がない。
      [ "$has_repo_flag_or_env" = 1 ] ;;
    issue:*|pr:*)
      [ "$has_repo_flag_or_env" = 1 ] || [[ $token =~ $issue_url_pattern ]] ;;
    *)
      # release / label の位置引数はタグ名・ラベル名でリポジトリではない。
      [ "$has_repo_flag_or_env" = 1 ] ;;
  esac
}

fail_noun=''
fail_verb=''
for ((i = 0; i < ${#occ_nouns[@]}; i++)); do
  if ! occurrence_is_explicit "${occ_nouns[$i]}" "${occ_verbs[$i]}" "${occ_tokens[$i]}"; then
    fail_noun=${occ_nouns[$i]}
    fail_verb=${occ_verbs[$i]}
    break
  fi
done

if [ -z "$fail_noun" ]; then
  exit 0
fi

# 復旧手順は、通過条件が noun ごとに違うため出現に合わせて出し分ける
# （固定の -R 例だけを出すと、-R を持たない gh repo で「満たせない」と誤解される）。
repo_flag_hint='  ※ 現在のディレクトリの remote が正しいと確信できる場合も、
       gh repo view --json nameWithOwner -q .nameWithOwner
     で取得した値を -R に渡して明示する'
case "$fail_noun:$fail_verb" in
  repo:rename)
    recovery="  1. 対象リポジトリを -R owner/repo で明示して再実行する
       例: gh repo rename new-name -R owner/repo
     gh repo rename の位置引数は新しいリポジトリ名で、リポジトリの明示にはなりません。
$repo_flag_hint" ;;
  repo:*)
    recovery="  1. 対象リポジトリを verb 直後の位置引数で明示して再実行する
       例: gh repo $fail_verb owner/repo ...
     形式は OWNER/REPO・HOST/OWNER/REPO・リポジトリ URL のいずれかです。
     bare な REPO は gh が認証ユーザーで補完するため明示になりません。
  2. gh repo $fail_verb に -R/--repo はなく、環境変数によるリポジトリ指定も効きません。
     位置引数はフラグより前（verb の直後）に置いてください。" ;;
  issue:create|pr:create)
    recovery="  1. 対象リポジトリを -R owner/repo で明示して再実行する
       例: gh $fail_noun create -R owner/repo ...
     create は selector を取らないため、URL でリポジトリを示す形はありません。
$repo_flag_hint" ;;
  issue:*|pr:*)
    # selector を取る verb だけが URL 形を使えるので、create とは別に扱う
    # （create に URL 例を出すと、通らない復旧手順を提示することになる）。
    if [ "$fail_noun" = pr ]; then
      url_example='https://github.com/owner/repo/pull/123'
    else
      url_example='https://github.com/owner/repo/issues/123'
    fi
    recovery="  1. 対象リポジトリを -R owner/repo で明示して再実行する
       例: gh $fail_noun $fail_verb -R owner/repo ...
  2. 対象を完全な URL で指定する（URL にリポジトリが含まれます）
       例: gh $fail_noun $fail_verb $url_example ...
     番号のみ・ブランチ名は cwd の remote で解決されるため明示になりません。
$repo_flag_hint" ;;
  *)
    recovery="  1. 対象リポジトリを -R owner/repo で明示して再実行する
       例: gh $fail_noun $fail_verb -R owner/repo ...
$repo_flag_hint" ;;
esac

current_dir=$(pwd)
current_remote=$(git remote get-url origin 2>/dev/null || printf '(取得不可: git リポジトリ外、または origin が未設定)')

cat >&2 <<EOF
gh の書き込み系サブコマンドは、対象リポジトリをコマンド上で明示することを必須としています。

実行しようとしたコマンド:
  $command

明示が確認できなかった箇所: gh $fail_noun $fail_verb
現在の作業ディレクトリ: $current_dir
現在の origin remote: $current_remote

このまま実行すると、上記の origin (もしくは gh が解決する remote) が対象になります。
別リポジトリの調査中などに cwd を取り違えると、意図しないリポジトリに書き込みが行われます。

対処:
$recovery
EOF

exit 2
