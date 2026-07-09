#!/bin/bash

# PreToolUse フック: gh の書き込み系サブコマンドの事故防止ガード
#
# ルール1: -R/--repo の指定を必須化する
#   別リポジトリへ調査目的で cd した状態で、cwd の git remote が
#   暗黙参照されることによる「意図しないリポジトリへの Issue/PR 作成」
#   事故を防ぐ。
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

# gh の書き込み系サブコマンドにマッチするか判定。
# noun ごとに verb を限定し、-R が文脈上意味を持たないもの（gh repo create / fork /
# set-default、gh repo clone 等）は対象外とする。
write_pattern='(^|[^A-Za-z0-9_])gh[[:space:]]+(issue[[:space:]]+(create|comment|edit|close|reopen|delete|develop|lock|unlock|pin|unpin|transfer)|pr[[:space:]]+(create|comment|edit|close|reopen|lock|unlock|merge|ready|revert|review|update-branch|checkout)|release[[:space:]]+(create|edit|delete|upload|delete-asset)|repo[[:space:]]+(edit|delete|archive|unarchive|rename|sync)|label[[:space:]]+(create|edit|delete|clone))([[:space:]]|$)'

if ! printf '%s' "$command" | grep -qE "$write_pattern"; then
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

distinct_refs=0
body_source=''
body_file_pattern='(^|[[:space:]])(--body-file|-F)([[:space:]]+|=)("[^"]*"|'\''[^'\'']*'\''|[^[:space:]]+)'
if [[ $command =~ $body_file_pattern ]]; then
  body_path=${BASH_REMATCH[4]}
  if [[ $body_path == \"*\" || $body_path == \'*\' ]]; then
    body_path=${body_path:1:${#body_path}-2}
  fi
  if [ "$body_path" != "-" ] && [ -r "$body_path" ] && [ -f "$body_path" ]; then
    distinct_refs=$(count_bare_hash_refs < "$body_path")
    body_source="--body-file $body_path"
  fi
elif [ -n "$inline_body" ]; then
  distinct_refs=$(printf '%s\n' "$inline_body" | count_bare_hash_refs)
  body_source='--body/-b の本文'
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

# -R <value> / --repo <value> / --repo=<value> のいずれかがあれば許可。
repo_flag_pattern='(^|[[:space:]])(-R|--repo)([[:space:]=])'
if printf '%s' "$command" | grep -qE "$repo_flag_pattern"; then
  exit 0
fi

# GH_REPO 環境変数がコマンド前置で明示されている場合も許可。
# 例: GH_REPO=owner/repo gh issue create ...
if printf '%s' "$command" | grep -qE '(^|[[:space:]])GH_REPO='; then
  exit 0
fi

current_dir=$(pwd)
current_remote=$(git remote get-url origin 2>/dev/null || printf '(取得不可: git リポジトリ外、または origin が未設定)')

cat >&2 <<EOF
gh の書き込み系サブコマンドは -R/--repo によるリポジトリ明示を必須としています。

実行しようとしたコマンド:
  $command

現在の作業ディレクトリ: $current_dir
現在の origin remote: $current_remote

このまま実行すると、上記の origin (もしくは gh が解決する remote) が対象になります。
別リポジトリの調査中などに cwd を取り違えると、意図しないリポジトリに書き込みが行われます。

対処:
  1. 対象リポジトリを -R owner/repo で明示して再実行する
       例: gh issue create -R 178inaba/dotfiles --title "..." --body "..."
  2. 現在のディレクトリの remote が正しいと確信できる場合も、
       gh repo view --json nameWithOwner -q .nameWithOwner
     で取得した値を -R に渡して明示する
EOF

exit 2
