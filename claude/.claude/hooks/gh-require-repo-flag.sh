#!/bin/bash

# PreToolUse フック: gh の書き込み系サブコマンド実行時に -R/--repo の指定を必須化する
#
# 目的: 別リポジトリへ調査目的で cd した状態で、cwd の git remote が
#       暗黙参照されることによる「意図しないリポジトリへの Issue/PR 作成」
#       事故を防ぐ。
#
# 仕様:
#   - 入力: stdin に PreToolUse の JSON
#   - 対象: tool_name == "Bash" かつ command が gh の write サブコマンド
#   - -R / --repo / --repo= が無ければ exit 2 (Claude にエラー返却)
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
