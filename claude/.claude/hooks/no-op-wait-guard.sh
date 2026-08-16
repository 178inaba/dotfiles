#!/bin/bash

# PreToolUse フック: 完了待ちを目的とした no-op コマンドのブロック
#
# 防ぐ事故:
#   バックグラウンドのコマンド・タスク・サブエージェントの完了を待つ際に、
#   ターンを終えずに `echo idle1` … `echo idle159` のような no-op Bash 呼び出しを
#   数秒間隔で反復する busy-wait。1 セッションで 541 回の実測があり、トークンの
#   浪費とトランスクリプトのノイズに加え、「親はターンを終えて待つ」ことを前提に
#   通知を抑止する idle-notify.sh のガードも無効化する。
#
# 1 回目でブロックする理由:
#   ループ内には thinking が一切なく、最初の no-op 以降はカウンタを
#   インクリメントするだけの反射的コピーになる。ターンを終える選択が残っている
#   唯一の点が 1 回目のため、閾値方式（N 回目からブロック）は機能しない。
#   よってカウント・状態ファイルを持たない。
#
# 判定:
#   コマンド全体の空白（スペース・タブ・改行）を 1 個のスペースに畳んで
#   前後をトリムし、正規化後の文字列が NO_OP_WAIT_PATTERN に**全体一致**する
#   場合のみブロックする。対象は以下の形に限る:
#     - [sleep <duration>;] echo|printf [<token>]
#     - [sleep <duration>;] true / :
#     - sleep <duration>
#   <duration> は小数部と s/m/h suffix を任意で取る（`sleep 5m` を抜け道に
#   しないため suffix 形も含める）。<token> は 24 文字以内の bare word で、
#   `'` か `"` の 1 組で囲まれていてもよい（引用してあっても no-op のため）。
#   `;` の前後の空白はどちらも任意。
#
#   全体一致にしているため、`|` `&&` `||` リダイレクト `$` バッククォート、
#   先頭の `sleep N;` 以外の `;` を含むコマンドは許可された文字集合に無く、
#   個別の除外リストを持たずに対象外になる（実コマンドを伴う polling ループ、
#   `sleep N; <real command>` 等はここで素通りする）。改行も空白として畳むのは、
#   複数行スクリプト中の echo は他行が残って全体一致しない一方、`sleep 1;` と
#   `echo w` を改行で分ける抜け道は塞げるため。
#
#   末尾に `;` が付く形（`echo idle1;`）や pwd・git status 等の別種の no-op は
#   対象外（形が変わったら本パターンを追う前にハーネス側へ報告する方針）。
#
# 仕様:
#   - 入力: stdin に PreToolUse の JSON
#   - 対象: tool_name == "Bash" かつ command が上記の no-op wait 形
#   - 該当すれば exit 2 (Claude にエラー返却)、それ以外は exit 0 で素通り
#   - 入力の解析に失敗した場合は fail-open（exit 2 にはならない）

set -euo pipefail

input=$(cat)

tool_name=$(printf '%s' "$input" | jq -r '.tool_name // empty')
[ "$tool_name" = "Bash" ] || exit 0

command=$(printf '%s' "$input" | jq -r '.tool_input.command // empty')
[ -n "$command" ] || exit 0

# 空白の連続を 1 個のスペースに畳み、前後をトリムする。
normalized=$(printf '%s' "$command" | tr -s '[:space:]' ' ')
normalized=${normalized# }
normalized=${normalized% }

duration='([0-9]+(\.[0-9]+)?|\.[0-9]+)[smh]?'
token='([A-Za-z0-9_-]{1,24}|'\''[A-Za-z0-9_-]{1,24}'\''|"[A-Za-z0-9_-]{1,24}")'
no_op_wait_pattern="^((sleep $duration ?; ?)?((echo|printf)( $token)?|true|:)|sleep $duration)\$"

if ! printf '%s' "$normalized" | grep -qE "$no_op_wait_pattern"; then
  exit 0
fi

cat >&2 <<EOF
バックグラウンド処理の完了待ちを目的とした no-op コマンドはブロックしています。

実行しようとしたコマンド:
  $command

対処:
  完了待ちはターンを終えて行ってください。バックグラウンドのコマンド・タスク・
  サブエージェントが完了すると通知が届き、セッションはそこから自動再開します。
  pwd・git status・true 等、別の no-op コマンドで置き換えるのも同じ busy-wait
  になるため避けてください。同一ターン内で結果が必要な場合は、バックグラウンド
  ではなくフォアグラウンドで実行し、Bash ツールの timeout パラメータ
  （最大 600000ms）で待ち時間の上限を指定してください。
EOF

exit 2
