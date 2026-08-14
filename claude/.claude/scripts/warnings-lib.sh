# スキルスクリプト共有の fatal / warnings 配管（source して使う。単体実行しない）
#
# skill-authoring の出力契約 — 前提不成立は英語 stderr + 非ゼロ exit、非致命の注意は
# JSON の warnings[] で返す — は全スクリプトで同一なのに、その綴りだけが割れていた
# （add_warning の連結2種、JSON 変換が関数 / inline jq / 別名関数の3種）。契約が同じものは
# 実装も1つに保たないと、スクリプトが増えるたびにドリフトが広がるためここへ一本化する。
# テストは scripts/tests/test-warnings-lib.sh。
#
# 提供する関数と、蓄積先のグローバル変数:
#   warnings                 蓄積先。source 時に空で初期化するので、set -u の呼び出し元が
#                            add_warning を1度も通らない経路でも参照できる（再 source は
#                            蓄積済みの内容を保つ）
#   fatal <msg>              英語メッセージを stderr に出して exit 1
#   add_warning <msg>        非致命の注意を1行ずつ warnings に追記する
#   to_string_array <list>   改行区切りの文字列リスト → JSON 文字列配列（空行は落とす）
#   warnings_json            蓄積した warnings を JSON 文字列配列で stdout に出す
#
# jq を要求するのは to_string_array / warnings_json の実行時のみ（source 時ではない）ため、
# 呼び出し元の `command -v jq` チェックより前に source してよい。

fatal() {
  printf '%s\n' "$1" >&2
  exit 1
}

# 初回 source でのみ空に初期化する。無条件の代入にしないのは、呼び出し元と、それが source する
# 別 lib の両方から読まれる形になったときに蓄積済みの warning を後勝ちで消さないため。
# 一方 `: "${warnings:=}"` だと親プロセスが export した warnings を継承してしまい、その値が
# 出力 JSON に混ざる（しかも改行終端されないので最初の実 warning と結合する）ため、
# 「同一プロセス内で蓄積済み」と「環境からの継承」を sentinel で区別する
if [ -z "${__warnings_lib_sourced:-}" ]; then
  __warnings_lib_sourced=1
  warnings=""
fi

add_warning() {
  warnings="${warnings}${1}
"
}

to_string_array() {
  printf '%s' "$1" | jq -Rs 'split("\n") | map(select(length > 0))'
}

warnings_json() {
  to_string_array "$warnings"
}
