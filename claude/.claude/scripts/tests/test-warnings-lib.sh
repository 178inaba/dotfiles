#!/bin/bash

# warnings-lib.sh のリグレッションテスト
#
# 実行: bash claude/.claude/scripts/tests/test-warnings-lib.sh
# lib は source して使う関数のみなので、実環境には触れない（fatal の stderr 捕捉に
# 使い捨てディレクトリだけ使う）。失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
LIB="$SCRIPT_DIR/../warnings-lib.sh"

if [ ! -f "$LIB" ]; then
  printf 'ERROR: lib not found: %s\n' "$LIB" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

pass=0
fail=0

assert() {
  local name=$1 cond=$2 detail=${3:-}
  if eval "$cond"; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s\n' "$name"
    [ -n "$detail" ] && printf '      %s\n' "$detail"
  fi
}

# --- ケース1: fatal は英語メッセージを stderr に出して exit 1 ---
# 実物の呼び出し元と同じくスクリプト全体を止めるため、サブシェルで駆動する
out=$(. "$LIB"; fatal 'boom happened' 2>"$TMP/err.txt")
status=$?
err=$(cat "$TMP/err.txt")

assert "fatal: exit code 1" "[ $status -eq 1 ]" "status=$status"
assert "fatal: message on stderr" "[ \"\$err\" = 'boom happened' ]" "stderr=$err"
assert "fatal: stdout empty" "[ -z \"\$out\" ]" "stdout=$out"

# 以降は lib を本体に読み込んでドライブする（fatal は呼ばない）
. "$LIB"

# --- ケース2: warnings は source 時に初期化される（set -u の呼び出し元でも参照できる） ---
assert "warnings: initialized at source time" "[ -z \"\$warnings\" ]" "warnings=$warnings"
assert "warnings_json: empty accumulator is []" \
  "[ \"\$(warnings_json)\" = '[]' ]" "$(warnings_json)"

# --- ケース3: add_warning の蓄積と順序（JSON 配列は投入順） ---
add_warning 'first warning'
add_warning 'second warning'
add_warning 'third warning'
json=$(warnings_json)

assert "add_warning: entries accumulated in input order" \
  "[ \"\$(printf '%s' \"\$json\" | jq -r '@tsv')\" = \$'first warning\tsecond warning\tthird warning' ]" \
  "$json"

# 再 source が蓄積済みの warning を消さない（呼び出し元と別 lib の両方から読まれる形への備え）
. "$LIB"
assert "re-source: accumulated warnings kept" \
  "[ \"\$(warnings_json | jq 'length')\" = '3' ]" "$(warnings_json)"

# 親プロセスが export した warnings は継承せずクリアする（環境が出力 JSON に混ざらない）
inherited=$(warnings='LEAKED FROM ENV' bash -c ". '$LIB'; add_warning 'real warning'; warnings_json")
assert "env inheritance: exported warnings does not leak" \
  "[ \"\$(printf '%s' \"\$inherited\" | jq -r '.[0]')\" = 'real warning' ]" "$inherited"

# --- ケース4: JSON 特殊文字を含む warning がエスケープされる ---
# 手書きエスケープを避けて jq -Rs に任せていることの回帰ガード
warnings=""
add_warning 'quote " and backslash \ inside'
json=$(warnings_json)

assert "escaping: valid JSON" "printf '%s' \"\$json\" | jq -e . >/dev/null" "$json"
assert "escaping: value round-trips unchanged" \
  "[ \"\$(printf '%s' \"\$json\" | jq -r '.[0]')\" = 'quote \" and backslash \\ inside' ]" "$json"

# --- ケース5: to_string_array は改行区切りリストを JSON 文字列配列にする ---
assert "to_string_array: empty input is []" \
  "[ \"\$(to_string_array '')\" = '[]' ]" "$(to_string_array '')"

list=$'alpha\nbravo\n'
array=$(to_string_array "$list")
assert "to_string_array: entries in order" \
  "[ \"\$(printf '%s' \"\$array\" | jq -r '@tsv')\" = \$'alpha\tbravo' ]" "$array"

# 空行は落とす（末尾改行や連続改行で空文字列要素が混ざらないこと）
blank_list=$'alpha\n\n\nbravo\n'
blank_array=$(to_string_array "$blank_list")
assert "to_string_array: blank lines dropped" \
  "[ \"\$(printf '%s' \"\$blank_array\" | jq 'length')\" = '2' ]" "$blank_array"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
