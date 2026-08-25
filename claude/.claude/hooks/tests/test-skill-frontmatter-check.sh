#!/bin/bash

# skill-frontmatter-check.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-skill-frontmatter-check.sh
# 失敗したケースがあれば exit 1 で終了する。
#
# フックに PostToolUse のペイロードを stdin で食わせ、exit code と stderr を見る。
# フィクスチャは mktemp -d 配下に <skill>/SKILL.md を組む（実リポジトリには触れない）。
# unquoted_flow / missing_field / name_mismatch のフィクスチャは frontmatter を
# **解析できる形**に保つ。壊れていると check-skill-frontmatter.sh の排他規則で
# invalid_yaml だけが返り、そのケースが何も検証しないため。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../skill-frontmatter-check.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

# 検査スクリプトが yq を要求するため、スイート側でも前提を明示する
command -v yq >/dev/null || {
  printf 'ERROR: yq is required to run this suite\n' >&2
  exit 1
}

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

pass=0
fail=0

# write_skill <skill> <frontmatter 行...>  — 標準的な body を付けて書き、パスを stdout に返す
write_skill() {
  local skill=$1
  shift
  mkdir -p "$TMP/$skill"
  {
    printf -- '---\n'
    printf '%s\n' "$@"
    printf -- '---\n\n# /%s\n' "$skill"
  } >"$TMP/$skill/SKILL.md"
  printf '%s' "$TMP/$skill/SKILL.md"
}

payload() {
  local tool=$1 field=$2 target=$3 cwd=${4:-$TMP}
  printf '{"tool_name":"%s","tool_input":{"%s":"%s"},"cwd":"%s"}' "$tool" "$field" "$target" "$cwd"
}

# run_hook <input> — stdout / stderr をファイルに落とし、exit code を返す
STDOUT="$TMP/stdout.txt"
STDERR="$TMP/stderr.txt"
run_hook() {
  printf '%s' "$1" | "$HOOK" >"$STDOUT" 2>"$STDERR"
}

record() {
  local ok=$1 name=$2 detail=${3:-}
  if [ "$ok" -eq 0 ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s %s\n' "$name" "$detail"
  fi
}

assert_exit() {
  local name=$1 actual=$2 expected=$3
  if [ "$actual" -eq "$expected" ]; then
    record 0 "$name"
  else
    record 1 "$name" "(exit $actual, expected $expected)"
  fi
}

assert_stderr_has() {
  local name=$1 needle=$2
  if grep -qF -- "$needle" "$STDERR"; then
    record 0 "$name"
  else
    record 1 "$name" "(stderr: $(tr '\n' ' ' <"$STDERR"))"
  fi
}

# 「無出力」は stdout と stderr の両方で見る（受け入れ条件が "produce no output"）
assert_silent() {
  local name=$1
  if [ ! -s "$STDOUT" ] && [ ! -s "$STDERR" ]; then
    record 0 "$name"
  else
    record 1 "$name" "(stdout: $(tr '\n' ' ' <"$STDOUT") stderr: $(tr '\n' ' ' <"$STDERR"))"
  fi
}

# --- 4 種の違反それぞれで exit 2 + stderr への描画 ---

# unquoted_flow: 解析はできるが値が flow sequence になる
f=$(write_skill seqhint 'name: seqhint' 'description: 説明' 'argument-hint: [--yes]')
run_hook "$(payload Edit file_path "$f")"
assert_exit 'unquoted_flow: exit 2' $? 2
assert_stderr_has 'unquoted_flow: type on stderr' 'unquoted_flow'
assert_stderr_has 'unquoted_flow: key on stderr' 'argument-hint'
assert_stderr_has 'unquoted_flow: line on stderr' 'line 4'
# 描画パスはスクリプトの相対 file（<skill>/SKILL.md）ではなくペイロードの元パス
assert_stderr_has 'unquoted_flow: payload path on stderr' "$f"
# 再検査の案内は実際に呼んだスクリプト（相対トラバースで解決したもの）を出す。
# `~` 直書きだと、リポジトリ側で検査スクリプトを編集中に stow 済みの古いコピーを案内する
assert_stderr_has 'unquoted_flow: recheck hint names the invoked script' \
  "$(cd "$SCRIPT_DIR/../../skills/skill-authoring/scripts" && pwd)/check-skill-frontmatter.sh"

# invalid_yaml: 1 行に複数の flow sequence が並び解析不能
f=$(write_skill badyaml 'name: badyaml' 'description: 説明' 'argument-hint: [<a>] [--b]')
run_hook "$(payload Edit file_path "$f")"
assert_exit 'invalid_yaml: exit 2' $? 2
assert_stderr_has 'invalid_yaml: type on stderr' 'invalid_yaml'

# missing_field: description が無い
f=$(write_skill nodesc 'name: nodesc')
run_hook "$(payload Edit file_path "$f")"
assert_exit 'missing_field: exit 2' $? 2
assert_stderr_has 'missing_field: type on stderr' 'missing_field'
assert_stderr_has 'missing_field: field name on stderr' 'description'

# name_mismatch: name がディレクトリ名と不一致
f=$(write_skill mismatched 'name: something-else' 'description: 説明')
run_hook "$(payload Edit file_path "$f")"
assert_exit 'name_mismatch: exit 2' $? 2
assert_stderr_has 'name_mismatch: type on stderr' 'name_mismatch'
assert_stderr_has 'name_mismatch: expected on stderr' 'mismatched'
assert_stderr_has 'name_mismatch: actual on stderr' 'something-else'

# --- 対象ツールの網羅 ---

f=$(write_skill viawrite 'name: viawrite' 'description: 説明' 'argument-hint: [--yes]')
run_hook "$(payload Write file_path "$f")"
assert_exit 'Write: exit 2' $? 2

# NotebookEdit は notebook_path で対象を渡す
f=$(write_skill vianotebook 'name: vianotebook' 'description: 説明' 'argument-hint: [--yes]')
run_hook "$(payload NotebookEdit notebook_path "$f")"
assert_exit 'NotebookEdit: reads notebook_path' $? 2

# --- 素通りすべきケース ---

f=$(write_skill clean 'name: clean' 'description: 説明' 'argument-hint: "[--yes]"')
run_hook "$(payload Edit file_path "$f")"
assert_exit 'clean SKILL.md: exit 0' $? 0
assert_silent 'clean SKILL.md: no output'

printf 'readme\n' >"$TMP/README.md"
run_hook "$(payload Edit file_path "$TMP/README.md")"
assert_exit 'non-SKILL.md target: exit 0' $? 0
assert_silent 'non-SKILL.md target: no output'

# SKILL.md を接尾辞に持つだけのファイル。ペイロードには部分文字列 SKILL.md が現れるので
# jq 前の足切りを通過し、basename の完全一致で弾かれる経路を通る
printf 'x\n' >"$TMP/NOT-SKILL.md"
run_hook "$(payload Edit file_path "$TMP/NOT-SKILL.md")"
assert_exit 'suffix-only match: exit 0' $? 0
assert_silent 'suffix-only match: no output'

run_hook '{"tool_name":"Bash","tool_input":{"command":"ls"},"cwd":"'"$TMP"'"}'
assert_exit 'non-target tool: exit 0' $? 0
assert_silent 'non-target tool: no output'

# 入力の解析失敗・対象欠落は fail-open。stderr は jq のパースエラーが出るため
# 無出力は要求しない（既存フックと同じ挙動）
run_hook 'not-json'
assert_exit 'malformed json: exit 0' $? 0

run_hook ''
assert_exit 'empty stdin: exit 0' $? 0

run_hook '{"tool_name":"Edit","tool_input":{},"cwd":"'"$TMP"'"}'
assert_exit 'missing file_path: exit 0' $? 0
assert_silent 'missing file_path: no output'

# --- 相対パスは cwd 起点で解決する ---

write_skill relskill 'name: relskill' 'description: 説明' 'argument-hint: [--yes]' >/dev/null
run_hook "$(payload Edit file_path "relskill/SKILL.md" "$TMP")"
assert_exit 'relative path resolved against cwd: exit 2' $? 2
assert_stderr_has 'relative path: type on stderr' 'unquoted_flow'

# --- 検査を完了できなかった場合 ---

run_hook "$(payload Edit file_path "$TMP/ghost/SKILL.md")"
assert_exit 'missing target: exit 2' $? 2
assert_stderr_has 'missing target: says it was not checked' 'not checked'

# YQ_BIN はフックが子プロセスへ環境をそのまま渡すことで効く（#58 が残した差し替え口）。
# 関数呼び出しの前置代入は export 有無がシェル依存なので明示的に export する
f=$(write_skill needsyq 'name: needsyq' 'description: 説明')
export YQ_BIN="$TMP/does-not-exist/yq"
run_hook "$(payload Edit file_path "$f")"
assert_exit 'yq missing: exit 2' $? 2
assert_stderr_has 'yq missing: says it was not checked' 'not checked'
assert_stderr_has 'yq missing: relays the script stderr' 'yq is required'
unset YQ_BIN

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
