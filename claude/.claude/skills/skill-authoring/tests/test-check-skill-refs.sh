#!/bin/bash

# check-skill-refs.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/skill-authoring/tests/test-check-skill-refs.sh
# 使い捨ての skills ディレクトリにフィクスチャ SKILL.md を組んで検査結果を検証する。
# 最後のケースだけは実リポジトリの skills/ を検査し、規約違反ゼロを担保する
# （このテストが repo の lint を兼ねる）。失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/check-skill-refs.sh"
REAL_SKILLS_DIR="$SCRIPT_DIR/../.."

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
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
    printf 'FAIL  %s %s\n' "$name" "$detail"
  fi
}

assert_json() {
  local name=$1 json=$2 expr=$3
  if printf '%s' "$json" | jq -e "$expr" >/dev/null 2>&1; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (json: %s)\n' "$name" "$(printf '%s' "$json" | jq -c . 2>/dev/null || printf '%s' "$json")"
  fi
}

assert_exit() {
  local name=$1 actual=$2 expected=$3
  assert "$name" "[ $actual -eq $expected ]" "(exit $actual, expected $expected)"
}

# フィクスチャ用 skills ディレクトリを作る。stdout にパスを返す
new_skills_dir() {
  local name=$1
  mkdir -p "$TMP/$name"
  printf '%s' "$TMP/$name"
}

# write_skill <skills-dir> <skill> <body>  — <skills-dir>/<skill>/SKILL.md を書く
write_skill() {
  local dir=$1 skill=$2 body=$3
  mkdir -p "$dir/$skill"
  printf -- '---\nname: %s\n---\n%s\n' "$skill" "$body" > "$dir/$skill/SKILL.md"
}

ref() {
  printf '@~/.claude/skills/%s' "$1"
}

# --- ケース1: 葉への 1 段参照のみ → 違反なし ---
d=$(new_skills_dir case1)
write_skill "$d" a "手順は $(ref b/SKILL.md) に従う。"
write_skill "$d" b "葉スキル。"
out=$(bash "$SCRIPT" "$d")
assert_exit 'leaf ref: exit 0' $? 0
assert_json 'leaf ref: no violations' "$out" '.violations == []'
assert_json 'leaf ref: warnings empty' "$out" '.warnings == []'
assert_json 'leaf ref: skills_dir reported' "$out" ".skills_dir == \"$d\""

# --- ケース2: 未被覆の入れ子 a → b → c → uncovered_nested ---
d=$(new_skills_dir case2)
write_skill "$d" a "手順は $(ref b/SKILL.md) に従う。"
write_skill "$d" b "プロトコルは $(ref c/SKILL.md) に従う。"
write_skill "$d" c "共有知識。"
out=$(bash "$SCRIPT" "$d")
assert_exit 'uncovered nested: exit 0' $? 0
assert_json 'uncovered nested: one violation' "$out" '.violations | length == 1'
assert_json 'uncovered nested: type/file/line/ref/nested' "$out" \
  '.violations[0] == {type: "uncovered_nested", file: "a/SKILL.md", line: 4, ref: "b/SKILL.md", nested: "c/SKILL.md"}'

# --- ケース3: 入れ子を root が直接参照して被覆 → 違反なし ---
d=$(new_skills_dir case3)
write_skill "$d" a "手順は $(ref b/SKILL.md) に従う。知識は $(ref c/SKILL.md) を使う。"
write_skill "$d" b "プロトコルは $(ref c/SKILL.md) に従う。"
write_skill "$d" c "共有知識。"
out=$(bash "$SCRIPT" "$d")
assert_json 'covered nested: no violations' "$out" '.violations == []'

# --- ケース4: 相互参照（b → a）は root 自身への戻りなので未被覆扱いにしない ---
d=$(new_skills_dir case4)
write_skill "$d" a "併用時は $(ref b/SKILL.md) を先に。"
write_skill "$d" b "併用時は $(ref a/SKILL.md) を後に。"
out=$(bash "$SCRIPT" "$d")
assert_json 'mutual ref: no violations' "$out" '.violations == []'

# --- ケース5: 存在しない参照先 → missing_target（依存にも数えない） ---
d=$(new_skills_dir case5)
write_skill "$d" a "手順は $(ref zzz/SKILL.md) に従う。"
out=$(bash "$SCRIPT" "$d")
assert_json 'missing target: one violation' "$out" '.violations | length == 1'
assert_json 'missing target: fields' "$out" \
  '.violations[0] == {type: "missing_target", file: "a/SKILL.md", line: 4, ref: "zzz/SKILL.md"}'

# --- ケース6: インラインコード内の参照 → ref_in_code、かつ依存にならない ---
d=$(new_skills_dir case6)
write_skill "$d" a "失敗時は \`$(ref b/SKILL.md)\` の「7-2」に準拠。"
write_skill "$d" b "プロトコルは $(ref c/SKILL.md) に従う。"
write_skill "$d" c "共有知識。"
out=$(bash "$SCRIPT" "$d")
assert_json 'inline code: one violation' "$out" '.violations | length == 1'
assert_json 'inline code: ref_in_code' "$out" \
  '.violations[0] == {type: "ref_in_code", file: "a/SKILL.md", line: 4, ref: "b/SKILL.md"}'

# 同じ行でコード外の参照が先にあっても、後続のコード内参照だけが in_code になる
d=$(new_skills_dir case6b)
write_skill "$d" a "$(ref b/SKILL.md) を使う（\`$(ref c/SKILL.md)\` は言及のみ）。"
write_skill "$d" b "葉。"
write_skill "$d" c "葉。"
out=$(bash "$SCRIPT" "$d")
assert_json 'inline code mixed line: one violation' "$out" '.violations | length == 1'
assert_json 'inline code mixed line: only the code-span ref' "$out" \
  '.violations[0].type == "ref_in_code" and .violations[0].ref == "c/SKILL.md"'

# --- ケース7: フェンスコードブロック内の参照 → ref_in_code ---
d=$(new_skills_dir case7)
write_skill "$d" a "$(printf '例:\n```\n%s\n```\n本文は %s に従う。' "$(ref b/SKILL.md)" "$(ref c/SKILL.md)")"
write_skill "$d" b "葉。"
write_skill "$d" c "葉。"
out=$(bash "$SCRIPT" "$d")
assert_json 'fenced code: one violation' "$out" '.violations | length == 1'
assert_json 'fenced code: ref_in_code at fenced line' "$out" \
  '.violations[0] == {type: "ref_in_code", file: "a/SKILL.md", line: 6, ref: "b/SKILL.md"}'

# --- ケース8: SKILL.md 以外の参照先ファイルも入れ子検査の対象 ---
d=$(new_skills_dir case8)
write_skill "$d" a "詳細は $(ref b/notes.md) を参照。"
write_skill "$d" b "葉。"
printf '補足。%s に従う。\n' "$(ref c/SKILL.md)" > "$d/b/notes.md"
write_skill "$d" c "共有知識。"
out=$(bash "$SCRIPT" "$d")
assert_json 'non-SKILL target: uncovered nested detected' "$out" \
  '.violations == [{type: "uncovered_nested", file: "a/SKILL.md", line: 4, ref: "b/notes.md", nested: "c/SKILL.md"}]'

# --- ケース9: 出力は file, line, type で安定ソート ---
d=$(new_skills_dir case9)
write_skill "$d" z "$(ref b/SKILL.md) と $(ref none/SKILL.md)。"
write_skill "$d" a "\`$(ref b/SKILL.md)\` に言及。"
write_skill "$d" b "$(ref c/SKILL.md) に従う。"
write_skill "$d" c "葉。"
out=$(bash "$SCRIPT" "$d")
assert_json 'sorted: order by file then line' "$out" \
  '[.violations[] | .file + ":" + (.line|tostring) + ":" + .type] == ["a/SKILL.md:4:ref_in_code", "z/SKILL.md:4:missing_target", "z/SKILL.md:4:uncovered_nested"]'

# --- ケース10: 前提不成立 → 非ゼロ exit + stderr ---
bash "$SCRIPT" "$TMP/does-not-exist" >/dev/null 2>"$TMP/err10a.txt"
assert_exit 'missing dir: non-zero exit' $? 1
assert 'missing dir: stderr present' "[ -s '$TMP/err10a.txt' ]"

mkdir -p "$TMP/empty"
bash "$SCRIPT" "$TMP/empty" >/dev/null 2>"$TMP/err10b.txt"
assert_exit 'no SKILL.md: non-zero exit' $? 1
assert 'no SKILL.md: stderr present' "[ -s '$TMP/err10b.txt' ]"

# --- ケース11: 実リポジトリの skills/ は規約違反ゼロ（repo lint を兼ねる） ---
out=$(bash "$SCRIPT" "$REAL_SKILLS_DIR")
assert_exit 'real skills dir: exit 0' $? 0
assert_json 'real skills dir: no violations' "$out" '.violations == []'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
