#!/bin/bash

# check-skill-frontmatter.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/skill-authoring/tests/test-check-skill-frontmatter.sh
# 使い捨ての skills ディレクトリにフィクスチャ SKILL.md を組んで検査結果を検証する。
# 4 種の違反それぞれに、成立するフィクスチャと成立しない対照フィクスチャを置く。
# 最後のケースだけは実リポジトリの skills/ を検査し、規約違反ゼロを担保する
# （このテストが repo の lint を兼ねる）。失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/check-skill-frontmatter.sh"
REAL_SKILLS_DIR="$SCRIPT_DIR/../.."

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

YQ_PATH=$(command -v yq) || {
  printf 'ERROR: yq is required to run this suite\n' >&2
  exit 1
}

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

# assert_stderr <name> <file> <expected>  — stderr の全文一致を見る。
# 「非ゼロ exit かつ stderr 非空」だけだと、前提チェックへ到達する前に別の理由で
# 落ちた実行（PATH 破壊による command not found 等）まで PASS してしまう
assert_stderr() {
  local name=$1 file=$2 expected=$3 actual
  actual=$(cat "$file")
  if [ "$actual" = "$expected" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (stderr: %s)\n' "$name" "$actual"
  fi
}

# フィクスチャ用 skills ディレクトリを作る。stdout にパスを返す
new_skills_dir() {
  local name=$1
  mkdir -p "$TMP/$name"
  printf '%s' "$TMP/$name"
}

# write_skill <skills-dir> <skill> <frontmatter 行...>  — 標準的な body を付けて書く
write_skill() {
  local dir=$1 skill=$2
  shift 2
  mkdir -p "$dir/$skill"
  {
    printf -- '---\n'
    printf '%s\n' "$@"
    printf -- '---\n\n# /%s\n' "$skill"
  } >"$dir/$skill/SKILL.md"
}

# --- ケース1: 規約どおりのフィクスチャ → 違反なし ---
d=$(new_skills_dir case1)
write_skill "$d" alpha 'name: alpha' 'description: 何かをする'
out=$(bash "$SCRIPT" "$d")
assert_exit 'clean: exit 0' $? 0
assert_json 'clean: no violations' "$out" '.violations == []'
assert_json 'clean: warnings empty' "$out" '.warnings == []'
assert_json 'clean: target is the absolute dir' "$out" ".target == \"$d\""
assert_json 'clean: target is absolute' "$out" '.target | startswith("/")'

# --- ケース2: unquoted_flow と、その対照 ---
d=$(new_skills_dir case2)
write_skill "$d" seqhint 'name: seqhint' 'description: 説明' 'argument-hint: [--yes]'
write_skill "$d" maphint 'name: maphint' 'description: 説明' 'argument-hint: {a: 1}'
out=$(bash "$SCRIPT" "$d")
assert_exit 'unquoted flow: exit 0' $? 0
assert_json 'unquoted flow: mapping value flagged with key and line' "$out" \
  '.violations[0] == {type: "unquoted_flow", file: "maphint/SKILL.md", key: "argument-hint", line: 4}'
assert_json 'unquoted flow: sequence value flagged too' "$out" \
  '.violations[1] == {type: "unquoted_flow", file: "seqhint/SKILL.md", key: "argument-hint", line: 4}'
assert_json 'unquoted flow: exactly two' "$out" '.violations | length == 2'

# 対照: 引用符付き・`<` 始まり・bool・インデントされたブロックスカラーの中身
d=$(new_skills_dir case2b)
write_skill "$d" quoted 'name: quoted' 'description: 説明' 'argument-hint: "[--yes]"'
write_skill "$d" angle 'name: angle' 'description: 説明' 'argument-hint: <issue-number> [--worktree]'
write_skill "$d" boolean 'name: boolean' 'description: 説明' 'user-invocable: false'
write_skill "$d" blockscalar 'name: blockscalar' 'description: |' '  [not a sequence]'
out=$(bash "$SCRIPT" "$d")
assert_json 'unquoted flow control: quoted/angle/bool/block scalar all clean' "$out" '.violations == []'

# --- ケース3: missing_field と、その対照 ---
d=$(new_skills_dir case3)
write_skill "$d" nodesc 'name: nodesc'
write_skill "$d" emptydesc 'name: emptydesc' 'description: ""'
write_skill "$d" noname 'description: 説明'
# frontmatter ブロックそのものが無いファイル（invalid_yaml ではなく missing_field 2 件）
mkdir -p "$d/nofence"
printf '# /nofence\n\n本文だけのファイル。\n' >"$d/nofence/SKILL.md"
# `---`/`---` の空ブロックは解析できて null になるので、同じく missing_field
mkdir -p "$d/emptyblock"
printf -- '---\n---\n\n# /emptyblock\n' >"$d/emptyblock/SKILL.md"
out=$(bash "$SCRIPT" "$d")
assert_exit 'missing field: exit 0' $? 0
assert_json 'missing field: empty description flagged' "$out" \
  '.violations | any(. == {type: "missing_field", file: "emptydesc/SKILL.md", field: "description"})'
assert_json 'missing field: absent description flagged' "$out" \
  '.violations | any(. == {type: "missing_field", file: "nodesc/SKILL.md", field: "description"})'
assert_json 'missing field: absent name flagged without touching description' "$out" \
  '[.violations[] | select(.file == "noname/SKILL.md")]
   == [{type: "missing_field", file: "noname/SKILL.md", field: "name"}]'
assert_json 'missing field: no frontmatter block yields both fields' "$out" \
  '[.violations[] | select(.file == "nofence/SKILL.md")]
   == [{type: "missing_field", file: "nofence/SKILL.md", field: "name"},
       {type: "missing_field", file: "nofence/SKILL.md", field: "description"}]'
assert_json 'missing field: no frontmatter block is not invalid_yaml' "$out" \
  '[.violations[] | select(.file == "nofence/SKILL.md" and .type == "invalid_yaml")] == []'
assert_json 'missing field: empty frontmatter block yields both fields' "$out" \
  '[.violations[] | select(.file == "emptyblock/SKILL.md") | .field] == ["name", "description"]'

d=$(new_skills_dir case3b)
write_skill "$d" complete 'name: complete' 'description: 説明'
out=$(bash "$SCRIPT" "$d")
assert_json 'missing field control: both fields present is clean' "$out" '.violations == []'

# --- ケース4: name_mismatch と、その対照 ---
d=$(new_skills_dir case4)
write_skill "$d" actualdir 'name: someothername' 'description: 説明'
out=$(bash "$SCRIPT" "$d")
assert_json 'name mismatch: expected/actual reported' "$out" \
  '.violations == [{type: "name_mismatch", file: "actualdir/SKILL.md",
                    expected: "actualdir", actual: "someothername"}]'

d=$(new_skills_dir case4b)
write_skill "$d" matching 'name: matching' 'description: 説明'
out=$(bash "$SCRIPT" "$d")
assert_json 'name mismatch control: matching name is clean' "$out" '.violations == []'

# --- ケース5: invalid_yaml は単独で報告され、他 3 種を巻き込まない ---
# 値が複数の flow sequence に割れて解析できない形。name も description も無いので、
# 排他が効いていなければ missing_field / unquoted_flow が同時に出るはず
d=$(new_skills_dir case5)
mkdir -p "$d/broken"
printf -- '---\nargument-hint: [a] [b]\n---\n\n# /broken\n' >"$d/broken/SKILL.md"
out=$(bash "$SCRIPT" "$d")
assert_exit 'invalid yaml: exit 0' $? 0
assert_json 'invalid yaml: the only violation for that file' "$out" \
  '[.violations[] | select(.file == "broken/SKILL.md") | .type] == ["invalid_yaml"]'
assert_json 'invalid yaml: message present' "$out" \
  '.violations[0].message | length > 0'
assert_json 'invalid yaml: temp path stripped from message' "$out" \
  '.violations[0].message | test("bad file|/var/folders|/tmp/") | not'

# ケース5 が message の中身に踏み込まないのは、yq のエラー文言がバージョンで変わるため
# （`yaml: ...` と `go-yaml load error ...` の両方を観測済み）。「yq の stderr が message に
# 届き、ファイル名の前置きだけが剥がれる」という契約は、YQ_BIN に「stderr へ1行吐いて
# 失敗するだけの偽 yq」を挿して等値で固定する。stderr の中身は環境変数から読ませ、
# 偽 yq 本体へ埋め込まない（クォートの入れ子を避けるため）
mkdir -p "$TMP/fakeyq"
printf '#!/bin/bash\nprintf "%%s\\n" "$FAKE_YQ_STDERR" >&2\nexit 1\n' >"$TMP/fakeyq/yq"
chmod +x "$TMP/fakeyq/yq"

# 偽 yq は失敗しか返さないので、検査対象には解析できないスキルだけを置く（成功経路を
# 通らせない）
d=$(new_skills_dir case5b)
mkdir -p "$d/broken"
printf -- '---\nargument-hint: [a] [b]\n---\n\n# /broken\n' >"$d/broken/SKILL.md"

# 旧形式: temp パスの前置きが付く → 前置きだけが剥がれて説明がそのまま残る
out=$(FAKE_YQ_STDERR="Error: bad file '/var/folders/x/temp123': CANARY explanation" \
  YQ_BIN="$TMP/fakeyq/yq" bash "$SCRIPT" "$d")
assert_json 'invalid yaml: file prefix stripped, explanation kept verbatim' "$out" \
  '.violations == [{type: "invalid_yaml", file: "broken/SKILL.md",
                    message: "CANARY explanation"}]'

# 新形式: 前置きが無い → 剥がしが no-op になって行全体が message になる。runner の yq が
# 前置きを本当に省くのかは観測できない（観測した message は剥がした後の値）が、前置きの
# 無い行がそのまま通ることはどちらであっても要る性質
out=$(FAKE_YQ_STDERR="CANARY explanation without a file prefix" \
  YQ_BIN="$TMP/fakeyq/yq" bash "$SCRIPT" "$d")
assert_json 'invalid yaml: prefix-less stderr passes through unchanged' "$out" \
  '.violations == [{type: "invalid_yaml", file: "broken/SKILL.md",
                    message: "CANARY explanation without a file prefix"}]'

# --- ケース6: 単一ファイル引数 ---
d=$(new_skills_dir case6)
write_skill "$d" target 'name: target' 'description: 説明' 'argument-hint: [--flag]'
write_skill "$d" other 'name: other' 'description: 説明' 'argument-hint: [--other]'
out=$(bash "$SCRIPT" "$d/target/SKILL.md")
assert_exit 'single file: exit 0' $? 0
assert_json 'single file: only the given file inspected' "$out" \
  '.violations == [{type: "unquoted_flow", file: "target/SKILL.md",
                    key: "argument-hint", line: 4}]'
assert_json 'single file: target is the absolute file path' "$out" \
  ".target == \"$d/target/SKILL.md\""
assert_json 'single file: target is absolute' "$out" '.target | startswith("/")'

# --- ケース7: ディレクトリ引数はそのディレクトリだけを見る ---
d=$(new_skills_dir case7)
write_skill "$d" inside 'name: inside' 'description: 説明' 'argument-hint: [--flag]'
d2=$(new_skills_dir case7-other)
write_skill "$d2" outside 'name: outside' 'description: 説明' 'argument-hint: [--flag]'
out=$(bash "$SCRIPT" "$d")
assert_json 'directory: sibling tree not inspected' "$out" \
  '[.violations[] | .file] == ["inside/SKILL.md"]'
assert_json 'directory: target is absolute' "$out" '.target | startswith("/")'

# 直下の */SKILL.md だけを見る（さらに下の階層は対象外）
d=$(new_skills_dir case7b)
write_skill "$d" top 'name: top' 'description: 説明'
mkdir -p "$d/top/nested"
printf -- '---\nname: wrong\n---\n\n# nested\n' >"$d/top/nested/SKILL.md"
out=$(bash "$SCRIPT" "$d")
assert_json 'directory: not recursive' "$out" '.violations == []'

# --- ケース8: SKILL.md を持たないディレクトリ → warnings のみ ---
d=$(new_skills_dir case8)
write_skill "$d" withskill 'name: withskill' 'description: 説明'
mkdir -p "$d/noskill"
printf 'notes\n' >"$d/noskill/README.md"
out=$(bash "$SCRIPT" "$d")
assert_exit 'no SKILL.md dir: exit 0' $? 0
assert_json 'no SKILL.md dir: warned' "$out" \
  '.warnings == ["no SKILL.md in noskill/"]'
assert_json 'no SKILL.md dir: not a violation' "$out" \
  '[.violations[] | select(.file | startswith("noskill/"))] == []'

# --- ケース9: 前提不成立 → 非ゼロ exit + 期待どおりの stderr ---
bash "$SCRIPT" "$TMP/does-not-exist" >/dev/null 2>"$TMP/err-missing.txt"
assert_exit 'missing target: non-zero exit' $? 1
assert_stderr 'missing target: stderr names the target' "$TMP/err-missing.txt" \
  "target not found: $TMP/does-not-exist"

mkdir -p "$TMP/empty"
bash "$SCRIPT" "$TMP/empty" >/dev/null 2>"$TMP/err-empty.txt"
assert_exit 'no SKILL.md under dir: non-zero exit' $? 1
assert_stderr 'no SKILL.md under dir: stderr names the dir' "$TMP/err-empty.txt" \
  "no */SKILL.md found under $TMP/empty"

YQ_BIN="$TMP/does-not-exist/yq" bash "$SCRIPT" >/dev/null 2>"$TMP/err-yq.txt"
assert_exit 'yq missing: non-zero exit' $? 1
assert_stderr 'yq missing: stderr says so' "$TMP/err-yq.txt" 'yq is required'

# jq 欠如は PATH を潰して再現する。ただし空の PATH では jq のガードに到達する前に
# script_dir 解決の dirname が落ち、warnings-lib.sh の source ごと失敗して exit 127 に
# なる（= ガードを何も検証しないままテストが通る）。dirname だけを通し、yq は絶対パスで
# 渡し、シェル自身も絶対パスで起動する
mkdir -p "$TMP/stubbin"
ln -sf "$(command -v dirname)" "$TMP/stubbin/dirname"
PATH="$TMP/stubbin" YQ_BIN="$YQ_PATH" /bin/bash "$SCRIPT" >/dev/null 2>"$TMP/err-jq.txt"
assert_exit 'jq missing: non-zero exit' $? 1
assert_stderr 'jq missing: stderr says so' "$TMP/err-jq.txt" 'jq is required'

# --- ケース10: 実リポジトリの skills/ は規約違反ゼロ（repo lint を兼ねる） ---
out=$(bash "$SCRIPT" "$REAL_SKILLS_DIR")
assert_exit 'real skills dir: exit 0' $? 0
assert_json 'real skills dir: no violations' "$out" '.violations == []'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
