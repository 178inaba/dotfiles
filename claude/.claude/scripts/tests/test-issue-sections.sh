#!/bin/bash

# issue-sections.sh のリグレッションテスト
#
# 実行: bash claude/.claude/scripts/tests/test-issue-sections.sh
# 対象スクリプトはネットワーク・gh に触れない純粋なテキスト処理なので、スタブは要らず
# 使い捨てディレクトリ上のフィクスチャだけで完結する。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../issue-sections.sh"

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
    printf 'FAIL  %s\n' "$name"
    [ -n "$detail" ] && printf '      %s\n' "$detail"
  fi
}

# run <args...> を実行し、グローバルの out / err / status に結果を置く
run() {
  out=$(bash "$SCRIPT" "$@" 2>"$TMP/err")
  status=$?
  err=$(cat "$TMP/err")
}

# --- フィクスチャ ---

cat > "$TMP/ja-sub.md" <<'EOF'
## 背景・目的

親 Issue の第 1 段。

## 依存

なし

## 要件

1. 表を持つ。

## 受け入れ条件

- [ ] テストが通る。

## 影響範囲・関連コード

- `claude/.claude/scripts/issue-sections.sh`

## スコープ外

スキルの配線。
EOF

cat > "$TMP/en-sub.md" <<'EOF'
## Background / Purpose

First stage of the parent issue.

## Depends on

None

## Requirements

1. Own the table.

## Acceptance criteria

- [ ] The tests pass.

## Affected code

- `claude/.claude/scripts/issue-sections.sh`

## Out of scope

Wiring the skills.
EOF

# ja ドラフトに en の canonical 見出しが 1 つ混ざったもの（規則 4 の検出対象）
sed 's/^## 要件$/## Requirements/' "$TMP/ja-sub.md" > "$TMP/ja-mixed.md"

# 必須キー（受け入れ条件）だけが無い ja ドラフト
cat > "$TMP/ja-missing.md" <<'EOF'
## 背景・目的

親 Issue の第 1 段。

## 依存

なし

## 要件

1. 表を持つ。

## 影響範囲・関連コード

- `claude/.claude/scripts/issue-sections.sh`

## スコープ外

スキルの配線。
EOF

# リポジトリの issue テンプレートに沿った en ドラフト（machine-consumed キーだけ canonical）
cat > "$TMP/en-template.md" <<'EOF'
## Background

First stage of the parent issue.

## Depends on

None

## What to build

1. Own the table.

## Done when

- [ ] The tests pass.

## Touched files

- `claude/.claude/scripts/issue-sections.sh`

## Not included

Wiring the skills.
EOF

cat > "$TMP/mapping.txt" <<'EOF'
# key <空白> テンプレートの見出し
background Background
requirements What to build
acceptance Done when
affected_code Touched files
out_of_scope Not included
EOF

# machine-consumed キー（depends_on）まで mapping に載せた版
cp "$TMP/mapping.txt" "$TMP/mapping-bad.txt"
printf 'depends_on Prerequisites\n' >> "$TMP/mapping-bad.txt"
sed 's/^## Depends on$/## Prerequisites/' "$TMP/en-template.md" > "$TMP/en-template-bad.md"

# ja 慣例のリポジトリが英語の issue テンプレートに従った場合。
# mapped 見出しは英語のまま通り、テンプレートに対応の無い必須キー（依存）は ja canonical で追記される
sed 's/^## Depends on$/## 依存/' "$TMP/en-template.md" > "$TMP/ja-with-en-template.md"

cat > "$TMP/parent-ja.md" <<'EOF'
## 背景・目的

リリース単位。

## リリース時の手動作業

なし（全 Sub のマージで完了）

## スコープ外

なし
EOF

cat > "$TMP/parent-en.md" <<'EOF'
## Background / Purpose

The release unit.

## Manual release steps

None (completed by merging all Subs).

## Out of scope

None
EOF

# フェンス内に見出しらしき行を含むドラフト（節境界にも検査対象にもしない）
cat > "$TMP/ja-fenced.md" <<'EOF'
## 背景・目的

テンプレートは次の形。

```markdown
## Requirements

1. ...
```

## 依存

なし

## 要件

1. 表を持つ。

## 受け入れ条件

- [ ] テストが通る。

## 影響範囲・関連コード

- `foo.sh`

## スコープ外

なし
EOF

# 必須キー欠落・未知の見出し・ロケール不一致を同時に含むドラフト
sed 's/^## 要件$/## Requirements/' "$TMP/ja-missing.md" > "$TMP/ja-multi.md"
printf '\n## Bogus Section\n\nなにか\n' >> "$TMP/ja-multi.md"

# --- ケース1: clean な ja ドラフトが通る ---
run check "$TMP/ja-sub.md" --locale ja --kind sub
assert "check: clean ja draft passes" "[ $status -eq 0 ]" "status=$status err=$err"
assert "check: clean ja draft is silent on stderr" "[ -z \"\$err\" ]" "err=$err"

# --- ケース2: clean な en ドラフトが通る ---
run check "$TMP/en-sub.md" --locale en --kind sub
assert "check: clean en draft passes" "[ $status -eq 0 ]" "status=$status err=$err"

# --- ケース3: 見出しの混在が落ち、stderr が当該見出しを名指しする ---
run check "$TMP/ja-mixed.md" --locale ja --kind sub
assert "check: mixed-heading draft fails with the locale-mismatch code" \
  "[ $status -eq 5 ]" "status=$status err=$err"
assert "check: mixed-heading reason names the offending heading" \
  "printf '%s' \"\$err\" | grep -q 'Requirements'" "err=$err"
assert "check: mixed-heading reason is a single line" \
  "[ \"\$(printf '%s\n' \"\$err\" | wc -l | tr -d ' ')\" = 1 ]" "err=$err"

# --- ケース4: 必須キー欠落が落ちる ---
run check "$TMP/ja-missing.md" --locale ja --kind sub
assert "check: missing required key fails with the missing code" \
  "[ $status -eq 2 ]" "status=$status err=$err"
assert "check: missing reason names the key" \
  "printf '%s' \"\$err\" | grep -q 'acceptance'" "err=$err"

# --- ケース5: mapping ありの template 見出しは通る / machine-consumed キーの mapping は落ちる ---
run check "$TMP/en-template.md" --locale en --kind sub --mapping "$TMP/mapping.txt"
assert "check: mapped template headings pass" "[ $status -eq 0 ]" "status=$status err=$err"

run check "$TMP/en-template-bad.md" --locale en --kind sub --mapping "$TMP/mapping-bad.txt"
assert "check: mapping a machine-consumed key fails" \
  "[ $status -eq 4 ]" "status=$status err=$err"
assert "check: machine-consumed reason names the key" \
  "printf '%s' \"\$err\" | grep -q 'depends_on'" "err=$err"

# --- ケース6: ja ドラフトの mapped 見出しが英語でも通る（追記された必須キーは ja canonical） ---
run check "$TMP/ja-with-en-template.md" --locale ja --kind sub --mapping "$TMP/mapping.txt"
assert "check: ja draft following an English template passes" \
  "[ $status -eq 0 ]" "status=$status err=$err"

# 対照: 同じドラフトを mapping 無しで検査すると、テンプレート見出しが表の外なので拒否される
run check "$TMP/ja-with-en-template.md" --locale ja --kind sub
assert "check: the same draft without a mapping is rejected" \
  "[ $status -eq 2 ]" "status=$status err=$err"
assert "check: the rejection names an unmapped template heading" \
  "printf '%s' \"\$err\" | grep -q 'unknown heading: \"What to build\"'" "err=$err"

# --- ケース7: find が ja / en 両方の見出しを解決する ---
run find "$TMP/parent-ja.md" release_manual_steps
assert "find: resolves the ja canonical heading" "[ $status -eq 0 ]" "status=$status err=$err"
assert "find: reports the matched locale (ja)" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .locale)\" = ja ]" "out=$out"
assert "find: body excludes the heading line (ja)" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .body)\" = 'なし（全 Sub のマージで完了）' ]" "out=$out"

run find "$TMP/parent-en.md" release_manual_steps
assert "find: resolves the en canonical heading" "[ $status -eq 0 ]" "status=$status err=$err"
assert "find: reports the matched locale (en)" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .locale)\" = en ]" "out=$out"
assert "find: body excludes the heading line (en)" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .body)\" = 'None (completed by merging all Subs).' ]" "out=$out"

# 節本文が複数行のときも次の見出しの手前までを返す
run find "$TMP/ja-sub.md" affected_code
assert "find: body stops before the next heading" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .body)\" = '- \`claude/.claude/scripts/issue-sections.sh\`' ]" "out=$out"

# 節見出しの直後に次の見出しが続く（空行なし）本文。body は空になる
printf '## 依存\n## 要件\n\n1. foo\n' > "$TMP/adjacent.md"
run find "$TMP/adjacent.md" depends_on
assert "find: an empty section does not leak the next heading into the body" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .body)\" = '' ]" "out=$out"

# GitHub の Web UI で書かれた本文は API 経由で CRLF になる。消費側（#88）が生の文字列と
# 突き合わせられるよう、body に \r を残さない
printf '## 依存\r\n\r\nなし\r\n\r\n## 要件\r\n\r\n1. foo\r\n' > "$TMP/crlf.md"
run find "$TMP/crlf.md" depends_on
assert "find: body of a CRLF draft carries no carriage return" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .body)\" = 'なし' ]" "out=$(printf '%s' "$out" | jq -c .)"

# --- ケース8: 未対応 locale が前提不成立で落ちる ---
run check "$TMP/ja-sub.md" --locale fr --kind sub
assert "locale: unsupported locale fails as a precondition" \
  "[ $status -eq 1 ]" "status=$status err=$err"
assert "locale: error names the supported set" \
  "printf '%s' \"\$err\" | grep -q 'ja' && printf '%s' \"\$err\" | grep -q 'en'" "err=$err"
assert "locale: error points at the table in this script" \
  "printf '%s' \"\$err\" | grep -q 'table'" "err=$err"

run list --locale fr --kind sub
assert "locale: list rejects an unsupported locale too" "[ $status -eq 1 ]" "status=$status err=$err"

# --- ケース9: list が表の内容を露出する ---
run list --locale ja --kind sub
assert "list: succeeds" "[ $status -eq 0 ]" "status=$status err=$err"
assert "list: emits all 11 keys in table order" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections | length')\" = 11 ]" "out=$out"
assert "list: first key is background" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[0].key')\" = background ]" "out=$out"
assert "list: renders the ja heading" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[0].heading')\" = '背景・目的' ]" "out=$out"
assert "list: required is computed for the kind" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[] | select(.key==\"depends_on\") | .required')\" = true ]" "out=$out"
assert "list: a parent-only key is not required on sub" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[] | select(.key==\"cross_cutting\") | .required')\" = false ]" "out=$out"
assert "list: required_on carries the raw table column" \
  "[ \"\$(printf '%s' \"\$out\" | jq -c '.sections[] | select(.key==\"cross_cutting\") | .required_on')\" = '[\"parent\"]' ]" "out=$out"
assert "list: an optional key has an empty required_on" \
  "[ \"\$(printf '%s' \"\$out\" | jq -c '.sections[] | select(.key==\"impl_notes\") | .required_on')\" = '[]' ]" "out=$out"
assert "list: template_mappable is false for machine-consumed keys" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[] | select(.key==\"release_manual_steps\") | .template_mappable')\" = false ]" "out=$out"
assert "list: none_markers exposes both locales" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[] | select(.key==\"release_manual_steps\") | .none_markers.en')\" = 'None (completed by merging all Subs)' ]" "out=$out"
assert "list: none_markers of depends_on exposes the ja marker" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[] | select(.key==\"depends_on\") | .none_markers.ja')\" = 'なし' ]" "out=$out"
assert "list: keys without a none marker carry null" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[] | select(.key==\"requirements\") | .none_markers')\" = null ]" "out=$out"

run list --locale en --kind parent
assert "list: required follows the requested kind" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[] | select(.key==\"cross_cutting\") | .required')\" = true ]" "out=$out"
assert "list: renders the en heading" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r '.sections[0].heading')\" = 'Background / Purpose' ]" "out=$out"

run list --locale ja --kind bogus
assert "list: unsupported kind fails as a precondition" "[ $status -eq 1 ]" "status=$status err=$err"

# --- ケース10: find の not-found ---
run find "$TMP/ja-sub.md" release_manual_steps
assert "find: missing section uses its own exit code" "[ $status -eq 6 ]" "status=$status err=$err"
assert "find: missing section writes nothing to stdout" "[ -z \"\$out\" ]" "out=$out"

# 空の入力は「節が無い」ではなく前提不成立。取得に失敗した本文（`gh ... > file` は gh が
# 落ちても空ファイルを先に作る）が not-found に化けると、消費側は「節の無い Issue」として
# 分岐してしまう
: > "$TMP/empty.md"
run find "$TMP/empty.md" release_manual_steps
assert "find: empty input fails as a precondition" "[ $status -eq 1 ]" "status=$status err=$err"
assert "find: the empty-input reason names the file" \
  "printf '%s' \"\$err\" | grep -qF \"\$TMP/empty.md\"" "err=$err"
printf '   \n\n' > "$TMP/blank.md"
run find "$TMP/blank.md" release_manual_steps
assert "find: whitespace-only input fails the same way" "[ $status -eq 1 ]" "status=$status err=$err"

# --- ケース11: schema の出力と未知キー ---
run schema release_manual_steps
assert "schema: emits the ja canonical heading" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .headings.ja)\" = 'リリース時の手動作業' ]" "out=$out"
assert "schema: emits the en canonical heading" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .headings.en)\" = 'Manual release steps' ]" "out=$out"
# 消費側はロケールを知らないまま両方のマーカーを受け取る（#85 注記 4）
assert "schema: emits the ja none marker" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .none_markers.ja)\" = 'なし（全 Sub のマージで完了）' ]" "out=$out"
assert "schema: emits the en none marker" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .none_markers.en)\" = 'None (completed by merging all Subs)' ]" "out=$out"
assert "schema: carries the row's required_on and template_mappable" \
  "[ \"\$(printf '%s' \"\$out\" | jq -c '[.required_on, .template_mappable]')\" = '[[\"parent\"],false]' ]" "out=$out"

run schema background
assert "schema: a key without markers reports null" \
  "[ \"\$(printf '%s' \"\$out\" | jq -r .none_markers)\" = 'null' ]" "out=$out"

run schema bogus_key
assert "schema: unknown key fails as a precondition" "[ $status -eq 1 ]" "status=$status err=$err"
run find "$TMP/ja-sub.md" bogus_key
assert "find: unknown key fails as a precondition" "[ $status -eq 1 ]" "status=$status err=$err"

# 未知のキーを含む mapping も前提不成立（起案モデルの書き間違いを黙って通さない）
printf 'bogus_key Whatever\n' > "$TMP/mapping-unknown.txt"
run check "$TMP/en-sub.md" --locale en --kind sub --mapping "$TMP/mapping-unknown.txt"
assert "check: unknown key in the mapping fails as a precondition" \
  "[ $status -eq 1 ]" "status=$status err=$err"

# mapping の重複は起案モデルの書き間違いなので、黙って片方を採用せず前提不成立にする
printf 'requirements Stuff\nrequirements Other\n' > "$TMP/mapping-dup-key.txt"
run check "$TMP/en-sub.md" --locale en --kind sub --mapping "$TMP/mapping-dup-key.txt"
assert "check: a duplicated key in the mapping fails as a precondition" \
  "[ $status -eq 1 ]" "status=$status err=$err"

printf 'requirements Stuff\nacceptance Stuff\n' > "$TMP/mapping-dup-heading.txt"
run check "$TMP/en-sub.md" --locale en --kind sub --mapping "$TMP/mapping-dup-heading.txt"
assert "check: two keys mapped to one heading fail as a precondition" \
  "[ $status -eq 1 ]" "status=$status err=$err"

run check "$TMP/en-sub.md" --locale en --kind sub --mapping "$TMP/nope.txt"
assert "check: missing mapping file fails as a precondition" "[ $status -eq 1 ]" "status=$status err=$err"
run check "$TMP/nope.md" --locale en --kind sub
assert "check: missing draft file fails as a precondition" "[ $status -eq 1 ]" "status=$status err=$err"

# --- ケース12: フェンス内の見出しは検査にも節境界にも使わない ---
run check "$TMP/ja-fenced.md" --locale ja --kind sub
assert "fence: headings inside a code fence are ignored by check" \
  "[ $status -eq 0 ]" "status=$status err=$err"

run find "$TMP/ja-fenced.md" background
assert "fence: find does not stop at a heading inside a code fence" \
  "printf '%s' \"\$out\" | jq -r .body | grep -q '^## Requirements$'" "out=$out"

# --- ケース13: 複数クラスの違反は全理由が出て、exit は評価順の先頭 ---
run check "$TMP/ja-multi.md" --locale ja --kind sub
assert "multi: exit code is the first failing class in evaluation order" \
  "[ $status -eq 2 ]" "status=$status err=$err"
assert "multi: every reason is reported" \
  "[ \"\$(printf '%s\n' \"\$err\" | wc -l | tr -d ' ')\" = 3 ]" "err=$err"
assert "multi: reasons cover the missing key" \
  "printf '%s' \"\$err\" | grep -q 'acceptance'" "err=$err"
assert "multi: reasons cover the unknown heading" \
  "printf '%s' \"\$err\" | grep -q 'Bogus Section'" "err=$err"
assert "multi: reasons cover the locale mismatch" \
  "printf '%s' \"\$err\" | grep -q 'Requirements'" "err=$err"

# --- 使い方の誤り ---
run
assert "usage: no arguments fails" "[ $status -eq 1 ]" "status=$status err=$err"
run bogus-subcommand
assert "usage: unknown subcommand fails" "[ $status -eq 1 ]" "status=$status err=$err"
run check "$TMP/ja-sub.md" --locale ja
assert "usage: check without --kind fails" "[ $status -eq 1 ]" "status=$status err=$err"
run schema background --locale ja
assert "usage: a flag the subcommand does not take fails" "[ $status -eq 1 ]" "status=$status err=$err"
run find "$TMP/ja-sub.md" background --locale ja
assert "usage: find takes no flags" "[ $status -eq 1 ]" "status=$status err=$err"
run list --locale ja --kind sub --bogus x
assert "usage: an undefined flag fails" "[ $status -eq 1 ]" "status=$status err=$err"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
