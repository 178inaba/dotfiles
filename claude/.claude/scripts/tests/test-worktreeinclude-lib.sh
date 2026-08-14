#!/bin/bash

# worktreeinclude-lib.sh のリグレッションテスト
#
# 実行: bash claude/.claude/scripts/tests/test-worktreeinclude-lib.sh
# 使い捨ての bare リポジトリを origin 代わりに使う。実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。
#
# lib は source して使う関数のため、呼び出し元が担う add_warning / fatal をここで定義して
# ドライブする（呼び出し元スクリプト経由ではなく lib 単体の挙動を見る）。実体は
# warnings-lib.sh だが、それを source すると fatal が本当に exit して以降のケースが走らない
# ため、ここでは記録に留めるスタブを置く。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
LIB="$SCRIPT_DIR/../worktreeinclude-lib.sh"

if [ ! -f "$LIB" ]; then
  printf 'ERROR: lib not found: %s\n' "$LIB" >&2
  exit 1
fi

# --- 呼び出し元が定義する前提の関数（実物は warnings-lib.sh 由来） ---
warnings=""
add_warning() {
  warnings="${warnings}${1}
"
}

# 実物は exit するが、テストでは以降のケースを続けたいので記録に留める
fatal_msg=""
fatal() {
  fatal_msg=$1
}

. "$LIB"

# macOS の /var → /private/var symlink で git の返す物理パスと食い違わないよう物理化する
TMP=$(cd "$(mktemp -d)" && pwd -P)
trap 'rm -rf "$TMP"' EXIT

# --- origin として使う bare リポジトリを組む ---
git init -q --bare -b main "$TMP/origin.git"
git clone -q "$TMP/origin.git" "$TMP/seed" 2>/dev/null
(
  cd "$TMP/seed"
  git config user.email test@example.com
  git config user.name test
  git commit -q --allow-empty -m "initial"
  # .gitignore はコピー元ルートの ignore 判定に効くため main に置く（現実の構成と同じ）。
  # .worktreeinclude は「worktree の checkout 内容を見る」ことの検証のため別 branch に置く
  printf '.env\nconfig/secrets.json\n' > .gitignore
  git add .gitignore
  git commit -q -m "add gitignore"
  git push -q origin main

  git switch -qc include
  printf '.env\nconfig/secrets.json\nnot-ignored.txt\n' > .worktreeinclude
  git add .worktreeinclude
  git commit -q -m "add worktreeinclude"
  git push -q origin include

  # escape ガード検証用: config が worktree 外を指す committed symlink の branch
  mkdir -p "$TMP/outside"
  git switch -qc escape main
  printf 'config/secrets.json\n' > .worktreeinclude
  ln -s "$TMP/outside" config
  git add .worktreeinclude config
  git commit -q -m "escape fixture"
  git push -q origin escape

  # leaf escape 検証用: コピー先そのものが worktree 外を指す committed symlink の branch。
  # gitignored なパスを追跡させるため add -f を使う（攻撃者が仕込める形の再現）
  git switch -qc leaf-escape main
  printf '.env\n' > .worktreeinclude
  ln -s "$TMP/outside/stolen" .env
  git add .worktreeinclude
  git add -f .env
  git commit -q -m "leaf escape fixture"
  git push -q origin leaf-escape

  # .worktreeinclude 自体が committed symlink の branch
  git switch -qc wtinc-symlink main
  printf '.env\n' > real-include
  ln -s real-include .worktreeinclude
  git add real-include .worktreeinclude
  git commit -q -m "symlinked worktreeinclude fixture"
  git push -q origin wtinc-symlink

  git switch -q main
)

# コピー元ルート（clone）と、<start-ref> を checkout した worktree を用意する。
# stdout に "<src_root>\t<worktree_path>" を返す
setup_case() {
  local name=$1 start_ref=$2
  local src="$TMP/$name"
  git clone -q "$TMP/origin.git" "$src" 2>/dev/null
  git -C "$src" config user.email test@example.com
  git -C "$src" config user.name test
  local wt="$src/.claude/worktrees/wt"
  git -C "$src" worktree add -q --detach "$wt" "origin/$start_ref"
  printf '%s\t%s' "$src" "$wt"
}

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

# 各ケースの前に呼び出し元側の状態をリセットする
reset_caller_state() {
  warnings=""
  fatal_msg=""
  WORKTREEINCLUDE_COPIED=-1   # lib が無条件に初期化することの検証用に番兵を置く
}

# --- ケース1: gitignored のみコピー（ネストしたパスも、パターン一致でも非 ignore は対象外） ---
IFS=$'\t' read -r src wt < <(setup_case case1 include)
printf 'SECRET=1\n' > "$src/.env"
mkdir -p "$src/config"
printf '{}\n' > "$src/config/secrets.json"
printf 'plain\n' > "$src/not-ignored.txt"   # パターン一致するが gitignored でない
reset_caller_state
copy_worktreeinclude "$src" "$wt"

assert "case1: copied 2 files" "[ '$WORKTREEINCLUDE_COPIED' -eq 2 ]"
assert "case1: .env copied" "[ -f '$wt/.env' ]"
assert "case1: nested file copied" "[ -f '$wt/config/secrets.json' ]"
assert "case1: non-ignored file not copied" "[ ! -e '$wt/not-ignored.txt' ]"
assert "case1: content preserved" "grep -q 'SECRET=1' '$wt/.env'"
assert "case1: no warnings" "[ -z '$warnings' ]" "$warnings"
assert "case1: no fatal" "[ -z '$fatal_msg' ]" "$fatal_msg"

# --- ケース2: コピー元が symlink ならスキップして warning ---
IFS=$'\t' read -r src wt < <(setup_case case2 include)
printf 'real\n' > "$src/real-file"
ln -s real-file "$src/.env"
reset_caller_state
copy_worktreeinclude "$src" "$wt"

assert "case2: symlink not copied" "[ ! -e '$wt/.env' ]"
assert "case2: copied 0 files" "[ '$WORKTREEINCLUDE_COPIED' -eq 0 ]"
assert "case2: warning mentions symlink" \
  "printf '%s' \"\$warnings\" | grep -q 'skipped symlink'" "$warnings"

# --- ケース3: .claude/worktrees/ 配下はコピー元にしない ---
IFS=$'\t' read -r src wt < <(setup_case case3 include)
printf 'SECRET=1\n' > "$src/.env"
mkdir -p "$src/.claude/worktrees/other"
printf 'LEAK=1\n' > "$src/.claude/worktrees/other/.env"
reset_caller_state
copy_worktreeinclude "$src" "$wt"

assert "case3: copied 1 file" "[ '$WORKTREEINCLUDE_COPIED' -eq 1 ]"
assert "case3: other worktree env not nested-copied" \
  "[ ! -e '$wt/.claude/worktrees/other/.env' ]"

# --- ケース4: コピー先が committed symlink 経由で worktree 外へ出る場合はスキップ ---
IFS=$'\t' read -r src wt < <(setup_case case4 escape)
mkdir -p "$src/config"
printf 'SECRET=1\n' > "$src/config/secrets.json"
reset_caller_state
copy_worktreeinclude "$src" "$wt"

assert "case4: copied 0 files" "[ '$WORKTREEINCLUDE_COPIED' -eq 0 ]"
assert "case4: warning mentions escape" \
  "printf '%s' \"\$warnings\" | grep -q 'escapes worktree'" "$warnings"
assert "case4: no write outside worktree" "[ ! -e '$TMP/outside/secrets.json' ]"

# --- ケース4b: コピー先そのものが worktree 外を指す committed symlink ならスキップ ---
# ケース4 は途中のディレクトリの symlink。末端が素通りすると cp が symlink を辿り、
# 他人の PR branch を checkout する create-fallback 経路で secret が worktree 外へ流出する
IFS=$'\t' read -r src wt < <(setup_case case4b leaf-escape)
printf 'SECRET=1\n' > "$src/.env"
reset_caller_state
copy_worktreeinclude "$src" "$wt"

assert "case4b: copied 0 files" "[ '$WORKTREEINCLUDE_COPIED' -eq 0 ]"
assert "case4b: warning mentions committed symlink destination" \
  "printf '%s' \"\$warnings\" | grep -q 'destination is a committed symlink'" "$warnings"
assert "case4b: secret did not leak outside worktree" "[ ! -e '$TMP/outside/stolen' ]"

# --- ケース5: .worktreeinclude が checkout に無ければ何もしない ---
IFS=$'\t' read -r src wt < <(setup_case case5 main)   # main には .worktreeinclude なし
printf 'SECRET=1\n' > "$src/.env"
reset_caller_state
copy_worktreeinclude "$src" "$wt"

assert "case5: copied 0 files" "[ '$WORKTREEINCLUDE_COPIED' -eq 0 ]"
assert "case5: .env not copied" "[ ! -e '$wt/.env' ]"
assert "case5: no warnings" "[ -z '$warnings' ]" "$warnings"

# --- ケース6: .worktreeinclude 自体が symlink なら何もしない ---
IFS=$'\t' read -r src wt < <(setup_case case6 wtinc-symlink)
printf 'SECRET=1\n' > "$src/.env"
reset_caller_state
copy_worktreeinclude "$src" "$wt"

assert "case6: copied 0 files" "[ '$WORKTREEINCLUDE_COPIED' -eq 0 ]"
assert "case6: .env not copied" "[ ! -e '$wt/.env' ]"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
