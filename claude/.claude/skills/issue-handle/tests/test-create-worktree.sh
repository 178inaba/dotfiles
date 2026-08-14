#!/bin/bash

# create-worktree.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/issue-handle/tests/test-create-worktree.sh
# 使い捨ての bare リポジトリを origin 代わりに使う。実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/create-worktree.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

# macOS の /var → /private/var symlink で git の返す物理パスと食い違わないよう物理化する
TMP=$(cd "$(mktemp -d)" && pwd -P)
trap 'rm -rf "$TMP"' EXIT

BRANCH="feature/42-add-widget"
WT_NAME="feature-42-add-widget"
BASE="develop"

# --- origin として使う bare リポジトリを組む ---
git init -q --bare -b main "$TMP/origin.git"
git clone -q "$TMP/origin.git" "$TMP/seed" 2>/dev/null
(
  cd "$TMP/seed"
  git config user.email test@example.com
  git config user.name test
  git commit -q --allow-empty -m "initial"
  # .gitignore はメインツリーの ignore 判定にも効くため main に置く（現実の構成と同じ）。
  # .worktreeinclude は「起点 commit に含まれる場合のみ」の検証のため develop のみに置く
  printf '.env\nconfig/secrets.json\n' > .gitignore
  git add .gitignore
  git commit -q -m "add gitignore"
  git push -q origin main
  git switch -qc "$BASE"
  printf '.env\nconfig/secrets.json\nnot-ignored.txt\n' > .worktreeinclude
  printf 'tracked\n' > tracked.txt
  git add .worktreeinclude tracked.txt
  git commit -q -m "add worktreeinclude"
  git commit -q --allow-empty -m "base tip"
  git push -q origin "$BASE"
  # escape ガード検証用: config が worktree 外を指す committed symlink の branch
  mkdir -p "$TMP/outside"
  git switch -qc escape main
  printf 'config/secrets.json\n' > .worktreeinclude
  ln -s "$TMP/outside" config
  git add .worktreeinclude config
  git commit -q -m "escape fixture"
  git push -q origin escape
  # .worktreeinclude 自体が committed symlink の branch
  git switch -qc wtinc-symlink main
  printf '.env\n' > real-include
  ln -s real-include .worktreeinclude
  git add real-include .worktreeinclude
  git commit -q -m "symlinked worktreeinclude fixture"
  git push -q origin wtinc-symlink
  git switch -q main
)

# 使い捨ての対象リポジトリ（clone。remote-tracking ref は clone が作る）を作る。stdout にパスを返す
setup_repo() {
  local name=$1
  git clone -q "$TMP/origin.git" "$TMP/$name" 2>/dev/null
  git -C "$TMP/$name" config user.email test@example.com
  git -C "$TMP/$name" config user.name test
  printf '%s' "$TMP/$name"
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

assert_json() {
  local name=$1 json=$2 expr=$3
  if printf '%s' "$json" | jq -e "$expr" >/dev/null 2>&1; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (json: %s)\n' "$name" "$json"
  fi
}

# --- ケース1: origin/<base> を起点に worktree を作成、メインツリーは無傷 ---
repo=$(setup_repo case1)
(cd "$repo" && printf 'SECRET=1\n' > .env)
before_head=$(git -C "$repo" rev-parse HEAD)
before_branch=$(git -C "$repo" branch --show-current)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" "$BASE")
rc=$?
wt="$repo/.claude/worktrees/$WT_NAME"

assert "case1: exit 0" "[ $rc -eq 0 ]" "$out"
assert_json "case1: status ok" "$out" '.status == "ok"'
assert_json "case1: start_ref is origin/base" "$out" ".start_ref == \"origin/$BASE\""
assert "case1: worktree created" "[ -d '$wt' ]"
assert "case1: worktree on branch" \
  "[ \"\$(git -C '$wt' branch --show-current)\" = '$BRANCH' ]"
assert "case1: worktree at origin/$BASE tip" \
  "[ \"\$(git -C '$wt' rev-parse HEAD)\" = \"\$(git -C '$repo' rev-parse origin/$BASE)\" ]"
assert "case1: main HEAD untouched" \
  "[ \"\$(git -C '$repo' rev-parse HEAD)\" = '$before_head' ]"
assert "case1: main branch untouched" \
  "[ \"\$(git -C '$repo' branch --show-current)\" = '$before_branch' ]"

# --- ケース1b: detect が作成済み worktree を現行命名で見つける ---
out=$(cd "$repo" && bash "$SCRIPT" detect 42)
assert_json "case1b: detect found" "$out" '.found == true'
assert_json "case1b: detect branch" "$out" ".branch == \"$BRANCH\""
assert_json "case1b: detect path" "$out" ".worktree_path == \"$wt\""

# --- ケース1c: detect は Issue 番号を完全一致させる（42 が 142 に一致しない） ---
out=$(cd "$repo" && bash "$SCRIPT" detect 4)
assert_json "case1c: detect 4 not found" "$out" '.found == false'
out=$(cd "$repo" && bash "$SCRIPT" detect 142)
assert_json "case1c: detect 142 not found" "$out" '.found == false'

# --- ケース2: .worktreeinclude コピー（gitignored のみ、パターン一致でも非 ignore は対象外） ---
repo=$(setup_repo case2)
(
  cd "$repo"
  printf 'SECRET=1\n' > .env
  mkdir -p config
  printf '{}\n' > config/secrets.json
  printf 'plain\n' > not-ignored.txt   # パターン一致するが gitignored でない
)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" "$BASE")
wt="$repo/.claude/worktrees/$WT_NAME"

assert_json "case2: copied_files is 2" "$out" '.copied_files == 2'
assert "case2: .env copied" "[ -f '$wt/.env' ]"
assert "case2: nested file copied" "[ -f '$wt/config/secrets.json' ]"
assert "case2: non-ignored file not copied" "[ ! -f '$wt/not-ignored.txt' ]"

# --- ケース3: symlink はスキップして warning ---
repo=$(setup_repo case3)
(
  cd "$repo"
  printf 'real\n' > real-file
  ln -s real-file .env
)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" "$BASE")
wt="$repo/.claude/worktrees/$WT_NAME"

assert "case3: symlink not copied" "[ ! -e '$wt/.env' ]"
assert_json "case3: warning emitted" "$out" '.warnings | length > 0'

# --- ケース4: 他 worktree 配下のファイルはコピー元にしない ---
repo=$(setup_repo case4)
(
  cd "$repo"
  printf 'SECRET=1\n' > .env
  mkdir -p .claude/worktrees/other
  printf 'LEAK=1\n' > .claude/worktrees/other/.env
)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" "$BASE")
wt="$repo/.claude/worktrees/$WT_NAME"

assert_json "case4: copied_files is 1" "$out" '.copied_files == 1'
assert "case4: other worktree env not nested-copied" \
  "[ ! -e '$wt/.claude/worktrees/other/.env' ]"

# --- ケース5: origin/<base> 不在ならローカル base へフォールバック ---
repo=$(setup_repo case5)
(
  cd "$repo"
  git switch -qc local-only "origin/$BASE"
  git commit -q --allow-empty -m "local base work"
  git switch -q main
)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" local-only)
wt="$repo/.claude/worktrees/$WT_NAME"

assert_json "case5: status ok" "$out" '.status == "ok"'
assert_json "case5: start_ref is local branch" "$out" '.start_ref == "local-only"'
assert_json "case5: fallback warning emitted" "$out" '.warnings | length > 0'
assert "case5: worktree at local base tip" \
  "[ \"\$(git -C '$wt' rev-parse HEAD)\" = \"\$(git -C '$repo' rev-parse local-only)\" ]"

# --- ケース6: base 不在は非ゼロ exit + stderr ---
repo=$(setup_repo case6)
err=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" no-such-base 2>&1 >/dev/null)
rc=$?
assert "case6: non-zero exit" "[ $rc -ne 0 ]"
assert "case6: stderr mentions base" "printf '%s' \"\$err\" | grep -q 'base branch not found'" "$err"

# --- ケース7: branch 既存は status branch_exists で停止（worktree は作らない） ---
repo=$(setup_repo case7)
git -C "$repo" branch "$BRANCH" "origin/$BASE" >/dev/null 2>&1
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" "$BASE")
rc=$?
assert "case7: exit 0" "[ $rc -eq 0 ]"
assert_json "case7: status branch_exists" "$out" '.status == "branch_exists"'
assert "case7: worktree not created" "[ ! -e '$repo/.claude/worktrees/$WT_NAME' ]"

# --- ケース8: worktree パス既存は status path_exists で停止 ---
repo=$(setup_repo case8)
mkdir -p "$repo/.claude/worktrees/$WT_NAME"
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" "$BASE")
assert_json "case8: status path_exists" "$out" '.status == "path_exists"'
assert "case8: branch not created" \
  "! git -C '$repo' show-ref --verify --quiet 'refs/heads/$BRANCH'"

# --- ケース9: .worktreeinclude が起点 commit に無ければコピーは発生しない ---
repo=$(setup_repo case9)
(cd "$repo" && printf 'SECRET=1\n' > .env)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" main)   # main には .worktreeinclude なし
wt="$repo/.claude/worktrees/$WT_NAME"
assert_json "case9: status ok" "$out" '.status == "ok"'
assert_json "case9: copied_files is 0" "$out" '.copied_files == 0'
assert "case9: .env not copied" "[ ! -e '$wt/.env' ]"

# --- ケース10: detect — 旧命名（worktree- prefix）も見つける ---
repo=$(setup_repo case10)
git -C "$repo" worktree add -q "$repo/.claude/worktrees/legacy" \
  -b "worktree-feature-42-old-style" "origin/$BASE"
out=$(cd "$repo" && bash "$SCRIPT" detect 42)
assert_json "case10: legacy naming found" "$out" '.found == true'
assert_json "case10: legacy branch" "$out" '.branch == "worktree-feature-42-old-style"'

# --- ケース11: detect — メインツリーが該当 branch を checkout 中でも再開対象にしない ---
repo=$(setup_repo case11)
git -C "$repo" switch -qc "$BRANCH" "origin/$BASE"
out=$(cd "$repo" && bash "$SCRIPT" detect 42)
assert_json "case11: main worktree excluded" "$out" '.found == false'

# --- ケース12: detect — 該当なしは found: false ---
repo=$(setup_repo case12)
out=$(cd "$repo" && bash "$SCRIPT" detect 42)
assert_json "case12: not found" "$out" '.found == false'
assert_json "case12: path is null" "$out" '.worktree_path == null'

# --- ケース13: ローカル base が origin より ahead なら warning（起点は origin のまま） ---
repo=$(setup_repo case13)
(
  cd "$repo"
  git switch -q "$BASE" >/dev/null 2>&1
  git commit -q --allow-empty -m "unpushed local work"
  git switch -q main
)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" "$BASE")
assert_json "case13: start_ref stays origin" "$out" ".start_ref == \"origin/$BASE\""
assert_json "case13: ahead warning emitted" "$out" \
  '.warnings | any(contains("not on origin"))'

# --- ケース14: コピー先が committed symlink 経由で worktree 外へ出る場合はスキップ + warning ---
repo=$(setup_repo case14)
(
  cd "$repo"
  mkdir -p config
  printf 'SECRET=1\n' > config/secrets.json
)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" escape)
assert_json "case14: escape warning emitted" "$out" \
  '.warnings | any(contains("escapes worktree"))'
assert_json "case14: nothing copied" "$out" '.copied_files == 0'
assert "case14: no write outside worktree" "[ ! -e '$TMP/outside/secrets.json' ]"

# --- ケース15: .worktreeinclude 自体が symlink ならコピーは発生しない ---
repo=$(setup_repo case15)
(cd "$repo" && printf 'SECRET=1\n' > .env)
out=$(cd "$repo" && bash "$SCRIPT" create "$WT_NAME" "$BRANCH" wtinc-symlink)
wt="$repo/.claude/worktrees/$WT_NAME"
assert_json "case15: copied_files is 0" "$out" '.copied_files == 0'
assert "case15: .env not copied" "[ ! -e '$wt/.env' ]"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
