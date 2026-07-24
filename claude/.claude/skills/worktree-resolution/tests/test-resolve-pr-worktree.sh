#!/bin/bash

# resolve-pr-worktree.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/worktree-resolution/tests/test-resolve-pr-worktree.sh
# 使い捨ての bare リポジトリを origin 代わりに使い、gh は GH_BIN スタブで差し替える。
# 実 gh・実 GitHub・実リポジトリには触れない。失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/resolve-pr-worktree.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

# macOS の /var → /private/var symlink で git の返す物理パスと食い違わないよう物理化する
TMP=$(cd "$(mktemp -d)" && pwd -P)
trap 'rm -rf "$TMP"' EXIT

HEAD_REF="feature/42-topic"
WT_NAME="feature-42-topic"

# --- origin として使う bare リポジトリを組む（main + PR head branch） ---
git init -q --bare -b main "$TMP/origin.git"
git clone -q "$TMP/origin.git" "$TMP/seed" 2>/dev/null
(
  cd "$TMP/seed"
  git config user.email test@example.com
  git config user.name test
  git commit -q --allow-empty -m "initial"
  git push -q origin main
  git switch -qc "$HEAD_REF"
  git commit -q --allow-empty -m "pr work"
  git push -q origin "$HEAD_REF"
  git switch -q main
)

# origin の head branch に commit を1つ足す（fetch・同期の検証に使う）。
# 他の経路（worktree からの push 等）で origin が進んでいても壊れないよう origin に揃えてから積む
push_origin_commit() {
  local msg=$1
  (
    cd "$TMP/seed"
    git fetch -q origin "$HEAD_REF"
    git switch -q "$HEAD_REF"
    git reset -q --hard "origin/$HEAD_REF"
    git commit -q --allow-empty -m "$msg"
    git push -q origin "$HEAD_REF"
    git switch -q main
  )
}

# 使い捨ての対象リポジトリ（clone）を作る。stdout にパスを返す
setup_repo() {
  local name=$1
  git clone -q "$TMP/origin.git" "$TMP/$name" 2>/dev/null
  git -C "$TMP/$name" config user.email test@example.com
  git -C "$TMP/$name" config user.name test
  printf '%s' "$TMP/$name"
}

# --- gh スタブ ---
mkdir -p "$TMP/stub"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
printf '%s\n' "$*" >> "$GH_STUB_LOG"
case "$*" in
  *headRefName*)
    [ -n "${GH_STUB_HEAD_REF:-}" ] || exit 1
    printf '%s\n' "$GH_STUB_HEAD_REF"
    ;;
  *"--json number"*)
    [ -n "${GH_STUB_PR_NUMBER:-}" ] || exit 1
    printf '%s\n' "$GH_STUB_PR_NUMBER"
    ;;
  *defaultBranchRef*)
    printf 'main\n'
    ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_LOG="$TMP/gh-log"
export GH_STUB_HEAD_REF="$HEAD_REF"
export GH_STUB_PR_NUMBER=42

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
    printf 'FAIL  %s (json: %s)\n' "$name" "$json"
  fi
}

assert_exit() {
  local name=$1 got=$2 want=$3
  if [ "$got" -eq "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got exit %d, want %d)\n' "$name" "$got" "$want"
  fi
}

# --- ケース1: resolve — 既存 worktree なし・カレント branch は main → action: create ---
repo=$(setup_repo case1)
push_origin_commit "case1 remote advance"
: > "$GH_STUB_LOG"
out=$(cd "$repo" && bash "$SCRIPT" resolve 42)
assert_exit 'resolve create: exit 0' $? 0
assert_json 'resolve create: status ok' "$out" '.status == "ok"'
assert_json 'resolve create: action create' "$out" '.action == "create"'
assert_json 'resolve create: worktree_name sanitized' "$out" ".worktree_name == \"$WT_NAME\""
assert_json 'resolve create: head_ref' "$out" ".head_ref == \"$HEAD_REF\""
assert_json 'resolve create: pr_number 42' "$out" '.pr_number == 42'
assert_json 'resolve create: worktree_path null' "$out" '.worktree_path == null'
assert_json 'resolve create: not evacuated' "$out" '.evacuated == false'
# fetch が実施済みであること（remote tracking ref が origin の最新に到達している）
origin_head=$(git -C "$TMP/seed" rev-parse "$HEAD_REF")
local_tracking=$(git -C "$repo" rev-parse "origin/$HEAD_REF")
assert 'resolve create: fetch updated remote tracking ref' "[ '$local_tracking' = '$origin_head' ]"

# --- ケース2: resolve — PR 番号省略はカレント branch から推論 ---
out=$(cd "$repo" && bash "$SCRIPT" resolve)
assert_exit 'resolve inferred: exit 0' $? 0
assert_json 'resolve inferred: pr_number from gh' "$out" '.pr_number == 42'

# --- ケース3: resolve — 推論失敗（PR なし）→ 非ゼロ exit + stderr ---
out=$(cd "$repo" && GH_STUB_PR_NUMBER= bash "$SCRIPT" resolve 2>"$TMP/err3.txt")
assert_exit 'resolve infer failure: non-zero exit' $? 1
assert 'resolve infer failure: stderr present' "[ -s '$TMP/err3.txt' ]"

# --- ケース4: resolve — 既存 worktree あり・同期済み → enter_existing ---
repo=$(setup_repo case4)
wt="$repo/.claude/worktrees/$WT_NAME"
git -C "$repo" worktree add -q "$wt" "$HEAD_REF" 2>/dev/null
out=$(cd "$repo" && bash "$SCRIPT" resolve 42)
assert_exit 'resolve existing in-sync: exit 0' $? 0
assert_json 'resolve existing in-sync: status ok' "$out" '.status == "ok"'
assert_json 'resolve existing in-sync: action enter_existing' "$out" '.action == "enter_existing"'
assert_json 'resolve existing in-sync: worktree_path' "$out" ".worktree_path == \"$wt\""
assert_json 'resolve existing in-sync: not synced' "$out" '.synced == false'

# --- ケース5: resolve — 既存 worktree が behind・clean → ff 同期して ok ---
push_origin_commit "case5 remote advance"
out=$(cd "$repo" && bash "$SCRIPT" resolve 42)
assert_exit 'resolve existing behind-clean: exit 0' $? 0
assert_json 'resolve existing behind-clean: status ok' "$out" '.status == "ok"'
assert_json 'resolve existing behind-clean: synced true' "$out" '.synced == true'
origin_head=$(git -C "$TMP/seed" rev-parse "$HEAD_REF")
wt_head=$(git -C "$wt" rev-parse HEAD)
assert 'resolve existing behind-clean: worktree fast-forwarded' "[ '$wt_head' = '$origin_head' ]"

# --- ケース6: resolve — 既存 worktree が behind・dirty → behind_dirty で停止 ---
# ケース5で worktree は origin と同期済み。tracked ファイルを積んで push → origin をさらに
# 進める → worktree を dirty にする、の順で「behind かつ dirty」を作る
echo tracked > "$wt/tracked.txt"
git -C "$wt" add tracked.txt
git -C "$wt" commit -qm "add tracked file"
git -C "$wt" push -q origin "$HEAD_REF"
push_origin_commit "case6 remote advance"
echo dirty >> "$wt/tracked.txt"
before=$(git -C "$wt" rev-parse HEAD)
out=$(cd "$repo" && bash "$SCRIPT" resolve 42)
assert_exit 'resolve existing behind-dirty: exit 0' $? 0
assert_json 'resolve existing behind-dirty: status behind_dirty' "$out" '.status == "behind_dirty"'
after=$(git -C "$wt" rev-parse HEAD)
assert 'resolve existing behind-dirty: HEAD untouched' "[ '$before' = '$after' ]"
assert 'resolve existing behind-dirty: dirty file intact' "grep -q dirty '$wt/tracked.txt'"
git -C "$wt" checkout -q -- tracked.txt

# --- ケース7: resolve — 既存 worktree にローカル独自 commit → diverged で停止 ---
git -C "$wt" merge -q --ff-only "origin/$HEAD_REF"
git -C "$wt" commit -q --allow-empty -m "local only work"
before=$(git -C "$wt" rev-parse HEAD)
out=$(cd "$repo" && bash "$SCRIPT" resolve 42)
assert_exit 'resolve existing diverged: exit 0' $? 0
assert_json 'resolve existing diverged: status diverged' "$out" '.status == "diverged"'
after=$(git -C "$wt" rev-parse HEAD)
assert 'resolve existing diverged: HEAD untouched' "[ '$before' = '$after' ]"

# --- ケース8: resolve — メインリポジトリが head branch を checkout 中・clean → 退避 ---
repo=$(setup_repo case8)
git -C "$repo" switch -q "$HEAD_REF"
out=$(cd "$repo" && bash "$SCRIPT" resolve 42)
assert_exit 'resolve evacuation: exit 0' $? 0
assert_json 'resolve evacuation: status ok' "$out" '.status == "ok"'
assert_json 'resolve evacuation: action create' "$out" '.action == "create"'
assert_json 'resolve evacuation: evacuated true' "$out" '.evacuated == true'
main_branch=$(git -C "$repo" rev-parse --abbrev-ref HEAD)
assert 'resolve evacuation: main repo switched to default branch' "[ '$main_branch' = 'main' ]"

# --- ケース9: resolve — メインリポジトリが head branch を checkout 中・dirty → evacuation_dirty ---
repo=$(setup_repo case9)
git -C "$repo" switch -q "$HEAD_REF"
echo dirty > "$repo/tracked.txt"
git -C "$repo" add tracked.txt
out=$(cd "$repo" && bash "$SCRIPT" resolve 42)
assert_exit 'resolve evacuation-dirty: exit 0' $? 0
assert_json 'resolve evacuation-dirty: status evacuation_dirty' "$out" '.status == "evacuation_dirty"'
cur_branch=$(git -C "$repo" rev-parse --abbrev-ref HEAD)
assert 'resolve evacuation-dirty: branch unchanged' "[ '$cur_branch' = '$HEAD_REF' ]"

# --- ケース10: resolve — worktree 内の cwd からでも動く ---
repo=$(setup_repo case10)
wt="$repo/.claude/worktrees/$WT_NAME"
git -C "$repo" worktree add -q "$wt" "$HEAD_REF" 2>/dev/null
out=$(cd "$wt" && bash "$SCRIPT" resolve 42)
assert_exit 'resolve from inside worktree: exit 0' $? 0
assert_json 'resolve from inside worktree: finds itself' "$out" ".worktree_path == \"$wt\""

# --- ケース11: create-fallback — detached add + switch + 同期 ---
repo=$(setup_repo case11)
out=$(cd "$repo" && bash "$SCRIPT" create-fallback "$WT_NAME" "$HEAD_REF")
assert_exit 'create-fallback: exit 0' $? 0
expected_wt="$repo/.claude/worktrees/$WT_NAME"
assert_json 'create-fallback: worktree_path' "$out" ".worktree_path == \"$expected_wt\""
assert 'create-fallback: worktree exists' "[ -d '$expected_wt' ]"
wt_branch=$(git -C "$expected_wt" rev-parse --abbrev-ref HEAD)
assert 'create-fallback: on head branch' "[ '$wt_branch' = '$HEAD_REF' ]"
origin_head=$(git -C "$TMP/seed" rev-parse "$HEAD_REF")
wt_head=$(git -C "$expected_wt" rev-parse HEAD)
assert 'create-fallback: head matches origin' "[ '$wt_head' = '$origin_head' ]"

# --- ケース12: create-fallback — 古い同名ローカル branch（behind）→ origin に ff 同期 ---
repo=$(setup_repo case12)
git -C "$repo" branch -q "$HEAD_REF" "origin/$HEAD_REF"
push_origin_commit "case12 remote advance"
git -C "$repo" fetch -q origin "$HEAD_REF"
out=$(cd "$repo" && bash "$SCRIPT" create-fallback "$WT_NAME" "$HEAD_REF")
assert_exit 'create-fallback stale-behind: exit 0' $? 0
assert_json 'create-fallback stale-behind: synced true' "$out" '.synced == true'
origin_head=$(git -C "$TMP/seed" rev-parse "$HEAD_REF")
wt_head=$(git -C "$repo/.claude/worktrees/$WT_NAME" rev-parse HEAD)
assert 'create-fallback stale-behind: fast-forwarded to origin' "[ '$wt_head' = '$origin_head' ]"

# --- ケース13: create-fallback — 古い同名ローカル branch（独自 commit あり）→ diverged で停止 ---
repo=$(setup_repo case13)
git -C "$repo" switch -qc "$HEAD_REF" "origin/$HEAD_REF"
git -C "$repo" commit -q --allow-empty -m "stale local only"
git -C "$repo" switch -q main
push_origin_commit "case13 remote advance"
git -C "$repo" fetch -q origin "$HEAD_REF"
out=$(cd "$repo" && bash "$SCRIPT" create-fallback "$WT_NAME" "$HEAD_REF")
assert_exit 'create-fallback stale-diverged: exit 0' $? 0
assert_json 'create-fallback stale-diverged: status diverged' "$out" '.status == "diverged"'

# --- ケース14: finalize — EnterWorktree(name:) 後の switch + temp branch 削除 ---
repo=$(setup_repo case14)
wt="$repo/.claude/worktrees/$WT_NAME"
git -C "$repo" fetch -q origin "$HEAD_REF"
git -C "$repo" worktree add -q -b "worktree-$WT_NAME" "$wt" 2>/dev/null
out=$(cd "$wt" && bash "$SCRIPT" finalize "$WT_NAME" "$HEAD_REF")
assert_exit 'finalize: exit 0' $? 0
assert_json 'finalize: status ok' "$out" '.status == "ok"'
wt_branch=$(git -C "$wt" rev-parse --abbrev-ref HEAD)
assert 'finalize: on head branch' "[ '$wt_branch' = '$HEAD_REF' ]"
assert 'finalize: temp branch deleted' "! git -C '$repo' show-ref -q --verify 'refs/heads/worktree-$WT_NAME'"
origin_head=$(git -C "$TMP/seed" rev-parse "$HEAD_REF")
wt_head=$(git -C "$wt" rev-parse HEAD)
assert 'finalize: head matches origin' "[ '$wt_head' = '$origin_head' ]"

# --- ケース15: finalize — temp branch 不在でも成功（warning 扱い） ---
repo=$(setup_repo case15)
wt="$repo/.claude/worktrees/$WT_NAME"
git -C "$repo" fetch -q origin "$HEAD_REF"
git -C "$repo" worktree add -q --detach "$wt" 2>/dev/null
out=$(cd "$wt" && bash "$SCRIPT" finalize "$WT_NAME" "$HEAD_REF")
assert_exit 'finalize without temp branch: exit 0' $? 0
assert_json 'finalize without temp branch: status ok' "$out" '.status == "ok"'
assert_json 'finalize without temp branch: warning recorded' "$out" '.warnings | length >= 1'

# --- ケース16: 引数不正 → 非ゼロ exit + stderr ---
repo=$(setup_repo case16)
(cd "$repo" && bash "$SCRIPT" 2>"$TMP/err16a.txt")
assert_exit 'no subcommand: non-zero exit' $? 1
assert 'no subcommand: stderr present' "[ -s '$TMP/err16a.txt' ]"

(cd "$repo" && bash "$SCRIPT" unknown-subcommand 2>"$TMP/err16b.txt")
assert_exit 'unknown subcommand: non-zero exit' $? 1
assert 'unknown subcommand: stderr present' "[ -s '$TMP/err16b.txt' ]"

(cd "$repo" && bash "$SCRIPT" resolve not-a-number 2>"$TMP/err16c.txt")
assert_exit 'non-numeric pr number: non-zero exit' $? 1

(cd "$repo" && bash "$SCRIPT" create-fallback 2>"$TMP/err16d.txt")
assert_exit 'create-fallback missing args: non-zero exit' $? 1

(cd "$repo" && bash "$SCRIPT" finalize "$WT_NAME" 2>"$TMP/err16e.txt")
assert_exit 'finalize missing head-ref: non-zero exit' $? 1

# --- ケース17: リポジトリ外 → 非ゼロ exit ---
mkdir -p "$TMP/not-a-repo"
(cd "$TMP/not-a-repo" && bash "$SCRIPT" resolve 42 2>"$TMP/err17.txt")
assert_exit 'outside repo: non-zero exit' $? 1
assert 'outside repo: stderr present' "[ -s '$TMP/err17.txt' ]"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
