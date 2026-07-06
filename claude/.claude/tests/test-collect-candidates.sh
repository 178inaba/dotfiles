#!/bin/bash

# collect-candidates.sh のリグレッションテスト
#
# 実行: bash claude/.claude/tests/test-collect-candidates.sh
# 使い捨ての git リポジトリと gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../skills/cleanup-merged/scripts/collect-candidates.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# --- gh スタブ ---
# pr list は GH_STUB_DATA/<branch の / を - に置換>.json があればその内容、無ければ [] を返す
mkdir -p "$TMP/stub" "$TMP/prdata"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
if [ "${GH_STUB_FAIL:-0}" = "1" ]; then exit 1; fi
case "$1" in
  repo) printf 'owner/repo\n' ;;
  pr)
    head=""
    prev=""
    for a in "$@"; do
      if [ "$prev" = "--head" ]; then head=$a; fi
      prev=$a
    done
    f="$GH_STUB_DATA/${head//\//-}.json"
    if [ -f "$f" ]; then cat "$f"; else printf '[]\n'; fi
    ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_DATA="$TMP/prdata"

# --- リポジトリフィクスチャ ---
git init -q --bare -b main "$TMP/origin.git"
git clone -q "$TMP/origin.git" "$TMP/repo" 2>/dev/null
cd "$TMP/repo" || exit 1
git config user.email test@example.com
git config user.name test
git commit -q --allow-empty -m init
git push -q -u origin main 2>/dev/null
git remote set-head origin main

commit_file() {
  printf '%s\n' "$2" > "$1"
  git add "$1"
  git commit -q -m "add $1"
}

# merged-nopr: main にマージ済み・PR なし → merged_no_pr 候補
git switch -qc merged-nopr
commit_file a.txt a
git push -q -u origin merged-nopr 2>/dev/null
git switch -q main
git merge -q merged-nopr
git push -q origin main 2>/dev/null

# pr-merged-br: ローカル未マージだが PR は MERGED → pr_merged 候補
git switch -qc pr-merged-br
commit_file b.txt b
git push -q -u origin pr-merged-br 2>/dev/null
git switch -q main
printf '[{"number":123,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z"}]\n' > "$TMP/prdata/pr-merged-br.json"

# inflight: 未マージ・PR なし → どこにも出ない（in-flight 保持）
git switch -qc inflight
commit_file c.txt c
git push -q -u origin inflight 2>/dev/null
git switch -q main

# unpushed-br: PR は MERGED だが未 push commit あり → skip
git switch -qc unpushed-br
commit_file d.txt d
git push -q -u origin unpushed-br 2>/dev/null
commit_file d2.txt d2
git switch -q main
printf '[{"number":124,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z"}]\n' > "$TMP/prdata/unpushed-br.json"

# develop: main にマージ済みでも保護 branch として常に除外
git branch -q develop main

# closedpr: PR が CLOSED（未マージ）→ --include-closed 時のみ pr_closed 候補
git switch -qc closedpr
commit_file e.txt e
git push -q -u origin closedpr 2>/dev/null
git switch -q main
printf '[{"number":7,"state":"CLOSED","mergedAt":null}]\n' > "$TMP/prdata/closedpr.json"

# closedmerged: CLOSED だが mergedAt 非 null → --include-closed でも除外（絞り込みの検証）
git switch -qc closedmerged
commit_file f.txt f
git push -q -u origin closedmerged 2>/dev/null
git switch -q main
printf '[{"number":8,"state":"CLOSED","mergedAt":"2026-01-01T00:00:00Z"}]\n' > "$TMP/prdata/closedmerged.json"

# reopened: CLOSED 未マージ PR と OPEN PR が併存 → --include-closed でも in-flight として保持
git switch -qc reopened
commit_file r.txt r
git push -q -u origin reopened 2>/dev/null
git switch -q main
printf '[{"number":30,"state":"OPEN","mergedAt":null},{"number":29,"state":"CLOSED","mergedAt":null}]\n' > "$TMP/prdata/reopened.json"

# wt-merged: worktree でチェックアウト中（--merged 出力で + プレフィックス）かつ main にマージ済み
git worktree add -q "$TMP/wt-merged" -b wt-merged main
(cd "$TMP/wt-merged" && commit_file g.txt g)
git merge -q wt-merged
git push -q origin main 2>/dev/null

# wt-dirty: PR は MERGED だが未コミット変更あり → skip
git worktree add -q "$TMP/wt-dirty" -b wt-dirty main
printf 'dirty\n' > "$TMP/wt-dirty/dirty.txt"
printf '[{"number":125,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z"}]\n' > "$TMP/prdata/wt-dirty.json"

# wt-noupstream: PR は MERGED だが upstream 未設定 & 自前 commit あり → skip（branch 側と同じ保険）
git worktree add -q "$TMP/wt-noupstream" -b wt-noupstream main
(cd "$TMP/wt-noupstream" && commit_file h.txt h)
printf '[{"number":126,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z"}]\n' > "$TMP/prdata/wt-noupstream.json"

# wt-detached: detached HEAD の worktree → detached として別枠報告
git worktree add -q --detach "$TMP/wt-detached" main

# --- 実行 ---
out_normal=$(bash "$SCRIPT")
normal_exit=$?
out_closed=$(bash "$SCRIPT" --include-closed)
out_degraded=$(GH_STUB_FAIL=1 bash "$SCRIPT")

pass=0
fail=0

assert() {
  local name=$1 json=$2 expr=$3
  if printf '%s' "$json" | jq -e "$expr" >/dev/null 2>&1; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s\n' "$name"
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

assert_exit 'exit code 0' "$normal_exit" 0

# 通常モード
assert 'merged_no_pr candidate detected' "$out_normal" \
  'any(.candidates.branches[]; .branch == "merged-nopr" and .verdict == "merged_no_pr")'
assert 'pr_merged candidate with PR number' "$out_normal" \
  'any(.candidates.branches[]; .branch == "pr-merged-br" and .verdict == "pr_merged" and (.detail | contains("123")))'
assert 'in-flight branch absent everywhere' "$out_normal" \
  '([.candidates.branches[].branch] | index("inflight") | not) and ([.skipped[].target] | index("inflight") | not)'
assert 'unpushed commits skipped' "$out_normal" \
  'any(.skipped[]; .target == "unpushed-br" and .reason == "unpushed_commits")'
assert 'protected branch (develop) excluded' "$out_normal" \
  '[.candidates.branches[].branch] | index("develop") | not'
assert 'worktree candidate via + prefix branch' "$out_normal" \
  'any(.candidates.worktrees[]; .branch == "wt-merged" and .verdict == "merged_no_pr")'
assert 'dirty worktree skipped with display detail' "$out_normal" \
  'any(.skipped[]; .type == "worktree" and .branch == "wt-dirty" and .reason == "uncommitted_changes" and .detail == "未コミット変更あり")'
assert 'no-upstream worktree with own commits skipped' "$out_normal" \
  'any(.skipped[]; .type == "worktree" and .branch == "wt-noupstream" and .reason == "no_upstream_with_commits")'
assert 'detached worktree reported separately' "$out_normal" \
  'any(.detached[]; endswith("wt-detached"))'
assert 'closed PR excluded without flag' "$out_normal" \
  '([.candidates.branches[].branch] | index("closedpr") | not) and .degraded == false'

# --include-closed
assert 'closed-unmerged PR included with flag' "$out_closed" \
  'any(.candidates.branches[]; .branch == "closedpr" and .verdict == "pr_closed" and (.detail | contains("7")))'
assert 'closed-but-merged (mergedAt != null) still excluded' "$out_closed" \
  '[.candidates.branches[].branch] | index("closedmerged") | not'
assert 'open PR coexisting with closed PR kept in-flight' "$out_closed" \
  '([.candidates.branches[].branch] | index("reopened") | not) and ([.skipped[].target] | index("reopened") | not)'

# degraded（gh 不通）
assert 'degraded flag set on gh failure' "$out_degraded" \
  '.degraded == true and (.warnings | length > 0)'
assert 'offline judgment still detects merged branch' "$out_degraded" \
  'any(.candidates.branches[]; .branch == "merged-nopr" and .verdict == "merged_no_pr")'
assert 'pr_merged branch absent in degraded mode' "$out_degraded" \
  '[.candidates.branches[].branch] | index("pr-merged-br") | not'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
