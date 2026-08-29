#!/bin/bash

# collect-candidates.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/cleanup-merged/tests/test-collect-candidates.sh
# 使い捨ての git リポジトリと gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/collect-candidates.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(mktemp -d)
HOLDER_PID=""
trap '[ -n "$HOLDER_PID" ] && kill "$HOLDER_PID" 2>/dev/null; rm -rf "$TMP"' EXIT

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
# headRefOid == ローカル head なので「マージされた head と一致」ケースの検証も兼ねる
git switch -qc pr-merged-br
commit_file b.txt b
git push -q -u origin pr-merged-br 2>/dev/null
git switch -q main
printf '[{"number":123,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z","headRefOid":"%s"}]\n' \
  "$(git rev-parse refs/heads/pr-merged-br)" > "$TMP/prdata/pr-merged-br.json"

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
# headRefOid に未 push commit を含むローカル head を与える。実在の PR では起こりえない状態だが、
# マージ済み head 照合を通過させて unpushed_commits のアサーションを元の経路のまま残すため
printf '[{"number":124,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z","headRefOid":"%s"}]\n' \
  "$(git rev-parse refs/heads/unpushed-br)" > "$TMP/prdata/unpushed-br.json"

# develop: main にマージ済みでも保護 branch として常に除外
git branch -q develop main

# closedpr: PR が CLOSED（未マージ）・local head == PR head → 常に pr_closed 候補
git switch -qc closedpr
commit_file e.txt e
git push -q -u origin closedpr 2>/dev/null
git switch -q main
printf '[{"number":7,"state":"CLOSED","mergedAt":null,"headRefOid":"%s"}]\n' \
  "$(git rev-parse closedpr)" > "$TMP/prdata/closedpr.json"
# branch と同名のタグ: gitrevisions は tags を heads より先に解決するため、refs/heads/ を
# 明示しない rev-parse だとタグ OID で照合してしまう（-D 経路のゲート誤判定の回帰検証）
git tag closedpr main

# closed-local-ahead: CLOSED 未マージ PR + PR head 不一致（PR 後にローカル commit）→ skip
git switch -qc closed-local-ahead
commit_file i.txt i
git push -q -u origin closed-local-ahead 2>/dev/null
printf '[{"number":9,"state":"CLOSED","mergedAt":null,"headRefOid":"%s"}]\n' \
  "$(git rev-parse closed-local-ahead)" > "$TMP/prdata/closed-local-ahead.json"
commit_file i2.txt i2
git switch -q main

# closed-noup: CLOSED 未マージ PR + upstream なし + PR head 一致 → 候補
# （pr_closed に unpushed 系チェックを適用すると no_upstream_with_commits で誤爆する回帰の検証）
git switch -qc closed-noup
commit_file j.txt j
git switch -q main
printf '[{"number":10,"state":"CLOSED","mergedAt":null,"headRefOid":"%s"}]\n' \
  "$(git rev-parse closed-noup)" > "$TMP/prdata/closed-noup.json"

# merged-local-ahead: MERGED PR + マージ後に push した commit あり → skip（PR #98 で実際に起きた
# データ喪失の再現。unpushed / no_upstream のどのセーフティにも掛からないため専用の判定が要る）
git switch -qc merged-local-ahead
commit_file n.txt n
git push -q -u origin merged-local-ahead 2>/dev/null
printf '[{"number":127,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z","headRefOid":"%s"}]\n' \
  "$(git rev-parse refs/heads/merged-local-ahead)" > "$TMP/prdata/merged-local-ahead.json"
commit_file n2.txt n2
git push -q origin merged-local-ahead 2>/dev/null
git switch -q main

# merged-local-behind: MERGED PR + ローカル head がマージされた head の祖先（単に behind）→ 候補
# reset で外した commit はオブジェクト DB に残るため --is-ancestor が解決できる
git switch -qc merged-local-behind
commit_file o.txt o
git push -q -u origin merged-local-behind 2>/dev/null
commit_file o2.txt o2
merged_behind_oid=$(git rev-parse refs/heads/merged-local-behind)
git reset -q --hard HEAD~1
git switch -q main
printf '[{"number":128,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z","headRefOid":"%s"}]\n' \
  "$merged_behind_oid" > "$TMP/prdata/merged-local-behind.json"

# closedmerged: CLOSED だが mergedAt 非 null → 除外（未マージ CLOSED への絞り込みの検証）
git switch -qc closedmerged
commit_file f.txt f
git push -q -u origin closedmerged 2>/dev/null
git switch -q main
printf '[{"number":8,"state":"CLOSED","mergedAt":"2026-01-01T00:00:00Z"}]\n' > "$TMP/prdata/closedmerged.json"

# reopened: CLOSED 未マージ PR と OPEN PR が併存 → in-flight として保持
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
printf '[{"number":125,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z","headRefOid":"%s"}]\n' \
  "$(git rev-parse refs/heads/wt-dirty)" > "$TMP/prdata/wt-dirty.json"

# wt-noupstream: PR は MERGED だが upstream 未設定 & 自前 commit あり → skip（branch 側と同じ保険）
git worktree add -q "$TMP/wt-noupstream" -b wt-noupstream main
(cd "$TMP/wt-noupstream" && commit_file h.txt h)
printf '[{"number":126,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z","headRefOid":"%s"}]\n' \
  "$(git rev-parse refs/heads/wt-noupstream)" > "$TMP/prdata/wt-noupstream.json"

# wt-detached: detached HEAD の worktree → detached として別枠報告
git worktree add -q --detach "$TMP/wt-detached" main

# wt-closed-dirty: PR CLOSED（head 一致）だが未コミット変更あり → uncommitted_changes で skip
# （worktree_skip_reason で uncommitted チェックが pr_closed の early-return より先にある
#   順序が -D 経路に残る最後の dirty ガードであることの回帰検証）
git worktree add -q "$TMP/wt-closed-dirty" -b wt-closed-dirty main
(cd "$TMP/wt-closed-dirty" && commit_file k.txt k)
printf '[{"number":11,"state":"CLOSED","mergedAt":null,"headRefOid":"%s"}]\n' \
  "$(git rev-parse refs/heads/wt-closed-dirty)" > "$TMP/prdata/wt-closed-dirty.json"
printf 'dirty\n' > "$TMP/wt-closed-dirty/x.txt"

# wt-inuse: マージ済みだが別プロセス（他セッション相当の sleep）が cwd に居る →
# in_use_by_process で skip（is_current でない worktree のみが対象）
git worktree add -q "$TMP/wt-inuse" -b wt-inuse main
git merge -q wt-inuse 2>/dev/null || true
(cd "$TMP/wt-inuse" && exec sleep 120) &
HOLDER_PID=$!
sleep 1

# wt-closed-noup: PR CLOSED（head 一致）・clean・upstream なし → 候補
# （pr_closed が unpushed 系チェックを bypass する worktree 側の回帰検証）
git worktree add -q "$TMP/wt-closed-noup" -b wt-closed-noup main
(cd "$TMP/wt-closed-noup" && commit_file l.txt l)
printf '[{"number":12,"state":"CLOSED","mergedAt":null,"headRefOid":"%s"}]\n' \
  "$(git rev-parse refs/heads/wt-closed-noup)" > "$TMP/prdata/wt-closed-noup.json"

# wt-merged-ahead: merged-local-ahead と同じ状況を worktree で。worktree も judge_branch を通るため
# 同じ判定が効くこと（かつセーフティチェックより先に評価されること）の検証
git worktree add -q "$TMP/wt-merged-ahead" -b wt-merged-ahead main
(cd "$TMP/wt-merged-ahead" && commit_file p.txt p && git push -q -u origin wt-merged-ahead 2>/dev/null)
printf '[{"number":129,"state":"MERGED","mergedAt":"2026-01-01T00:00:00Z","headRefOid":"%s"}]\n' \
  "$(git rev-parse refs/heads/wt-merged-ahead)" > "$TMP/prdata/wt-merged-ahead.json"
(cd "$TMP/wt-merged-ahead" && commit_file p2.txt p2 && git push -q origin wt-merged-ahead 2>/dev/null)

# --- 実行 ---
out_normal=$(bash "$SCRIPT")
normal_exit=$?
bash "$SCRIPT" --include-closed >/dev/null 2>&1
removed_flag_exit=$?
out_degraded=$(GH_STUB_FAIL=1 bash "$SCRIPT")
# カレント worktree からの実行 → その worktree 自身が is_current 付き候補になる
out_current=$(cd "$TMP/wt-merged" && bash "$SCRIPT")
# bare リポジトリ + worktree 構成: bare な main は wt_list に現れないため、main_worktree の
# 検出を誤ると最初の linked worktree が main 扱いで候補から silent に消える（回帰検証）
git clone -q --bare "$TMP/origin.git" "$TMP/bare.git" 2>/dev/null
git -C "$TMP/bare.git" worktree add -q "$TMP/bare-wt" -b bare-feat main
out_bare=$(cd "$TMP/bare-wt" && bash "$SCRIPT")
# main worktree でマージ済みブランチをチェックアウトした状態 → branch 候補に is_current が付く
git switch -q merged-nopr
out_curbr=$(bash "$SCRIPT")
git switch -q main

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
# detail は完全一致で見る: cls から番号を切り出し損ねると head OID が detail に混入するため
assert 'pr_merged candidate with PR number' "$out_normal" \
  'any(.candidates.branches[]; .branch == "pr-merged-br" and .verdict == "pr_merged" and .detail == "PR #123 MERGED")'
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
assert 'closed-unmerged PR always included' "$out_normal" \
  'any(.candidates.branches[]; .branch == "closedpr" and .verdict == "pr_closed" and (.detail | contains("7")))'
assert 'closed-but-merged (mergedAt != null) excluded' "$out_normal" \
  '[.candidates.branches[].branch] | index("closedmerged") | not'
assert 'open PR coexisting with closed PR kept in-flight' "$out_normal" \
  '([.candidates.branches[].branch] | index("reopened") | not) and ([.skipped[].target] | index("reopened") | not)'
assert 'PR head mismatch skipped with reason' "$out_normal" \
  'any(.skipped[]; .target == "closed-local-ahead" and .reason == "local_commits_beyond_pr" and (.detail | contains("9"))) and ([.candidates.branches[].branch] | index("closed-local-ahead") | not)'
assert 'closed PR without upstream still candidate' "$out_normal" \
  'any(.candidates.branches[]; .branch == "closed-noup" and .verdict == "pr_closed") and ([.skipped[].target] | index("closed-noup") | not)'
assert 'pr_closed candidate carries verified head_oid' "$out_normal" \
  'any(.candidates.branches[]; .branch == "closedpr" and .head_oid == "'"$(git rev-parse refs/heads/closedpr)"'")'
assert 'dirty pr_closed worktree skipped' "$out_normal" \
  'any(.skipped[]; .type == "worktree" and .branch == "wt-closed-dirty" and .reason == "uncommitted_changes")'
assert 'clean no-upstream pr_closed worktree is candidate' "$out_normal" \
  'any(.candidates.worktrees[]; .branch == "wt-closed-noup" and .verdict == "pr_closed")'
assert 'commits beyond merged PR head skipped with reason' "$out_normal" \
  'any(.skipped[]; .type == "branch" and .target == "merged-local-ahead" and .reason == "commits_beyond_merged_pr" and (.detail | contains("127") and contains("add n2.txt"))) and ([.candidates.branches[].branch] | index("merged-local-ahead") | not)'
assert 'local head behind merged PR head still candidate' "$out_normal" \
  'any(.candidates.branches[]; .branch == "merged-local-behind" and .verdict == "pr_merged") and ([.skipped[].target] | index("merged-local-behind") | not)'
assert 'worktree with commits beyond merged PR head skipped' "$out_normal" \
  'any(.skipped[]; .type == "worktree" and .branch == "wt-merged-ahead" and .reason == "commits_beyond_merged_pr") and ([.candidates.worktrees[].branch] | index("wt-merged-ahead") | not)'
assert_exit '--include-closed flag rejected' "$removed_flag_exit" 1

# 使用中 worktree
assert 'in-use worktree skipped with holder process' "$out_normal" \
  'any(.skipped[]; .type == "worktree" and .branch == "wt-inuse" and .reason == "in_use_by_process" and (.detail | contains("sleep (PID '"$HOLDER_PID"')")))'
assert 'in-use worktree not in candidates' "$out_normal" \
  '[.candidates.worktrees[].branch] | index("wt-inuse") | not'

# カレント worktree / ブランチ
assert 'non-current worktree has is_current false' "$out_normal" \
  'any(.candidates.worktrees[]; .branch == "wt-merged" and .is_current == false)'
assert 'current worktree becomes candidate with is_current' "$out_current" \
  'any(.candidates.worktrees[]; .branch == "wt-merged" and .is_current == true)'
assert 'current_session skip reason removed' "$out_current" \
  '[.skipped[].reason] | index("current_session") | not'
assert 'current branch on main worktree becomes candidate' "$out_curbr" \
  'any(.candidates.branches[]; .branch == "merged-nopr" and .is_current == true)'
assert 'current branch of linked worktree not in branch candidates' "$out_current" \
  '[.candidates.branches[].branch] | index("wt-merged") | not'

# bare リポジトリ + worktree 構成
assert 'linked worktree of bare main is a candidate' "$out_bare" \
  'any(.candidates.worktrees[]; .branch == "bare-feat" and .is_current == true)'
assert 'current branch passthrough blocked under bare main' "$out_bare" \
  '[.candidates.branches[].branch] | index("bare-feat") | not'

# degraded（gh 不通）
assert 'degraded flag set on gh failure' "$out_degraded" \
  '.degraded == true and (.warnings | length > 0)'
assert 'offline judgment still detects merged branch' "$out_degraded" \
  'any(.candidates.branches[]; .branch == "merged-nopr" and .verdict == "merged_no_pr")'
assert 'pr_merged branch absent in degraded mode' "$out_degraded" \
  '[.candidates.branches[].branch] | index("pr-merged-br") | not'

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
