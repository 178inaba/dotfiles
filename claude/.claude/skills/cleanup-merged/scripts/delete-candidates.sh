#!/bin/bash

# /cleanup-merged の削除実行スクリプト
#
# collect-candidates.sh の出力 JSON（承認後、部分承認なら AI が候補を間引いたもの）を
# stdin で受け取り、worktree と branch を削除して結果を JSON で stdout に出力する。
#
# 削除は `git branch -d` 等の単純コマンドの列だが、あえてスクリプト化している:
# - AI がアドホックにループを書くと、zsh（Bash ツールの実行シェル）では変数名 path が
#   PATH に連動する特殊配列のため、`path=` 代入で PATH を破壊する事故が実際に起きた。
#   候補 JSON のフィールド名が path である限り同じ変数名に誘導されるため、コードに固定する
# - verdict による -d / -D の分岐をテストで担保できる場所に集約する
#
# 使用方法: delete-candidates.sh < candidates.json
# 入力契約: {"candidates": {"worktrees": [{"path","branch","verdict",...}], "branches": [{"branch","verdict",...}]}}
# 出力契約: {"removed": {"worktrees": [...], "branches": [...]}, "failures": [{"type","target","error"}]}
#   個別の削除失敗は failures に記録して処理を継続し、exit 0 で返す
#   （非ゼロ exit は前提不成立のみ。check-skill-refs.sh と同じ流儀）

set -u

# skills/<skill>/scripts/ → .claude/scripts/ の相対深さは、リポジトリ側と stow 済みの
# ~/.claude 側で同一なので相対トラバースで解決する
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)/scripts/warnings-lib.sh"
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/inuse-lib.sh"

git rev-parse --git-dir >/dev/null 2>&1 || fatal 'not a git repository'
command -v jq >/dev/null 2>&1 || fatal 'jq is required'
command -v "$LSOF_BIN" >/dev/null 2>&1 || fatal 'lsof is required'

input=$(cat)
printf '%s' "$input" | jq -e . >/dev/null 2>&1 || fatal 'invalid JSON on stdin'
printf '%s' "$input" | jq -e '.candidates' >/dev/null 2>&1 || fatal 'stdin JSON missing .candidates'

# git は誰かのプロセスが cwd にしている worktree の remove を拒否しない（clean なら成功し、
# そのプロセスは cwd 消滅で以降の全コマンドが失敗する）。削除直前に cwd 保持プロセスを
# 検出して拒否する。自セッション（ExitWorktree 漏れ）も他セッション（デプロイ見届け等の
# 常駐）も同じ「cwd に居るプロセス」なので、この1つの検査で両方を止める
load_cwd_table || fatal 'lsof failed to enumerate process cwds'

removed_wts=""
removed_brs=""
failures=""

add_failure() {
  failures+=$(jq -nc --arg t "$1" --arg target "$2" --arg e "$3" \
    '{type: $t, target: $target, error: $e}')$'\n'
}

# verdict に応じた branch 削除。pr_closed のみ -D（PR head 照合済みで gh pr checkout により
# 復元可能）。それ以外は -d に留め、git 自身のマージ判定を二重セーフティとして残す
delete_branch() {
  local branch=$1 verdict=$2 head_oid=$3 flag=-d err cur
  if [ "$verdict" = "pr_closed" ]; then
    flag=-D
    # -D は git 自身のマージ判定が効かない唯一の経路。collect の照合時点から branch に
    # commit が積まれていないか、削除直前に再照合する（照合〜承認〜削除の間の TOCTOU ガード）
    cur=$(git rev-parse "refs/heads/$branch" 2>/dev/null)
    if [ -z "$head_oid" ] || [ "$cur" != "$head_oid" ]; then
      add_failure branch "$branch" "refusing -D: branch head no longer matches verified PR head (expected ${head_oid:-<missing>}, got ${cur:-<unresolved>})"
      return
    fi
  fi
  if err=$(git branch "$flag" "$branch" 2>&1 >/dev/null); then
    removed_brs+="$branch"$'\n'
  else
    add_failure branch "$branch" "$err"
  fi
}

while IFS=$'\t' read -r wt_path branch verdict head_oid; do
  [ -z "$wt_path" ] && continue
  holders=$(cwd_holders "$wt_path")
  if [ -n "$holders" ]; then
    add_failure worktree "$wt_path" "refusing to remove: in use by $holders"
    continue
  fi
  if err=$(git worktree remove "$wt_path" 2>&1 >/dev/null); then
    removed_wts+="$wt_path"$'\n'
    delete_branch "$branch" "$verdict" "$head_oid"
  else
    add_failure worktree "$wt_path" "$err"
  fi
done < <(printf '%s' "$input" | jq -r '.candidates.worktrees[]? | [.path, .branch, .verdict, .head_oid // ""] | @tsv')

while IFS=$'\t' read -r branch verdict head_oid; do
  [ -z "$branch" ] && continue
  delete_branch "$branch" "$verdict" "$head_oid"
done < <(printf '%s' "$input" | jq -r '.candidates.branches[]? | [.branch, .verdict, .head_oid // ""] | @tsv')

jq -n \
  --argjson worktrees "$(to_string_array "$removed_wts")" \
  --argjson branches "$(to_string_array "$removed_brs")" \
  --argjson failures "$(printf '%s' "$failures" | jq -s '.')" \
  '{removed: {worktrees: $worktrees, branches: $branches}, failures: $failures}'
