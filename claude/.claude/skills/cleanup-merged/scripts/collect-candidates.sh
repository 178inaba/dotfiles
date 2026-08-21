#!/bin/bash

# /cleanup-merged の削除候補収集スクリプト
#
# worktree/branch の収集・マージ判定・セーフティチェック（決定的処理）を一括実行し、
# 結果を JSON で stdout に出力する。削除の実行は行わない（AI 側が承認フローを経て実行する）。
#
# 使用方法: collect-candidates.sh
# 出力契約: SKILL.md の「出力 JSON の契約」を参照
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）

set -u

GH_BIN=${GH_BIN:-gh}

# skills/<skill>/scripts/ → .claude/scripts/ の相対深さは、リポジトリ側と stow 済みの
# ~/.claude 側で同一なので相対トラバースで解決する
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)/scripts/warnings-lib.sh"

for arg in "$@"; do
  fatal "unknown argument: $arg"
done

git rev-parse --git-dir >/dev/null 2>&1 || fatal 'not a git repository'
command -v jq >/dev/null 2>&1 || fatal 'jq is required'

degraded=false

default_branch=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@')
[ -z "$default_branch" ] && default_branch="main"

if ! git fetch origin "$default_branch" >/dev/null 2>&1; then
  add_warning "git fetch origin $default_branch に失敗（ローカル $default_branch が stale の可能性あり）"
fi

current_worktree=$(git rev-parse --show-toplevel)
current_branch=$(git branch --show-current)

# マージ判定の基準 ref は origin 側を優先（local stale を回避）、無ければローカルへフォールバック
merged_base="origin/$default_branch"
if ! git rev-parse --verify --quiet "$merged_base" >/dev/null; then
  merged_base=$default_branch
  add_warning "origin/$default_branch が存在しないためローカル $default_branch で判定"
fi

# awk '{print $NF}' で先頭マーカー（* カレント / + 他 worktree checked out）を除去
merged_branches=$(git branch --merged "$merged_base" 2>/dev/null | awk '{print $NF}')

if ! repo=$("$GH_BIN" repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null) || [ -z "$repo" ]; then
  degraded=true
  add_warning "gh が利用できないためオフライン判定（PR 情報なし）"
fi

is_protected() {
  case "$1" in
    main | master | develop) return 0 ;;
  esac
  [ "$1" = "$default_branch" ]
}

# 改行区切りリスト $1 に行 $2 が含まれるか（branch 名に glob 特殊文字は使えないため安全）
contains_line() {
  [[ $'\n'"$1"$'\n' == *$'\n'"$2"$'\n'* ]]
}

# マージ判定。verdict / detail を設定する（両方空 = in-flight として候補から除外）。
# judge_skip / judge_skip_detail は判定段階で確定する skip（セーフティ不成立）
verdict=""
detail=""
judge_skip=""
judge_skip_detail=""
judge_branch() {
  local branch=$1 prs cls num oid local_head
  verdict=""
  detail=""
  judge_skip=""
  judge_skip_detail=""
  if [ "$degraded" = false ]; then
    if prs=$("$GH_BIN" pr list --head "$branch" --state all --json number,state,mergedAt,headRefOid --limit 20 -R "$repo" 2>/dev/null); then
      # 1パスで分類: "open" / "merged <番号>" / "no_pr" / "has_pr <未マージCLOSED番号|空> <head OID|空>"
      # - OPEN の PR がある branch は他の判定より優先して in-flight 扱い
      #   （MERGED/CLOSED な旧 PR が併存していても、進行中の作業を削除候補にしない）
      # - gh の CLOSED には MERGED も含まれるため mergedAt == null で未マージのみに絞る
      cls=$(printf '%s' "$prs" | jq -r '
        if any(.[]; .state == "OPEN") then
          "open"
        elif any(.[]; .state == "MERGED") then
          "merged \([.[] | select(.state == "MERGED")][0].number)"
        elif length == 0 then
          "no_pr"
        else
          [.[] | select(.state == "CLOSED" and .mergedAt == null)][0] as $c
            | "has_pr \($c.number // "") \($c.headRefOid // "")"
        end' 2>/dev/null)
      case "$cls" in
        merged\ *)
          verdict="pr_merged"
          detail="PR #${cls#merged } MERGED"
          ;;
        no_pr)
          if contains_line "$merged_branches" "$branch"; then
            verdict="merged_no_pr"
            detail="$default_branch にマージ済み（PRなし）"
          fi
          ;;
        has_pr\ *)
          read -r num oid <<<"${cls#has_pr }"
          if [ -n "$num" ]; then
            # 未マージ CLOSED は git branch -D が必要（-d の二重セーフティが効かない）ため、
            # local head == PR head の照合で置き換える。一致すれば GitHub 側に refs/pull/N/head
            # が恒久的に残り `gh pr checkout N` で完全復元できる。不一致 = PR に含まれない
            # ローカル commit があるので削除しない
            local_head=$(git rev-parse "$branch" 2>/dev/null)
            if [ -n "$oid" ] && [ "$local_head" = "$oid" ]; then
              verdict="pr_closed"
              detail="PR #$num CLOSED（未マージ・PR head 一致）"
            else
              judge_skip="local_commits_beyond_pr"
              judge_skip_detail="PR #$num CLOSED（未マージ）だが PR head と不一致（ローカル限定 commit あり）"
            fi
          fi
          ;;
      esac
      return
    fi
    degraded=true
    add_warning "gh pr list が失敗したためオフライン判定に切替（branch: $branch 以降）"
  fi
  if contains_line "$merged_branches" "$branch"; then
    verdict="merged_no_pr"
    detail="$default_branch にマージ済み（PRなし・オフライン判定）"
  fi
}

# セーフティチェック。skip 理由を出力する（安全なら空）。
# pr_closed では unpushed 系チェックを適用しない: 同じ懸念（PR に含まれない commit）を
# judge_branch の PR head 照合が直接検証済みで、かつ CLOSED PR はリモート branch 削除済みの
# ことが多く no_upstream_with_commits が誤爆して常時対象化が機能しなくなるため
worktree_skip_reason() {
  local wt_path=$1 verdict=$2
  if [ -n "$(git -C "$wt_path" status --porcelain 2>/dev/null)" ]; then
    printf 'uncommitted_changes'
  elif [ "$verdict" = "pr_closed" ]; then
    return
  elif [ -n "$(git -C "$wt_path" log @{u}..HEAD --oneline 2>/dev/null)" ]; then
    printf 'unpushed_commits'
  # upstream 未設定 & 自前 commit あり: branch 側と同じ保険（@{u} が無いと上の判定が silent に素通りするため）
  elif ! git -C "$wt_path" rev-parse --abbrev-ref '@{u}' >/dev/null 2>&1 &&
    [ -n "$(git -C "$wt_path" log "$default_branch..HEAD" --oneline 2>/dev/null)" ]; then
    printf 'no_upstream_with_commits'
  fi
}

branch_skip_reason() {
  local branch=$1 verdict=$2
  [ "$verdict" = "pr_closed" ] && return
  if [ -n "$(git log "$branch@{u}..$branch" --oneline 2>/dev/null)" ]; then
    printf 'unpushed_commits'
    return
  fi
  # upstream 未設定 & 自前 commit あり: --merged 判定をすり抜けるローカル限定 branch の保険
  if ! git rev-parse --abbrev-ref "$branch@{u}" >/dev/null 2>&1 &&
    [ -n "$(git log "$default_branch..$branch" --oneline 2>/dev/null)" ]; then
    printf 'no_upstream_with_commits'
  fi
}

# skip 理由コード → 一覧表示用の文字列
skip_detail() {
  case "$1" in
    uncommitted_changes) printf '未コミット変更あり' ;;
    unpushed_commits) printf '未 push commit あり' ;;
    no_upstream_with_commits) printf 'upstream 未設定 & 自前 commit あり' ;;
  esac
}

wt_candidates=""
br_candidates=""
skipped=""
detached=""
wt_branches=""

# porcelain 出力を「path<TAB>branch」（detached は branch 空）に整形。
# パスに空白が含まれても壊れないよう $2 ではなく substr で切り出す
wt_porcelain=$(git worktree list --porcelain)
wt_list=$(printf '%s\n' "$wt_porcelain" | awk '
  /^worktree / { path = substr($0, 10) }
  /^branch /   { print path "\t" substr($0, 19) }
  /^detached$/ { print path "\t" }
')
main_worktree=$(printf '%s\n' "$wt_list" | head -n1 | cut -f1)

# bare リポジトリ + worktree 構成の検出。bare な main は wt_list（branch/detached 行のみ抽出）に
# 現れず main_worktree が最初の linked worktree を指してしまうため、porcelain の先頭エントリで判定する
first_wt=$(printf '%s\n' "$wt_porcelain" | head -n1)
main_is_bare=$(git -C "${first_wt#worktree }" rev-parse --is-bare-repository 2>/dev/null)

while IFS=$'\t' read -r wt_path branch; do
  [ -z "$wt_path" ] && continue
  [ -n "$branch" ] && wt_branches+="$branch"$'\n'
  [ "$wt_path" = "$main_worktree" ] && continue
  if [ -z "$branch" ]; then
    detached+="$wt_path"$'\n'
    continue
  fi
  judge_branch "$branch"
  if [ -n "$judge_skip" ]; then
    skipped+=$(jq -nc --arg target "$wt_path" --arg b "$branch" --arg r "$judge_skip" --arg d "$judge_skip_detail" \
      '{type: "worktree", target: $target, branch: $b, reason: $r, detail: $d}')$'\n'
    continue
  fi
  [ -z "$verdict" ] && continue
  reason=$(worktree_skip_reason "$wt_path" "$verdict")
  if [ -n "$reason" ]; then
    skipped+=$(jq -nc --arg target "$wt_path" --arg b "$branch" --arg r "$reason" --arg d "$(skip_detail "$reason")" \
      '{type: "worktree", target: $target, branch: $b, reason: $r, detail: $d}')$'\n'
  else
    ic=false
    [ "$wt_path" = "$current_worktree" ] && ic=true
    wt_candidates+=$(jq -nc --arg p "$wt_path" --arg b "$branch" --arg v "$verdict" --arg d "$detail" --argjson ic "$ic" \
      '{path: $p, branch: $b, verdict: $v, detail: $d, is_current: $ic}')$'\n'
  fi
done <<<"$wt_list"

while IFS= read -r branch; do
  [ -z "$branch" ] && continue
  is_protected "$branch" && continue
  if contains_line "$wt_branches" "$branch"; then
    # どこかの worktree でチェックアウト中の branch は原則対象外（linked worktree のものは
    # worktree 候補側で扱い、他ツリーのものは git branch -d が拒否する）。例外として
    # main worktree でチェックアウト中のカレントブランチのみ通過させる（git switch 後に削除可能）。
    # bare + worktree 構成では main_worktree 検出が最初の linked worktree を指すため通過させない
    if [ "$branch" != "$current_branch" ] || [ "$current_worktree" != "$main_worktree" ] || [ "$main_is_bare" = true ]; then
      continue
    fi
  fi
  judge_branch "$branch"
  if [ -n "$judge_skip" ]; then
    skipped+=$(jq -nc --arg target "$branch" --arg r "$judge_skip" --arg d "$judge_skip_detail" \
      '{type: "branch", target: $target, reason: $r, detail: $d}')$'\n'
    continue
  fi
  [ -z "$verdict" ] && continue
  reason=$(branch_skip_reason "$branch" "$verdict")
  if [ -n "$reason" ]; then
    skipped+=$(jq -nc --arg target "$branch" --arg r "$reason" --arg d "$(skip_detail "$reason")" \
      '{type: "branch", target: $target, reason: $r, detail: $d}')$'\n'
  else
    ic=false
    [ "$branch" = "$current_branch" ] && ic=true
    br_candidates+=$(jq -nc --arg b "$branch" --arg v "$verdict" --arg d "$detail" --argjson ic "$ic" \
      '{branch: $b, verdict: $v, detail: $d, is_current: $ic}')$'\n'
  fi
done < <(git branch --format='%(refname:short)')

to_json_array() {
  printf '%s' "$1" | jq -s '.'
}

jq -n \
  --argjson degraded "$degraded" \
  --arg default_branch "$default_branch" \
  --arg current_worktree "$current_worktree" \
  --argjson worktrees "$(to_json_array "$wt_candidates")" \
  --argjson branches "$(to_json_array "$br_candidates")" \
  --argjson skipped "$(to_json_array "$skipped")" \
  --argjson detached "$(to_string_array "$detached")" \
  --argjson warnings "$(warnings_json)" \
  '{
    degraded: $degraded,
    default_branch: $default_branch,
    current_worktree: $current_worktree,
    candidates: {worktrees: $worktrees, branches: $branches},
    skipped: $skipped,
    detached: $detached,
    warnings: $warnings
  }'
