#!/bin/bash

# PR worktree 解決の配管スクリプト（worktree-resolution スキル所有。
# /deep-review・/review-response の --worktree から SKILL.md 経由で使われる）
#
# 「PR worktree 解決手順」の決定的処理を3サブコマンドで提供する。worktree 作成プリミティブの
# 選択（EnterWorktree(name:) / create-fallback）と session 切替は AI 側に残す2フェーズ契約:
# session の状態（EnterWorktree の可否・cwd）はスクリプトから観測できないため。
#
# 使用方法:
#   resolve-pr-worktree.sh resolve [<pr-number>]
#     対象リポジトリ内の任意の cwd で実行。PR 番号確定（省略時はカレント branch から推論）→
#     head branch 取得 → worktree 名計算（"/" → "-"）→ 既存 worktree 検索。
#     既存あり: origin への同期（安全な ff のみ自動）まで実施。
#     既存なし: メインリポジトリの退避（カレント branch == head branch かつ clean の場合のみ
#     デフォルト branch へ switch）と head branch の fetch まで実施。
#   resolve-pr-worktree.sh create-fallback <worktree-name> <head-ref>
#     EnterWorktree(name:) が使えないサブエージェント用の作成経路。
#     <メインworktreeルート>/.claude/worktrees/<worktree-name> に detached worktree を作成し、
#     head branch へ switch + origin へ同期する。resolve の fetch 実施が前提。
#   resolve-pr-worktree.sh finalize <worktree-name> <head-ref>
#     EnterWorktree(name:) で作成した worktree 内（cwd）で実行。head branch へ switch +
#     origin へ同期し、temp branch worktree-<worktree-name> を削除する（削除失敗は warning）。
#
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）
#
# stdout は JSON のみ（契約の正はここ。各 SKILL.md には自スキルが使うフィールドの解釈のみ書く）:
#   status         "ok" | "behind_dirty" | "diverged" | "evacuation_dirty"
#                  ok 以外は続行不可の停止条件。未コミット変更・ローカル独自 commit を
#                  破棄しないため自動解決せず、対応の判断は AI（ユーザー確認）に委ねる
#   action         resolve のみ。"enter_existing"（既存 worktree に切替）| "create"（新規作成へ進む）
#   pr_number      解決した PR 番号（resolve のみ）
#   head_ref       PR の head branch 名（resolve のみ）
#   worktree_name  head_ref の "/" を "-" に置換した worktree 名（resolve のみ）
#   worktree_path  対象 worktree の絶対パス（resolve は enter_existing 時のみ非 null）
#   evacuated      メインリポジトリをデフォルト branch へ退避したか（resolve のみ）
#   synced         origin への ff 同期を実施したか
#   ahead / behind ローカル branch と origin の乖離 commit 数（同期判定時のみ、それ以外は 0）
#   warnings[]     非致命の注意（temp branch 削除失敗等）。空でなければ AI が報告に併記する
#
# 前提不成立（リポジトリ外・jq/gh 欠如・PR 解決失敗・git 操作の機械的失敗）は非ゼロ exit +
# 英語 stderr。fork 由来 PR（head branch が origin に無い）は fetch 失敗としてここに含まれる。

set -u

GH_BIN=${GH_BIN:-gh}

fatal() {
  printf '%s\n' "$1" >&2
  exit 1
}

command -v jq >/dev/null 2>&1 || fatal 'jq is required'
command -v git >/dev/null 2>&1 || fatal 'git is required'

subcommand=${1:-}
[ -n "$subcommand" ] || fatal 'usage: resolve-pr-worktree.sh <resolve|create-fallback|finalize> [args]'

git rev-parse --git-dir >/dev/null 2>&1 || fatal 'not inside a git repository'

warnings=""
add_warning() {
  warnings="${warnings}${1}
"
}

warnings_json() {
  printf '%s' "$warnings" | jq -Rs 'split("\n") | map(select(length > 0))'
}

# 変更あり（untracked は除く）なら真
is_dirty() {
  local dir=$1
  [ -n "$(git -C "$dir" status --porcelain | grep -v '^??' | head -n1)" ]
}

# ローカル branch と origin/<branch> の乖離を分類して ahead/behind をグローバルに設定
# 戻り status: ok（乖離なし）| synced（ff 同期済み）| behind_dirty | diverged
sync_with_origin() {
  local dir=$1 branch=$2
  ahead=0
  behind=0
  local counts
  counts=$(git -C "$dir" rev-list --left-right --count "$branch...origin/$branch" 2>/dev/null) \
    || fatal "failed to compare $branch with origin/$branch"
  ahead=$(printf '%s' "$counts" | awk '{print $1}')
  behind=$(printf '%s' "$counts" | awk '{print $2}')
  if [ "$ahead" -gt 0 ]; then
    printf 'diverged'
    return
  fi
  if [ "$behind" -eq 0 ]; then
    printf 'ok'
    return
  fi
  if is_dirty "$dir"; then
    printf 'behind_dirty'
    return
  fi
  git -C "$dir" merge --ff-only -q "origin/$branch" >/dev/null 2>&1 \
    || fatal "fast-forward merge of origin/$branch failed (untracked file collision?)"
  printf 'synced'
}

# git worktree list --porcelain から branch <ref> を checkout 中の linked worktree パスを返す。
# 先頭エントリ（メイン worktree）は除外する — メインが head branch を checkout 中のケースは
# 「既存 worktree あり」ではなく退避（create 経路）で扱う。除外しないと退避が到達不能になる
find_worktree_for_branch() {
  local branch=$1
  git worktree list --porcelain | awk -v ref="refs/heads/$branch" '
    $1 == "worktree" { path = substr($0, 10); n++ }
    $1 == "branch" && $2 == ref && n > 1 { print path; exit }
  '
}

main_worktree_path() {
  git worktree list --porcelain | awk '$1 == "worktree" { print substr($0, 10); exit }'
}

worktrees_root() {
  printf '%s/.claude/worktrees' "$(dirname "$(git rev-parse --path-format=absolute --git-common-dir)")"
}

case "$subcommand" in
  resolve)
    pr_number=${2:-}
    if [ -n "$pr_number" ]; then
      case "$pr_number" in
        *[!0-9]*) fatal "invalid pr number: $pr_number" ;;
      esac
    else
      pr_number=$("$GH_BIN" pr view --json number -q .number 2>/dev/null) || pr_number=""
      [ -n "$pr_number" ] \
        || fatal 'failed to infer PR number from current branch (gh pr view); pass <pr-number> explicitly'
    fi

    head_ref=$("$GH_BIN" pr view "$pr_number" --json headRefName -q .headRefName 2>/dev/null) || head_ref=""
    [ -n "$head_ref" ] || fatal "failed to get head branch of PR #$pr_number (gh pr view)"

    worktree_name=$(printf '%s' "$head_ref" | tr '/' '-')

    emit_resolve() {
      local status=$1 action=$2 path=$3 evacuated=$4 synced=$5
      jq -n \
        --arg status "$status" \
        --arg action "$action" \
        --argjson pr "$pr_number" \
        --arg head_ref "$head_ref" \
        --arg name "$worktree_name" \
        --arg path "$path" \
        --argjson evacuated "$evacuated" \
        --argjson synced "$synced" \
        --argjson ahead "${ahead:-0}" \
        --argjson behind "${behind:-0}" \
        --argjson warnings "$(warnings_json)" \
        '{status: $status, action: $action, pr_number: $pr, head_ref: $head_ref,
          worktree_name: $name, worktree_path: (if $path == "" then null else $path end),
          evacuated: $evacuated, synced: $synced, ahead: $ahead, behind: $behind,
          warnings: $warnings}'
    }

    found=$(find_worktree_for_branch "$head_ref")
    if [ -n "$found" ]; then
      git fetch -q origin "$head_ref" 2>/dev/null \
        || fatal "git fetch origin $head_ref failed (network issue, or fork PR whose head branch is not on origin — out of scope)"
      sync_status=$(sync_with_origin "$found" "$head_ref")
      case "$sync_status" in
        ok)     emit_resolve ok enter_existing "$found" false false ;;
        synced) emit_resolve ok enter_existing "$found" false true ;;
        *)      emit_resolve "$sync_status" enter_existing "$found" false false ;;
      esac
      exit 0
    fi

    # 既存 worktree なし → 新規作成の前提を整える（退避 + fetch）
    evacuated=false
    main_path=$(main_worktree_path)
    main_branch=$(git -C "$main_path" rev-parse --abbrev-ref HEAD)
    if [ "$main_branch" = "$head_ref" ]; then
      if is_dirty "$main_path"; then
        emit_resolve evacuation_dirty create "" false false
        exit 0
      fi
      default_branch=$(git symbolic-ref refs/remotes/origin/HEAD --short 2>/dev/null | sed 's|^origin/||')
      if [ -z "$default_branch" ]; then
        default_branch=$("$GH_BIN" repo view --json defaultBranchRef -q .defaultBranchRef.name 2>/dev/null) || default_branch=""
      fi
      [ -n "$default_branch" ] || fatal 'failed to determine default branch for main repository evacuation'
      git -C "$main_path" switch -q "$default_branch" \
        || fatal "failed to switch main repository to $default_branch"
      evacuated=true
    fi

    git fetch -q origin "$head_ref" 2>/dev/null \
      || fatal "git fetch origin $head_ref failed (network issue, or fork PR whose head branch is not on origin — out of scope)"

    emit_resolve ok create "" "$evacuated" false
    ;;

  create-fallback)
    worktree_name=${2:-}
    head_ref=${3:-}
    { [ -n "$worktree_name" ] && [ -n "$head_ref" ]; } \
      || fatal 'usage: resolve-pr-worktree.sh create-fallback <worktree-name> <head-ref>'

    git rev-parse -q --verify "refs/remotes/origin/$head_ref" >/dev/null \
      || fatal "origin/$head_ref not found locally; run the resolve subcommand first (it fetches the head branch)"

    wt_path="$(worktrees_root)/$worktree_name"
    git worktree add -q --detach "$wt_path" >/dev/null 2>&1 \
      || fatal "git worktree add failed for $wt_path"
    git -C "$wt_path" switch -q "$head_ref" \
      || fatal "git switch $head_ref failed inside $wt_path"

    sync_status=$(sync_with_origin "$wt_path" "$head_ref")
    case "$sync_status" in
      ok)     status=ok; synced=false ;;
      synced) status=ok; synced=true ;;
      *)      status=$sync_status; synced=false ;;
    esac

    jq -n \
      --arg status "$status" \
      --arg path "$wt_path" \
      --argjson synced "$synced" \
      --argjson ahead "${ahead:-0}" \
      --argjson behind "${behind:-0}" \
      --argjson warnings "$(warnings_json)" \
      '{status: $status, worktree_path: $path, synced: $synced,
        ahead: $ahead, behind: $behind, warnings: $warnings}'
    ;;

  finalize)
    worktree_name=${2:-}
    head_ref=${3:-}
    { [ -n "$worktree_name" ] && [ -n "$head_ref" ]; } \
      || fatal 'usage: resolve-pr-worktree.sh finalize <worktree-name> <head-ref>'

    wt_path=$(git rev-parse --show-toplevel)
    git switch -q "$head_ref" || fatal "git switch $head_ref failed in $wt_path"

    sync_status=$(sync_with_origin "$wt_path" "$head_ref")
    case "$sync_status" in
      ok)     status=ok; synced=false ;;
      synced) status=ok; synced=true ;;
      *)      status=$sync_status; synced=false ;;
    esac

    temp_branch="worktree-$worktree_name"
    if git show-ref -q --verify "refs/heads/$temp_branch"; then
      git branch -q -d "$temp_branch" 2>/dev/null \
        || add_warning "failed to delete temp branch $temp_branch (not fully merged?); delete manually if unneeded"
    else
      add_warning "temp branch $temp_branch not found (nothing to delete)"
    fi

    jq -n \
      --arg status "$status" \
      --arg path "$wt_path" \
      --argjson synced "$synced" \
      --argjson ahead "${ahead:-0}" \
      --argjson behind "${behind:-0}" \
      --argjson warnings "$(warnings_json)" \
      '{status: $status, worktree_path: $path, synced: $synced,
        ahead: $ahead, behind: $behind, warnings: $warnings}'
    ;;

  *)
    fatal "unknown subcommand: $subcommand (expected resolve | create-fallback | finalize)"
    ;;
esac
