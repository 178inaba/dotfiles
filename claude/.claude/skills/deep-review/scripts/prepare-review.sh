#!/bin/bash

# /deep-review の準備スクリプト
#
# レビュー開始前の決定的な配管を1回の呼び出しに集約する: フラグ検証 → PR 存在プローブ →
# branch 一致確認 → PR コンテキスト取得（fetch-pr-context.sh、truncation 時の自動再実行込み）→
# 3モード判定 → PR head との鮮度確認（check-pr-freshness.sh）→ ベースブランチ判定。
# レビュー自体（差分の評価・指摘の判断）は行わない — AI 側が本スクリプトの出力を前提に実施する。
#
# 使用方法: prepare-review.sh <scratchpad-dir> [<pr-number>] [--issue N] [--worktree] [--local-only] [--no-autofix]
#   対象リポジトリ内（--worktree 時は解決済み worktree 内）の cwd で実行すること
#
# 出力契約: SKILL.md の「prepare-review.sh の出力 JSON の契約」を参照
#
# 「PR なし」（番号省略かつカレント branch に PR が無い）はローカルレビューへの正常な縮退
# （pr_exists: false）。それ以外の失敗 — 明示指定 PR の不在・PR があるのにコンテキスト取得や
# 鮮度確認が失敗 — は縮退させず非ゼロ exit + 英語 stderr で停止する（「PR なし」と混同すると
# is_own_pr 不在のまま自動対応 ON へ倒れる事故になるため）。
#
# 環境変数: GH_BIN                — gh コマンドの差し替え（テスト用スタブ）
#           FETCH_PR_CONTEXT_BIN  — fetch-pr-context.sh の差し替え（同上）
#           CHECK_PR_FRESHNESS_BIN — check-pr-freshness.sh の差し替え（同上）

set -u

GH_BIN=${GH_BIN:-gh}
FETCH_PR_CONTEXT_BIN=${FETCH_PR_CONTEXT_BIN:-$HOME/.claude/scripts/fetch-pr-context.sh}
CHECK_PR_FRESHNESS_BIN=${CHECK_PR_FRESHNESS_BIN:-$HOME/.claude/skills/worktree-resolution/scripts/check-pr-freshness.sh}

USAGE='usage: prepare-review.sh <scratchpad-dir> [<pr-number>] [--issue N] [--worktree] [--local-only] [--no-autofix]'

fatal() {
  printf '%s\n' "$1" >&2
  exit 1
}

command -v jq >/dev/null 2>&1 || fatal 'jq is required'
command -v git >/dev/null 2>&1 || fatal 'git is required'

scratch_dir=${1:-}
[ -n "$scratch_dir" ] || fatal "$USAGE"
[ -d "$scratch_dir" ] || fatal "scratchpad directory not found: $scratch_dir"
shift

git rev-parse --git-dir >/dev/null 2>&1 || fatal 'not inside a git repository'

# --- フラグ解析（未定義フラグは定義済み一覧を添えてエラー — typo が黙って無視され
#     意図しないモードで実行される事故を防ぐ） ---
pr_number=""
issue_number=""
worktree=false
local_only=false
no_autofix=false
while [ $# -gt 0 ]; do
  case "$1" in
    --issue)
      [ -n "${2:-}" ] || fatal '--issue requires a number'
      case "$2" in *[!0-9]*|'') fatal "invalid issue number: $2" ;; esac
      issue_number=$2
      shift 2
      ;;
    --worktree)   worktree=true; shift ;;
    --local-only) local_only=true; shift ;;
    --no-autofix) no_autofix=true; shift ;;
    --*)
      fatal "unknown flag: $1 (defined flags: --issue N, --worktree, --local-only, --no-autofix)"
      ;;
    *)
      case "$1" in *[!0-9]*) fatal "invalid argument: $1 ($USAGE)" ;; esac
      [ -z "$pr_number" ] || fatal "duplicate pr number argument: $1"
      pr_number=$1
      shift
      ;;
  esac
done

warnings=""
add_warning() {
  warnings="${warnings}${1}
"
}

# --- PR 存在プローブ（fetch より先に「PR なし」か「取得失敗」かを確定させる） ---
pr_exists=true
head_ref=""
explicit_pr=false
[ -z "$pr_number" ] || explicit_pr=true
if [ -n "$pr_number" ]; then
  probe=$("$GH_BIN" pr view "$pr_number" --json number,headRefName 2>/dev/null) \
    || fatal "gh pr view failed for PR #$pr_number (not found, unauthenticated, or network error)"
else
  if probe=$("$GH_BIN" pr view --json number,headRefName 2>/dev/null); then
    pr_number=$(printf '%s' "$probe" | jq -r '.number')
  else
    pr_exists=false
  fi
fi
if [ "$pr_exists" = "true" ]; then
  head_ref=$(printf '%s' "$probe" | jq -r '.headRefName')
  [ -n "$head_ref" ] && [ "$head_ref" != "null" ] || fatal 'failed to read headRefName from gh pr view'
fi

# 出力フィールドはグローバル変数から組む。emit 時点で確定している変数だけが非 null になる
# （停止 status では context_path 以降が未確定のまま出力される）
context_path=""
base_branch=""
modes=null
freshness=null
issues='[]'

emit() {
  local status=$1
  jq -n \
    --arg status "$status" \
    --argjson pr "${pr_number:-null}" \
    --argjson issue "${issue_number:-null}" \
    --argjson worktree "$worktree" \
    --argjson local_only "$local_only" \
    --argjson no_autofix "$no_autofix" \
    --argjson pr_exists "$pr_exists" \
    --arg head_ref "$head_ref" \
    --arg context_path "$context_path" \
    --arg base_branch "$base_branch" \
    --argjson modes "$modes" \
    --argjson freshness "$freshness" \
    --argjson issues "$issues" \
    --argjson warnings "$(printf '%s' "$warnings" | jq -Rs 'split("\n") | map(select(length > 0))')" \
    '{status: $status,
      flags: {pr_number: $pr, issue: $issue, worktree: $worktree, local_only: $local_only, no_autofix: $no_autofix},
      pr_exists: $pr_exists,
      head_ref: (if $head_ref == "" then null else $head_ref end),
      context_path: (if $context_path == "" then null else $context_path end),
      base_branch: (if $base_branch == "" then null else $base_branch end),
      modes: $modes, freshness: $freshness, issues: $issues, warnings: $warnings}'
  exit 0
}

# --- モード決定表（PR なし / 自分の PR / 他人の PR） ---
decide_modes() {
  local is_own=$1
  local comment personal autofix
  if [ "$pr_exists" = "false" ] || [ "$is_own" = "true" ]; then
    comment=false; personal=true; autofix=true
  else
    comment=true; personal=false; autofix=false
  fi
  [ "$local_only" = "false" ] || comment=false
  [ "$no_autofix" = "false" ] || autofix=false
  jq -n --argjson c "$comment" --argjson p "$personal" --argjson a "$autofix" \
    '{comment: $c, personal_rules: $p, autofix: $a}'
}

# --- PR なし: ローカルレビューへの縮退（pr_exists: false がその表明） ---
if [ "$pr_exists" = "false" ]; then
  default_branch=$(git symbolic-ref refs/remotes/origin/HEAD --short 2>/dev/null | sed 's|^origin/||')
  default_branch=${default_branch:-main}
  # stale な remote tracking ref への diff は base 取り込み済みの変更を誤検出するため、
  # PR なしでも base の fetch は省略しない。ただしオフラインのローカルレビューは塞がず warning に留める
  git fetch -q origin "$default_branch" 2>/dev/null \
    || add_warning "git fetch origin $default_branch failed; diff may be computed against a stale remote tracking ref"
  base_branch="origin/$default_branch"
  modes=$(decide_modes false)
  emit ok
fi

# --- branch 一致確認（<pr-number> 明示 かつ --worktree なし。推論経路は一致が自明。
#     別 branch の差分を誤レビューしないための前提ガード） ---
if [ "$explicit_pr" = "true" ] && [ "$worktree" = "false" ]; then
  current_branch=$(git rev-parse --abbrev-ref HEAD)
  if [ "$current_branch" != "$head_ref" ]; then
    emit branch_mismatch
  fi
fi

# --- PR コンテキスト取得（truncation 時は MAX_COMMENTS を総数へ引き上げて1回だけ再実行） ---
fetch_out=$("$FETCH_PR_CONTEXT_BIN" "$scratch_dir" "$pr_number") \
  || fatal 'fetch-pr-context.sh failed while the PR exists; fix the environment issue instead of falling back to a no-PR review'
context_path=$(printf '%s' "$fetch_out" | jq -r '.path')
[ -n "$context_path" ] && [ -f "$context_path" ] || fatal 'fetch-pr-context.sh did not return a valid path'

if jq -e '.comments_truncated == true' "$context_path" >/dev/null 2>&1; then
  total=$(jq -r '.comments_total_count' "$context_path")
  fetch_out=$(MAX_COMMENTS="$total" "$FETCH_PR_CONTEXT_BIN" "$scratch_dir" "$pr_number") \
    || fatal 'fetch-pr-context.sh failed on the MAX_COMMENTS rerun'
  context_path=$(printf '%s' "$fetch_out" | jq -r '.path')
  if jq -e '.comments_truncated == true' "$context_path" >/dev/null 2>&1; then
    add_warning "comments still truncated after raising MAX_COMMENTS to $total; rerun fetch-pr-context.sh with a larger MAX_COMMENTS before reading comments"
  fi
fi

is_own_pr=$(jq -r 'if (.is_own_pr | type) == "boolean" then (.is_own_pr | tostring) else "" end' "$context_path")
[ -n "$is_own_pr" ] || fatal "is_own_pr missing in $context_path"

# --- 鮮度確認 ---
freshness=$("$CHECK_PR_FRESHNESS_BIN" "$context_path") \
  || fatal 'check-pr-freshness.sh failed'
freshness_status=$(printf '%s' "$freshness" | jq -r '.status')

# --- ベースブランチ・Issue リスト ---
base_branch="origin/$(jq -r '.pr.base_ref' "$context_path")"
if [ -n "$issue_number" ]; then
  issues=$(jq -n --argjson n "$issue_number" '[{repo: null, number: $n}]')
else
  issues=$(jq '.linked_issues // []' "$context_path")
fi

modes=$(decide_modes "$is_own_pr")

case "$freshness_status" in
  ok|synced|ahead_own) top_status=ok ;;
  *) top_status=$freshness_status ;;
esac

emit "$top_status"
