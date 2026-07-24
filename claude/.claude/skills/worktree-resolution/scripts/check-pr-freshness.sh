#!/bin/bash

# PR head との鮮度確認スクリプト（worktree-resolution スキル所有。
# /deep-review・/review-response が差分取得・修正適用の前に SKILL.md 経由で使う）
#
# ローカル HEAD が PR の最新 head と整合しているかを確認し、安全（fast-forward のみ）なら
# 自動同期する。stale なコードを対象にすると誤スコープのレビュー・修正や、コメント投稿時の
# 行番号ずれによる 422 が起きるため、その前提ガードを決定的に実行する。
#
# 使用方法: check-pr-freshness.sh <pr-context.json>
#   <pr-context.json> fetch-pr-context.sh の出力ファイル。pr.head_oid / pr.head_ref /
#                     pr.base_ref / is_own_pr を読む。対象リポジトリ内の cwd で実行すること
#
# 内部で git fetch origin <base_ref> <head_ref> を実行してから比較する（fetch 込みなので
# 呼び出し側での事前 fetch は不要）。
#
# stdout は JSON のみ（契約の正はここ。各 SKILL.md には自スキルが使うフィールドの解釈のみ書く）:
#   status      "ok"             HEAD == head_oid。続行可
#               "synced"         behind のみ・clean だったため head_oid へ ff 同期済み。続行可
#               "ahead_own"      自分の PR でローカルに未 push commit があるだけ。続行可
#               "behind_dirty"   behind だが未コミット変更あり（untracked のみは無視）。
#                                停止 — コミット/stash をユーザーに促す
#               "diverged"       ローカルに origin に無い commit がある（他人の PR への
#                                ローカル commit 含む）。停止 — 自動解決は破棄リスクがあるため
#                                手動での rebase・退避をユーザーに促す
#               "branch_mismatch" カレント branch（detached HEAD 含む）が head_ref と不一致。
#                                停止 — 別 branch を誤って同期・レビューしないための前提ガード
#               "fetch_failed"   git fetch origin <base> <head> が失敗。fork 由来 PR
#                                （head branch が origin に無い — 対象外）の可能性が高い。停止
#   head_ref    PR の head branch 名
#   head_oid    PR の最新 head commit SHA（入力の pr.head_oid）
#   local_head  実行時点のローカル HEAD SHA（synced の場合は同期後）
#
# 前提不成立（引数欠如・ファイル/フィールド欠落・リポジトリ外・jq 欠如・ff 失敗）は
# 非ゼロ exit + 英語 stderr。

set -u

. "$(cd "$(dirname "$0")" && pwd)/sync-lib.sh"

fatal() {
  printf '%s\n' "$1" >&2
  exit 1
}

command -v jq >/dev/null 2>&1 || fatal 'jq is required'
command -v git >/dev/null 2>&1 || fatal 'git is required'

context_file=${1:-}
[ -n "$context_file" ] || fatal 'usage: check-pr-freshness.sh <pr-context.json>'
[ -f "$context_file" ] || fatal "pr context file not found: $context_file"

git rev-parse --git-dir >/dev/null 2>&1 || fatal 'not inside a git repository'

head_oid=$(jq -er '.pr.head_oid' "$context_file" 2>/dev/null) \
  || fatal "pr.head_oid missing in $context_file"
head_ref=$(jq -er '.pr.head_ref' "$context_file" 2>/dev/null) \
  || fatal "pr.head_ref missing in $context_file"
base_ref=$(jq -er '.pr.base_ref' "$context_file" 2>/dev/null) \
  || fatal "pr.base_ref missing in $context_file"
# jq -e は値が false のとき exit 1 になるため boolean は型チェックで検証する
is_own_pr=$(jq -r 'if (.is_own_pr | type) == "boolean" then (.is_own_pr | tostring) else "" end' "$context_file" 2>/dev/null)
[ -n "$is_own_pr" ] || fatal "is_own_pr missing in $context_file"

emit() {
  local status=$1
  jq -n \
    --arg status "$status" \
    --arg head_ref "$head_ref" \
    --arg head_oid "$head_oid" \
    --arg local_head "$(git rev-parse HEAD)" \
    '{status: $status, head_ref: $head_ref, head_oid: $head_oid,
      local_head: $local_head}'
  exit 0
}

if ! git fetch -q origin "$base_ref" "$head_ref" 2>/dev/null; then
  emit fetch_failed
fi

current_branch=$(git rev-parse --abbrev-ref HEAD)
[ "$current_branch" = "$head_ref" ] || emit branch_mismatch

local_head=$(git rev-parse HEAD)
[ "$local_head" != "$head_oid" ] || emit ok

if [ "$is_own_pr" = "true" ] && git merge-base --is-ancestor "$head_oid" HEAD 2>/dev/null; then
  emit ahead_own
fi

if git merge-base --is-ancestor HEAD "$head_oid" 2>/dev/null; then
  # コマンド置換内の exit はサブシェルしか止めないため、失敗を明示的に伝播させる
  sync=$(safe_ff_or_dirty . "$head_oid") || exit 1
  emit "$sync"
fi

emit diverged
