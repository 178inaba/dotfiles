#!/bin/bash

# /deep-review のレビュー投稿スクリプト
#
# レビュー結果（総合評価・body・行コメント）を受け取り、投稿前検証を通してから
# GitHub REST API でレビューを投稿する。検証は決定的に実行する:
#   1. ローカル HEAD == pr.head_oid の再確認（鮮度確認後に head が動いた場合の 422 を防ぐ）
#   2. 行コメントの path/line を最新 diff（origin/<base>...HEAD の新ファイル側行番号、
#      追加行 + context 行）と突き合わせ、diff に無い行があれば投稿せず非ゼロ exit で
#      違反エントリを stderr に列挙する（422 を「投稿後の失敗」から「投稿前の検証」へ前倒し。
#      行の付け直しは AI 側の判断）
# 「総合評価 → event」の決定表もここが所有する（レビュアーの遠慮・忖度で event が
# 揺れないよう機械的に変換する）。
#
# 使用方法: post-review.sh <pr-context.json> <review.json>
#   <pr-context.json> fetch-pr-context.sh の出力ファイル（repo / pr.number / pr.base_ref /
#                     pr.head_oid を読む）
#   <review.json>     入力契約は SKILL.md の「review.json の入力契約」を参照
#   対象リポジトリ内の cwd で実行すること
#
# 出力契約: SKILL.md の「post-review.sh の出力 JSON の契約」を参照
#
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）

set -u

GH_BIN=${GH_BIN:-gh}

fatal() {
  printf '%s\n' "$1" >&2
  exit 1
}

command -v jq >/dev/null 2>&1 || fatal 'jq is required'
command -v git >/dev/null 2>&1 || fatal 'git is required'

context_file=${1:-}
review_file=${2:-}
{ [ -n "$context_file" ] && [ -n "$review_file" ]; } \
  || fatal 'usage: post-review.sh <pr-context.json> <review.json>'
[ -f "$context_file" ] || fatal "pr context file not found: $context_file"
[ -f "$review_file" ] || fatal "review file not found: $review_file"

git rev-parse --git-dir >/dev/null 2>&1 || fatal 'not inside a git repository'

repo=$(jq -er '.repo' "$context_file" 2>/dev/null) || fatal "repo missing in $context_file"
pr_number=$(jq -er '.pr.number' "$context_file" 2>/dev/null) || fatal "pr.number missing in $context_file"
base_ref=$(jq -er '.pr.base_ref' "$context_file" 2>/dev/null) || fatal "pr.base_ref missing in $context_file"
head_oid=$(jq -er '.pr.head_oid' "$context_file" 2>/dev/null) || fatal "pr.head_oid missing in $context_file"

assessment=$(jq -er '.assessment' "$review_file" 2>/dev/null) || fatal "assessment missing in $review_file"
jq -e '.body | type == "string"' "$review_file" >/dev/null 2>&1 || fatal "body missing in $review_file"
jq -e '(.comments | type == "array") and ([.comments[] | select((.path | type) != "string" or (.line | type) != "number" or (.body | type) != "string")] | length == 0)' \
  "$review_file" >/dev/null 2>&1 \
  || fatal "comments must be an array of {path: string, line: number, body: string} in $review_file"

case "$assessment" in
  'Approve可能')  event=APPROVE ;;
  '修正が必要')   event=REQUEST_CHANGES ;;
  '要議論')       event=COMMENT ;;
  *) fatal "invalid assessment: $assessment (expected: Approve可能 | 修正が必要 | 要議論)" ;;
esac

# --- 投稿前検証1: HEAD == pr.head_oid ---
local_head=$(git rev-parse HEAD)
[ "$local_head" = "$head_oid" ] \
  || fatal "local HEAD ($local_head) differs from PR head ($head_oid); rerun the freshness check before posting"

# --- 投稿前検証2: 行コメントの path/line が最新 diff に存在すること ---
comment_count=$(jq -r '.comments | length' "$review_file")
if [ "$comment_count" -gt 0 ]; then
  valid_lines=$(git diff "origin/$base_ref...HEAD" | awk '
    /^\\ / { next }
    /^\+\+\+ / {
      file = substr($0, 5)
      sub(/^b\//, "", file)
      next
    }
    /^@@ / {
      if (match($0, /\+[0-9]+(,[0-9]+)?/)) {
        split(substr($0, RSTART + 1, RLENGTH - 1), a, ",")
        n = a[1]
        inhunk = 1
      }
      next
    }
    inhunk && /^\+/ { print file ":" n; n++; next }
    inhunk && /^-/ { next }
    inhunk && /^ / { print file ":" n; n++; next }
    { inhunk = 0 }
  ')
  invalid=$(jq -r '.comments[] | "\(.path):\(.line)"' "$review_file" \
    | grep -Fxv -f <(printf '%s\n' "$valid_lines")) || true
  if [ -n "$invalid" ]; then
    printf 'the following review comments point to lines absent from the current diff (origin/%s...HEAD); re-anchor them before posting:\n%s' \
      "$base_ref" "$invalid" >&2
    exit 1
  fi
fi

# --- 投稿 ---
payload=$(jq \
  --arg commit_id "$head_oid" \
  --arg event "$event" \
  '{commit_id: $commit_id, event: $event, body, comments: [.comments[] | {path, line, body}]}' \
  "$review_file")

response=$(printf '%s' "$payload" | "$GH_BIN" api "repos/$repo/pulls/$pr_number/reviews" --method POST --input -) \
  || fatal 'failed to post review (gh api)'

url=$(printf '%s' "$response" | jq -r '.html_url // empty')
[ -n "$url" ] || fatal 'review posted but html_url missing in the API response'

jq -n --arg url "$url" '{url: $url}'
