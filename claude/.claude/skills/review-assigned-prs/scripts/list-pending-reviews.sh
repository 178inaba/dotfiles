#!/bin/bash

# /review-assigned-prs の候補収集スクリプト
#
# 自分に user-review-requested が付いている open PR のうち、Bot 以外のレビューが
# まだ 1 件も付いていないもの（= 自分が最初の人のレビュアーになりうる PR）を JSON で
# stdout に出力する。レビュー実行は行わない（AI 側がサブエージェント経由で /deep-review を呼ぶ）。
#
# Draft PR は業界標準に合わせて対象外（作者が Ready for review にした時点で拾う）。
# gh 側で --draft=false でサーバー側フィルタしている。
#
# フィルタは user.type != "Bot" で一律。Copilot・github-actions・CodeRabbit 等の Bot
# レビューは全て「人間未レビュー」扱いになる。自分の既存レビューは user.type == "User"
# として弾かれるため、/loop で繰り返しても投稿済み PR は自動的に対象外になる。
#
# 使用方法: list-pending-reviews.sh
# 出力契約: SKILL.md の「出力 JSON の契約」を参照
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）

set -u

GH_BIN=${GH_BIN:-gh}

if ! command -v jq >/dev/null 2>&1; then
  printf 'jq is required\n' >&2
  exit 1
fi

degraded=false
warnings=""

if ! search=$("$GH_BIN" search prs --state=open --draft=false "user-review-requested:@me" --json url,number,repository 2>/dev/null); then
  printf 'failed to search PRs (gh search prs)\n' >&2
  exit 1
fi

filtered_items=""
while IFS=$'\t' read -r owner repo number url; do
  [ -z "$owner" ] && continue
  # --paginate で全ページ集約（30件超のレビューが Bot だけの状況で人間レビューを取りこぼさないため）
  if reviews_json=$("$GH_BIN" api --paginate "repos/$owner/$repo/pulls/$number/reviews" 2>/dev/null); then
    human_count=$(jq '[.[] | select(.user.type != "Bot")] | length' <<< "$reviews_json")
    if [ "$human_count" = "0" ]; then
      filtered_items+=$(jq -nc \
        --arg owner "$owner" --arg repo "$repo" \
        --argjson number "$number" --arg url "$url" \
        '{owner: $owner, repo: $repo, number: $number, url: $url}')$'\n'
    fi
  else
    degraded=true
    warnings+="failed to fetch reviews for $owner/$repo#$number"$'\n'
  fi
done < <(jq -r '.[] | [(.repository.nameWithOwner | split("/")[0]), (.repository.nameWithOwner | split("/")[1]), .number, .url] | @tsv' <<< "$search")

jq -n \
  --argjson degraded "$degraded" \
  --argjson prs "$(jq -s '.' <<< "$filtered_items")" \
  --argjson warnings "$(jq -Rs 'split("\n") | map(select(length > 0))' <<< "$warnings")" \
  '{prs: $prs, degraded: $degraded, warnings: $warnings}'
