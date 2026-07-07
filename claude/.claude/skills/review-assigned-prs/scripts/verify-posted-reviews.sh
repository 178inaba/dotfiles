#!/bin/bash

# /review-assigned-prs の投稿検証スクリプト
#
# サブエージェントが完了報告した各 PR について、自分のレビューが実際に投稿されたかを
# API で事実確認し、判定を JSON で stdout に出力する。自己申告だけで「投稿済み」と
# 報告すると、投稿スキップ事故がすり抜けて同じ PR が /loop の次イテレーション以降も
# 繰り返しレビューされ続けるため。
#
# 判定は「自分の提出済みレビューが1件以上存在するか」であり「今回投稿されたか」ではない。
# 候補収集（list-pending-reviews.sh）が自分のレビュー済み PR を候補から除外するため、
# 検証対象 PR は候補時点で自分のレビュー0件であることを前提にしている。
#
# 使用方法: verify-posted-reviews.sh <owner>/<repo>#<number> [...]
# 出力契約: SKILL.md の「verify-posted-reviews.sh の出力 JSON 契約」を参照
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）

set -u

GH_BIN=${GH_BIN:-gh}

if ! command -v jq >/dev/null 2>&1; then
  printf 'jq is required\n' >&2
  exit 1
fi

if [ "$#" -eq 0 ]; then
  printf 'usage: verify-posted-reviews.sh <owner>/<repo>#<number> [...]\n' >&2
  exit 1
fi

for arg in "$@"; do
  if ! printf '%s' "$arg" | grep -Eq '^[^/#[:space:]]+/[^/#[:space:]]+#[0-9]+$'; then
    printf 'invalid PR spec: %s (expected <owner>/<repo>#<number>)\n' "$arg" >&2
    exit 1
  fi
done

if ! login=$("$GH_BIN" api user -q .login 2>/dev/null) || [ -z "$login" ]; then
  printf 'failed to get authenticated user (gh api user)\n' >&2
  exit 1
fi

degraded=false
warnings=""
results=""

for arg in "$@"; do
  owner=${arg%%/*}
  rest=${arg#*/}
  repo=${rest%%#*}
  number=${rest##*#}

  # --paginate で全ページ集約（レビュー30件超の PR で自分のレビューを取りこぼし、
  # 誤って「未投稿」判定しないため）。--paginate はページごとの JSON 配列を連結して
  # 返すので、jq -s + add で1配列に潰してから数える
  if reviews_json=$("$GH_BIN" api --paginate "repos/$owner/$repo/pulls/$number/reviews" 2>/dev/null); then
    # state == "PENDING" は自分にしか見えない未提出 draft（event なし POST で残る）のため、
    # 投稿済みとしてカウントしない
    own_count=$(jq -s --arg login "$login" \
      'add // [] | map(select(.user.login == $login and .state != "PENDING")) | length' <<< "$reviews_json")
    posted=$([ "$own_count" -gt 0 ] && echo true || echo false)
    results+=$(jq -nc \
      --arg owner "$owner" --arg repo "$repo" \
      --argjson number "$number" --argjson posted "$posted" \
      '{owner: $owner, repo: $repo, number: $number, posted: $posted}')$'\n'
  else
    degraded=true
    warnings+="failed to fetch reviews for $owner/$repo#$number"$'\n'
  fi
done

jq -n \
  --argjson degraded "$degraded" \
  --argjson results "$(jq -s '.' <<< "$results")" \
  --argjson warnings "$(jq -Rs 'split("\n") | map(select(length > 0))' <<< "$warnings")" \
  '{results: $results, degraded: $degraded, warnings: $warnings}'
