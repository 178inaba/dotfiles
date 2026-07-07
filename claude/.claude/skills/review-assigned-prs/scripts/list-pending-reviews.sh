#!/bin/bash

# /review-assigned-prs の候補収集スクリプト
#
# 自分に user-review-requested が付いている open PR のうち、以下のいずれかに該当する
# ものを JSON で stdout に出力する。レビュー実行は行わない（AI 側がサブエージェント
# 経由で /deep-review を呼ぶ）:
#   - 誰もまだ人間としてレビューしていない（自分が初回レビュアーになりうる）
#   - 自分が過去にレビュー済みで、再レビュー依頼として requested_reviewers に戻された
#
# 「他人が先にレビュー済み・自分は未レビュー」の PR は除外する（他人のレビューに
# 機械的に上乗せしないため）。担当者が明示的に自分を再指名した場合（自分の過去
# レビューがある）は、他人のレビュー有無に関わらず対象とする。
#
# Draft PR は業界標準に合わせて対象外（作者が Ready for review にした時点で拾う）。
# gh 側で --draft=false でサーバー側フィルタしている。
#
# Copilot・github-actions・CodeRabbit 等の Bot レビューは全て「人間未レビュー」
# 扱いになる（判定は user.type != "Bot" で行う）。
#
# PR 作成者自身のレビューコメント（Reply スレッドが GitHub 内部で COMMENTED state の
# review として記録される）は「他人」に含めない。含めると、作者が Copilot 指摘へ
# 返信しただけの PR が「他人が先に対応済み」として誤除外される。
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

if ! me=$("$GH_BIN" api user --cache 24h --jq .login 2>/dev/null) || [ -z "$me" ]; then
  printf 'failed to fetch authenticated user (gh api user)\n' >&2
  exit 1
fi

degraded=false
warnings=""

if ! search=$("$GH_BIN" search prs --state=open --draft=false "user-review-requested:@me" --json url,number,repository,author 2>/dev/null); then
  printf 'failed to search PRs (gh search prs)\n' >&2
  exit 1
fi

filtered_items=""
while IFS=$'\t' read -r owner repo number url author; do
  [ -z "$owner" ] && continue
  if [ -z "$author" ]; then
    # author 除外が黙って無効化される事故を防ぐため degraded 経路に落とす（$me の
    # 空チェックと対称に扱う）。実データでは削除ユーザー等で null になり得る想定。
    degraded=true
    warnings+="missing author for $owner/$repo#$number"$'\n'
    continue
  fi
  # --paginate で全ページ集約（30件超のレビューが Bot だけの状況で人間レビューを取りこぼさないため）。
  # --paginate はページごとの JSON 配列を連結して返すので、jq -s + add で1配列に潰してから判定する
  # （ページごとに判定すると複数ページの PR で誤判定になる）。
  # ※ 「他人」は Bot でも自分でも PR 作成者でもない人間。作者の Reply スレッドは
  # COMMENTED state の review として API から返るため、作者を除外しないと「Copilot
  # 指摘に作者が返信しただけ」の PR が誤除外される。
  # ※ この判定は search 側で `@me` が現在 requested_reviewers に居る PR に絞られている
  # 前提で成立する（レビュー投稿後は GitHub 側で外れ、再指名で戻る挙動を利用）。search
  # クエリを変更する場合はここも見直すこと。
  if reviews_json=$("$GH_BIN" api --paginate "repos/$owner/$repo/pulls/$number/reviews" 2>/dev/null); then
    include=$(jq -s --arg me "$me" --arg author "$author" '
      add // [] |
      (any(.[]; .user.type != "Bot" and .user.login != $me and .user.login != $author) | not) or
      any(.[]; .user.type != "Bot" and .user.login == $me)
    ' <<< "$reviews_json")
    if [ "$include" = "true" ]; then
      filtered_items+=$(jq -nc \
        --arg owner "$owner" --arg repo "$repo" \
        --argjson number "$number" --arg url "$url" \
        '{owner: $owner, repo: $repo, number: $number, url: $url}')$'\n'
    fi
  else
    degraded=true
    warnings+="failed to fetch reviews for $owner/$repo#$number"$'\n'
  fi
done < <(jq -r '.[] | [(.repository.nameWithOwner | split("/")[0]), (.repository.nameWithOwner | split("/")[1]), .number, .url, .author.login] | @tsv' <<< "$search")

jq -n \
  --argjson degraded "$degraded" \
  --argjson prs "$(jq -s '.' <<< "$filtered_items")" \
  --argjson warnings "$(jq -Rs 'split("\n") | map(select(length > 0))' <<< "$warnings")" \
  '{prs: $prs, degraded: $degraded, warnings: $warnings}'
