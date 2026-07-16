#!/bin/bash

# PR コンテキスト一括取得スクリプト（/review-response・/deep-review 共有）
#
# PR メタ情報・通常コメント・レビュー本文・レビュースレッドを 1 回で取得し、
# 正規化した JSON を stdout に出力する。3 種のコメントは GitHub 上で別管理のため、
# 個別取得だと取りこぼしが起きる — スーパーセットの一括取得をスクリプトで保証する。
#
# 使用方法: fetch-pr-context.sh [<pr-number>]
#   <pr-number> 省略時はカレント branch の PR を推論（失敗時は非ゼロ exit + stderr）
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）
#           MAX_COMMENTS — comments 取得の打ち切り上限（既定 500）。打ち切り発生時に引き上げて再実行する
#
# 出力 JSON の契約（正はここ。各 SKILL.md には自スキルが使うフィールドの解釈のみ書く）:
#   repo              owner/name 形式
#   current_user      実行ユーザーの login
#   is_own_pr         PR 作成者 == current_user
#   pr                number / title / body / url / state / author / head_ref / base_ref / head_oid
#   linked_issues[]   PR 本文の closing keyword から検出した {repo, number}（repo: null は同リポ）。
#                     URL 形式・キーワードなしの素の #N は対象外（GitHub の自動 close 対象に揃える）
#   comments_total_count / comments_truncated
#                     PR 上の通常コメント総数と、MAX_COMMENTS 打ち切りの発生フラグ
#   comments[]        通常コメント {author, author_type, body, created_at, url, is_skill_comment}。
#                     ページネーションで全量取得（MAX_COMMENTS 件で打ち切り）。author_type は
#                     GraphQL Actor の __typename（"User" / "Bot" 等）で、CI bot の機械的判別に使う
#   reviews_total_count / reviews_truncated
#                     レビュー総数と、取得窓（最新50件）からの欠落発生フラグ
#   reviews[]         レビュー本文 {author, state, body, url, submitted_at}
#   review_threads[]  {id, is_resolved, is_outdated, path, line, resolved_by, comments[]}

set -u

GH_BIN=${GH_BIN:-gh}

pr_number=${1:-}
if [ -n "$pr_number" ]; then
  case "$pr_number" in
    *[!0-9]*)
      printf 'invalid pr number: %s\n' "$pr_number" >&2
      exit 1
      ;;
  esac
fi

if ! command -v jq >/dev/null 2>&1; then
  printf 'jq is required\n' >&2
  exit 1
fi

if ! repo=$("$GH_BIN" repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null) || [ -z "$repo" ]; then
  printf 'failed to resolve repository (gh repo view)\n' >&2
  exit 1
fi
owner=${repo%%/*}
name=${repo#*/}

pr_fields="number,title,body,url,state,author,headRefName,baseRefName,headRefOid"
if [ -z "$pr_number" ]; then
  # カレント branch からの推論と meta 取得を 1 回の呼び出しで済ませる
  if ! pr_meta=$("$GH_BIN" pr view --json "$pr_fields" 2>/dev/null) || [ -z "$pr_meta" ]; then
    printf 'could not infer PR from current branch; specify <pr-number> explicitly\n' >&2
    exit 1
  fi
  pr_number=$(printf '%s' "$pr_meta" | jq -r '.number')
else
  if ! pr_meta=$("$GH_BIN" pr view "$pr_number" --json "$pr_fields" -R "$repo"); then
    printf 'failed to fetch PR #%s\n' "$pr_number" >&2
    exit 1
  fi
fi

# reviews は提出日時昇順で返るため last:50 で最新側を取る（first だと CI 通知・ボットレビュー等で
# 50 件を超えた際に未対応の修正依頼を取りこぼす）。comments は固定ウィンドウだと CI bot の
# sticky コメントが人間のコメントを窓外へ押し出して黙って欠落するため（実 PR で 97 件中
# 人間コメント 3 件を含む 47 件が欠落した事例あり）、後段でページネーションして全量取得する
if ! gql=$("$GH_BIN" api graphql -f query='
query($owner: String!, $name: String!, $number: Int!) {
  viewer { login }
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      comments(first: 100) {
        totalCount
        pageInfo { hasNextPage endCursor }
        nodes { author { login __typename } body createdAt url }
      }
      reviews(last: 50) {
        totalCount
        nodes { author { login } state body url submittedAt }
      }
      reviewThreads(first: 100) {
        nodes {
          id
          isResolved
          isOutdated
          path
          line
          resolvedBy { login }
          comments(first: 20) {
            nodes { author { login } body createdAt url }
          }
        }
      }
    }
  }
}' -f owner="$owner" -f name="$name" -F number="$pr_number"); then
  printf 'failed to fetch PR comments/reviews/threads (GraphQL)\n' >&2
  exit 1
fi

# 異常に大きい PR で取得コスト・出力サイズが際限なく伸びないよう MAX_COMMENTS で打ち切る
# （ページネーション自体は hasNextPage で必ず終端するため、上限はコストガード）。
# 打ち切り発生は出力の comments_truncated で消費側に伝える
MAX_COMMENTS=${MAX_COMMENTS:-500}
case "$MAX_COMMENTS" in
  '' | *[!0-9]*)
    printf 'invalid MAX_COMMENTS: %s\n' "$MAX_COMMENTS" >&2
    exit 1
    ;;
esac

# ページの蓄積はファイルで行う（シェル変数に持って --argjson で渡すと、ページごとに
# 全体を再パースする O(n^2) の無駄と、execve の引数上限（ARG_MAX）超過リスクがあるため）
comments_pages=$(mktemp)
trap 'rm -f "$comments_pages"' EXIT

printf '%s' "$gql" | jq -c '.data.repository.pullRequest.comments.nodes' > "$comments_pages"
comment_count=$(printf '%s' "$gql" | jq '.data.repository.pullRequest.comments.nodes | length')
has_next=$(printf '%s' "$gql" | jq -r '.data.repository.pullRequest.comments.pageInfo.hasNextPage')
cursor=$(printf '%s' "$gql" | jq -r '.data.repository.pullRequest.comments.pageInfo.endCursor')
while [ "$has_next" = "true" ] && [ "$comment_count" -lt "$MAX_COMMENTS" ]; do
  if ! page=$("$GH_BIN" api graphql -f query='
query($owner: String!, $name: String!, $number: Int!, $cursor: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      comments(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes { author { login __typename } body createdAt url }
      }
    }
  }
}' -f owner="$owner" -f name="$name" -F number="$pr_number" -f cursor="$cursor"); then
    printf 'failed to fetch PR comments page (GraphQL)\n' >&2
    exit 1
  fi
  printf '%s' "$page" | jq -c '.data.repository.pullRequest.comments.nodes' >> "$comments_pages"
  comment_count=$((comment_count + $(printf '%s' "$page" | jq '.data.repository.pullRequest.comments.nodes | length')))
  has_next=$(printf '%s' "$page" | jq -r '.data.repository.pullRequest.comments.pageInfo.hasNextPage')
  cursor=$(printf '%s' "$page" | jq -r '.data.repository.pullRequest.comments.pageInfo.endCursor')
done

# linked_issues: GitHub closing keyword 仕様に準拠した関連 Issue 検出
#   - 同リポ `#N` / クロスリポ `OWNER/REPO#N`、大文字小文字・コロン付き許容
#   - URL 形式・キーワードなしの素の `#N` は対象外（GitHub の自動 close 対象外に揃える）
# is_skill_comment: /review-response の投稿マーカー。引用返信（`> ` 付き）を誤検知しないよう先頭一致
jq -n \
  --arg repo "$repo" \
  --argjson pr "$pr_meta" \
  --argjson gql "$gql" \
  --slurpfile comment_pages "$comments_pages" \
  '
  def issue_refs:
    [match("\\b(close[sd]?|fix(es|ed)?|resolve[sd]?):?\\s+(?:(?<xrepo>[\\w.-]+/[\\w.-]+))?#(?<num>[0-9]+)"; "gi")
      | {
          repo: (.captures | map(select(.name == "xrepo"))[0].string),
          number: (.captures | map(select(.name == "num"))[0].string | tonumber)
        }]
    | unique;
  ($gql.data.repository.pullRequest) as $p
  | ($gql.data.viewer.login) as $current_user
  | ($comment_pages | add) as $comments
  | {
      repo: $repo,
      current_user: $current_user,
      is_own_pr: ($pr.author.login == $current_user),
      pr: {
        number: $pr.number,
        title: $pr.title,
        body: $pr.body,
        url: $pr.url,
        state: $pr.state,
        author: $pr.author.login,
        head_ref: $pr.headRefName,
        base_ref: $pr.baseRefName,
        head_oid: $pr.headRefOid
      },
      linked_issues: (($pr.body // "") | issue_refs),
      comments_total_count: $p.comments.totalCount,
      comments_truncated: ($p.comments.totalCount > ($comments | length)),
      comments: [$comments[] | {
        author: .author.login,
        author_type: (.author.__typename // null),
        body,
        created_at: .createdAt,
        url,
        is_skill_comment: (.body | startswith("<!-- review-response -->"))
      }],
      reviews_total_count: $p.reviews.totalCount,
      reviews_truncated: ($p.reviews.totalCount > ($p.reviews.nodes | length)),
      reviews: [$p.reviews.nodes[] | {
        author: .author.login,
        state,
        body,
        url,
        submitted_at: .submittedAt
      }],
      review_threads: [$p.reviewThreads.nodes[] | {
        id,
        is_resolved: .isResolved,
        is_outdated: .isOutdated,
        path,
        line,
        resolved_by: (.resolvedBy.login // null),
        comments: [.comments.nodes[] | {
          author: .author.login,
          body,
          created_at: .createdAt,
          url
        }]
      }]
    }'
