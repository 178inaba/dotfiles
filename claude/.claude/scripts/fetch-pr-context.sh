#!/bin/bash

# PR コンテキスト一括取得スクリプト（/review-response・/deep-review 共有）
#
# PR メタ情報・通常コメント・レビュー本文・レビュースレッドを 1 回で取得し、
# 正規化した JSON を stdout に出力する。3 種のコメントは GitHub 上で別管理のため、
# 個別取得だと取りこぼしが起きる — スーパーセットの一括取得をスクリプトで保証する。
#
# 使用方法: fetch-pr-context.sh [<pr-number>]
#   <pr-number> 省略時はカレント branch の PR を推論（失敗時は非ゼロ exit + stderr）
# 出力契約: 各スキルの SKILL.md を参照
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）

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

if [ -z "$pr_number" ]; then
  if ! pr_number=$("$GH_BIN" pr view --json number -q .number 2>/dev/null) || [ -z "$pr_number" ]; then
    printf 'カレント branch の PR を特定できませんでした。<pr-number> を明示指定してください\n' >&2
    exit 1
  fi
fi

if ! pr_meta=$("$GH_BIN" pr view "$pr_number" \
  --json number,title,body,url,state,author,headRefName,baseRefName,headRefOid -R "$repo"); then
  printf 'failed to fetch PR #%s\n' "$pr_number" >&2
  exit 1
fi

if ! current_user=$("$GH_BIN" api user --jq .login) || [ -z "$current_user" ]; then
  printf 'failed to fetch current user\n' >&2
  exit 1
fi

# comments は作成日時昇順で返るため last:50 で最新側を取る
# （first だと CI 通知等で 50 件を超えた際に未対応の修正依頼を取りこぼす）
if ! gql=$("$GH_BIN" api graphql -f query='
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      comments(last: 50) {
        nodes { author { login } body createdAt url }
      }
      reviews(first: 50) {
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
}' -F owner="$owner" -F name="$name" -F number="$pr_number"); then
  printf 'failed to fetch PR comments/reviews/threads (GraphQL)\n' >&2
  exit 1
fi

# linked_issues: GitHub closing keyword 仕様に準拠した関連 Issue 検出
#   - 同リポ `#N` / クロスリポ `OWNER/REPO#N`、大文字小文字・コロン付き許容
#   - URL 形式・キーワードなしの素の `#N` は対象外（GitHub の自動 close 対象外に揃える）
# is_skill_comment: /review-response の投稿マーカー。引用返信（`> ` 付き）を誤検知しないよう先頭一致
jq -n \
  --arg repo "$repo" \
  --arg current_user "$current_user" \
  --argjson pr "$pr_meta" \
  --argjson gql "$gql" \
  '
  def issue_refs:
    [match("\\b(close[sd]?|fix(es|ed)?|resolve[sd]?):?\\s+(?:(?<xrepo>[\\w.-]+/[\\w.-]+))?#(?<num>[0-9]+)"; "gi")
      | {
          repo: (.captures | map(select(.name == "xrepo"))[0].string),
          number: (.captures | map(select(.name == "num"))[0].string | tonumber)
        }]
    | unique;
  ($gql.data.repository.pullRequest) as $p
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
      comments: [$p.comments.nodes[] | {
        author: .author.login,
        body,
        created_at: .createdAt,
        url,
        is_skill_comment: (.body | startswith("<!-- review-response -->"))
      }],
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
