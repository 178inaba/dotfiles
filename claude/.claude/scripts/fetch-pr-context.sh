#!/bin/bash

# PR コンテキスト一括取得スクリプト（/review-response・/deep-review 共有。
# worktree-resolution の鮮度確認サブ手順も出力の pr.head_oid・is_own_pr・pr.head_ref を消費する）
#
# PR メタ情報・通常コメント・レビュー本文・レビュースレッドを 1 回で取得し、
# 正規化した JSON を <out-dir> 配下のファイルに出力する。3 種のコメントは GitHub 上で別管理のため、
# 個別取得だと取りこぼしが起きる — スーパーセットの一括取得をスクリプトで保証する。
#
# 使用方法: fetch-pr-context.sh <out-dir> [<pr-number>]
#   <out-dir>   コンテキストファイルの書き込み先ディレクトリ（既存であること。通常は scratchpad）
#   <pr-number> 省略時はカレント branch の PR を推論（失敗時は非ゼロ exit + stderr）
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）
#           MAX_COMMENTS — comments 取得の打ち切り上限（既定 500）。打ち切り発生時に引き上げて再実行する
#           MAX_THREADS — review_threads 取得の打ち切り上限（既定 300）。同上
#           MAX_THREAD_COMMENTS — 1 スレッドあたりのコメント取得上限（既定 200）。同上
#           いずれも初回ページは上限に関わらず 100 件取るため、実際の停止件数は上限を超えうる
#
# stdout は {"path": "<out-dir>/pr-context-<owner>@<repo>-<PR番号>.json"} のみ。
# コンテキスト本体は path のファイルに書く。ファイル名の一意化（repo・PR 番号の埋め込み）を
# スクリプト側で保証する理由:
#   - 並列サブエージェントは同一セッションの scratchpad を共有するため、呼び出し側のプロンプト
#     指示に命名を委ねると固定名での衝突が起きる（別リポジトリの PR コンテキストを読む事故が実際に発生）
#   - 番号省略の推論経路では、呼び出し側は実行前に番号を知り得ない
# 書き込みは一時ファイル経由の atomic rename（途中失敗で部分 JSON を残さない）。
# 出力が数百 KB に達する PR があるため、本体はモデルのリダイレクト組み立てを経由させず直接書く。
#
# 出力ファイル JSON の契約（正はここ。各 SKILL.md には自スキルが使うフィールドの解釈のみ書く）:
#   repo              owner/name 形式
#   current_user      実行ユーザーの login
#   is_own_pr         PR 作成者 == current_user
#   pr                number / title / body / url / state / author / head_ref / base_ref / head_oid
#   head_committed_at pr.head_oid が指すコミットの committedDate（ISO-8601 UTC）。
#                     review_threads[].awaiting_my_confirmation の時刻条件の比較相手。
#                     近似であることの既知の限界（どちらの方向にも外れる）:
#                       - 偽陽性: コミット日時はコミッタのマシン時刻由来なので時計のずれで
#                         早まりうる。また該当スレッドの path に無関係なコミットでも成立する。
#                         結果は確認返信が余分に出ることで、resolve 自体は SKILL.md が
#                         差分確認を別途要求するため誤 resolve には至らない
#                       - 偽陰性: committedDate は push 時刻ではない。作者がローカルで
#                         コミット → こちらが返信 → 作者が push の順序だと committedDate が
#                         返信より古く、コードが動いているのにスレッドを拾えない（次の
#                         コミットが来るまで復帰の契機がない silent な取りこぼし）。
#                         push 時刻を持つフィールド（Commit.pushedDate）は現在ほぼ null を
#                         返すため代替にできず、近似のまま許容している
#                     絞り込みの改善余地: 「該当スレッドの path に触れたコミットの最新日時」に
#                     すれば偽陽性は減る（偽陰性は解消しない）。現状は PR 全体の head 日時
#   linked_issues[]   PR 本文の closing keyword から検出した {repo, number}（repo: null は同リポ）。
#                     URL 形式・キーワードなしの素の #N は対象外（GitHub の自動 close 対象に揃える）
#   comments_total_count / comments_truncated
#                     PR 上の通常コメント総数と、MAX_COMMENTS 打ち切りの発生フラグ。
#                     どちらも review_threads[] 要素内に同名で存在する（別物）。
#                     トップレベル = 通常コメント、要素内 = 当該スレッドのコメント
#   comments[]        通常コメント {author, author_type, body, created_at, url, is_skill_comment}。
#                     ページネーションで全量取得（MAX_COMMENTS 件で打ち切り）。author_type は
#                     GraphQL Actor の __typename（"User" / "Bot" 等）で、CI bot の機械的判別に使う
#   reviews_total_count / reviews_truncated
#                     レビュー総数と、取得窓（最新50件）からの欠落発生フラグ
#   reviews[]         レビュー本文 {author, state, body, url, submitted_at}
#   threads_total_count / threads_truncated
#                     レビュースレッド総数と、MAX_THREADS 打ち切りの発生フラグ
#   review_threads[]  {id, is_resolved, is_outdated, path, line, resolved_by, comments[],
#                      comments_total_count, comments_truncated, last_comment,
#                      waiting_for_response, awaiting_my_confirmation}。
#                     総数フィールドは打ち切り時の引き上げ先（prepare-review.sh の
#                     自動再実行が消費する）。
#                     comments[] は昇順（古い順）で MAX_THREAD_COMMENTS まで全量取得。
#                     last_comment は当該スレッドの最新コメント {author, body, created_at, url}
#                     （コメント 0 件なら null）。comments[] とは別クエリ（comments(last: 1)）で
#                     取るため、打ち切り時は comments[] に含まれない要素になる —
#                     消費側は「comments[] の最終要素」と同一視しないこと。
#                     前方ページングは両端（最初の指摘と最新の返信）を残すための選択で、
#                     欠けるのは中間。末尾判定は必ず last_comment を使う。
#                     waiting_for_response は「未解決 かつ 末尾が自分の返信 かつ 自分の PR」。
#                     レビュアー側の PR では「末尾が自分」の意味が反転する（相手の応答待ちで
#                     あって自分の対応漏れではない）ため is_own_pr で絞る。
#                     awaiting_my_confirmation は「未解決 かつ 起点が自分 かつ
#                     （末尾が自分でない または 自分の最終コメント以降に head が動いた）」
#                     = 自分が出した指摘の解消判定（返信・resolve）のボールが自分に
#                     戻っているスレッド（/deep-review が消費）。「起点が自分」を条件に
#                     するのは、resolve が指摘者の権限行為であり、他レビュアーのスレッドの
#                     解消判定を代行しないため。起点判定は comments[0]（昇順ページングのため
#                     打ち切りが起きても先頭要素は必ず残る）。waiting_for_response と違い
#                     is_own_pr で絞らないのは、「起点が自分 かつ 末尾が相手」が PR の所有者に
#                     依らず「ボールが自分にある」を意味し、意味の反転が起きないため。
#                     後半の時刻条件は「作者がスレッドに返信せずコミットだけ push した」
#                     ケース（GitHub では一般的）を拾うためにある — 末尾条件だけだと自分の指摘が
#                     末尾のまま残り、無言で修正されたスレッドが永久に未解決で積み上がる。
#                     同時に冪等性も時刻条件が担っており（返信すると自分の最終コメントが
#                     head より新しくなってフラグが落ち、その後さらにコミットが来ると再び立つ）、
#                     返信本文に識別マーカーは要らない。head_committed_at が null のときは
#                     時刻条件を成立させない（末尾条件だけの判定へ縮退し、余分な返信を出さない）。
#                     この null ガードは現在の比較向き（created_at < head）では jq の順序
#                     （null が最小）と同値で冗長だが、向きを反転する改修が入ると
#                     "文字列 > null" が true になり、head 日時が取れないときに全スレッドが
#                     true へ化けて毎回二重返信する — dead code ではないので削除しないこと

set -u

GH_BIN=${GH_BIN:-gh}

. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/warnings-lib.sh"

out_dir=${1:-}
[ -n "$out_dir" ] || fatal 'usage: fetch-pr-context.sh <out-dir> [<pr-number>]'
[ -d "$out_dir" ] || fatal "output directory not found: $out_dir"

pr_number=${2:-}
if [ -n "$pr_number" ]; then
  case "$pr_number" in
    *[!0-9]*) fatal "invalid pr number: $pr_number" ;;
  esac
fi

command -v jq >/dev/null 2>&1 || fatal 'jq is required'

if ! repo=$("$GH_BIN" repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null) || [ -z "$repo" ]; then
  fatal 'failed to resolve repository (gh repo view)'
fi
owner=${repo%%/*}
name=${repo#*/}

pr_fields="number,title,body,url,state,author,headRefName,baseRefName,headRefOid"
if [ -z "$pr_number" ]; then
  # カレント branch からの推論と meta 取得を 1 回の呼び出しで済ませる
  if ! pr_meta=$("$GH_BIN" pr view --json "$pr_fields" 2>/dev/null) || [ -z "$pr_meta" ]; then
    fatal 'could not infer PR from current branch; specify <pr-number> explicitly'
  fi
  pr_number=$(printf '%s' "$pr_meta" | jq -r '.number')
else
  pr_meta=$("$GH_BIN" pr view "$pr_number" --json "$pr_fields" -R "$repo") \
    || fatal "failed to fetch PR #$pr_number"
fi

# owner と repo の区切りは両者の名前に使えない「@」にする（hyphen 区切りだと
# a-b/c と a/b-c の同番号 PR が同名に潰れ、一意性保証に穴が開くため）
out_file="$out_dir/pr-context-${owner}@${name}-${pr_number}.json"

# スレッドのノード選択は初回ページと継続ページで完全に同一でなければならない
# （ズレると 2 ページ目以降のスレッドだけコメント・last_comment が空になる）。
# 両方の query へ同じ変数を差し込むことで構造的に揃える。
# tail は comments と同じフィールドの別引数エイリアス。cap による打ち切りは最新側で起きるため、
# ページネーションだけでは末尾を保証できず、末尾判定（review-response の反応待ち分類）が
# silent に誤る — tail を併用して打ち切り時も last_comment を正確に保つ
thread_node_fields='
  id
  isResolved
  isOutdated
  path
  line
  resolvedBy { login }
  comments(first: 100) {
    totalCount
    pageInfo { hasNextPage endCursor }
    nodes { author { login } body createdAt url }
  }
  tail: comments(last: 1) { nodes { author { login } body createdAt url } }
'

# reviews は提出日時昇順で返るため last:50 で最新側を取る（first だと CI 通知・ボットレビュー等で
# 50 件を超えた際に未対応の修正依頼を取りこぼす）。comments は固定ウィンドウだと CI bot の
# sticky コメントが人間のコメントを窓外へ押し出して黙って欠落するため（実 PR で 97 件中
# 人間コメント 3 件を含む 47 件が欠落した事例あり）、後段でページネーションして全量取得する。
# reviewThreads とスレッド内コメントも同じ理由で全量取得する（こちらは昇順のため
# 打ち切ると最新側が落ち、末尾判定が壊れる）
# head commit の committedDate も取る。awaiting_my_confirmation の「自分の最終コメント以降に
# コードが動いたか」の判定に使う（is_outdated は真偽値で「いつ動いたか」を持たず、返信後も
# true のままになるため冪等性の判定に使えない）。pr_meta の headRefOid で対象コミットを pin し、
# 鮮度確認が見ている head と同じコミットを指すようにする
head_oid_for_query=$(printf '%s' "$pr_meta" | jq -r '.headRefOid')
if ! gql=$("$GH_BIN" api graphql -f query='
query($owner: String!, $name: String!, $number: Int!, $headOid: GitObjectID!) {
  viewer { login }
  repository(owner: $owner, name: $name) {
    headCommit: object(oid: $headOid) { ... on Commit { committedDate } }
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
        totalCount
        pageInfo { hasNextPage endCursor }
        nodes {'"$thread_node_fields"'}
      }
    }
  }
}' -f owner="$owner" -f name="$name" -F number="$pr_number" -f headOid="$head_oid_for_query"); then
  fatal 'failed to fetch PR comments/reviews/threads (GraphQL)'
fi

# 異常に大きい PR で取得コスト・出力サイズが際限なく伸びないよう上限で打ち切る
# （ページネーション自体は hasNextPage で必ず終端するため、上限はコストガード）。
# 打ち切り発生は出力の *_truncated フラグで消費側に伝える。
# MAX_THREAD_COMMENTS だけ 1 スレッドあたりの上限にしているのは、全スレッド合計にすると
# 40 スレッド × 5 コメントで現実的に到達し、以降のスレッドの議論経緯が欠けるため
require_uint() {
  case "$2" in
    '' | *[!0-9]*) fatal "invalid $1: $2" ;;
  esac
}
MAX_COMMENTS=${MAX_COMMENTS:-500}
MAX_THREADS=${MAX_THREADS:-300}
MAX_THREAD_COMMENTS=${MAX_THREAD_COMMENTS:-200}
require_uint MAX_COMMENTS "$MAX_COMMENTS"
require_uint MAX_THREADS "$MAX_THREADS"
require_uint MAX_THREAD_COMMENTS "$MAX_THREAD_COMMENTS"

# ページの蓄積はファイルで行う（シェル変数に持って --argjson で渡すと、ページごとに
# 全体を再パースする O(n^2) の無駄と、execve の引数上限（ARG_MAX）超過リスクがあるため）
comments_pages=$(mktemp)
threads_pages=$(mktemp)
thread_comment_pages=$(mktemp)
tmp_out=''
trap 'rm -f "$comments_pages" "$threads_pages" "$thread_comment_pages" "$tmp_out"' EXIT
# atomic rename を保証するため、一時ファイルは out_dir と同一ファイルシステム上に作る
tmp_out=$(mktemp "$out_dir/.pr-context.XXXXXX") || exit 1

# pullRequest 直下の接続（comments / reviewThreads）を全量取得し、1 ページ 1 行の
# JSON 配列として蓄積ファイルへ書く。初回ページは既に $gql にあるので継続分だけ取りに行く。
# 引数: <接続名> <上限> <蓄積ファイル> <継続クエリ> <失敗時メッセージ>
paginate_pr_connection() {
  local conn=$1 max=$2 out=$3 query=$4 err_msg=$5
  local path=".data.repository.pullRequest.$conn"
  local count has_next cursor page

  printf '%s' "$gql" | jq -c "$path.nodes" > "$out"
  count=$(printf '%s' "$gql" | jq "$path.nodes | length")
  has_next=$(printf '%s' "$gql" | jq -r "$path.pageInfo.hasNextPage")
  cursor=$(printf '%s' "$gql" | jq -r "$path.pageInfo.endCursor")
  while [ "$has_next" = "true" ] && [ "$count" -lt "$max" ]; do
    if ! page=$("$GH_BIN" api graphql -f query="$query" \
      -f owner="$owner" -f name="$name" -F number="$pr_number" -f cursor="$cursor"); then
      fatal "$err_msg"
    fi
    printf '%s' "$page" | jq -c "$path.nodes" >> "$out"
    count=$((count + $(printf '%s' "$page" | jq "$path.nodes | length")))
    has_next=$(printf '%s' "$page" | jq -r "$path.pageInfo.hasNextPage")
    cursor=$(printf '%s' "$page" | jq -r "$path.pageInfo.endCursor")
  done
}

paginate_pr_connection comments "$MAX_COMMENTS" "$comments_pages" '
query($owner: String!, $name: String!, $number: Int!, $cursor: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      comments(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes { author { login __typename } body createdAt url }
      }
    }
  }
}' 'failed to fetch PR comments page (GraphQL)'

paginate_pr_connection reviewThreads "$MAX_THREADS" "$threads_pages" '
query($owner: String!, $name: String!, $number: Int!, $cursor: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviewThreads(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes {'"$thread_node_fields"'}
      }
    }
  }
}' 'failed to fetch review threads page (GraphQL)'

# スレッド内コメントの続きは node(id:) で 1 スレッドずつ辿る。取得したページは
# {thread_id, nodes} で記録し、後段の jq がスレッド ID で束ね直す（通常コメントのような
# 単一ストリームではないため、フラットな append では取り違える）。
# gh が stdin を消費してループを壊さないよう、リストは fd 3 から読む
while IFS=$'\t' read -r tc_id tc_cursor tc_count <&3; do
  tc_has_next=true
  while [ "$tc_has_next" = "true" ] && [ "$tc_count" -lt "$MAX_THREAD_COMMENTS" ]; do
    if ! tc_page=$("$GH_BIN" api graphql -f query='
query($threadId: ID!, $cursor: String!) {
  node(id: $threadId) {
    ... on PullRequestReviewThread {
      comments(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes { author { login } body createdAt url }
      }
    }
  }
}' -f threadId="$tc_id" -f cursor="$tc_cursor"); then
      fatal 'failed to fetch review thread comments page (GraphQL)'
    fi
    printf '%s' "$tc_page" | jq -c --arg tid "$tc_id" '{thread_id: $tid, nodes: .data.node.comments.nodes}' >> "$thread_comment_pages"
    tc_count=$((tc_count + $(printf '%s' "$tc_page" | jq '.data.node.comments.nodes | length')))
    tc_has_next=$(printf '%s' "$tc_page" | jq -r '.data.node.comments.pageInfo.hasNextPage')
    tc_cursor=$(printf '%s' "$tc_page" | jq -r '.data.node.comments.pageInfo.endCursor')
  done
done 3< <(jq -r '.[] | select(.comments.pageInfo.hasNextPage) | [.id, .comments.pageInfo.endCursor, (.comments.nodes | length)] | @tsv' "$threads_pages")

# linked_issues: GitHub closing keyword 仕様に準拠した関連 Issue 検出
#   - 同リポ `#N` / クロスリポ `OWNER/REPO#N`、大文字小文字・コロン付き許容
#   - URL 形式・キーワードなしの素の `#N` は対象外（GitHub の自動 close 対象外に揃える）
# is_skill_comment: /review-response の投稿マーカー。引用返信（`> ` 付き）を誤検知しないよう先頭一致
if ! jq -n \
  --arg repo "$repo" \
  --argjson pr "$pr_meta" \
  --argjson gql "$gql" \
  --slurpfile comment_pages "$comments_pages" \
  --slurpfile thread_pages "$threads_pages" \
  --slurpfile thread_comment_pages "$thread_comment_pages" \
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
  | ($pr.author.login == $current_user) as $is_own_pr
  # ISO-8601 の UTC（Z 終端）同士なので辞書順比較が時刻順比較になる
  | ($gql.data.repository.headCommit.committedDate // null) as $head_committed_at
  | ($comment_pages | add) as $comments
  | ($thread_pages | add) as $threads
  | ($thread_comment_pages
      | group_by(.thread_id)
      | map({key: .[0].thread_id, value: (map(.nodes // []) | add // [])})
      | from_entries) as $extra_thread_comments
  | {
      repo: $repo,
      current_user: $current_user,
      is_own_pr: $is_own_pr,
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
      head_committed_at: $head_committed_at,
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
      threads_total_count: $p.reviewThreads.totalCount,
      threads_truncated: ($p.reviewThreads.totalCount > ($threads | length)),
      review_threads: [$threads[]
        | . as $t
        | (($t.comments.nodes // []) + ($extra_thread_comments[$t.id] // [])) as $thread_comments
        | ($t.tail.nodes[0] | if . == null then null else {
            author: .author.login,
            body,
            created_at: .createdAt,
            url
          } end) as $last_comment
        | {
            id: $t.id,
            is_resolved: $t.isResolved,
            is_outdated: $t.isOutdated,
            path: $t.path,
            line: $t.line,
            resolved_by: ($t.resolvedBy.login // null),
            comments_total_count: $t.comments.totalCount,
            comments_truncated: ($t.comments.totalCount > ($thread_comments | length)),
            comments: [$thread_comments[] | {
              author: .author.login,
              body,
              created_at: .createdAt,
              url
            }],
            last_comment: $last_comment,
            waiting_for_response: ($is_own_pr
              and ($t.isResolved | not)
              and $last_comment != null
              and $last_comment.author == $current_user),
            awaiting_my_confirmation: (($t.isResolved | not)
              and (($thread_comments[0].author.login // null) == $current_user)
              and $last_comment != null
              and ($last_comment.author != $current_user
                or ($head_committed_at != null
                  and $last_comment.created_at < $head_committed_at)))
          }]
    }' > "$tmp_out"; then
  fatal 'failed to build output JSON'
fi

mv "$tmp_out" "$out_file" || exit 1
jq -n --arg path "$out_file" '{path: $path}'
