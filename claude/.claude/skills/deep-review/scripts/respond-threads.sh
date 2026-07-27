#!/bin/bash

# /deep-review のレビュースレッド返信・解決スクリプト
#
# 自分が過去に出した指摘スレッドについて、対応確認の返信を投稿し、解消済みのものを
# resolve する。resolve は作者の未対応リストからスレッドを消す片道の操作なので、
# 「誰のスレッドを触ってよいか」の判定はモデルの手順文に委ねず、ここで構造的に強制する:
#   1. 入力の全エントリを先に検証し、1 件でも不適格なら mutation を 1 度も打たずに停止する
#      （適格分だけ先に投稿する部分適用を作らない）
#   2. 適格条件は pr-context の awaiting_my_confirmation == true のみ。判定の実体は
#      fetch-pr-context.sh が所有する（他レビュアーのスレッドの解消判定を代行しない・
#      解決済みを触らない）
#   3. 同一 id の重複指定を弾く（二重返信になる）
#   4. run をまたぐ再送も弾く。適格性は fetch 時点で凍結された context のフラグで判定するため、
#      同じ入力ファイルを再実行すると素通りして二重投稿になる。投稿済み id を
#      <threads-file>.posted に記録し、既記録の id を含む入力を拒否する
#   5. 差分を読んだ時点から head が動いていないことを確認する（post-review.sh と対）。
#      resolve の根拠は「差分で解消を確認した」ことなので、head が動いていると
#      取り消された修正に対して resolve しうる
#
# body は省略可で、省略時は「返信せず resolve のみ」を意味する。用途は (a) 前回の自分の返信から
# 判定が変わっていない場合の再返信の抑止、(b) write 権限がないリポジトリで resolve だけが
# 恒久的に失敗する状態からの回復（返信を重ねずに resolve を再試行する）。
#
# 失敗の切り分け:
#   - 返信失敗は停止する（返信が主目的の投稿物であり、黙って落ちると確認結果が消える）。
#     再実行で二重返信しないよう、投稿済み・未処理を stderr で切り分けて報告する
#     （切り分けは posted_log 基準。url 欠落の停止経路では返信が既に成立しているため）
#   - resolve 失敗は縮退する（fork PR や write 権限なしで起きる。返信は残っているため
#     レビュー運用全体を止める必要がない）。出力の resolve_failed / warnings で報告する
#
# mutation をスレッドごとに逐次実行するのは、GraphQL の alias 一括投稿だと resolve の
# 権限エラーが同一リクエスト内の返信ごと巻き込み、どこまで適用されたかの切り分けが
# 効かなくなるため（対象スレッド数は実運用で 1 桁のため逐次でコストに問題はない）。
#
# 使用方法: respond-threads.sh <pr-context.json> <threads-file>
#   <pr-context.json> fetch-pr-context.sh の出力ファイル（review_threads[] を読む）
#   <threads-file>    入力契約は SKILL.md の「threads_path に書く JSON の入力契約」を参照。
#                     パスは prepare-review.sh の threads_path を使う（末尾が
#                     <owner>@<repo>-<PR番号>.json であることを検証する — 下記コメント参照）
#
# 出力契約: SKILL.md の「respond-threads.sh の出力 JSON の契約」を参照
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

# shellcheck source=input-name-lib.sh
. "$(dirname "$0")/input-name-lib.sh"

context_file=${1:-}
threads_file=${2:-}
{ [ -n "$context_file" ] && [ -n "$threads_file" ]; } \
  || fatal 'usage: respond-threads.sh <pr-context.json> <threads-file>'
[ -f "$context_file" ] || fatal "pr context file not found: $context_file"
[ -f "$threads_file" ] || fatal "threads file not found: $threads_file"

jq -e '.review_threads | type == "array"' "$context_file" >/dev/null 2>&1 \
  || fatal "review_threads missing in $context_file"
head_oid=$(jq -er '.pr.head_oid' "$context_file" 2>/dev/null) || fatal "pr.head_oid missing in $context_file"

# 適格性検証でも取り違えは止まるが（スレッド ID は PR を跨いで一意）、そちらのエラーは
# 「対象が確認待ちでない」と読めてしまい原因を誤診させるため、名前の側で先に落とす
# （規約と理由は input-name-lib.sh のヘッダー参照）
require_pr_bound_filename "$threads_file" threads_path "$context_file"

# post-review.sh と同じ投稿前検証。resolve の判断根拠は「差分で解消を確認した」ことなので、
# 差分を読んだ時点から head が動いていると、取り消された修正に対して resolve しうる
git rev-parse --git-dir >/dev/null 2>&1 || fatal 'not inside a git repository'
local_head=$(git rev-parse HEAD)
[ "$local_head" = "$head_oid" ] \
  || fatal "local HEAD ($local_head) differs from PR head ($head_oid); rerun the freshness check before replying or resolving"

# --- 入力契約の検証 ---
jq -e '.threads | type == "array"' "$threads_file" >/dev/null 2>&1 \
  || fatal "threads must be an array in $threads_file"
jq -e '[.threads[] | select((.id | type) != "string" or (.resolve | type) != "boolean"
        or (has("body") and (.body | type) != "string"))] | length == 0' \
  "$threads_file" >/dev/null 2>&1 \
  || fatal "threads must be an array of {id: string, resolve: boolean, body?: string} in $threads_file"

blank=$(jq -r '[.threads[] | select(has("body") and (.body | gsub("\\s"; "") == "")) | .id] | join(", ")' "$threads_file")
[ -z "$blank" ] || fatal "reply body is present but blank for thread(s): $blank (omit body entirely to resolve without replying)"

# body 省略は「返信せず resolve のみ」の意味なので、resolve: false との組み合わせは何もしない指定になる
noop=$(jq -r '[.threads[] | select((has("body") | not) and .resolve == false) | .id] | join(", ")' "$threads_file")
[ -z "$noop" ] || fatal "thread(s) with neither a reply body nor resolve: true do nothing: $noop"

dupes=$(jq -r '[.threads[].id] | group_by(.) | map(select(length > 1) | .[0]) | join(", ")' "$threads_file")
[ -z "$dupes" ] || fatal "duplicate thread id(s) would post duplicate replies: $dupes"

# run をまたぐ二重返信の拒否。返信失敗時の再実行はこのスクリプトが案内する正規フローであり、
# 適格性は fetch 時点で凍結された context のフラグで判定されるため、同じ入力を再実行すると
# 素通りして二重投稿になる。投稿済み id を threads_file の sidecar に記録して構造的に止める
# （threads_file 自体が PR 束縛済みの名前なので sidecar の一意性も自動的に担保される）
posted_log="$threads_file.posted"

# posted_log と入力の id の積集合／差集合。停止時の「投稿済み／未処理」の切り分けにも使う。
# 起動時点で積集合が空であることを検証済みなので、以降に積集合へ入る id はこの run の投稿分だけ
input_ids_in_posted_log() {
  [ -f "$posted_log" ] || return 0
  jq -rR --slurpfile ids <(jq -c '[.threads[].id]' "$threads_file") \
    'select(. != "") | select(IN($ids[0][]))' "$posted_log" | sort -u | paste -sd, -
}
input_ids_not_in_posted_log() {
  local done_csv=$1
  jq -r --arg done "$done_csv" \
    '($done | split(",") | map(select(. != ""))) as $d
     | [.threads[].id | select(IN($d[]) | not)] | join(",")' "$threads_file"
}

resent=$(input_ids_in_posted_log)
[ -z "$resent" ] || fatal "thread(s) already replied to in an earlier run of this file: $resent
remove them from $threads_file (resending would post duplicate replies); the record is in $posted_log"

# 適格性は pr-context 側のフラグに委ねる（判定の実体は fetch-pr-context.sh が所有）
ineligible=$(jq -r --slurpfile ctx "$context_file" '
  ($ctx[0].review_threads
    | map(select(.awaiting_my_confirmation == true) | .id)) as $eligible
  | [.threads[].id | select(IN($eligible[]) | not)]
  | join(", ")' "$threads_file")
if [ -n "$ineligible" ]; then
  fatal "thread(s) not awaiting our confirmation (not opened by us, already resolved, or we replied last): $ineligible
resolve/reply is limited to threads flagged awaiting_my_confirmation in $context_file"
fi

count=$(jq -r '.threads | length' "$threads_file")
if [ "$count" -eq 0 ]; then
  jq -n '{replied: [], resolved: [], resolve_failed: [], warnings: []}'
  exit 0
fi

# --- mutation ---
reply_mutation='
mutation($threadId: ID!, $body: String!) {
  addPullRequestReviewThreadReply(input: {pullRequestReviewThreadId: $threadId, body: $body}) {
    comment { url }
  }
}'
resolve_mutation='
mutation($threadId: ID!) {
  resolveReviewThread(input: {threadId: $threadId}) {
    thread { isResolved }
  }
}'

replied=$(mktemp)
resolved=$(mktemp)
resolve_failed=$(mktemp)
# stderr は stdout と混ぜない（gh の警告出力が混入すると成功時の JSON パースが壊れる）
gh_err=$(mktemp)
trap 'rm -f "$replied" "$resolved" "$resolve_failed" "$gh_err"' EXIT
: > "$replied"; : > "$resolved"; : > "$resolve_failed"

# 返信が1件でも失敗したら停止する。停止時は投稿済み・未処理を切り分けて報告する
# （再実行で二重返信しないための情報。sidecar への記録は投稿の直後に行うので、
# この報告を読み飛ばしても構造的なガードは効く）
abort_with_progress() {
  local tid=$1 reason=$2 done_csv
  # 投稿済み判定は posted_log 基準にする。url 欠落の停止経路では当該スレッドの返信が
  # 既に成立しているため、$replied（url 確認後に追記）を基準にすると投稿済みの
  # スレッドを「未処理」に混ぜて報告してしまう
  done_csv=$(input_ids_in_posted_log)
  {
    printf 'failed to reply to thread %s:\n%s\n' "$tid" "$reason"
    [ -z "$done_csv" ] \
      || printf 'already replied (do NOT resend on retry): %s\n' "$done_csv"
    printf 'not processed: %s\n' "$(input_ids_not_in_posted_log "$done_csv")"
  } >&2
  exit 1
}

i=0
while [ "$i" -lt "$count" ]; do
  entry=$(jq -c --argjson i "$i" '.threads[$i]' "$threads_file")
  tid=$(printf '%s' "$entry" | jq -r '.id')
  has_body=$(printf '%s' "$entry" | jq -r 'has("body")')
  do_resolve=$(printf '%s' "$entry" | jq -r '.resolve')

  if [ "$has_body" = "true" ]; then
    body=$(printf '%s' "$entry" | jq -r '.body')
    if ! reply_out=$("$GH_BIN" api graphql -f query="$reply_mutation" -f threadId="$tid" -f body="$body" 2>"$gh_err"); then
      abort_with_progress "$tid" "$(cat "$gh_err")"
    fi
    # 投稿が成立した事実を、以降の検証より先に永続化する（url 欠落等でこの後に失敗しても
    # 次回実行が二重返信を拒否できるようにするため）
    printf '%s\n' "$tid" >> "$posted_log"

    url=$(printf '%s' "$reply_out" | jq -r '.data.addPullRequestReviewThreadReply.comment.url // empty')
    [ -n "$url" ] \
      || abort_with_progress "$tid" 'reply was posted but comment url is missing in the API response'
    jq -nc --arg id "$tid" --arg url "$url" '{id: $id, url: $url}' >> "$replied"
  fi

  if [ "$do_resolve" = "true" ]; then
    if "$GH_BIN" api graphql -f query="$resolve_mutation" -f threadId="$tid" >/dev/null 2>"$gh_err"; then
      jq -nc --arg id "$tid" '$id' >> "$resolved"
    else
      jq -nc --arg id "$tid" --arg error "$(cat "$gh_err")" '{id: $id, error: $error}' >> "$resolve_failed"
    fi
  fi

  i=$((i + 1))
done

jq -nc \
  --slurpfile replied "$replied" \
  --slurpfile resolved "$resolved" \
  --slurpfile resolve_failed "$resolve_failed" \
  '{
     replied: $replied,
     resolved: $resolved,
     resolve_failed: $resolve_failed,
     warnings: (if ($resolve_failed | length) > 0 then
       ["replied but could not resolve \($resolve_failed | length) thread(s) (write access to the repository is required to resolve): \($resolve_failed | map(.id) | join(", ")). The replies are posted; resolve them manually or ask the author to."]
     else [] end)
   }'
