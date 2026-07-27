#!/bin/bash

# /deep-review のレビュースレッド返信・解決スクリプト
#
# 自分が過去に出した指摘スレッドについて、対応確認の返信を投稿し、解消済みのものを
# resolve する。resolve は作者の未対応リストからスレッドを消す片道の操作なので、
# 「誰のスレッドを触ってよいか」の判定はモデルの手順文に委ねず、ここで構造的に強制する:
#   1. 入力の全エントリを先に検証し、1 件でも不適格なら mutation を 1 度も打たずに停止する
#      （適格分だけ先に投稿する部分適用を作らない）
#   2. 適格条件は pr-context の awaiting_my_confirmation == true のみ。
#      「未解決 かつ 起点が自分 かつ 末尾が自分でない」の実体は fetch-pr-context.sh が所有する
#      （他レビュアーのスレッドの解消判定を代行しない・解決済みを触らない・二重返信しない）
#   3. 同一 id の重複指定も弾く（二重返信になる）
#
# 失敗の切り分け:
#   - 返信失敗は停止する（返信が主目的の投稿物であり、黙って落ちると確認結果が消える）。
#     再実行で二重返信しないよう、投稿済み・未処理を stderr で切り分けて報告する
#   - resolve 失敗は縮退する（fork PR や write 権限なしで起きる。返信は残っているため
#     レビュー運用全体を止める必要がない）。出力の resolve_failed / warnings で報告する
#
# mutation をスレッドごとに逐次実行するのは、GraphQL の alias 一括投稿だと resolve の
# 権限エラーが同一リクエスト内の返信ごと巻き込み、どこまで適用されたかの切り分けが
# 効かなくなるため（対象スレッド数は実運用で 1 桁のため逐次でコストに問題はない）。
#
# 使用方法: respond-threads.sh <pr-context.json> <threads.json>
#   <pr-context.json> fetch-pr-context.sh の出力ファイル（review_threads[] を読む）
#   <threads.json>    入力契約は SKILL.md の「threads.json の入力契約」を参照
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

context_file=${1:-}
threads_file=${2:-}
{ [ -n "$context_file" ] && [ -n "$threads_file" ]; } \
  || fatal 'usage: respond-threads.sh <pr-context.json> <threads.json>'
[ -f "$context_file" ] || fatal "pr context file not found: $context_file"
[ -f "$threads_file" ] || fatal "threads file not found: $threads_file"

jq -e '.review_threads | type == "array"' "$context_file" >/dev/null 2>&1 \
  || fatal "review_threads missing in $context_file"

# --- 入力契約の検証 ---
jq -e '.threads | type == "array"' "$threads_file" >/dev/null 2>&1 \
  || fatal "threads must be an array in $threads_file"
jq -e '[.threads[] | select((.id | type) != "string" or (.body | type) != "string" or (.resolve | type) != "boolean")] | length == 0' \
  "$threads_file" >/dev/null 2>&1 \
  || fatal "threads must be an array of {id: string, body: string, resolve: boolean} in $threads_file"

blank=$(jq -r '[.threads[] | select(.body | gsub("\\s"; "") == "") | .id] | join(", ")' "$threads_file")
[ -z "$blank" ] || fatal "reply body is blank for thread(s): $blank"

dupes=$(jq -r '[.threads[].id] | group_by(.) | map(select(length > 1) | .[0]) | join(", ")' "$threads_file")
[ -z "$dupes" ] || fatal "duplicate thread id(s) would post duplicate replies: $dupes"

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

# 未処理スレッドの報告のため、失敗時点以降の id を残す
i=0
while [ "$i" -lt "$count" ]; do
  entry=$(jq -c --argjson i "$i" '.threads[$i]' "$threads_file")
  tid=$(printf '%s' "$entry" | jq -r '.id')
  body=$(printf '%s' "$entry" | jq -r '.body')
  do_resolve=$(printf '%s' "$entry" | jq -r '.resolve')

  if ! reply_out=$("$GH_BIN" api graphql -f query="$reply_mutation" -f threadId="$tid" -f body="$body" 2>"$gh_err"); then
    {
      printf 'failed to reply to thread %s:\n%s\n' "$tid" "$(cat "$gh_err")"
      if [ -s "$replied" ]; then
        printf 'already replied (do NOT resend on retry): %s\n' "$(jq -rs 'map(.id) | join(", ")' "$replied")"
      fi
      remaining=$(jq -r --argjson i "$i" '[.threads[$i:][].id] | join(", ")' "$threads_file")
      printf 'not processed: %s\n' "$remaining"
    } >&2
    exit 1
  fi

  url=$(printf '%s' "$reply_out" | jq -r '.data.addPullRequestReviewThreadReply.comment.url // empty')
  [ -n "$url" ] || fatal "replied to thread $tid but comment url missing in the API response"
  jq -nc --arg id "$tid" --arg url "$url" '{id: $id, url: $url}' >> "$replied"

  if [ "$do_resolve" = "true" ]; then
    if "$GH_BIN" api graphql -f query="$resolve_mutation" -f threadId="$tid" >/dev/null 2>"$gh_err"; then
      printf '%s\n' "$tid" >> "$resolved"
    else
      jq -nc --arg id "$tid" --arg error "$(cat "$gh_err")" '{id: $id, error: $error}' >> "$resolve_failed"
    fi
  fi

  i=$((i + 1))
done

jq -nc \
  --slurpfile replied "$replied" \
  --slurpfile resolve_failed "$resolve_failed" \
  --rawfile resolved_raw "$resolved" \
  '($resolved_raw | split("\n") | map(select(. != ""))) as $resolved
   | {
       replied: $replied,
       resolved: $resolved,
       resolve_failed: $resolve_failed,
       warnings: (if ($resolve_failed | length) > 0 then
         ["replied but could not resolve \($resolve_failed | length) thread(s) (write access to the repository is required to resolve): \($resolve_failed | map(.id) | join(", ")). The replies are posted; resolve them manually or ask the author to."]
       else [] end)
     }'
