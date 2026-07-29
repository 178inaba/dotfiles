#!/bin/bash

# /deep-review のレビュー投稿スクリプト
#
# レビュー結果（総合評価・body・行コメント）を受け取り、投稿前検証を通してから
# GitHub REST API でレビューを投稿する。本文は body_file / comments[].body_file で
# work_dir 直下の素の Markdown ファイルを参照でき、投稿前に本文へ解決する。
# 検証は決定的に実行する:
#   1. ローカル HEAD == pr.head_oid の再確認（鮮度確認後に head が動いた場合の 422 を防ぐ）
#   2. 行コメントの path/line を最新 diff（origin/<base>...HEAD の新ファイル側行番号、
#      追加行 + context 行）と突き合わせ、diff に無い行があれば投稿せず非ゼロ exit で
#      違反エントリを stderr に列挙する（422 を「投稿後の失敗」から「投稿前の検証」へ前倒し。
#      行の付け直しは AI 側の判断）
# 「総合評価 → event」の決定表もここが所有する（レビュアーの遠慮・忖度で event が
# 揺れないよう機械的に変換する）。
#
# 使用方法: post-review.sh <pr-context.json> <review-file>
#   <pr-context.json> fetch-pr-context.sh の出力ファイル（repo / pr.number / pr.base_ref /
#                     pr.head_oid を読む）
#   <review-file>     入力契約は SKILL.md の「review_path に書く JSON の入力契約」を参照。
#                     パスは prepare-review.sh の review_path を使う（context と対になる
#                     作業ディレクトリの直下にあることを検証する — 下記コメント参照）
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

# shellcheck source=review-dir-lib.sh
. "$(dirname "$0")/review-dir-lib.sh"

context_file=${1:-}
review_file=${2:-}
{ [ -n "$context_file" ] && [ -n "$review_file" ]; } \
  || fatal 'usage: post-review.sh <pr-context.json> <review-file>'
[ -f "$context_file" ] || fatal "pr context file not found: $context_file"
[ -f "$review_file" ] || fatal "review file not found: $review_file"

require_valid_json "$context_file"
require_valid_json "$review_file"

git rev-parse --git-dir >/dev/null 2>&1 || fatal 'not inside a git repository'

repo=$(jq -er '.repo' "$context_file" 2>/dev/null) || fatal "repo missing in $context_file"
pr_number=$(jq -er '.pr.number' "$context_file" 2>/dev/null) || fatal "pr.number missing in $context_file"
base_ref=$(jq -er '.pr.base_ref' "$context_file" 2>/dev/null) || fatal "pr.base_ref missing in $context_file"
head_oid=$(jq -er '.pr.head_oid' "$context_file" 2>/dev/null) || fatal "pr.head_oid missing in $context_file"

# 行コメントの path/line 検証は comments[] が空のレビューでは取り違えを検出できないため、
# 入力ファイルの置き場所の側で構造的に止める（規約と理由は review-dir-lib.sh のヘッダー参照）
require_in_review_work_dir "$review_file" review_path "$context_file"

assessment=$(jq -er '.assessment' "$review_file" 2>/dev/null) || fatal "assessment missing in $review_file"

# 本文はインライン（body）と file 参照（body_file）の排他。長文プロースを JSON 文字列として
# 手書きするとエスケープ1文字の欠落で全体が無効になるため、素の Markdown を Write して
# body_file で参照する経路を正規に受け付ける。排他条件は def に閉じて共有する
# （ダブルクォート展開で jq プログラム側に \" エスケープを持ち込まない）
body_ok='def body_ok:
  if has("body") then (has("body_file") | not) and (.body | type == "string")
  elif has("body_file") then (.body_file | type == "string") and (.body_file | length > 0)
  else false end;'
jq -e "$body_ok"' body_ok' "$review_file" >/dev/null 2>&1 \
  || fatal "exactly one of body (string) / body_file (non-empty string) is required in $review_file"
jq -e "$body_ok"' (.comments | type == "array")
  and all(.comments[]?; (.path | type) == "string" and (.line | type) == "number" and body_ok)' \
  "$review_file" >/dev/null 2>&1 \
  || fatal "comments must be an array of {path: string, line: number, body xor body_file: string} in $review_file"

# body_file 参照を本文へ解決する。参照は work_dir 直下のベース名に限定し、review_path と
# 同じディレクトリ束縛を file 参照で迂回させない。参照ファイル名 → 内容のマップを先に
# 作り、単一の jq パスで一括解決する（body / comments[] で解決ロジックを複製しない）
work_dir=$(dirname "$review_file")

check_body_file() {
  case "$1" in
    */*) fatal "body_file must be a bare filename in the review work dir (no path separators): $1" ;;
  esac
  [ -f "$work_dir/$1" ] || fatal "body_file not found in the review work dir: $work_dir/$1"
}

contents='{}'
while IFS= read -r ref; do
  check_body_file "$ref"
  contents=$(jq --arg k "$ref" --rawfile v "$work_dir/$ref" '. + {($k): $v}' <<<"$contents") \
    || fatal "failed to read body_file: $ref"
done < <(jq -r '[.body_file] + [.comments[].body_file] | map(select(. != null)) | unique | .[]' "$review_file")

resolved_review=$(mktemp)
trap 'rm -f "$resolved_review"' EXIT
jq --argjson contents "$contents" '
  def resolve: if has("body_file") then del(.body_file) + {body: $contents[.body_file]} else . end;
  resolve | .comments |= map(resolve)' "$review_file" > "$resolved_review" \
  || fatal 'failed to resolve body_file references'
comment_count=$(jq -r '.comments | length' "$resolved_review")

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
if [ "$comment_count" -gt 0 ]; then
  # -c color.diff=false / --no-ext-diff: ユーザーの git 設定（色・外部 diff）に左右されず
  # unified diff を決定的に得る。awk は hunk 内の行規則をファイルヘッダ規則より先に評価する —
  # "++ " で始まる追加行は "+++ ..." と描画されヘッダと衝突するため（実ヘッダは
  # "diff --git" 行で inhunk が 0 に戻った後にしか現れない）
  valid_lines=$(git -c color.diff=false diff --no-ext-diff "origin/$base_ref...HEAD" | awk '
    /^\\ / { next }
    inhunk && /^\+/ { print file ":" n; n++; next }
    inhunk && /^-/ { next }
    inhunk && /^ / { print file ":" n; n++; next }
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
    { inhunk = 0 }
  ')
  invalid=$(jq -r '.comments[] | "\(.path):\(.line)"' "$resolved_review" \
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
  "$resolved_review")

response=$(printf '%s' "$payload" | "$GH_BIN" api "repos/$repo/pulls/$pr_number/reviews" --method POST --input -) \
  || fatal 'failed to post review (gh api)'

url=$(printf '%s' "$response" | jq -r '.html_url // empty')
[ -n "$url" ] || fatal 'review posted but html_url missing in the API response'

jq -n --arg url "$url" '{url: $url}'
