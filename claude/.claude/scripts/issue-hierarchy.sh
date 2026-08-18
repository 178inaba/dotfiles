#!/bin/bash

# Issue 親子関係（GitHub Sub-Issues）の解決スクリプト（issue-handle・deep-review・issue-draft 共有）
#
# 「葉 Issue = 1 PR、リリース単位は親 Issue が束ねる」運用で、葉を読むスキルが親を継承して読み、
# 親を渡されたスキルが Sub の完了状態を見るための配管。gh CLI に Sub-Issues の専用コマンドは無く、
# 親は専用エンドポイント（404 = 親なし）、Sub 一覧はページネーション付きの別エンドポイントで、
# 呼び出し側が毎回組み立てると 404 の扱い・ページ結合・自分自身の除外がぶれるためここへ一本化する。
# テストは scripts/tests/test-issue-hierarchy.sh。
#
# 使用方法: issue-hierarchy.sh <issue-number> [-R owner/repo] [--with-prs] [--with-deps]
#   -R 省略時はカレントリポジトリ（gh repo view）を使う
#   --with-prs は sub_issues[] の各要素に、その Sub を閉じる PR の状態を付ける（親の充足検証 → close で
#   「全 Sub のマージ先がベースブランチか」を見るため。Sub ごとに 2 往復増えるので既定では付けない）
#   --with-deps は sub_issues[] の各要素に blocker を付ける（親の停止パスで「次に着手できる Sub」を
#   出すため。blocker を持つ Sub の数だけ往復が増えるので既定では付けない）
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）
#
# stdout は JSON のみ。契約（正はここ。各 SKILL.md には自スキルが使うフィールドの解釈のみ書く）:
#   repo                  owner/name 形式
#   number / title / state / url
#                         対象 Issue 自身
#   kind                  "standalone" | "parent" | "sub" | "parent_and_sub"
#                         parent 判定は sub_issues_summary.total > 0、sub 判定は parent の有無
#   parent                {number, title, state, url, repo, same_repo} | null（親なし、または照会失敗 — 後者は warnings に載る）
#                         repo は親の owner/name。Sub-Issues は同一 owner 内の別リポジトリにも張れるため、
#                         same_repo: false の親は番号だけでは指せない（本文では owner/repo#N 形式で参照する）
#   blocked_by[]          対象 Issue を blocking している Issue 一覧。要素は parent と同じ形
#                         {number, title, state, url, repo, same_repo}（別リポジトリの blocker も同じ形で載る）。
#                         issue_dependencies_summary.total_blocked_by が 0 なら往復せず []。
#                         取得失敗時は null + warnings。closed の blocker も一覧に残る（GitHub の挙動）
#   blockers_closed       blocker が 0 件、または全件取得できて（summary と一致）全て closed のとき true。
#                         取得失敗・件数不一致は false（all_sub_issues_closed と同じ安全側の慣習）
#   sub_issues[]          対象 Issue の Sub 一覧 {number, title, state, url}（全ページ結合。取得失敗時は [] + warnings）
#                         --with-prs 時は各要素に prs[] {number, state, base_ref, merged, url} を追加する
#                         （closedByPullRequestsReferences で紐づく PR。無ければ []。取得失敗は warnings + prs: null。
#                         Sub・PR とも URL で引くので別リポジトリでも正しい対象を指す）
#                         --with-deps 時は各要素に blocked_by[] / blockers_closed を追加する（意味は上と同じ）
#   sub_issues_summary    {total, completed}（GitHub 側の集計値。sub_issues[] の取得可否によらず得られる）
#   all_sub_issues_closed sub_issues[] を全件取得できて（summary.total と一致）かつ全て closed のとき true。
#                         Sub が 0 件・取得失敗・不一致は false（「全 Sub 完了の親」判定に使うため安全側）
#   siblings[]            親がある場合、親の Sub 一覧から自分を除いたもの {number, title, state, url}。親なしは []。
#                         親が別リポジトリのときは取得せず [] + warnings（別リポの Sub 一覧まで追わない）
#   all_siblings_closed   親があり siblings[] が全て closed（0 件を含む）のとき true。親なし・別リポ・取得失敗は false
#                         （「最後の Sub か」= 自分の PR に親の closing keyword を付けてよいかの判定に使う）
#   warnings[]            非致命の縮退（親照会の 404 以外の失敗、Sub 一覧・blocker 一覧の取得失敗・件数不一致）
# 前提不成立（引数不正・対象 Issue の取得失敗）は英語 stderr + 非ゼロ exit。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
. "$SCRIPT_DIR/warnings-lib.sh"

USAGE='usage: issue-hierarchy.sh <issue-number> [-R owner/repo] [--with-prs] [--with-deps]'

command -v jq >/dev/null 2>&1 || fatal 'jq is required'
GH_BIN=${GH_BIN:-gh}

issue_number=""
repo=""
with_prs=false
with_deps=false
while [ $# -gt 0 ]; do
  case "$1" in
    --with-prs)
      with_prs=true
      shift
      ;;
    --with-deps)
      with_deps=true
      shift
      ;;
    -R)
      [ -n "${2:-}" ] || fatal "-R requires owner/repo
$USAGE"
      repo=$2
      shift 2
      ;;
    -*) fatal "unknown flag: $1
$USAGE" ;;
    *)
      [ -z "$issue_number" ] || fatal "unexpected argument: $1
$USAGE"
      case "$1" in *[!0-9]*|'') fatal "invalid issue number: $1
$USAGE" ;; esac
      issue_number=$1
      shift
      ;;
  esac
done
[ -n "$issue_number" ] || fatal "$USAGE"

if [ -z "$repo" ]; then
  repo=$("$GH_BIN" repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null) && [ -n "$repo" ] \
    || fatal 'failed to resolve the current repository (run inside a GitHub repository or pass -R owner/repo)'
fi

base="repos/$repo/issues"

# Issue オブジェクト → {number, title, state, url, repo, same_repo}（--arg repo が要る）。
# 親と blocker で同じ形にするのは、どちらも「別リポジトリの対象は番号だけでは指せない」判断を
# 呼び出し側へ渡す必要があるため。repository オブジェクトではなく repository_url を読むのは、
# 後者だけが issue スキーマの required だから
ISSUE_REF_JQ='(.repository_url | split("/") | .[-2:] | join("/")) as $prepo
  | {number, title, state, url: .html_url, repo: $prepo, same_repo: ($prepo == $repo)}'

# 対象 Issue 自身（sub_issues_summary は Issue オブジェクトに含まれる）
self=$("$GH_BIN" api "$base/$issue_number" 2>/dev/null) && [ -n "$self" ] \
  || fatal "failed to fetch issue #$issue_number in $repo"

# 親: 専用エンドポイントの 404 が「親なし」の正常系。それ以外の失敗は縮退（null + warning）
parent='null'
parent_err=$(mktemp)
if parent_raw=$("$GH_BIN" api "$base/$issue_number/parent" 2>"$parent_err"); then
  parent=$(printf '%s' "$parent_raw" | jq -c --arg repo "$repo" "$ISSUE_REF_JQ")
elif ! grep -q 'HTTP 404' "$parent_err"; then
  add_warning "parent lookup failed for #$issue_number: $(tr '\n' ' ' < "$parent_err" | sed 's/[[:space:]]*$//')"
fi
rm -f "$parent_err"

# Sub 一覧を全ページ取得して 1 配列に結合する。--paginate は各ページの JSON 配列をそのまま連結して
# 出力するため jq -s で読み直して add する（--jq で要素展開すると失敗時の空出力と 0 件が区別できない）
fetch_sub_issues() {
  # fetch_sub_issues <issue-number> → stdout に JSON 配列。失敗時は非ゼロ
  # total_blocked_by は --with-deps が Sub ごとの往復要否を判定するための内部フィールドで、
  # 出力前に落とす（siblings[] と sub_issues[] の公開形は従来どおり 4 キー）。一覧のレスポンスも
  # Issue オブジェクトなので issue_dependencies_summary がそのまま載り、追加の往復は要らない
  local raw
  raw=$("$GH_BIN" api --paginate "$base/$1/sub_issues?per_page=100" 2>/dev/null) || return 1
  printf '%s' "$raw" | jq -sc 'add // [] | map({number, title, state, url: .html_url,
    total_blocked_by: (.issue_dependencies_summary.total_blocked_by // 0)})'
}

issue_api_path() {
  # issue_api_path <html_url> → repos/<owner>/<repo>/issues/<number>
  # 番号ではなく URL から組み立てるのは、別リポジトリの Sub でも正しい対象を指すため
  # （--with-prs が gh issue view に URL を渡すのと同じ理由）
  printf '%s' "$1" | sed -E 's#^https?://[^/]+/([^/]+)/([^/]+)/issues/([0-9]+)$#repos/\1/\2/issues/\3#'
}

fetch_blocked_by() {
  # fetch_blocked_by <api-path> → stdout に JSON 配列。失敗時は非ゼロ
  # ページネーションと jq -s で読み直す理由は fetch_sub_issues と同じ
  local raw
  raw=$("$GH_BIN" api --paginate "$1/dependencies/blocked_by?per_page=100" 2>/dev/null) || return 1
  printf '%s' "$raw" | jq -sc --arg repo "$repo" "add // [] | map($ISSUE_REF_JQ)"
}

resolve_blockers() {
  # resolve_blockers <api-path> <total_blocked_by> <label> <out-file>
  # <out-file> に {blocked_by, blockers_closed} を書く。戻り値を $( ) で受けないのは、
  # サブシェルで実行すると add_warning の追記（warnings-lib.sh のプロセスローカル変数）が消えるため
  local path=$1 total=$2 label=$3 out=$4 list count
  # summary が 0 なら往復を省く（依存の無い Issue が大多数で、起動時の自動実行ごとに走るため）。
  # ゲートに total_blocked_by を使うのは、これが closed も含む全 blocker 数だから
  # （summary.blocked_by は open のみで、blocker が全て closed の Issue を 0 件と誤判定する）
  if [ "$total" = 0 ]; then
    printf '{"blocked_by":[],"blockers_closed":true}' > "$out"
    return
  fi
  if ! list=$(fetch_blocked_by "$path"); then
    add_warning "blocked_by lookup failed for $label"
    printf '{"blocked_by":null,"blockers_closed":false}' > "$out"
    return
  fi
  count=$(printf '%s' "$list" | jq 'length')
  if [ "$total" != "$count" ]; then
    add_warning "blocked_by count mismatch for $label: summary=$total fetched=$count"
    printf '%s' "$list" | jq -c '{blocked_by: ., blockers_closed: false}' > "$out"
    return
  fi
  printf '%s' "$list" | jq -c '{blocked_by: ., blockers_closed: all(.state == "closed")}' > "$out"
}

# 自分の Sub 一覧。summary が 0 件なら往復を省く（standalone / sub が大半で、起動時の自動実行ごとに走るため）
summary_total=$(printf '%s' "$self" | jq -r '.sub_issues_summary.total // 0')
sub_issues_fetched=false
if [ "$summary_total" = 0 ]; then
  sub_issues='[]'
  sub_issues_fetched=true
elif sub_issues=$(fetch_sub_issues "$issue_number"); then
  sub_issues_fetched=true
else
  sub_issues='[]'
  add_warning "sub_issues lookup failed for #$issue_number"
fi

# 対象 Issue 自身の blocker
blockers_file=$(mktemp)
resolve_blockers "$base/$issue_number" \
  "$(printf '%s' "$self" | jq -r '.issue_dependencies_summary.total_blocked_by // 0')" \
  "#$issue_number" "$blockers_file"
blockers=$(cat "$blockers_file")

siblings='[]'
siblings_fetched=false
if [ "$parent" != 'null' ] && [ "$(printf '%s' "$parent" | jq -r .same_repo)" != true ]; then
  add_warning "parent #$(printf '%s' "$parent" | jq -r .number) is in another repository ($(printf '%s' "$parent" | jq -r .repo)); siblings unknown"
elif [ "$parent" != 'null' ]; then
  parent_number=$(printf '%s' "$parent" | jq -r .number)
  if parent_subs=$(fetch_sub_issues "$parent_number"); then
    siblings=$(printf '%s' "$parent_subs" | jq -c --argjson me "$issue_number" \
      'map(select(.number != $me) | del(.total_blocked_by))')
    siblings_fetched=true
  else
    add_warning "sub_issues lookup failed for parent #$parent_number (siblings unknown)"
  fi
fi

# --with-prs: 各 Sub を閉じる PR の状態を付ける（issue view → pr view の 2 段。数値の突き合わせを
# 呼び出し側に組み立てさせない）
if [ "$with_prs" = true ]; then
  annotated='[]'
  for sub_number in $(printf '%s' "$sub_issues" | jq -r '.[].number'); do
    sub_url=$(printf '%s' "$sub_issues" | jq -r --argjson n "$sub_number" '.[] | select(.number == $n) | .url')
    prs='null'
    # Sub も PR も URL で引く（-R "$repo" だと別リポジトリの Sub / PR を同番号の無関係な対象に取り違える）
    if pr_urls=$("$GH_BIN" issue view "$sub_url" --json closedByPullRequestsReferences \
        -q '.closedByPullRequestsReferences[].url' 2>/dev/null); then
      prs='[]'
      for pr_url in $pr_urls; do
        if pr_json=$("$GH_BIN" pr view "$pr_url" --json number,state,baseRefName,url 2>/dev/null); then
          prs=$(printf '%s' "$prs" | jq -c --argjson pr "$pr_json" \
            '. + [{number: $pr.number, state: $pr.state, base_ref: $pr.baseRefName, merged: ($pr.state == "MERGED"), url: $pr.url}]')
        else
          add_warning "pr lookup failed for $pr_url (closing Sub #$sub_number)"
          prs='null'
          break
        fi
      done
    else
      add_warning "closing PR lookup failed for Sub #$sub_number"
    fi
    annotated=$(printf '%s' "$annotated" | jq -c --argjson n "$sub_number" --argjson prs "$prs" \
      --argjson subs "$sub_issues" '. + [($subs[] | select(.number == $n)) + {prs: $prs}]')
  done
  sub_issues=$annotated
fi

# --with-deps: 各 Sub の blocker を付ける（--with-prs と併用する場合はその注釈済み配列に足す）
if [ "$with_deps" = true ]; then
  annotated='[]'
  for sub_number in $(printf '%s' "$sub_issues" | jq -r '.[].number'); do
    sub=$(printf '%s' "$sub_issues" | jq -c --argjson n "$sub_number" '.[] | select(.number == $n)')
    resolve_blockers "$(issue_api_path "$(printf '%s' "$sub" | jq -r .url)")" \
      "$(printf '%s' "$sub" | jq -r .total_blocked_by)" "Sub #$sub_number" "$blockers_file"
    annotated=$(printf '%s' "$annotated" | jq -c --argjson sub "$sub" \
      --argjson blockers "$(cat "$blockers_file")" '. + [$sub + $blockers]')
  done
  sub_issues=$annotated
fi
rm -f "$blockers_file"

fetched_count=$(printf '%s' "$sub_issues" | jq 'length')
if [ "$sub_issues_fetched" = true ] && [ "$summary_total" != "$fetched_count" ]; then
  add_warning "sub_issues count mismatch for #$issue_number: summary=$summary_total fetched=$fetched_count"
  sub_issues_fetched=false
fi

jq -n \
  --arg repo "$repo" \
  --argjson self "$self" \
  --argjson parent "$parent" \
  --argjson sub_issues "$sub_issues" \
  --argjson sub_fetched "$sub_issues_fetched" \
  --argjson blockers "$blockers" \
  --argjson siblings "$siblings" \
  --argjson siblings_fetched "$siblings_fetched" \
  --argjson warnings "$(warnings_json)" \
  '($self.sub_issues_summary.total // 0) as $total
  | ($self.sub_issues_summary.completed // 0) as $completed
  | {
    repo: $repo,
    number: $self.number,
    title: $self.title,
    state: $self.state,
    url: $self.html_url,
    kind: (
      ($total > 0) as $is_parent
      | ($parent != null) as $is_sub
      | if $is_parent and $is_sub then "parent_and_sub"
        elif $is_parent then "parent"
        elif $is_sub then "sub"
        else "standalone" end
    ),
    parent: $parent,
    blocked_by: $blockers.blocked_by,
    blockers_closed: $blockers.blockers_closed,
    sub_issues: ($sub_issues | map(del(.total_blocked_by))),
    sub_issues_summary: {total: $total, completed: $completed},
    all_sub_issues_closed: (
      $sub_fetched and ($sub_issues | length) > 0 and ($sub_issues | all(.state == "closed"))
    ),
    siblings: $siblings,
    all_siblings_closed: (
      $parent != null and $siblings_fetched and ($siblings | all(.state == "closed"))
    ),
    warnings: $warnings
  }'
