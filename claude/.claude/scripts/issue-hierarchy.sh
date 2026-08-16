#!/bin/bash

# Issue 親子関係（GitHub Sub-Issues）の解決スクリプト（issue-handle・deep-review・issue-draft 共有）
#
# 「葉 Issue = 1 PR、リリース単位は親 Issue が束ねる」運用で、葉を読むスキルが親を継承して読み、
# 親を渡されたスキルが Sub の完了状態を見るための配管。gh CLI に Sub-Issues の専用コマンドは無く、
# 親は専用エンドポイント（404 = 親なし）、Sub 一覧はページネーション付きの別エンドポイントで、
# 呼び出し側が毎回組み立てると 404 の扱い・ページ結合・自分自身の除外がぶれるためここへ一本化する。
# テストは scripts/tests/test-issue-hierarchy.sh。
#
# 使用方法: issue-hierarchy.sh <issue-number> [-R owner/repo]
#   -R 省略時はカレントリポジトリ（gh repo view）を使う
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）
#
# stdout は JSON のみ。契約（正はここ。各 SKILL.md には自スキルが使うフィールドの解釈のみ書く）:
#   repo                  owner/name 形式
#   number / title / state / url
#                         対象 Issue 自身
#   kind                  "standalone" | "parent" | "sub" | "parent_and_sub"
#                         parent 判定は sub_issues_summary.total > 0、sub 判定は parent の有無
#   parent                {number, title, state, url} | null（親なし、または照会失敗 — 後者は warnings に載る）
#   sub_issues[]          対象 Issue の Sub 一覧 {number, title, state, url}（全ページ結合。取得失敗時は [] + warnings）
#   sub_issues_summary    {total, completed}（GitHub 側の集計値。sub_issues[] の取得可否によらず得られる）
#   all_sub_issues_closed sub_issues[] を全件取得できて（summary.total と一致）かつ全て closed のとき true。
#                         Sub が 0 件・取得失敗・不一致は false（「全 Sub 完了の親」判定に使うため安全側）
#   siblings[]            親がある場合、親の Sub 一覧から自分を除いたもの {number, title, state, url}。親なしは []
#   all_siblings_closed   親があり siblings[] が全て closed（0 件を含む）のとき true。親なし・取得失敗は false
#                         （「最後の Sub か」= 自分の PR に親の closing keyword を付けてよいかの判定に使う）
#   warnings[]            非致命の縮退（親照会の 404 以外の失敗、Sub 一覧の取得失敗・件数不一致）
# 前提不成立（引数不正・対象 Issue の取得失敗）は英語 stderr + 非ゼロ exit。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
. "$SCRIPT_DIR/warnings-lib.sh"

USAGE='usage: issue-hierarchy.sh <issue-number> [-R owner/repo]'

command -v jq >/dev/null 2>&1 || fatal 'jq is required'
GH_BIN=${GH_BIN:-gh}

issue_number=""
repo=""
while [ $# -gt 0 ]; do
  case "$1" in
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

# 対象 Issue 自身（sub_issues_summary は Issue オブジェクトに含まれる）
self=$("$GH_BIN" api "$base/$issue_number" 2>/dev/null) && [ -n "$self" ] \
  || fatal "failed to fetch issue #$issue_number in $repo"

# 親: 専用エンドポイントの 404 が「親なし」の正常系。それ以外の失敗は縮退（null + warning）
parent='null'
parent_err=$(mktemp)
if parent_raw=$("$GH_BIN" api "$base/$issue_number/parent" 2>"$parent_err"); then
  parent=$(printf '%s' "$parent_raw" | jq -c '{number, title, state, url: .html_url}')
elif ! grep -q 'HTTP 404' "$parent_err"; then
  add_warning "parent lookup failed for #$issue_number: $(tr '\n' ' ' < "$parent_err" | sed 's/[[:space:]]*$//')"
fi
rm -f "$parent_err"

# Sub 一覧を全ページ取得して 1 配列に結合する。--paginate は各ページの JSON 配列をそのまま連結して
# 出力するため jq -s で読み直して add する（--jq で要素展開すると失敗時の空出力と 0 件が区別できない）
fetch_sub_issues() {
  # fetch_sub_issues <issue-number> → stdout に JSON 配列。失敗時は非ゼロ
  local raw
  raw=$("$GH_BIN" api --paginate "$base/$1/sub_issues?per_page=100" 2>/dev/null) || return 1
  printf '%s' "$raw" | jq -sc 'add // [] | map({number, title, state, url: .html_url})'
}

sub_issues='[]'
sub_issues_fetched=false
if sub_issues=$(fetch_sub_issues "$issue_number"); then
  sub_issues_fetched=true
else
  sub_issues='[]'
  add_warning "sub_issues lookup failed for #$issue_number"
fi

siblings='[]'
siblings_fetched=false
if [ "$parent" != 'null' ]; then
  parent_number=$(printf '%s' "$parent" | jq -r .number)
  if parent_subs=$(fetch_sub_issues "$parent_number"); then
    siblings=$(printf '%s' "$parent_subs" | jq -c --argjson me "$issue_number" 'map(select(.number != $me))')
    siblings_fetched=true
  else
    add_warning "sub_issues lookup failed for parent #$parent_number (siblings unknown)"
  fi
fi

summary_total=$(printf '%s' "$self" | jq -r '.sub_issues_summary.total // 0')
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
  --argjson siblings "$siblings" \
  --argjson siblings_fetched "$siblings_fetched" \
  --argjson warnings "$(warnings_json)" \
  '{
    repo: $repo,
    number: $self.number,
    title: $self.title,
    state: $self.state,
    url: $self.html_url,
    kind: (
      (($self.sub_issues_summary.total // 0) > 0) as $is_parent
      | ($parent != null) as $is_sub
      | if $is_parent and $is_sub then "parent_and_sub"
        elif $is_parent then "parent"
        elif $is_sub then "sub"
        else "standalone" end
    ),
    parent: $parent,
    sub_issues: $sub_issues,
    sub_issues_summary: {
      total: ($self.sub_issues_summary.total // 0),
      completed: ($self.sub_issues_summary.completed // 0)
    },
    all_sub_issues_closed: (
      $sub_fetched and ($sub_issues | length) > 0 and ($sub_issues | all(.state == "closed"))
    ),
    siblings: $siblings,
    all_siblings_closed: (
      $parent != null and $siblings_fetched and ($siblings | all(.state == "closed"))
    ),
    warnings: $warnings
  }'
