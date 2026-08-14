#!/bin/bash

# issue-handle --worktree 用の worktree 配管スクリプト（issue-handle スキル所有）
#
# 「事前準備 Step 3. 既存 worktree 検出」と「Step 5. worktree 作成」の決定的処理を担う。
# EnterWorktree(name:) を使わず git worktree add で直接作成する理由: EnterWorktree(name:) は
# base branch を指定できず、ベースブランチを起点にするにはメインツリーの HEAD を動かす必要が
# あった（Claude Code 2.1.222 の worktree 隔離強化で、隔離後のメインツリー復元が不可能に
# なったため方式を変更 — 2026-08-14）。session 切替（EnterWorktree(path:)）は AI 側に残す:
# session の状態はスクリプトから観測できないため。
#
# 使用方法:
#   create-worktree.sh detect <issue-number>
#     メインツリー内の任意の cwd で実行。Issue 番号に対応する既存 worktree を
#     git worktree list --porcelain から検索する（再開シナリオの判定）。branch が
#     <type>/<issue-number>-*（現行命名）または worktree-<type>-<issue-number>-*
#     （旧 EnterWorktree(name:) 方式の命名）に一致する linked worktree が対象。
#     メイン worktree は除外する（メインが該当 branch を checkout 中でも再開対象にしない）。
#     Issue 番号は完全一致（42 が 142 に一致しない）。
#   create-worktree.sh create <worktree-name> <branch> <base-branch>
#     メインツリー内の任意の cwd で実行。origin/<base-branch>（無ければローカル
#     <base-branch>）を起点に <メインツリー>/.claude/worktrees/<worktree-name> へ
#     branch <branch> の worktree を作成する。メインツリーの HEAD・working tree には
#     一切触れない。fetch は行わない（事前準備 Step 2 の fetch が前提。失敗していても
#     ローカル base へフォールバックする）。
#
#     作成後、.worktreeinclude（起点 commit に含まれる場合）のネイティブ挙動を再現して
#     gitignored ファイルをメインツリーからコピーする: パターン一致かつ gitignored の
#     ファイルのみ対象（tracked・単なる untracked は対象外）、symlink はスキップ、
#     コピー先が committed symlink 経由で worktree 外へ出る場合もスキップ。
#     EnterWorktree(name:) 経路と違い WorktreeCreate hook は発火しない。
#
# stdout は JSON のみ:
#   detect:
#     found          既存 worktree が見つかったか
#     worktree_path  見つかった worktree の絶対パス（found: true 時のみ非 null）
#     branch         見つかった worktree の branch 名（同上）
#   create:
#     status         "ok" | "branch_exists" | "path_exists"
#                    ok 以外は続行不可の停止条件。既存 branch・既存ディレクトリは過去作業の
#                    残骸の可能性があり、破棄の判断は AI（ユーザー確認）に委ねる
#     worktree_path  作成した worktree の絶対パス（ok 時のみ非 null）
#     branch         作成した branch 名
#     start_ref      起点に使った ref（"origin/<base>" or "<base>"。ok 時のみ非 null）
#     copied_files   .worktreeinclude によりコピーしたファイル数
#     warnings[]     非致命の注意（symlink スキップ等）。空でなければ AI が報告に併記する
#
# 前提不成立（リポジトリ外・jq 欠如・base branch 不在・git 操作の機械的失敗）は
# 非ゼロ exit + 英語 stderr。

set -u

fatal() {
  printf '%s\n' "$1" >&2
  exit 1
}

command -v jq >/dev/null 2>&1 || fatal 'jq is required'
command -v git >/dev/null 2>&1 || fatal 'git is required'

subcommand=${1:-}

# メインツリーのルート（linked worktree の cwd でも共通 .git の親を返す）
common_dir=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) \
  || fatal 'not inside a git repository'
main_root=$(dirname "$common_dir")

warnings=""
add_warning() {
  warnings="${warnings}${1}
"
}

warnings_json() {
  printf '%s' "$warnings" | jq -Rs 'split("\n") | map(select(length > 0))'
}

# --- detect: Issue 番号に対応する既存 worktree の検索 ---
detect() {
  local issue_num=$1
  case "$issue_num" in
    *[!0-9]*|'') fatal 'usage: create-worktree.sh detect <issue-number>' ;;
  esac
  # 新旧命名とも <数字>- の前後を区切り文字で挟んで完全一致させる（42 が 142 に一致しない）
  git -C "$main_root" worktree list --porcelain | awk -v num="$issue_num" '
    $1 == "worktree" { path = substr($0, 10); n++ }
    $1 == "branch" && n > 1 {
      ref = substr($2, 12)  # refs/heads/ を剥がす
      if (ref ~ ("^[a-z]+/" num "-") || ref ~ ("^worktree-[a-z]+-" num "-")) {
        printf "%s\t%s\n", path, ref
        exit
      }
    }
  ' | jq -Rs 'split("\t")
      | if length < 2 then {found: false, worktree_path: null, branch: null}
        else {found: true, worktree_path: .[0], branch: (.[1] | rtrimstr("\n"))} end'
}

# --- create: base 起点の worktree 作成 + .worktreeinclude コピー ---
emit() {
  local status=$1 path=${2:-} start_ref=${3:-} copied=${4:-0}
  jq -n \
    --arg status "$status" \
    --arg path "$path" \
    --arg branch "$branch" \
    --arg start_ref "$start_ref" \
    --argjson copied "$copied" \
    --argjson warnings "$(warnings_json)" \
    '{status: $status, worktree_path: (if $path == "" then null else $path end),
      branch: $branch, start_ref: (if $start_ref == "" then null else $start_ref end),
      copied_files: $copied, warnings: $warnings}'
}

create() {
  local worktree_name=$1 base_branch=$2
  # branch は emit が参照するためグローバル（呼び出し元で設定済み）

  # 停止条件の検査（残骸の破棄判断は AI に委ねる）
  if git -C "$main_root" show-ref --verify --quiet "refs/heads/$branch"; then
    emit branch_exists
    exit 0
  fi

  local worktree_path="$main_root/.claude/worktrees/$worktree_name"
  if [ -e "$worktree_path" ]; then
    emit path_exists
    exit 0
  fi

  # 起点 ref の解決（origin 優先、無ければローカル base）
  local start_ref
  if git -C "$main_root" show-ref --verify --quiet "refs/remotes/origin/$base_branch"; then
    start_ref="origin/$base_branch"
    # ローカル base が ahead だと未 push commit が worktree に入らないため、silent にしない
    if git -C "$main_root" show-ref --verify --quiet "refs/heads/$base_branch" \
       && ! git -C "$main_root" merge-base --is-ancestor "refs/heads/$base_branch" "refs/remotes/origin/$base_branch"; then
      add_warning "local branch $base_branch has commits not on origin/$base_branch; worktree starts from origin/$base_branch"
    fi
  elif git -C "$main_root" show-ref --verify --quiet "refs/heads/$base_branch"; then
    start_ref="$base_branch"
    add_warning "origin/$base_branch not found; started from local branch $base_branch"
  else
    fatal "base branch not found: neither origin/$base_branch nor $base_branch exists"
  fi

  # worktree 作成（メインツリーの HEAD・working tree には触れない）
  git -C "$main_root" worktree add --quiet "$worktree_path" -b "$branch" "$start_ref" \
    || fatal "git worktree add failed for $worktree_path"

  # .worktreeinclude のネイティブ挙動再現（起点 commit に含まれる場合のみ）
  local copied=0
  if [ -f "$worktree_path/.worktreeinclude" ] && [ ! -L "$worktree_path/.worktreeinclude" ]; then
    local worktree_phys
    worktree_phys=$(cd "$worktree_path" && pwd -P) || fatal "failed to resolve: $worktree_path"
    # パターン一致の untracked を列挙 → 実際に gitignored のものへ絞る（tracked は --others が除外）
    local file dest_dir resolved_dir
    while IFS= read -r -d '' file; do
      case "$file" in
        .claude/worktrees/*) continue ;;  # 他 worktree 内のファイルはコピー元にしない
      esac
      if [ -L "$main_root/$file" ]; then
        add_warning "skipped symlink in .worktreeinclude: $file"
        continue
      fi
      dest_dir="$worktree_path/$(dirname "$file")"
      mkdir -p "$dest_dir" || fatal "failed to create directory: $dest_dir"
      # コピー先が committed symlink 経由で worktree 外へ出ていないか
      resolved_dir=$(cd "$dest_dir" && pwd -P) || fatal "failed to resolve: $dest_dir"
      case "$resolved_dir/" in
        "$worktree_phys/"*) ;;
        *)
          add_warning "skipped .worktreeinclude entry (destination escapes worktree): $file"
          continue
          ;;
      esac
      cp -p "$main_root/$file" "$worktree_path/$file" || fatal "failed to copy: $file"
      copied=$((copied + 1))
    done < <(git -C "$main_root" ls-files -z --others --ignored --exclude-from="$worktree_path/.worktreeinclude" \
               | git -C "$main_root" check-ignore -z --stdin)
  fi

  emit ok "$worktree_path" "$start_ref" "$copied"
}

case "$subcommand" in
  detect)
    detect "${2:-}"
    ;;
  create)
    worktree_name=${2:-}
    branch=${3:-}
    base_branch=${4:-}
    [ -n "$worktree_name" ] && [ -n "$branch" ] && [ -n "$base_branch" ] \
      || fatal 'usage: create-worktree.sh create <worktree-name> <branch> <base-branch>'
    create "$worktree_name" "$base_branch"
    ;;
  *)
    fatal 'usage: create-worktree.sh <detect|create> [args]'
    ;;
esac
