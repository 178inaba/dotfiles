#!/bin/bash

# /review-assigned-prs のレビュー用 clone dir を ensure するスクリプト
#
# 引数の <owner>/<repo> について、レビュー専用ワークスペースに clone を用意し、その絶対
# パスを JSON で stdout に出力する。既存 clone は git fetch --prune で更新する。
#
# clone 先パスの規約（正はここ・SKILL.md 側は「JSON の path フィールドを使う」旨のみ）:
#   ${XDG_DATA_HOME:-$HOME/.local/share}/claude-review-prs/{owner}/{repo}
#
# ユーザーの通常作業 clone とは別空間に置くことで、レビュー中の worktree が普段の作業を
# 汚さないようにする。
#
# 使用方法: ensure-clone.sh <owner>/<repo>
# 出力契約: SKILL.md の「出力 JSON の契約」を参照
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）
#           XDG_DATA_HOME — 未設定なら $HOME/.local/share

set -u

GH_BIN=${GH_BIN:-gh}

if ! command -v jq >/dev/null 2>&1; then
  printf 'jq is required\n' >&2
  exit 1
fi
if ! command -v git >/dev/null 2>&1; then
  printf 'git is required\n' >&2
  exit 1
fi

repo_ref=${1:-}
if [ -z "$repo_ref" ]; then
  printf 'usage: ensure-clone.sh <owner>/<repo>\n' >&2
  exit 1
fi

if ! [[ "$repo_ref" =~ ^[^/]+/[^/]+$ ]]; then
  printf 'invalid repo reference (expected <owner>/<repo>): %s\n' "$repo_ref" >&2
  exit 1
fi

owner=${repo_ref%%/*}
repo=${repo_ref#*/}

base="${XDG_DATA_HOME:-$HOME/.local/share}/claude-review-prs"
path="$base/$owner/$repo"

if [ -d "$path/.git" ]; then
  if ! git -C "$path" fetch --prune >/dev/null 2>&1; then
    printf 'failed to fetch %s\n' "$repo_ref" >&2
    exit 1
  fi
else
  if ! mkdir -p "$base/$owner"; then
    printf 'failed to create parent dir %s\n' "$base/$owner" >&2
    exit 1
  fi
  if ! "$GH_BIN" repo clone "$repo_ref" "$path" >/dev/null 2>&1; then
    # partial clone（.git 未作成のまま中断された残骸）を掃除して、次回イテレーションで
    # 再 clone できる状態にする（放置すると gh repo clone が「path exists」で永続失敗する）
    rm -rf "$path"
    printf 'failed to clone %s\n' "$repo_ref" >&2
    exit 1
  fi
fi

jq -n --arg path "$path" '{path: $path}'
