#!/bin/bash

# Claude Code Status Line Script
# 現在のディレクトリ、Gitブランチ、ステータス情報を表示

# 現在のディレクトリ（ホームディレクトリを ~ に短縮）
current_dir="${PWD/#$HOME/~}"

# Git情報を取得
if git rev-parse --git-dir > /dev/null 2>&1; then
  # Gitリポジトリ内の場合
  branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)

  # Git作業ツリーの状態をチェック
  git_status=""
  if [[ -n $(git status --porcelain 2>/dev/null) ]]; then
      git_status=" ●"  # 変更がある場合
  fi

  git_info=" (${branch}${git_status})"
else
  git_info=""
fi

# ステータス情報を組み合わせて出力
echo "${current_dir}${git_info}"
