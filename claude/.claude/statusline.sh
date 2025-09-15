#!/bin/bash

# Claude Code Status Line Script
# 開発生産性向上のための包括的ステータス表示

# 色定義（視認性向上）
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# 現在のディレクトリ（ホームディレクトリを ~ に短縮）
current_dir="${PWD/#$HOME/~}"

# 時刻表示（開発セッション管理用）
current_time=$(date +"%H:%M")

# Git情報を取得（拡張版）
if git rev-parse --git-dir > /dev/null 2>&1; then
  # Gitリポジトリ内の場合
  branch=$(git branch --show-current 2>/dev/null)

  # Git作業ツリーの状態をチェック（詳細版）
  git_status=""
  if [[ -n $(git status --porcelain 2>/dev/null) ]]; then
      # 変更ファイル数をカウント
      modified_count=$(git diff --name-only | wc -l | tr -d ' ')
      staged_count=$(git diff --cached --name-only | wc -l | tr -d ' ')

      if [[ $staged_count -gt 0 ]]; then
          git_status=" +${staged_count}"  # ステージされた変更
      fi
      if [[ $modified_count -gt 0 ]]; then
          git_status="${git_status} ~${modified_count}"  # 未ステージの変更
      fi
  fi

  # リモートとの同期状況（ahead/behind）
  sync_status=""
  if remote_branch=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null); then
      ahead=$(git rev-list --count HEAD ^${remote_branch} 2>/dev/null || echo 0)
      behind=$(git rev-list --count ${remote_branch} ^HEAD 2>/dev/null || echo 0)

      if [[ $ahead -gt 0 ]]; then
          sync_status=" ↑${ahead}"
      fi
      if [[ $behind -gt 0 ]]; then
          sync_status="${sync_status} ↓${behind}"
      fi
  fi

  git_info=" (${branch}${git_status}${sync_status})"
else
  git_info=""
fi

# システムリソース情報（軽量版）
# CPU使用率（top 1回実行で取得）
if command -v top >/dev/null 2>&1; then
    cpu_usage=$(top -l 1 -n 0 | grep "CPU usage" | awk '{print $3}' | sed 's/%//')
    if [[ -n "$cpu_usage" && "$cpu_usage" != "0.0" ]]; then
        system_info=" CPU:${cpu_usage}%"
    fi
fi

# メモリ使用率（memory_pressureコマンド使用）
if command -v memory_pressure >/dev/null 2>&1; then
    memory_free_pct=$(memory_pressure 2>/dev/null | grep "System-wide memory free percentage:" | awk '{print $5}' | sed 's/%//')
    if [[ -n "$memory_free_pct" ]]; then
        memory_used_pct=$((100 - memory_free_pct))
        system_info="${system_info} MEM:${memory_used_pct}%"
    fi
fi

# 開発コンテキスト情報
# Dockerコンテナが動作中かチェック
docker_info=""
if command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
    running_containers=$(docker ps -q | wc -l | tr -d ' ')
    if [[ $running_containers -gt 0 ]]; then
        docker_info=" 🐳:${running_containers}"
    fi
fi

# Node.jsプロジェクトの場合のnpmスクリプト情報
npm_info=""
if [[ -f "package.json" ]] && command -v node >/dev/null 2>&1; then
    node_version=$(node -v 2>/dev/null | sed 's/v//')
    if [[ -n "$node_version" ]]; then
        npm_info=" Node:${node_version}"
    fi
fi

# Goプロジェクトの場合
go_info=""
if [[ -f "go.mod" ]] && command -v go >/dev/null 2>&1; then
    go_version=$(go version 2>/dev/null | awk '{print $3}' | sed 's/go//')
    if [[ -n "$go_version" ]]; then
        go_info=" Go:${go_version}"
    fi
fi

# ステータス情報を組み合わせて出力（色付き）
echo -e "${CYAN}${current_time}${NC} ${BLUE}${current_dir}${NC}${GREEN}${git_info}${NC}${YELLOW}${system_info}${NC}${docker_info}${npm_info}${go_info}"
