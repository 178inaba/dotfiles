#!/bin/bash

# Claude Code Status Line Script
# 開発生産性向上のための包括的ステータス表示

# 色定義（視認性向上）
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

# ユーティリティ関数
safe_command() {
    command -v "$1" >/dev/null 2>&1
}

# コスト比較（bc依存性除去）
cost_above_threshold() {
    local cost="$1"
    local threshold="0.01"

    # 小数点を整数に変換して比較（0.01 = 1セント）
    local cost_cents=$(echo "$cost" | awk '{printf "%.0f", $1 * 100}')
    local threshold_cents=$(echo "$threshold" | awk '{printf "%.0f", $1 * 100}')

    [[ $cost_cents -ge $threshold_cents ]]
}

# Claude Code情報取得
get_claude_info() {
    local claude_info=""

    if [ -p /dev/stdin ]; then
        local input=$(timeout 0.1s cat 2>/dev/null || echo "")
        if [[ -n "$input" ]] && safe_command jq; then
            local model_name=$(echo "$input" | jq -r '.model.display_name // empty' 2>/dev/null)
            local total_cost=$(echo "$input" | jq -r '.cost.total_cost_usd // empty' 2>/dev/null)

            if [[ -n "$model_name" ]]; then
                claude_info=" [${model_name}]"

                # コストが0.01以上の場合のみ表示（小額は省略）
                if [[ -n "$total_cost" ]] && cost_above_threshold "$total_cost"; then
                    local cost_display=$(printf "%.2f" "$total_cost")
                    claude_info="${claude_info} \$${cost_display}"
                fi
            fi
        fi
    fi

    echo "$claude_info"
}

# Git情報取得（パフォーマンス最適化）
get_git_info() {
    local git_info=""

    if ! git rev-parse --git-dir >/dev/null 2>&1; then
        echo "$git_info"
        return
    fi

    local branch=$(git branch --show-current 2>/dev/null)
    local git_status=""

    # git status --porcelainの結果を一度だけ取得
    local porcelain_output=$(git status --porcelain 2>/dev/null)
    if [[ -n "$porcelain_output" ]]; then
        # ステージされた変更と未ステージ変更を効率的にカウント
        local staged_count=$(git diff --cached --name-only 2>/dev/null | wc -l | tr -d ' ')
        local modified_count=$(git diff --name-only 2>/dev/null | wc -l | tr -d ' ')

        if [[ $staged_count -gt 0 ]]; then
            git_status=" +${staged_count}"  # ステージされた変更
        fi
        if [[ $modified_count -gt 0 ]]; then
            git_status="${git_status} ~${modified_count}"  # 未ステージの変更
        fi
    fi

    # リモートとの同期状況（ahead/behind）
    local sync_status=""
    local remote_branch=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null)
    if [[ -n "$remote_branch" ]]; then
        local ahead=$(git rev-list --count HEAD ^${remote_branch} 2>/dev/null || echo 0)
        local behind=$(git rev-list --count ${remote_branch} ^HEAD 2>/dev/null || echo 0)

        if [[ $ahead -gt 0 ]]; then
            sync_status=" ↑${ahead}"
        fi
        if [[ $behind -gt 0 ]]; then
            sync_status="${sync_status} ↓${behind}"
        fi
    fi

    git_info=" (${branch}${git_status}${sync_status})"
    echo "$git_info"
}

# システムリソース情報取得
get_system_info() {
    local system_info=""

    # CPU使用率
    if safe_command top; then
        local cpu_usage=$(top -l 1 -n 0 2>/dev/null | grep "CPU usage" | awk '{print $3}' | sed 's/%//')
        if [[ -n "$cpu_usage" && "$cpu_usage" != "0.0" ]]; then
            system_info=" CPU:${cpu_usage}%"
        fi
    fi

    # メモリ使用率
    if safe_command memory_pressure; then
        local memory_free_pct=$(memory_pressure 2>/dev/null | grep "System-wide memory free percentage:" | awk '{print $5}' | sed 's/%//')
        if [[ -n "$memory_free_pct" ]]; then
            local memory_used_pct=$((100 - memory_free_pct))
            system_info="${system_info} MEM:${memory_used_pct}%"
        fi
    fi

    echo "$system_info"
}

# 開発コンテキスト情報取得
get_dev_context() {
    local context_info=""

    # Docker情報
    if safe_command docker && docker info >/dev/null 2>&1; then
        local running_containers=$(docker ps -q 2>/dev/null | wc -l | tr -d ' ')
        if [[ $running_containers -gt 0 ]]; then
            context_info=" 🐳:${running_containers}"
        fi
    fi

    # Node.js情報
    if [[ -f "package.json" ]] && safe_command node; then
        local node_version=$(node -v 2>/dev/null | sed 's/v//')
        if [[ -n "$node_version" ]]; then
            context_info="${context_info} Node:${node_version}"
        fi
    fi

    # Go情報
    if [[ -f "go.mod" ]] && safe_command go; then
        local go_version=$(go version 2>/dev/null | awk '{print $3}' | sed 's/go//')
        if [[ -n "$go_version" ]]; then
            context_info="${context_info} Go:${go_version}"
        fi
    fi

    echo "$context_info"
}

# メイン処理
main() {
    # 基本情報
    local current_time=$(date +"%H:%M")
    local current_dir="${PWD/#$HOME/~}"

    # 各セクション情報を取得
    local claude_info=$(get_claude_info)
    local git_info=$(get_git_info)
    local system_info=$(get_system_info)
    local dev_context=$(get_dev_context)

    # ステータス情報を組み合わせて出力（色付き）
    echo -e "${CYAN}${current_time}${NC} ${BLUE}${current_dir}${NC}${GREEN}${git_info}${NC}${PURPLE}${claude_info}${NC}${YELLOW}${system_info}${NC}${dev_context}"
}

# 実行
main
