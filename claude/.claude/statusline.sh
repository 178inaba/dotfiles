#!/bin/bash

# Claude Code Status Line Script

# 色定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
PURPLE='\033[0;35m'
NC='\033[0m'

# コスト比較（bc依存性除去）
cost_above_threshold() {
    local cost="$1"
    local cost_cents=$(echo "$cost" | awk '{printf "%.0f", $1 * 100}')
    [[ $cost_cents -ge 1 ]]
}

# Git情報取得
get_git_info() {
    if ! git rev-parse --git-dir >/dev/null 2>&1; then
        return
    fi

    local branch=$(git branch --show-current 2>/dev/null)
    local git_status=""

    local porcelain_output=$(git --no-optional-locks status --porcelain 2>/dev/null)
    if [[ -n "$porcelain_output" ]]; then
        local staged_count=$(git --no-optional-locks diff --cached --name-only 2>/dev/null | wc -l | tr -d ' ')
        local modified_count=$(git --no-optional-locks diff --name-only 2>/dev/null | wc -l | tr -d ' ')

        if [[ $staged_count -gt 0 ]]; then
            git_status=" +${staged_count}"
        fi
        if [[ $modified_count -gt 0 ]]; then
            git_status="${git_status} ~${modified_count}"
        fi
    fi

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

    echo " (${branch}${git_status}${sync_status})"
}

main() {
    local input=$(cat 2>/dev/null || echo "")

    local current_time=$(date +"%H:%M")
    local current_dir="${PWD/#$HOME/~}"
    local git_info=$(get_git_info)

    local output="${CYAN}${current_time}${NC} ${BLUE}${current_dir}${NC}${GREEN}${git_info}${NC}"

    if [[ -n "$input" ]] && command -v jq >/dev/null 2>&1; then
        local model_name=$(echo "$input" | jq -r '.model.display_name // empty' 2>/dev/null)
        local total_cost=$(echo "$input" | jq -r '.cost.total_cost_usd // empty' 2>/dev/null)
        local used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty' 2>/dev/null)
        local duration_ms=$(echo "$input" | jq -r '.cost.total_duration_ms // empty' 2>/dev/null)

        if [[ -n "$model_name" ]]; then
            local claude_info=" [${model_name}]"
            if [[ -n "$total_cost" ]] && cost_above_threshold "$total_cost"; then
                claude_info="${claude_info} \$$(printf "%.2f" "$total_cost")"
            fi
            output="${output}${PURPLE}${claude_info}${NC}"
        fi

        if [[ -n "$used_pct" ]]; then
            local bar_width=10
            local filled=$((used_pct * bar_width / 100))
            local empty=$((bar_width - filled))
            local bar=""
            [[ $filled -gt 0 ]] && bar=$(printf "%${filled}s" | tr ' ' '▓')
            [[ $empty -gt 0 ]] && bar="${bar}$(printf "%${empty}s" | tr ' ' '░')"

            local ctx_color="$GREEN"
            if [[ $used_pct -ge 90 ]]; then
                ctx_color="$RED"
            elif [[ $used_pct -ge 70 ]]; then
                ctx_color="$YELLOW"
            fi
            output="${output} ${ctx_color}${bar} ${used_pct}%${NC}"
        fi

        if [[ -n "$duration_ms" ]]; then
            local total_sec=$((duration_ms / 1000))
            local hours=$((total_sec / 3600))
            local mins=$(( (total_sec % 3600) / 60 ))
            local duration_str=""
            if [[ $hours -gt 0 ]]; then
                duration_str="${hours}h${mins}m"
            elif [[ $mins -gt 0 ]]; then
                duration_str="${mins}m"
            fi
            if [[ -n "$duration_str" ]]; then
                output="${output} ${CYAN}${duration_str}${NC}"
            fi
        fi
    fi

    echo -e "$output"
}

main
