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

GIT_CACHE_FILE="/tmp/claude-statusline-git-cache"
GIT_CACHE_MAX_AGE=5

# コストが表示閾値（1セント）以上か判定
cost_above_threshold() {
    local cost="$1"
    local cost_cents=$(awk '{printf "%.0f", $1 * 100}' <<< "$cost")
    [[ $cost_cents -ge 1 ]]
}

# 閾値ベース色判定（90%→赤、70%→黄、それ以外→緑）
threshold_color() {
    local pct="$1"
    if [[ $pct -ge 90 ]]; then
        echo "$RED"
    elif [[ $pct -ge 70 ]]; then
        echo "$YELLOW"
    else
        echo "$GREEN"
    fi
}

# 秒数を人間可読な時間文字列に変換
format_hm() {
    local total_sec="$1"
    [[ $total_sec -le 0 ]] && return
    local days=$((total_sec / 86400))
    local hours=$(( (total_sec % 86400) / 3600 ))
    local mins=$(( (total_sec % 3600) / 60 ))
    if [[ $days -gt 0 ]]; then
        echo "${days}d${hours}h"
    elif [[ $hours -gt 0 ]]; then
        echo "${hours}h${mins}m"
    elif [[ $mins -gt 0 ]]; then
        echo "${mins}m"
    fi
}

# リセットまでのカウントダウン文字列
format_countdown() {
    local resets_at="$1" now="$2"
    [[ -z "$resets_at" ]] && return
    resets_at="${resets_at%%.*}"
    local remaining=$((resets_at - now))
    format_hm "$remaining"
}

# Git情報取得（キャッシュ付き）
get_git_info() {
    local cache_key="$1" now="$2"

    if [[ -f "$GIT_CACHE_FILE" ]]; then
        local cache_age cached_key cached_result
        cache_age=$(( now - $(stat -f %m "$GIT_CACHE_FILE" 2>/dev/null || stat -c %Y "$GIT_CACHE_FILE" 2>/dev/null || echo 0) ))
        if [[ $cache_age -le $GIT_CACHE_MAX_AGE ]]; then
            {
                IFS= read -r cached_key
                IFS= read -r cached_result
            } < "$GIT_CACHE_FILE"
            if [[ "$cached_key" == "$cache_key" ]]; then
                echo "$cached_result"
                return
            fi
        fi
    fi

    local result=""
    if git rev-parse --git-dir >/dev/null 2>&1; then
        local branch=$(git branch --show-current 2>/dev/null)
        local git_status=""

        local porcelain_output=$(git --no-optional-locks status --porcelain 2>/dev/null)
        if [[ -n "$porcelain_output" ]]; then
            local staged_count=0
            local modified_count=0
            local line
            while IFS= read -r line; do
                [[ "${line:0:1}" != " " && "${line:0:1}" != "?" ]] && ((staged_count++))
                [[ "${line:1:1}" != " " && "${line:1:1}" != "?" ]] && ((modified_count++))
            done <<< "$porcelain_output"

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
            local ahead=$(git rev-list --count "HEAD" "^${remote_branch}" 2>/dev/null || echo 0)
            local behind=$(git rev-list --count "${remote_branch}" "^HEAD" 2>/dev/null || echo 0)

            if [[ $ahead -gt 0 ]]; then
                sync_status=" ↑${ahead}"
            fi
            if [[ $behind -gt 0 ]]; then
                sync_status="${sync_status} ↓${behind}"
            fi
        fi

        result=" (${branch}${git_status}${sync_status})"
    fi

    printf '%s\n%s' "$cache_key" "$result" > "$GIT_CACHE_FILE"
    echo "$result"
}

main() {
    local input=$(cat 2>/dev/null || echo "")
    local date_output=$(date +"%s %H:%M")
    local now="${date_output%% *}"
    local current_time="${date_output#* }"
    local current_dir="" project_dir="" model_name="" total_cost=""
    local used_pct="" duration_ms="" five_h="" seven_d=""
    local five_h_resets="" seven_d_resets=""

    if [[ -n "$input" ]] && command -v jq >/dev/null 2>&1; then
        # $()が末尾の空行を除去するため、末尾フィールドが空の場合はreadがEOFで空文字列を返す
        local jq_output
        jq_output=$(jq -r '[
            (.workspace.current_dir // ""),
            (.workspace.project_dir // ""),
            (.model.display_name // ""),
            (.cost.total_cost_usd // ""),
            (.context_window.used_percentage // ""),
            (.cost.total_duration_ms // ""),
            (.rate_limits.five_hour.used_percentage // ""),
            (.rate_limits.seven_day.used_percentage // ""),
            (.rate_limits.five_hour.resets_at // ""),
            (.rate_limits.seven_day.resets_at // "")
        ] | map(tostring) | .[]' <<< "$input" 2>/dev/null)
        {
            IFS= read -r current_dir
            IFS= read -r project_dir
            IFS= read -r model_name
            IFS= read -r total_cost
            IFS= read -r used_pct
            IFS= read -r duration_ms
            IFS= read -r five_h
            IFS= read -r seven_d
            IFS= read -r five_h_resets
            IFS= read -r seven_d_resets
        } <<< "$jq_output"
    fi

    current_dir="${current_dir:-$PWD}"
    project_dir="${project_dir:-$current_dir}"

    # --- ディレクトリ ---
    local display_project="${project_dir/#$HOME/~}"
    local display_dir="$display_project"
    if [[ "$current_dir" != "$project_dir" ]]; then
        local display_current="${current_dir/#$HOME/~}"
        display_dir="${display_project} > ${display_current}"
    fi

    # --- Git ---
    local git_info=$(get_git_info "$current_dir" "$now")

    # --- モデル + コスト ---
    local model_str=""
    local cost_str=""
    if [[ -n "$model_name" ]]; then
        model_str=" [${model_name}]"
        if [[ -n "$total_cost" ]] && cost_above_threshold "$total_cost"; then
            cost_str=" \$$(printf "%.2f" "$total_cost")"
        fi
    fi

    # --- コンテキスト ---
    local ctx_bar=""
    if [[ -n "$used_pct" ]]; then
        used_pct="${used_pct%%.*}"
        local ctx_color=$(threshold_color "$used_pct")
        local bar_width=10
        local filled=$((used_pct * bar_width / 100))
        local empty=$((bar_width - filled))
        local bar="" i
        for ((i=0; i<filled; i++)); do bar+="▓"; done
        for ((i=0; i<empty; i++)); do bar+="░"; done
        ctx_bar=" ${ctx_color}${bar} ${used_pct}%${NC}"
    fi

    # --- 経過時間 ---
    local duration_str=""
    if [[ -n "$duration_ms" ]]; then
        duration_ms="${duration_ms%%.*}"
        local dur=$(format_hm "$((duration_ms / 1000))")
        [[ -n "$dur" ]] && duration_str=" ${CYAN}${dur}${NC}"
    fi

    # --- レートリミット ---
    local rate_str=""
    if [[ -n "$five_h" ]] || [[ -n "$seven_d" ]]; then
        if [[ -n "$five_h" ]]; then
            local five_h_int=$(printf "%.0f" "$five_h")
            local color=$(threshold_color "$five_h_int")
            local countdown=$(format_countdown "$five_h_resets" "$now")
            rate_str="${color}5h:${five_h_int}%${NC}"
            [[ -n "$countdown" ]] && rate_str="${rate_str}(${countdown})"
        fi
        if [[ -n "$seven_d" ]]; then
            local seven_d_int=$(printf "%.0f" "$seven_d")
            local color=$(threshold_color "$seven_d_int")
            local countdown=$(format_countdown "$seven_d_resets" "$now")
            [[ -n "$rate_str" ]] && rate_str="${rate_str} "
            rate_str="${rate_str}${color}7d:${seven_d_int}%${NC}"
            [[ -n "$countdown" ]] && rate_str="${rate_str}(${countdown})"
        fi
        rate_str=" ${rate_str}"
    fi

    # --- 組み立て（コンテキスト→識別→残量→消費） ---
    local output="${CYAN}${current_time}${NC} ${BLUE}${display_dir}${NC}${GREEN}${git_info}${NC}"
    output="${output}${PURPLE}${model_str}${NC}${ctx_bar}${rate_str}${CYAN}${cost_str}${NC}${duration_str}"
    echo -e "$output"
}

main
