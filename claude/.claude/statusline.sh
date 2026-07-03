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

# テストから差し替え可能
GIT_CACHE_BASE="${GIT_CACHE_BASE:-/tmp/claude-statusline-git-cache}"
# settings.json の statusLine.refreshInterval と揃える（1更新サイクルあたり git 実行を最大1回に保つ）
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
    # 作業ディレクトリ単位のファイルに分離（並列セッションの上書き競合防止、同一ディレクトリ間では共有）。
    # 長いパスでのファイル名長超過に備えて切り詰め、衝突はファイル内のキー照合で吸収する
    local cache_file="${GIT_CACHE_BASE}-${cache_key//\//_}"
    cache_file="${cache_file:0:200}"

    if [[ -f "$cache_file" ]]; then
        # 1行目に書き込み時刻を持たせ、鮮度判定のための stat 呼び出しを不要にする
        local cached_at cached_key cached_result
        {
            IFS= read -r cached_at
            IFS= read -r cached_key
            IFS= read -r cached_result
        } < "$cache_file"
        if [[ "$cached_at" =~ ^[0-9]+$ ]] && (( now - cached_at <= GIT_CACHE_MAX_AGE )) \
            && [[ "$cached_key" == "$cache_key" ]]; then
            echo "$cached_result"
            return
        fi
    fi

    # 5秒ごとの定期実行がホットパスのため、branch名・ahead/behind・staged/modified を
    # porcelain v2 の1回の git 呼び出しでまとめて取得する（旧実装は最大6プロセス起動していた）
    local result="" status_output=""
    if status_output=$(git --no-optional-locks status --porcelain=v2 --branch 2>/dev/null); then
        local branch="" ahead=0 behind=0 staged_count=0 modified_count=0
        local line ab xy
        while IFS= read -r line; do
            case "$line" in
                "# branch.head "*)
                    branch="${line#"# branch.head "}"
                    [[ "$branch" == "(detached)" ]] && branch=""
                    ;;
                "# branch.ab "*)
                    # 形式: "+<ahead> -<behind>"（upstream がある場合のみ出現）
                    ab="${line#"# branch.ab "}"
                    ahead="${ab%% *}"; ahead="${ahead#+}"
                    behind="${ab##* }"; behind="${behind#-}"
                    ;;
                [12u]*)
                    # 1=変更 2=リネーム u=コンフリクト。XY の "." 以外がステージ/未ステージ変更
                    xy="${line:2:2}"
                    [[ "${xy:0:1}" != "." ]] && ((staged_count++))
                    [[ "${xy:1:1}" != "." ]] && ((modified_count++))
                    ;;
            esac
        done <<< "$status_output"

        local git_status=""
        if [[ $staged_count -gt 0 ]]; then
            git_status=" +${staged_count}"
        fi
        if [[ $modified_count -gt 0 ]]; then
            git_status="${git_status} ~${modified_count}"
        fi

        local sync_status=""
        if [[ $ahead -gt 0 ]]; then
            sync_status=" ↑${ahead}"
        fi
        if [[ $behind -gt 0 ]]; then
            sync_status="${sync_status} ↓${behind}"
        fi

        result=" (${branch}${git_status}${sync_status})"
    fi

    printf '%s\n%s\n%s' "$now" "$cache_key" "$result" > "$cache_file"
    echo "$result"
}

main() {
    local input=$(cat 2>/dev/null || echo "")
    local now=$(date +%s)
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

    # --- 組み立て（パス / ブランチ / セッション情報 の3行） ---
    local output="${BLUE}${display_dir}${NC}"
    [[ -n "$git_info" ]] && output="${output}\n${GREEN}${git_info# }${NC}"
    output="${output}\n${PURPLE}${model_str# }${NC}${ctx_bar}${rate_str}${CYAN}${cost_str}${NC}${duration_str}"
    printf '%b\n' "$output"
}

main
