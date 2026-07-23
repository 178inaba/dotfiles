#!/bin/bash

# Claude Code Status Line Script

# 色定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
PURPLE='\033[0;35m'
GRAY='\033[0;90m'
UNDERLINE='\033[4m'
UNDERLINE_OFF='\033[24m'
# PR 表示用の固定色（256色はテーマによるパレット再割り当てを受けない）。
# ANSI 90 のグレーや 1;33 の黄は Solarized 系テーマで別系統の色に再割り当て
# されるため、意図した見た目を保てる 256色で指定する
PR_GRAY='\033[38;5;246m'   # #949494
PR_YELLOW='\033[38;5;220m' # #ffd700
NC='\033[0m'

# テストから差し替え可能
GIT_CACHE_BASE="${GIT_CACHE_BASE:-/tmp/claude-statusline-git-cache}"
# settings.json の statusLine.refreshInterval と揃える（1更新サイクルあたり git 実行を最大1回に保つ）
GIT_CACHE_MAX_AGE=5

# テストから差し替え可能
USD_JPY_CACHE_FILE="${USD_JPY_CACHE_FILE:-/tmp/claude-statusline-usd-jpy}"
USD_JPY_API_URL="${USD_JPY_API_URL:-https://api.frankfurter.dev/v1/latest?base=USD&symbols=JPY}"
CURL_BIN="${CURL_BIN:-curl}"
# レートは全セッション共有のグローバルキャッシュ（セッション単位だと並列セッションで
# 重複フェッチし、長寿命セッションで古いレートに固定されるため）。
# Frankfurter（ECB公表値）の更新は平日1日1回なので半日TTLで十分
USD_JPY_CACHE_MAX_AGE=43200
# 取得失敗時の再試行間隔（オフライン時に5秒ごとの描画のたびに curl を投げない）
USD_JPY_RETRY_INTERVAL=60

# テストから差し替え可能
PR_CACHE_BASE="${PR_CACHE_BASE:-/tmp/claude-statusline-pr-cache}"
GH_BIN="${GH_BIN:-gh}"
# フッターの PR バッジ相当の更新間隔
PR_CACHE_MAX_AGE=60
# フェッチ中・失敗直後の再スポーン抑止（gh の多重起動防止）
PR_RETRY_INTERVAL=60

NUM_RE='^[0-9]+(\.[0-9]+)?$'

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

# USD/JPY レート取得（stale-while-revalidate）。5秒ごとの描画がホットパスのため
# 同期のネットワークアクセスは行わず、キャッシュが無い/古い場合はバックグラウンド更新を
# 投げて手元の値をそのまま返す。値が無ければ空を返し、呼び出し側で USD 表示にフォールバック
get_usd_jpy_rate() {
    local now="$1"
    local cached_at="" rate=""
    if [[ -f "$USD_JPY_CACHE_FILE" ]]; then
        {
            IFS= read -r cached_at
            IFS= read -r rate
        } < "$USD_JPY_CACHE_FILE"
        [[ "$rate" =~ $NUM_RE ]] || rate=""
    fi

    if [[ -z "$rate" ]] || ! [[ "$cached_at" =~ ^[0-9]+$ ]] || (( now - cached_at > USD_JPY_CACHE_MAX_AGE )); then
        local attempt_file="${USD_JPY_CACHE_FILE}.attempt" attempted_at=""
        [[ -f "$attempt_file" ]] && IFS= read -r attempted_at < "$attempt_file"
        if ! [[ "$attempted_at" =~ ^[0-9]+$ ]] || (( now - attempted_at > USD_JPY_RETRY_INTERVAL )); then
            echo "$now" > "$attempt_file"
            # サブシェル + 全FDの切り離しで完全にデタッチする（stdout を継承すると
            # 呼び出し元のコマンド置換が curl 完了まで EOF 待ちでブロックする）
            ( refresh_usd_jpy_cache "$now" </dev/null >/dev/null 2>&1 & )
        fi
    fi
    echo "$rate"
}

# キャッシュ更新（バックグラウンド実行前提）。取得失敗・不正値なら何も書かない
refresh_usd_jpy_cache() {
    local now="$1" rate
    rate=$("$CURL_BIN" -s --max-time 5 "$USD_JPY_API_URL" 2>/dev/null | jq -r '.rates.JPY // empty' 2>/dev/null)
    [[ "$rate" =~ $NUM_RE ]] || return
    printf '%s\n%s\n' "$now" "$rate" > "${USD_JPY_CACHE_FILE}.$$" \
        && mv "${USD_JPY_CACHE_FILE}.$$" "$USD_JPY_CACHE_FILE"
}

# PR 情報取得（stale-while-revalidate。方式は get_usd_jpy_rate と同じ）。
# Claude Code フッターの PR バッジはポーラーが1回の遅延フェッチ（>4s）で無音・恒久 disable
# される既知バグがあり（anthropics/claude-code#80209。stdin JSON の pr.* は同バッジのミラー
# のため一緒に消える）、意図的な冗長化として自前で gh フェッチする。修正リリース確認後に要再判断。
# キャッシュキーは作業ディレクトリ+ブランチ（ブランチ切替で即無効化、同一ディレクトリ間では共有）。
# 返り値は "<PR番号> <状態> <URL>"（状態: DRAFT / APPROVED / CHANGES_REQUESTED / NONE）または空
get_pr_info() {
    local cache_key="$1" branch="$2" now="$3"
    local cache_file="${PR_CACHE_BASE}-${cache_key//\//_}"
    cache_file="${cache_file:0:200}"

    local cached_at="" cached_key="" cached_result=""
    if [[ -f "$cache_file" ]]; then
        {
            IFS= read -r cached_at
            IFS= read -r cached_key
            IFS= read -r cached_result
        } < "$cache_file"
        [[ "$cached_key" == "$cache_key" ]] || { cached_at=""; cached_result=""; }
    fi

    if ! [[ "$cached_at" =~ ^[0-9]+$ ]] || (( now - cached_at > PR_CACHE_MAX_AGE )); then
        local attempt_file="${cache_file}.attempt" attempted_at=""
        [[ -f "$attempt_file" ]] && IFS= read -r attempted_at < "$attempt_file"
        if ! [[ "$attempted_at" =~ ^[0-9]+$ ]] || (( now - attempted_at > PR_RETRY_INTERVAL )); then
            echo "$now" > "$attempt_file"
            # サブシェル + 全FDの切り離しで完全にデタッチする（get_usd_jpy_rate と同じ理由）
            ( refresh_pr_cache "$cache_key" "$branch" "$cache_file" "$now" </dev/null >/dev/null 2>&1 & )
        fi
    fi
    echo "$cached_result"
}

# デフォルトブランチ判定（バックグラウンド実行前提）。clone 時に設定される origin/HEAD を
# 正とし、未設定なら gh にフォールバック。どちらも不明なら非デフォルト扱い（表示する側に倒す）
is_default_branch() {
    local branch="$1" default_branch
    default_branch=$(git symbolic-ref -q --short refs/remotes/origin/HEAD 2>/dev/null)
    default_branch="${default_branch#origin/}"
    [[ -z "$default_branch" ]] \
        && default_branch=$("$GH_BIN" repo view --json defaultBranchRef --jq '.defaultBranchRef.name' 2>/dev/null)
    [[ -n "$default_branch" && "$branch" == "$default_branch" ]]
}

# PR キャッシュ更新（バックグラウンド実行前提）。デフォルトブランチは release PR 等の head に
# なっていてもブランチ固有の作業文脈ではないため「無し」扱いし、フェッチ自体をスキップする。
# gh 失敗（PR 無し・オフライン・未認証等。gh の exit code では区別できない）も「無し」として
# キャッシュし、TTL で再試行を抑える
refresh_pr_cache() {
    local cache_key="$1" branch="$2" cache_file="$3" now="$4"
    local pr_json result=""
    if ! is_default_branch "$branch"; then
        pr_json=$("$GH_BIN" pr view --json number,reviewDecision,state,isDraft,url 2>/dev/null)
        if [[ -n "$pr_json" ]]; then
            result=$(jq -r 'select(.state == "OPEN")
                | "\(.number) \(if .isDraft then "DRAFT"
                    elif (.reviewDecision // "") == "" then "NONE"
                    else .reviewDecision end) \(.url // "")"' <<< "$pr_json" 2>/dev/null)
        fi
    fi
    printf '%s\n%s\n%s' "$now" "$cache_key" "$result" > "${cache_file}.$$" \
        && mv "${cache_file}.$$" "$cache_file"
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
    local five_h_resets="" seven_d_resets="" session_id=""

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
            (.rate_limits.seven_day.resets_at // ""),
            (.session_id // "")
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
            IFS= read -r session_id
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

    # --- PR ---
    local pr_str=""
    if [[ -n "$git_info" ]]; then
        # git_info は " (branch[ +N][ ~N][ ↑N][ ↓N])" 形式（branch 名に空白は入らない）
        local branch="${git_info#" ("}"
        branch="${branch%%)*}"
        branch="${branch%% *}"
        if [[ -n "$branch" ]]; then
            local pr_info=$(get_pr_info "${current_dir}:${branch}" "$branch" "$now")
            local pr_number="" pr_state="" pr_url=""
            read -r pr_number pr_state pr_url <<< "$pr_info"
            if [[ "$pr_number" =~ ^[0-9]+$ ]]; then
                # "PR " は固定グレー、番号部分を本家フッターバッジの色ドットと同じ
                # マッピングで色分けする（緑=approved / 黄=pending review /
                # 赤=changes requested / draft=無色 = テーマのデフォルト前景色）
                local pr_color="$NC"
                case "$pr_state" in
                    APPROVED) pr_color="$GREEN" ;;
                    CHANGES_REQUESTED) pr_color="$RED" ;;
                    DRAFT) ;;
                    *) pr_color="$PR_YELLOW" ;;
                esac
                local pr_text="#${pr_number}"
                # 下線付きの番号部分のみ OSC 8 ハイパーリンク（Cmd+クリックで PR を開く）にし、
                # 下線 = クリック可能範囲としてフッターバッジと見た目を揃える。非対応ターミナル
                # では Claude Code 側でプレーンテキスト表示にフォールバックされる
                [[ -n "$pr_url" ]] && pr_text="\033]8;;${pr_url}\a${UNDERLINE}#${pr_number}${UNDERLINE_OFF}\033]8;;\a"
                pr_str=" ${PR_GRAY}PR ${pr_color}${pr_text}${NC}"
            fi
        fi
    fi

    # --- モデル + コスト ---
    local model_str=""
    local cost_str=""
    if [[ -n "$model_name" ]]; then
        model_str=" [${model_name}]"
        if [[ -n "$total_cost" ]] && cost_above_threshold "$total_cost"; then
            local usd_jpy=$(get_usd_jpy_rate "$now")
            if [[ -n "$usd_jpy" ]]; then
                cost_str=" ¥$(awk -v c="$total_cost" -v r="$usd_jpy" 'BEGIN {printf "%.0f", c * r}')"
            else
                cost_str=" \$$(printf "%.2f" "$total_cost")"
            fi
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

    # --- 組み立て（パス / ブランチ+セッションID / セッション情報 の3行） ---
    local output="${BLUE}${display_dir}${NC}"
    # セッションIDはトランスクリプト調査・resume 用にセッション中でも参照できるよう常時表示。
    # 1行目は2パス表示（project > current）で長くなりうるため、通常最短のブランチ行側に置き、
    # 非 git ディレクトリでもIDが消えないよう単独でも2行目を出す
    local line2=""
    [[ -n "$git_info" ]] && line2="${GREEN}${git_info# }${NC}${pr_str}"
    if [[ -n "$session_id" ]]; then
        [[ -n "$line2" ]] && line2="${line2} "
        line2="${line2}${GRAY}${session_id}${NC}"
    fi
    [[ -n "$line2" ]] && output="${output}\n${line2}"
    output="${output}\n${PURPLE}${model_str# }${NC}${ctx_bar}${rate_str}${CYAN}${cost_str}${NC}${duration_str}"
    printf '%b\n' "$output"
}

main
