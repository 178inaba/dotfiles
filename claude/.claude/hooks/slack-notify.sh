#!/bin/bash

# 環境変数チェック
if [[ -z "${CLAUDE_SLACK_WEBHOOK}" ]]; then
  exit 0
fi

INPUT="$(cat)"
MESSAGE="$(jq -r '.message // empty' <<<"${INPUT}")"
TYPE="$(jq -r '.notification_type // "notification"' <<<"${INPUT}")"
CWD="$(jq -r '.cwd // empty' <<<"${INPUT}")"

# worktree では cwd の basename だと worktree 名しか出ず、どのプロジェクトの
# 通知か分からないため、本体リポジトリ名を git-common-dir から求めて
# "リポジトリ名:worktree名" 形式にする（本体ツリー内はリポジトリ名のみ）
project_label() {
  local cwd="$1" git_paths toplevel common main_root label wt
  if [[ -z "${cwd}" ]]; then
    return
  fi
  if ! git_paths="$(git -C "${cwd}" rev-parse --path-format=absolute --show-toplevel --git-common-dir 2>/dev/null)"; then
    basename "${cwd}"
    return
  fi
  toplevel="${git_paths%%$'\n'*}"
  common="${git_paths##*$'\n'}"
  main_root="$(dirname "${common}")"
  # ".git" 付きの common dir を剥がしてから basename すると
  # 通常リポジトリ（"repo/.git"）と bare（"repo.git"）が同じ式で扱える
  label="$(basename "${common%.git}")"
  if [[ "${toplevel}" == "${main_root}" ]]; then
    printf '%s\n' "${label}"
    return
  fi
  wt="${toplevel#"${main_root}"/.claude/worktrees/}"
  if [[ "${wt}" == "${toplevel}" ]]; then
    wt="$(basename "${toplevel}")"
  fi
  printf '%s:%s\n' "${label}" "${wt}"
}

PROJECT="$(project_label "${CWD}")"

if [[ -n "${MESSAGE}" ]]; then
  curl -X POST \
    -H 'Content-type: application/json' \
    --data "$(jq -nc --arg text "[${PROJECT}] (${TYPE}) ${MESSAGE}" '{text: $text}')" \
    "${CLAUDE_SLACK_WEBHOOK}"
fi
