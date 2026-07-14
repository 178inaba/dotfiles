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
  local cwd="$1" toplevel common main_root label wt
  if [[ -z "${cwd}" ]]; then
    return
  fi
  toplevel="$(git -C "${cwd}" rev-parse --show-toplevel 2>/dev/null)"
  if [[ -z "${toplevel}" ]]; then
    basename "${cwd}"
    return
  fi
  common="$(git -C "${cwd}" rev-parse --path-format=absolute --git-common-dir 2>/dev/null)"
  main_root="$(dirname "${common}")"
  label="$(basename "${common}")"
  if [[ "${label}" == ".git" ]]; then
    label="$(basename "${main_root}")"
  else
    # bare リポジトリは common dir 自体が "<名前>.git"
    label="${label%.git}"
  fi
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
