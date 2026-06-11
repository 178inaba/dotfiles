#!/bin/bash

# 環境変数チェック
if [[ -z "${CLAUDE_SLACK_WEBHOOK}" ]]; then
  exit 0
fi

INPUT="$(cat)"
MESSAGE="$(jq -r '.message // empty' <<<"${INPUT}")"
TYPE="$(jq -r '.notification_type // "notification"' <<<"${INPUT}")"
PROJECT="$(basename "$(jq -r '.cwd // empty' <<<"${INPUT}")")"

if [[ -n "${MESSAGE}" ]]; then
  curl -X POST \
    -H 'Content-type: application/json' \
    --data "$(jq -nc --arg text "[${PROJECT}] (${TYPE}) ${MESSAGE}" '{text: $text}')" \
    "${CLAUDE_SLACK_WEBHOOK}"
fi
