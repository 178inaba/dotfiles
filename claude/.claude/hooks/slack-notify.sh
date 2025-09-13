#!/bin/bash

# 環境変数チェック
if [[ -z "${CLAUDE_SLACK_WEBHOOK}" ]]; then
  exit 0
fi

# 標準入力からJSONを読み取り、messageフィールドを抽出
MESSAGE="$(cat | jq -r '.message // empty')"

if [[ -n "${MESSAGE}" ]]; then
  curl -X POST \
    -H 'Content-type: application/json' \
    --data "{\"text\": \"${MESSAGE}\"}" \
    "${CLAUDE_SLACK_WEBHOOK}"
fi
