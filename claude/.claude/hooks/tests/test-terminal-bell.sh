#!/bin/bash

# terminal-bell.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-terminal-bell.sh
# 失敗したケースがあれば exit 1 で終了する。
#
# 実環境には触れない（端末への出力は terminalSequence を受けた Claude Code が
# 担うため、スクリプト自体は stdout に JSON を出すだけ）。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../terminal-bell.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

TMP_BASE=$(mktemp -d)
trap 'rm -rf "$TMP_BASE"' EXIT
OUT="$TMP_BASE/stdout.log"

pass=0
fail=0

check() {
  local name=$1 rc=$2
  if [ "$rc" -eq 0 ]; then
    pass=$((pass + 1)); printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1)); printf 'FAIL  %s\n' "$name"
  fi
}

"$HOOK" > "$OUT" 2>/dev/null
check 'exit 0' "$?"

[ "$(wc -l < "$OUT")" -eq 1 ]
check 'stdout is a single line' "$?"

jq -e . "$OUT" >/dev/null 2>&1
check 'stdout is valid JSON' "$?"

[ "$(jq -r '.terminalSequence' "$OUT")" = "$(printf '\a')" ]
check 'terminalSequence decodes to a single BEL' "$?"

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
