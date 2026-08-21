#!/bin/bash

# inuse-lib.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/cleanup-merged/tests/test-inuse-lib.sh
# 実プロセス（バックグラウンドの sleep）と実 lsof を使うが、mktemp 配下で完結し
# 実リポジトリ・外部サービスには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
LIB="$SCRIPT_DIR/../scripts/inuse-lib.sh"

if [ ! -f "$LIB" ]; then
  printf 'ERROR: lib not found: %s\n' "$LIB" >&2
  exit 1
fi

TMP=$(mktemp -d)
HOLDER_PID=""
trap '[ -n "$HOLDER_PID" ] && kill "$HOLDER_PID" 2>/dev/null; rm -rf "$TMP"' EXIT

mkdir -p "$TMP/held" "$TMP/free"
(cd "$TMP/held" && exec sleep 60) &
HOLDER_PID=$!
sleep 1

. "$LIB"

pass=0
fail=0

check() {
  local name=$1 want=$2 got=$3
  if [ "$got" = "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got: %s / want: %s)\n' "$name" "$got" "$want"
  fi
}

load_cwd_table

# lsof は物理パスを返すため、mktemp の symlink 表記（macOS の /var → /private/var）を正規化して渡す
held_real=$(cd "$TMP/held" && pwd -P)
free_real=$(cd "$TMP/free" && pwd -P)

out_held=$(cwd_holders "$held_real")
check 'holder detected with comm and pid' "sleep (PID $HOLDER_PID)" "$out_held"

check 'free directory has no holders' "" "$(cwd_holders "$free_real")"

# 配下のサブディレクトリに居るプロセスも親パスの指定で検出される（prefix 判定）
mkdir -p "$TMP/held2/sub"
(cd "$TMP/held2/sub" && exec sleep 60) &
HOLDER2=$!
sleep 1
load_cwd_table
held2_real=$(cd "$TMP/held2" && pwd -P)
out_sub=$(cwd_holders "$held2_real")
kill "$HOLDER2" 2>/dev/null
check 'process in subdirectory detected via prefix' "sleep (PID $HOLDER2)" "$out_sub"

# 名前が前方一致するだけの別ディレクトリ（held-sibling）は誤検出しない
mkdir -p "$TMP/held-sibling"
check 'prefix does not leak to sibling with shared name prefix' "" "$(cwd_holders "$held_real-sibling")"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
