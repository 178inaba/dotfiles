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

load_cwd_table || {
  printf 'FAIL  load_cwd_table succeeds with real lsof\n'
  exit 1
}

# symlink 表記のパス（macOS の mktemp は /var → /private/var 配下）を生のまま渡しても、
# lib 側の物理パス解決で lsof の物理パス出力と突合できる
out_held=$(cwd_holders "$TMP/held")
check 'holder detected via raw symlinked path' "sleep (PID $HOLDER_PID)" "$out_held"

check 'free directory has no holders' "" "$(cwd_holders "$TMP/free")"

# 配下のサブディレクトリに居るプロセスも親パスの指定で検出される（prefix 判定）。
# 複数 holder はカンマ区切りの1行に結合される
mkdir -p "$TMP/held2/sub"
(cd "$TMP/held2/sub" && exec sleep 60) &
HOLDER2=$!
sleep 1
load_cwd_table
out_multi=$(cwd_holders "$TMP")
kill "$HOLDER2" 2>/dev/null
case "$out_multi" in
  *"sleep (PID $HOLDER_PID)"*", "*"sleep (PID $HOLDER2)"* | *"sleep (PID $HOLDER2)"*", "*"sleep (PID $HOLDER_PID)"*)
    check 'multiple holders joined with comma' ok ok ;;
  *)
    check 'multiple holders joined with comma' "both PIDs comma-joined" "$out_multi" ;;
esac

# 名前が前方一致するだけの別ディレクトリ（held-sibling）は誤検出しない
mkdir -p "$TMP/held-sibling"
check 'prefix does not leak to sibling with shared name prefix' "" "$(cwd_holders "$TMP/held-sibling")"

# lsof の実行時失敗（バイナリはあるが非ゼロ終了）は非ゼロ return で呼び出し元に伝わる。
# 空表を「使用中なし」と誤読するとガードが必要な時にだけ silent に無効化されるため
if LSOF_BIN=/usr/bin/false load_cwd_table; then
  check 'lsof runtime failure detected' "nonzero return" "returned 0"
else
  check 'lsof runtime failure detected' ok ok
fi

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
