#!/bin/bash

# 全シェルテストスイートの一括実行ランナー（ローカルと CI で同じエントリポイント）
#
# 実行: bash claude/.claude/tests/run-all.sh [<スキャンルート>]
# スキャンルート配下の */tests/test-*.sh を発見して逐次実行し、スイートごとに
# PASS / FAIL を出す。1 つでも失敗したら非ゼロ exit する。
# 引数省略時のルートは自スクリプトの親ディレクトリ（= claude/.claude/）。
# 引数を取るのは、リグレッションテスト（tests/test-run-all.sh）が実スイートへ再帰せず
# フィクスチャを検査できるようにするため。
#
# 一覧をハードコードせずパターンで発見するのは、新しいスイートを足したときに本スクリプトの
# 編集を要さないようにするため。起動を常に bash <path> にするのは、既存スイートの多数が
# 実行ビットを持たず、直接実行や find -perm では大半がスキップされるため。

set -u

usage() {
  printf 'usage: bash %s [<scan-root>]\n' "$0" >&2
  exit 1
}

[ "$#" -le 1 ] || usage

script_dir=$(cd "$(dirname "$0")" && pwd -P)

# pwd -P で物理パスへ解決するのが必須。~/.claude/tests は dotfiles リポジトリへの
# ディレクトリ symlink なので、論理パスのままだと親が ~/.claude になり、
# リポジトリ外（projects/ 等）まで走査対象に入る
root=${1:-$(dirname "$script_dir")}

if [ ! -d "$root" ]; then
  printf 'scan root is not a directory: %s\n' "$root" >&2
  exit 1
fi
root=$(cd "$root" && pwd -P)

tmp_dir=$(mktemp -d)
trap 'rm -rf "$tmp_dir"' EXIT

passed=0
failed=0
failed_suites=""

# プロセス置換で受けるのは、find | while だとスイートが stdin を読んだ時点で
# 残りのスイートが silent にスキップされるため（発見件数 0 のガードでは検出できない
# 別系統の失敗モード）。パイプではなくリダイレクトなのでカウンタもサブシェル化しない
while IFS= read -r suite; do
  out_file="$tmp_dir/out"
  # </dev/null: スイートが stdin を読んでも上流を食い潰さないようにする
  # 出力をファイルへ逃がすのは、$( ) キャプチャがパイプの書き込み端を持つ全プロセスの
  # 終了を待つため（バックグラウンドプロセスを残すスイートでブロックしうる）
  if bash "$suite" >"$out_file" 2>&1 </dev/null; then
    passed=$((passed + 1))
    printf 'PASS  %s\n' "$suite"
  else
    failed=$((failed + 1))
    failed_suites="$failed_suites$suite"$'\n'
    printf 'FAIL  %s\n' "$suite"
    # 失敗時のみ出力を見せる（成功時は静かに、失敗時は診断可能に）
    sed 's/^/      /' "$out_file"
  fi
done < <(find "$root" -path '*/tests/test-*.sh' | sort)

total=$((passed + failed))

# 0 件を成功扱いにすると、tests/ の移動やパターン破損で CI が silent に緑になる
if [ "$total" -eq 0 ]; then
  printf 'no test suites found under: %s\n' "$root" >&2
  exit 1
fi

printf '\n%d suites: %d passed, %d failed\n' "$total" "$passed" "$failed"

if [ "$failed" -ne 0 ]; then
  printf '\nfailed suites:\n' >&2
  printf '%s' "$failed_suites" | sed 's/^/  /' >&2
  exit 1
fi
