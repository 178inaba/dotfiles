# worktree 使用中検出の共有配管（source して使う。単体実行しない）
#
# 「あるディレクトリを cwd に持つプロセスが居るか」を lsof で調べる。collect（候補の skip）と
# delete（削除直前の拒否）の両方が同じ突合を必要とし、綴りを2箇所に持つとドリフトするため
# ここへ一本化する。cwd を見るのは、worktree 削除で即死するのが cwd を持つプロセス
# （シェル・Claude Code セッション）だから。開いているだけのファイルはエディタが保持しない
# （読み込んで即 close する）ため +D 検索は採らない（ツリーサイズにコストが比例する割に
# 追加カバレッジがほぼ無い）。
# テストは tests/test-inuse-lib.sh。この lib は他の lib を source しない。
#
# 提供する関数と、保持するグローバル変数:
#   cwd_table               load_cwd_table が構築する「PID<TAB>comm<TAB>cwd」の表
#   load_cwd_table          lsof を1回実行して cwd_table を構築する（プロセス数比例・
#                           ツリーサイズ非依存。呼び出し元で候補ループの前に1回呼ぶ）。
#                           lsof の実行時失敗（権限等）は非ゼロ return — 空表を「使用中なし」と
#                           誤読するとガードが silent に無効化されるため、呼び出し元は
#                           fail-closed にすること
#   cwd_holders <path>      cwd が <path> 自身または配下のプロセスを「comm (PID n), ...」の
#                           カンマ区切り1行で出力する（空 = 使用中プロセスなし）。<path> は
#                           関数内で物理パスへ解決するため symlink 表記のまま渡してよい
# 環境変数: LSOF_BIN — lsof コマンドの差し替え（環境差の吸収・テスト用）

LSOF_BIN=${LSOF_BIN:-lsof}

cwd_table=""

load_cwd_table() {
  # -F pcn は「p<PID> / c<コマンド名> / fcwd / n<パス>」の行群を吐く機械可読形式
  cwd_table=$("$LSOF_BIN" -a -d cwd -F pcn 2>/dev/null | awk '
    /^p/ { pid = substr($0, 2) }
    /^c/ { comm = substr($0, 2) }
    /^n/ { print pid "\t" comm "\t" substr($0, 2) }
  ')
  # 自己検証: この関数を呼んだプロセス自身の cwd は必ず表に載るはずなので、
  # 載っていなければ lsof の実行時失敗と判定できる
  printf '%s\n' "$cwd_table" | awk -F'\t' -v pid="$$" '$1 == pid { found = 1 } END { exit !found }'
}

cwd_holders() {
  # lsof は symlink 解決済みの物理パスを返すため、入力側も物理パスへ解決してから突合する
  # （macOS の /var → /private/var 等の表記揺れですり抜けると使用中 worktree を見逃す）
  local p
  p=$(cd "$1" 2>/dev/null && pwd -P) || p=$1
  printf '%s\n' "$cwd_table" | awk -F'\t' -v p="$p" '
    $3 == p || index($3, p "/") == 1 { printf "%s%s (PID %s)", sep, $2, $1; sep = ", " }
    END { if (sep != "") print "" }
  '
}
