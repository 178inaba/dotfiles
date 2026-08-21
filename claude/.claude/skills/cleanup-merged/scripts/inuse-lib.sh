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
#                           ツリーサイズ非依存。呼び出し元で候補ループの前に1回呼ぶ）
#   cwd_holders <path>      cwd が <path> 自身または配下のプロセスを「comm (PID n)」で
#                           1行ずつ出力する（空 = 使用中プロセスなし）。<path> は物理パス
#                           （symlink 解決済み）で渡すこと。lsof が返すのは物理パスのため
# 環境変数: LSOF_BIN — lsof コマンドの差し替え（テスト用スタブ）

LSOF_BIN=${LSOF_BIN:-lsof}

cwd_table=""

load_cwd_table() {
  # -F pcn は「p<PID> / c<コマンド名> / fcwd / n<パス>」の行群を吐く機械可読形式
  cwd_table=$("$LSOF_BIN" -a -d cwd -F pcn 2>/dev/null | awk '
    /^p/ { pid = substr($0, 2) }
    /^c/ { comm = substr($0, 2) }
    /^n/ { print pid "\t" comm "\t" substr($0, 2) }
  ')
}

cwd_holders() {
  printf '%s\n' "$cwd_table" | awk -F'\t' -v p="$1" '
    $3 == p || index($3, p "/") == 1 { printf "%s (PID %s)\n", $2, $1 }
  '
}
