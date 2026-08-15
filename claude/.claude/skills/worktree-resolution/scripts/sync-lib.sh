# worktree-resolution スクリプト共有の同期プリミティブ（source して使う。単体実行しない）
#
# resolve-pr-worktree.sh と check-pr-freshness.sh が共有する「dirty 判定 + 安全な
# fast-forward のみ自動同期」の定義。停止条件の意味論（何を dirty とみなすか・
# untracked 衝突時の挙動・status 名）が経路によって割れないよう、ここに一本化する。
# テストは source 元スクリプトのテスト（tests/test-*.sh）でカバーする。
#
# 呼び出し元が定義済みであることを前提とする関数（stderr・exit の配管は持たない）。
# 実体は warnings-lib.sh が提供するので、呼び出し元は両方を source する（この lib からは
# source しない — 二重 source による warnings の再初期化と source 順への依存を避けるため）:
#   fatal <msg>        前提不成立で非ゼロ exit する

# 変更あり（untracked は除く）なら真
is_dirty() {
  local dir=$1
  [ -n "$(git -C "$dir" status --porcelain | grep -v '^??' | head -n1)" ]
}

# 安全な ff 同期: dirty なら "behind_dirty"、clean なら <target> へ ff して "synced" を出力。
# ff 失敗（untracked 衝突等）は英語 stderr + exit 1
safe_ff_or_dirty() {
  local dir=$1 target=$2
  if is_dirty "$dir"; then
    printf 'behind_dirty'
    return
  fi
  git -C "$dir" merge --ff-only -q "$target" >/dev/null 2>&1 \
    || fatal "fast-forward merge to $target failed (untracked file collision?)"
  printf 'synced'
}
