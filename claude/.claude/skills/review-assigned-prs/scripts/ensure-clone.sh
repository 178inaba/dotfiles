#!/bin/bash

# /review-assigned-prs のレビュー用 clone dir を ensure するスクリプト
#
# 引数の <owner>/<repo> について、レビュー専用ワークスペースに clone を用意し、その絶対
# パスを JSON で stdout に出力する。既存 clone は git fetch --prune で更新する。
#
# clone 先パスの規約（正はここ・SKILL.md 側は「JSON の path フィールドを使う」旨のみ）:
#   ${XDG_DATA_HOME:-$HOME/.local/share}/claude-review-prs/{owner}/{repo}
#
# ユーザーの通常作業 clone とは別空間に置くことで、レビュー中の worktree が普段の作業を
# 汚さないようにする。
#
# 並行実行の契約: 同一 repo への同時呼び出し（同一リポジトリの複数 PR を並列レビューする
# サブエージェント）に安全。clone は一時ディレクトリで完成させてから mv で atomic に配置し、
# 最終パスには常に完成 clone しか存在しない（不完全状態を経由しない）。競合に負けた側は
# 自分の clone を捨てて勝者の clone を採用する（無駄になるのは初回 1 回分の clone のみ）。
# 待ち合わせロックを使わないのは、macOS に flock(1) が無く mkdir ロック + stale 検出の
# 複雑さがこの稀な無駄と釣り合わないため。
#
# 既知の制約（ロックなし設計のトレードオフとして許容）:
#   - 旧実装の残骸（.git 無し $path）の掃除は check-then-act のため、残骸が残る repo に
#     並行呼び出しが同時進入した場合に限り理論上のウィンドウが残る（過渡的条件のみで発現）
#   - EXIT trap を経ないクラッシュ（SIGKILL 等）では隠し一時ディレクトリ .<repo>.XXXXXX が
#     残る。自動回収は持たない（並行実行中の生存 tmp との判別機構が YAGNI）— 手動クリーン
#     アップ（SKILL.md 注意事項参照）の対象に含める
#
# 使用方法: ensure-clone.sh <owner>/<repo>
# 出力契約: SKILL.md の「出力 JSON の契約」を参照
# 環境変数: GH_BIN — gh コマンドの差し替え（テスト用スタブ）
#           XDG_DATA_HOME — 未設定なら $HOME/.local/share

set -u

GH_BIN=${GH_BIN:-gh}

if ! command -v jq >/dev/null 2>&1; then
  printf 'jq is required\n' >&2
  exit 1
fi
if ! command -v git >/dev/null 2>&1; then
  printf 'git is required\n' >&2
  exit 1
fi

repo_ref=${1:-}
if [ -z "$repo_ref" ]; then
  printf 'usage: ensure-clone.sh <owner>/<repo>\n' >&2
  exit 1
fi

if ! [[ "$repo_ref" =~ ^[^/]+/[^/]+$ ]]; then
  printf 'invalid repo reference (expected <owner>/<repo>): %s\n' "$repo_ref" >&2
  exit 1
fi

owner=${repo_ref%%/*}
repo=${repo_ref#*/}

# ドット成分は path traversal（例: owner「..」で $path が clone 空間の外を指す）により
# 残骸掃除の rm -rf が管理外を削除しうるため拒否する
for part in "$owner" "$repo"; do
  case "$part" in
    . | ..)
      printf 'invalid repo reference (dot components not allowed): %s\n' "$repo_ref" >&2
      exit 1
      ;;
  esac
done

base="${XDG_DATA_HOME:-$HOME/.local/share}/claude-review-prs"
path="$base/$owner/$repo"

if [ -d "$path/.git" ]; then
  if ! git -C "$path" fetch --prune >/dev/null 2>&1; then
    printf 'failed to fetch %s\n' "$repo_ref" >&2
    exit 1
  fi
else
  if ! mkdir -p "$base/$owner"; then
    printf 'failed to create parent dir %s\n' "$base/$owner" >&2
    exit 1
  fi
  # .git の無いディレクトリは旧実装（$path へ直接 clone）が中断された残骸。現実装では
  # $path に不完全状態が置かれることはないため、.git の有無だけで残骸と断定して掃除できる。
  # .git があるものは直前に他の並行呼び出しが publish した clone なので消さない（後続の
  # publish 手順が既存 clone の採用として解決する）
  if [ -d "$path" ] && [ ! -d "$path/.git" ]; then
    rm -rf "$path"
  fi
  if ! tmp=$(mktemp -d "$base/$owner/.$repo.XXXXXX"); then
    exit 1
  fi
  trap 'rm -rf "$tmp"' EXIT
  if ! "$GH_BIN" repo clone "$repo_ref" "$tmp" >/dev/null 2>&1; then
    # clone 中に他の並行呼び出しが $path を publish していればそれを採用する
    if [ -d "$path/.git" ]; then
      jq -n --arg path "$path" '{path: $path}'
      exit 0
    fi
    printf 'failed to clone %s\n' "$repo_ref" >&2
    exit 1
  fi
  # 先に他の並行呼び出しが publish 済みの場合、mv は $path の「中へ」移動する（POSIX mv の
  # 仕様）。その残骸を回収し、成否は .git の有無だけで判定する（tmp 自体の回収は trap が所有）
  mv "$tmp" "$path" 2>/dev/null
  rm -rf "$path/${tmp##*/}"
  if [ ! -d "$path/.git" ]; then
    printf 'failed to publish clone for %s\n' "$repo_ref" >&2
    exit 1
  fi
fi

jq -n --arg path "$path" '{path: $path}'
