# /deep-review スクリプト共有のレビュー作業ディレクトリの導出・検証（source して使う。単体実行しない）
#
# レビュー1件あたり1ディレクトリを割り当て、モデルが Write する入力ファイル（review / threads）と
# レビュー中に作る補助ファイルをその中に閉じ込める。同一セッションの scratchpad を共有する
# 並列サブエージェントが共有直下に固定名で書くと別 PR の内容に上書きされるため、束縛の単位は
# ファイル名ではなくディレクトリに置く（ファイル名側で束縛すると、スクリプトが払い出す2ファイルは
# 守れても、モデルがレビュー中に作る補助ファイルは規約の外に残る）。
# 払い出し（prepare-review.sh）と検証（post-review.sh / respond-threads.sh）で導出が割れないよう、
# ここに一本化する。テストは source 元スクリプトのテスト（tests/test-*.sh）でカバーする。
#
# 識別子は context ファイル名から `pr-context-` と拡張子を strip して得る。repo と PR 番号から
# 再構築しないのは、識別子の形式を定義しているのは fetch-pr-context.sh の出力名であり、
# 各消費側で再構築すると起点の命名変更に追従できないため。

# context ファイルと対になる作業ディレクトリのパスを返す（存在は保証しない）
review_work_dir() {
  local context_file=$1 token
  token=$(basename "$context_file" | sed -e 's/^pr-context-//' -e 's/\.json$//')
  printf '%s/deep-review-%s' "$(dirname "$context_file")" "$token"
}

# 入力ファイルが context と対になる作業ディレクトリの直下にあることを検証する。
# 引数: <入力ファイルパス> <prepare-review.sh の出力フィールド名> <context ファイルパス>
# 呼び出し元で fatal() が定義済みかつ入力ファイルの存在確認済みであること
# （dirname を辿って比較するため、存在しないパスでは検証が成立しない）
require_in_review_work_dir() {
  local file=$1 path_field=$2 context_file=$3 expected actual
  expected=$(review_work_dir "$context_file")
  [ -d "$expected" ] || fatal "review work dir not found: $expected
rerun prepare-review.sh to create it"
  # 同じディレクトリの別表記（相対パス・シンボリックリンク経由）で不一致にしないよう正規化する
  expected=$(cd "$expected" && pwd -P)
  actual=$(cd "$(dirname "$file")" && pwd -P)
  [ "$actual" = "$expected" ] || fatal "input file must be in the review work dir paired with $context_file: $file
use the $path_field emitted by prepare-review.sh (files outside it are overwritten by parallel reviews of other PRs)"
}
