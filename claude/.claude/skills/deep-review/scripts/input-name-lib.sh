#!/bin/bash

# /deep-review スクリプト共有の入力ファイル名検証（source して使う。単体実行しない）
#
# post-review.sh（review_path）と respond-threads.sh（threads_path）が共有する
# 「モデルが Write した入力ファイルが、対象 PR のコンテキストと対になっているか」の判定。
# 固定名の入力は、同一セッションの scratchpad を共有する並列サブエージェント間で
# 別 PR の内容に上書きされるため、名前の側で構造的に止める必要がある。
# 束縛の意味論（何を識別子とみなすか・末尾一致の規則）が2つの投稿経路で割れないよう、
# ここに一本化する。テストは source 元スクリプトのテスト（tests/test-*.sh）でカバーする。
#
# 識別子は context ファイル名から `pr-context-` を strip して得る。repo と PR 番号から
# 再構築しないのは、識別子の形式を定義しているのは fetch-pr-context.sh の出力名であり、
# 各消費側で再構築すると起点の命名変更に追従できず、かつ prepare-review.sh の
# derive_input_path（同じ strip で払い出す）と乖離するため。

# 入力ファイル名が context ファイルと対になっていることを検証する。
# 引数: <入力ファイルパス> <prepare-review.sh の出力フィールド名> <context ファイルパス>
# 呼び出し元で fatal() が定義済みであること
require_pr_bound_filename() {
  local file=$1 path_field=$2 context_file=$3
  local expected_token
  expected_token=$(basename "$context_file" | sed 's/^pr-context-//')
  case "$(basename "$file")" in
    *"$expected_token") ;;
    *) fatal "input file name must end with '$expected_token' to pair it with $context_file: $file
use the $path_field emitted by prepare-review.sh (a fixed name is overwritten by parallel reviews of other PRs)" ;;
  esac
}
