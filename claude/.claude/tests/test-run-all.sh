#!/bin/bash

# run-all.sh のリグレッションテスト
#
# 実行: bash claude/.claude/tests/test-run-all.sh
# 全ケースを mktemp -d のフィクスチャルートに対して実行する。実スイートを走らせないのは、
# 本テスト自身が run-all.sh の発見パターン（*/tests/test-*.sh）に一致するため — 実ルートを
# 渡すと再帰する。デフォルトルート解決も、フィクスチャ側に claude/.claude/tests/ の階層を
# 組んでそこへ runner をコピーすることで、実スイートを起動せずに検証する。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
RUNNER="$SCRIPT_DIR/run-all.sh"

if [ ! -f "$RUNNER" ]; then
  printf 'ERROR: script not found: %s\n' "$RUNNER" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

pass=0
fail=0

assert() {
  local name=$1 cond=$2 detail=${3:-}
  if eval "$cond"; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s %s\n' "$name" "$detail"
  fi
}

# フィクスチャのスイートを作る。実行ビットは意図的に付けない
# （runner が bash <path> で起動することの担保。実スイートも 26 件中 10 件しか持たない）
make_suite() {
  local suite_path=$1 body=$2
  mkdir -p "$(dirname "$suite_path")"
  printf '#!/bin/bash\n%s\n' "$body" > "$suite_path"
}

# runner を実行し、stdout+stderr を out、exit code を code に入れる
run_runner() {
  out=$("$@" 2>&1)
  code=$?
}

# ---- ケース1: 全 pass → exit 0 ----

ROOT_OK="$TMP/ok"
make_suite "$ROOT_OK/a/tests/test-one.sh" 'exit 0'
make_suite "$ROOT_OK/b/tests/test-two.sh" 'exit 0'

run_runner bash "$RUNNER" "$ROOT_OK"
assert 'all pass: exit 0' '[ "$code" -eq 0 ]' "(exit=$code)"
assert 'all pass: PASS line for test-one' 'printf "%s" "$out" | grep -q "^PASS .*a/tests/test-one\.sh$"' "(out=$out)"
assert 'all pass: PASS line for test-two' 'printf "%s" "$out" | grep -q "^PASS .*b/tests/test-two\.sh$"' "(out=$out)"
assert 'all pass: summary counts 2' 'printf "%s" "$out" | grep -q "^2 suites: 2 passed, 0 failed$"' "(out=$out)"

# 実行ビット無しで上が成立している = bash <path> 起動の担保
assert 'all pass: fixtures carry no execute bit' '[ ! -x "$ROOT_OK/a/tests/test-one.sh" ]'

# ---- ケース2: パターン発見（runner を編集せずに新スイートが走る）----

make_suite "$ROOT_OK/d/tests/test-new.sh" 'exit 0'

run_runner bash "$RUNNER" "$ROOT_OK"
assert 'discovery: newly added suite runs' 'printf "%s" "$out" | grep -q "^PASS .*d/tests/test-new\.sh$"' "(out=$out)"
assert 'discovery: count grows to 3' 'printf "%s" "$out" | grep -q "^3 suites: 3 passed, 0 failed$"' "(out=$out)"

# ---- ケース3: パターン外は拾わない ----

make_suite "$ROOT_OK/e/not-tests/test-x.sh" 'exit 1'
make_suite "$ROOT_OK/f/tests/helper.sh" 'exit 1'

run_runner bash "$RUNNER" "$ROOT_OK"
assert 'pattern: count unchanged at 3' 'printf "%s" "$out" | grep -q "^3 suites: 3 passed, 0 failed$"' "(out=$out)"
assert 'pattern: non-tests dir excluded' '! printf "%s" "$out" | grep -q "not-tests"' "(out=$out)"
assert 'pattern: non test- prefix excluded' '! printf "%s" "$out" | grep -q "helper\.sh"' "(out=$out)"
assert 'pattern: exit still 0' '[ "$code" -eq 0 ]' "(exit=$code)"

# ---- ケース4: 1件 fail → 非ゼロ exit、失敗スイートの出力が見える ----

ROOT_FAIL="$TMP/mixed"
make_suite "$ROOT_FAIL/a/tests/test-one.sh" 'exit 0'
make_suite "$ROOT_FAIL/b/tests/test-two.sh" 'exit 0'
make_suite "$ROOT_FAIL/c/tests/test-fail.sh" \
  'printf "marker-on-stdout\n"; printf "marker-on-stderr\n" >&2; exit 1'

run_runner bash "$RUNNER" "$ROOT_FAIL"
assert 'one fail: nonzero exit' '[ "$code" -ne 0 ]' "(exit=$code)"
assert 'one fail: FAIL line for test-fail' 'printf "%s" "$out" | grep -q "^FAIL .*c/tests/test-fail\.sh$"' "(out=$out)"
assert 'one fail: summary counts 3/2/1' 'printf "%s" "$out" | grep -q "^3 suites: 2 passed, 1 failed$"' "(out=$out)"
assert 'one fail: failing stdout shown' 'printf "%s" "$out" | grep -q "marker-on-stdout"' "(out=$out)"
assert 'one fail: failing stderr shown' 'printf "%s" "$out" | grep -q "marker-on-stderr"' "(out=$out)"

# 成功したスイートの出力は出さない（成功時は静か、失敗時だけ診断可能にする）
ROOT_QUIET="$TMP/quiet"
make_suite "$ROOT_QUIET/a/tests/test-noisy.sh" 'printf "should-not-appear\n"; exit 0'
run_runner bash "$RUNNER" "$ROOT_QUIET"
assert 'passing suite output suppressed' '! printf "%s" "$out" | grep -q "should-not-appear"' "(out=$out)"

# ---- ケース5: stdin を読むスイートが後続を食い潰さない ----
# find | while のパイプ実装だと、stdin を読むスイートが残りのスイートを silent に飲む

ROOT_STDIN="$TMP/stdin"
make_suite "$ROOT_STDIN/a/tests/test-aaa-greedy.sh" 'cat > /dev/null; exit 0'
make_suite "$ROOT_STDIN/b/tests/test-bbb.sh" 'exit 0'
make_suite "$ROOT_STDIN/c/tests/test-ccc.sh" 'exit 0'

run_runner bash "$RUNNER" "$ROOT_STDIN"
assert 'stdin-reading suite does not swallow the rest' 'printf "%s" "$out" | grep -q "^3 suites: 3 passed, 0 failed$"' "(out=$out)"

# ---- ケース6: バックグラウンドプロセスを残すスイートでブロックしない ----
# $( ) キャプチャはパイプの書き込み端を持つ全プロセスの終了を待つため、実スイートの
# test-inuse-lib.sh のように sleep を残すスイートでハングしうる

ROOT_BG="$TMP/bg"
make_suite "$ROOT_BG/a/tests/test-bg.sh" '(exec sleep 30) & exit 0'

start=$(date +%s)
run_runner bash "$RUNNER" "$ROOT_BG"
elapsed=$(( $(date +%s) - start ))
assert 'background process does not block the runner' '[ "$elapsed" -lt 10 ]' "(elapsed=${elapsed}s)"
assert 'background process: exit 0' '[ "$code" -eq 0 ]' "(exit=$code)"

# ---- ケース7: 発見件数 0 → 非ゼロ exit ----
# 0 件を成功扱いにすると、tests/ の移動・パターン破損で CI が silent に緑になる

ROOT_EMPTY="$TMP/empty"
mkdir -p "$ROOT_EMPTY"
run_runner bash "$RUNNER" "$ROOT_EMPTY"
assert 'no suites found: nonzero exit' '[ "$code" -ne 0 ]' "(exit=$code)"

# ---- ケース8: 存在しないルート → 非ゼロ exit ----

run_runner bash "$RUNNER" "$TMP/does-not-exist"
assert 'missing root: nonzero exit' '[ "$code" -ne 0 ]' "(exit=$code)"

# ---- ケース9: 引数省略時のデフォルトルート解決 ----
# フィクスチャ側に claude/.claude/tests/ の階層を組んで runner を置き、引数なしで実行する。
# デフォルトは「自スクリプトの親ディレクトリ」なので、走るのはフィクスチャの 1 件だけになる

FAKE_ROOT="$TMP/repo/claude/.claude"
mkdir -p "$FAKE_ROOT/tests"
cp "$RUNNER" "$FAKE_ROOT/tests/run-all.sh"
make_suite "$FAKE_ROOT/g/tests/test-default.sh" 'exit 0'

run_runner bash "$FAKE_ROOT/tests/run-all.sh"
assert 'default root: exit 0' '[ "$code" -eq 0 ]' "(exit=$code)"
assert 'default root: resolves to the script parent' 'printf "%s" "$out" | grep -q "^1 suites: 1 passed, 0 failed$"' "(out=$out)"
assert 'default root: runs the fixture suite' 'printf "%s" "$out" | grep -q "^PASS .*g/tests/test-default\.sh$"' "(out=$out)"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
