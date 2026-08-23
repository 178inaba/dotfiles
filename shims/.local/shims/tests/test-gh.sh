#!/bin/bash

# gh shim のリグレッションテスト
#
# 実行: bash shims/.local/shims/tests/test-gh.sh
# 失敗したケースがあれば exit 1 で終了する。
#
# 実 gh は一度も呼ばない。GH_BIN を stub に向け、stub が argv を記録して
# 既知の stdout / stderr / exit status を返すので、「exec されたか」と
# 「ブロックされたか」を副作用なしに判別できる。
#
# shim は bash <path> ではなく shebang 経由（"$SHIM" ...）で起動する。
# shim の実行環境は macOS の /bin/bash = 3.2 で、ローカルの bash は Homebrew の 5.x に
# 解決されるため、bash <path> だと 3.2 でしか出ない失敗（連想配列等）を取り逃がす。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd -P)
SHIM="$SCRIPT_DIR/../gh"

if [ ! -x "$SHIM" ]; then
  printf 'ERROR: shim not executable: %s\n' "$SHIM" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

STUB_DIR="$TMP/stub"
mkdir -p "$STUB_DIR"
cat > "$STUB_DIR/gh" <<'STUB'
#!/bin/bash
printf '%s\n' "$@" > "$GH_STUB_ARGV"
if [ -n "${GH_STUB_STDIN:-}" ]; then
  cat > "$GH_STUB_STDIN"
fi
printf 'stub-stdout\n'
printf 'stub-stderr\n' >&2
exit 7
STUB
chmod +x "$STUB_DIR/gh"

ARGV_LOG="$TMP/argv"
STDERR_LOG="$TMP/stderr"

export GH_STUB_ARGV="$ARGV_LOG"
export GH_BIN="$STUB_DIR/gh"
export CLAUDECODE=1
unset GH_REPO

# ブロック時の exit status。gh が文書化している 0/1/2/4 とも、シェル予約の 126/127 とも
# 衝突しない値を選んでいる（呼び出し側が gh 自身の失敗と区別できるようにするため）
BLOCK_EXIT=78
# stub の exit status。透過性（shim が exec して状態を素通しすること）の目印
STUB_EXIT=7

pass=0
fail=0

ok() {
  pass=$((pass + 1))
  printf 'PASS  %s\n' "$1"
}

ng() {
  fail=$((fail + 1))
  printf 'FAIL  %s %s\n' "$1" "$2"
}

# shim を起動し、stdout を stdout_out、stderr を stderr_out、exit status を code に入れる
invoke() {
  rm -f "$ARGV_LOG"
  stdout_out=$("$@" 2>"$STDERR_LOG")
  code=$?
  stderr_out=$(cat "$STDERR_LOG")
}

# exec された = stub が argv を残し、stub の exit status がそのまま返ってきた
assert_runs() {
  local name=$1
  shift
  invoke "$@"
  if [ ! -f "$ARGV_LOG" ]; then
    ng "$name" "(the stub was not executed; exit=$code, stderr=$stderr_out)"
    return
  fi
  if [ "$code" -ne "$STUB_EXIT" ]; then
    ng "$name" "(exit=$code, want $STUB_EXIT from the stub)"
    return
  fi
  ok "$name"
}

# ブロックされた = stub が起動しておらず、専用の exit status が返った
assert_blocked() {
  local name=$1
  shift
  invoke "$@"
  if [ -f "$ARGV_LOG" ]; then
    ng "$name" "(the stub was executed)"
    return
  fi
  if [ "$code" -ne "$BLOCK_EXIT" ]; then
    ng "$name" "(exit=$code, want $BLOCK_EXIT)"
    return
  fi
  ok "$name"
}

# ブロックメッセージの内容を検証する。復旧手順が noun/verb に合っていないと、
# exit status だけのテストでは「ブロックはされるが直し方が分からない」状態を見逃す。
# want_absent は、そのコマンド自体に現れない語にのみ使うこと
# （メッセージは実行しようとしたコマンドをエコーするため）。
assert_block_message() {
  local name=$1 want_present=$2 want_absent=$3
  shift 3
  invoke "$@"
  if [ "$code" -ne "$BLOCK_EXIT" ]; then
    ng "$name" "(exit=$code, want $BLOCK_EXIT)"
    return
  fi
  case $stderr_out in
    *"$want_present"*) ;;
    *)
      ng "$name" "(stderr missing $want_present)"
      return ;;
  esac
  if [ -n "$want_absent" ]; then
    case $stderr_out in
      *"$want_absent"*)
        ng "$name" "(stderr unexpectedly contains $want_absent)"
        return ;;
    esac
  fi
  ok "$name"
}

# ---- read subcommand は判定の前に exec される ----

assert_runs 'read: pr view'                  "$SHIM" pr view 1
assert_runs 'read: issue list'               "$SHIM" issue list
assert_runs 'read: repo clone'               "$SHIM" repo clone foo/bar
assert_runs 'read: pr view without CLAUDECODE' env -u CLAUDECODE "$SHIM" pr view 1
assert_runs 'read: api is not a guarded noun' "$SHIM" api repos/foo/bar
assert_runs 'excluded: repo create'          "$SHIM" repo create foo/bar --public
assert_runs 'excluded: repo fork'            "$SHIM" repo fork foo/bar

# 引数が足りない形（noun / verb を決められない）はそのまま gh に渡す
assert_runs 'no arguments'                   "$SHIM"
assert_runs 'noun only'                      "$SHIM" issue
assert_runs 'version flag'                   "$SHIM" --version

# ---- リポジトリが明示されていない write はブロックされる ----

assert_blocked 'issue create without -R'     "$SHIM" issue create --title x --body y
assert_blocked 'pr create without -R'        "$SHIM" pr create --title x --body y
assert_blocked 'issue comment with bare number' "$SHIM" issue comment 1 --body x
assert_blocked 'pr comment with bare number' "$SHIM" pr comment 55 --body x
assert_blocked 'pr edit with branch selector' "$SHIM" pr edit feature/54-add-eli5-mode --body x
assert_blocked 'pr merge without -R'         "$SHIM" pr merge 5 --squash
assert_blocked 'release create without -R'   "$SHIM" release create v1 --title v1
assert_blocked 'label create without -R'     "$SHIM" label create bug --color FF0000
assert_blocked 'repo edit without a positional' "$SHIM" repo edit --description x
assert_blocked 'repo edit with a bare name'  "$SHIM" repo edit dotfiles --description x
assert_blocked 'repo rename without -R'      "$SHIM" repo rename new-name
assert_blocked 'repo rename with OWNER/REPO as the new name' "$SHIM" repo rename 178inaba/dotfiles

# ---- リポジトリが明示されている write は通る ----

assert_runs 'issue create with -R'           "$SHIM" issue create -R foo/bar --title x --body y
assert_runs 'pr create with --repo'          "$SHIM" pr create --repo foo/bar --title x
assert_runs 'pr comment with --repo='        "$SHIM" pr comment --repo=foo/bar 1 --body x
assert_runs 'issue comment with -R attached' "$SHIM" issue comment -Rfoo/bar 1 --body x
assert_runs 'repo edit with OWNER/REPO'      "$SHIM" repo edit 178inaba/dotfiles --description x
assert_runs 'repo edit with HOST/OWNER/REPO' "$SHIM" repo edit github.com/178inaba/dotfiles --description x
assert_runs 'repo edit with a repository URL' "$SHIM" repo edit https://github.com/178inaba/dotfiles --description x
assert_runs 'repo rename with -R'            "$SHIM" repo rename new-name -R 178inaba/dotfiles
assert_runs 'issue close with an issue URL'  "$SHIM" issue close https://github.com/178inaba/dotfiles/issues/59
assert_runs 'pr comment with a PR URL'       "$SHIM" pr comment https://github.com/178inaba/dotfiles/pull/55 --body x
assert_runs 'release create with -R'         "$SHIM" release create v1 -R foo/bar --title v1

# ---- 位置引数はフラグより後ろにあってもよい（フックの既知の限界だった形）----

assert_runs 'repo delete with a flag before the positional' "$SHIM" repo delete --yes 178inaba/dotfiles
assert_blocked 'repo delete with a flag before a bare name' "$SHIM" repo delete --yes dotfiles
assert_runs 'issue close with a value flag before the URL' \
  "$SHIM" issue close -c done https://github.com/178inaba/dotfiles/issues/59

# ---- 値取りフラグの値を位置引数と取り違えない ----
# gh repo sync の位置引数は同期先で、-s/--source は同期元。--source の値を同期先と
# 誤認すると、リポジトリを明示していないコマンドが素通りする

assert_blocked 'repo sync does not read --source as the target' \
  "$SHIM" repo sync -s 178inaba/dotfiles dotfiles
assert_runs 'repo sync with an explicit target' \
  "$SHIM" repo sync -s 178inaba/upstream 178inaba/dotfiles
assert_blocked 'repo edit does not read --homepage as the target' \
  "$SHIM" repo edit --homepage https://github.com/178inaba/dotfiles --description x

# ---- -- 以降はフラグ解釈を打ち切る ----

assert_runs 'positional after --'            "$SHIM" repo edit -- 178inaba/dotfiles
assert_blocked '-R after -- is a positional, not explicitness' \
  "$SHIM" issue create -- -R foo/bar

# ---- verb 直後のヘルプ参照は write ではない ----

assert_runs 'repo edit --help'               "$SHIM" repo edit --help
assert_runs 'pr create -h'                   "$SHIM" pr create -h
# gh repo edit の -h は --homepage（値取り）なのでヘルプにならない
assert_blocked 'repo edit -h is --homepage, not help' \
  "$SHIM" repo edit -h https://example.com --description x

# ---- GH_REPO は環境変数として届く（argv には現れない）----

assert_runs 'GH_REPO covers issue create'    env GH_REPO=foo/bar "$SHIM" issue create --title x --body y
assert_runs 'GH_REPO covers repo rename'     env GH_REPO=foo/bar "$SHIM" repo rename new-name
# gh は -R を持たないサブコマンドで GH_REPO を解決しないため、明示にはならない
assert_blocked 'GH_REPO does not cover repo edit' env GH_REPO=foo/bar "$SHIM" repo edit --description x
assert_blocked 'empty GH_REPO is not explicitness' env GH_REPO= "$SHIM" issue create --title x

# ---- CLAUDECODE 未設定なら判定しない ----

assert_runs 'no CLAUDECODE: issue create'    env -u CLAUDECODE "$SHIM" issue create --title x --body y
assert_runs 'no CLAUDECODE: repo rename'     env -u CLAUDECODE "$SHIM" repo rename new-name
assert_runs 'no CLAUDECODE: repo delete with a bare name' env -u CLAUDECODE "$SHIM" repo delete --yes dotfiles

invoke env -u CLAUDECODE "$SHIM" issue create --title x --body y
if [ "$stderr_out" = "stub-stderr" ]; then
  ok 'no CLAUDECODE: the shim writes nothing of its own to stderr'
else
  ng 'no CLAUDECODE: the shim writes nothing of its own to stderr' "(stderr=$stderr_out)"
fi

# ---- exec の透過性 ----

invoke "$SHIM" issue create -R foo/bar --title x --body y
if [ "$stdout_out" = "stub-stdout" ] && [ "$stderr_out" = "stub-stderr" ] && [ "$code" -eq "$STUB_EXIT" ]; then
  ok 'allowed command passes stdout, stderr and exit status through unchanged'
else
  ng 'allowed command passes stdout, stderr and exit status through unchanged' \
    "(stdout=$stdout_out, stderr=$stderr_out, exit=$code)"
fi

if [ "$(cat "$ARGV_LOG")" = "$(printf 'issue\ncreate\n-R\nfoo/bar\n--title\nx\n--body\ny')" ]; then
  ok 'allowed command reaches gh with argv unchanged'
else
  ng 'allowed command reaches gh with argv unchanged' "(argv=$(cat "$ARGV_LOG" | tr '\n' ' '))"
fi

# stdin も素通しする（--body-file - で本文を渡す形が壊れない）
STDIN_LOG="$TMP/stdin"
printf 'piped-body\n' | GH_STUB_STDIN="$STDIN_LOG" "$SHIM" issue comment -R foo/bar 1 --body-file - >/dev/null 2>&1
if [ -f "$STDIN_LOG" ] && [ "$(cat "$STDIN_LOG")" = "piped-body" ]; then
  ok 'allowed command passes stdin through unchanged'
else
  ng 'allowed command passes stdin through unchanged' "(stdin=$(cat "$STDIN_LOG" 2>/dev/null))"
fi

# ---- 判定中の内部エラーは fail closed ----
# プロダクション側にテスト専用の穴は開けず、BASH_ENV で判定用の変数を readonly に固定して
# shim 自身の代入を失敗させる（bash が非対話シェルで BASH_ENV を source する仕様を使う）

LOCK_FILE="$TMP/lock.sh"
printf 'readonly has_repo=locked\n' > "$LOCK_FILE"

assert_blocked 'internal error during judgement does not exec' \
  env BASH_ENV="$LOCK_FILE" "$SHIM" issue create -R foo/bar --title x
assert_runs 'internal error during judgement does not stop reads' \
  env BASH_ENV="$LOCK_FILE" "$SHIM" pr view 1

# ---- real gh の解決 ----

# GH_BIN が shim 自身を指していても再帰しない（PATH から実体を探し直す）
assert_runs 'GH_BIN pointing at the shim falls back to PATH' \
  env GH_BIN="$SHIM" PATH="$STUB_DIR:$PATH" "$SHIM" pr view 1

# real gh がどこにも無ければ write は実行しない
assert_blocked 'a missing real gh does not silently succeed' \
  env -u GH_BIN PATH="$TMP/empty-path" "$SHIM" issue create -R foo/bar --title x

# ---- ブロックメッセージが noun/verb に応じた復旧手順を示す ----

assert_block_message 'message: repo edit shows the positional form' \
  'gh repo edit owner/repo' 'GH_REPO' \
  "$SHIM" repo edit --description x
assert_block_message 'message: pr comment shows the -R form' \
  '-R owner/repo' '' \
  "$SHIM" pr comment 55 --body x
assert_block_message 'message: repo rename shows the -R form' \
  '-R owner/repo' '' \
  "$SHIM" repo rename new-name
# create は selector を取らないので URL 例を出してはいけない（通らない復旧手順になる）。
# 不在判定にプレースホルダ入りの URL を使うのは、素の https:// だとメッセージが常に出す
# 「現在の origin remote」に一致してしまうため（origin が HTTPS の CI で誤検出する）
assert_block_message 'message: issue create omits the URL form' \
  '-R owner/repo' 'https://github.com/owner/repo' \
  "$SHIM" issue create --title x --body y
assert_block_message 'message: shows the command that was blocked' \
  'gh issue comment 1 --body x' '' \
  "$SHIM" issue comment 1 --body x

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
