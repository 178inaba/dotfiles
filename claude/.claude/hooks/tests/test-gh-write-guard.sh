#!/bin/bash

# gh-write-guard.sh のリグレッションテスト
#
# 実行: bash claude/.claude/hooks/tests/test-gh-write-guard.sh
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOOK="$SCRIPT_DIR/../gh-write-guard.sh"

if [ ! -x "$HOOK" ]; then
  printf 'ERROR: hook script not executable: %s\n' "$HOOK" >&2
  exit 1
fi

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

# ルール3（本文中の素の #N 検出）用の本文ファイル
printf -- '- #1 foo\n- #2 bar\n- #3 baz\n' > "$tmpdir/hash-numbering.md"
printf -- '1. foo\n2. bar\n3. baz\n' > "$tmpdir/ordered-list.md"
printf -- 'see #1 and #2 and #2\n' > "$tmpdir/two-distinct-refs.md"
printf -- '- `#1` foo\n- `#2` bar\n- `#3` baz\n' > "$tmpdir/backtick-refs.md"
printf -- 'before\n```\n#1 #2 #3\n```\nafter\n' > "$tmpdir/fenced-refs.md"
printf -- '- foo/bar#1 x\n- foo/bar#2 y\n- foo/bar#3 z\n' > "$tmpdir/cross-repo-refs.md"
printf -- 'refs #123 #456 #789\n' > "$tmpdir/multi-digit-refs.md"
printf -- 'colors #1a2b3c and #2f4f4f, place #3rd\n' > "$tmpdir/alnum-suffix-refs.md"

# ルール4（PR 本文のバッククォート付き closing keyword 検出）用の本文ファイル
printf -- 'Related\n\n`Closes #656`\n' > "$tmpdir/quoted-closes.md"
printf -- 'before\n```\ncloses #656\n```\nafter\n' > "$tmpdir/fenced-closes.md"
printf -- 'see `Resolves foo/bar#12` here\n' > "$tmpdir/quoted-cross-repo-closes.md"
printf -- 'Closes #656\n' > "$tmpdir/raw-closes.md"
printf -- 'docs update: `Closes #N` placeholder\n' > "$tmpdir/quoted-placeholder-closes.md"
printf -- 'call `closes the stream` explicitly\n' > "$tmpdir/quoted-closes-no-ref.md"
printf -- 'word `discloses #656` here\n' > "$tmpdir/quoted-discloses.md"

pass=0
fail=0

run_test() {
  local name=$1 input=$2 want_exit=$3
  local got_exit
  printf '%s' "$input" | "$HOOK" >/dev/null 2>&1
  got_exit=$?
  if [ "$got_exit" -eq "$want_exit" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got exit %d, want %d)\n' "$name" "$got_exit" "$want_exit"
  fi
}

# ブロックメッセージの内容を検証する。復旧手順が noun/verb に合っていないと、
# exit code だけのテストでは「ブロックはされるが直し方が分からない」状態を見逃す。
# want_absent は、そのコマンド文字列自体に現れない語にのみ使うこと
# （メッセージは「実行しようとしたコマンド」をエコーするため）。
run_message_test() {
  local name=$1 input=$2 want_present=$3 want_absent=${4:-}
  local stderr_out got_exit
  stderr_out=$(printf '%s' "$input" | "$HOOK" 2>&1 >/dev/null)
  got_exit=$?
  if [ "$got_exit" -ne 2 ]; then
    fail=$((fail + 1))
    printf 'FAIL  %s (got exit %d, want 2)\n' "$name" "$got_exit"
    return
  fi
  case $stderr_out in
    *"$want_present"*) ;;
    *)
      fail=$((fail + 1))
      printf 'FAIL  %s (stderr missing %s)\n' "$name" "$want_present"
      return ;;
  esac
  if [ -n "$want_absent" ]; then
    case $stderr_out in
      *"$want_absent"*)
        fail=$((fail + 1))
        printf 'FAIL  %s (stderr unexpectedly contains %s)\n' "$name" "$want_absent"
        return ;;
    esac
  fi
  pass=$((pass + 1))
  printf 'PASS  %s\n' "$name"
}

# 通過すべきケース (exit 0)
run_test 'non-gh command'                '{"tool_name":"Bash","tool_input":{"command":"ls -la"}}' 0
run_test 'gh read: issue list'           '{"tool_name":"Bash","tool_input":{"command":"gh issue list"}}' 0
run_test 'gh read: pr view'              '{"tool_name":"Bash","tool_input":{"command":"gh pr view 123"}}' 0
run_test 'gh read: repo clone'           '{"tool_name":"Bash","tool_input":{"command":"gh repo clone foo/bar"}}' 0
run_test 'gh repo create (excluded)'     '{"tool_name":"Bash","tool_input":{"command":"gh repo create foo/bar --public"}}' 0
run_test 'gh repo fork (excluded)'       '{"tool_name":"Bash","tool_input":{"command":"gh repo fork foo/bar"}}' 0
run_test 'gh write with -R'              '{"tool_name":"Bash","tool_input":{"command":"gh issue create -R foo/bar --title T --body B"}}' 0
run_test 'gh write with --repo'          '{"tool_name":"Bash","tool_input":{"command":"gh pr create --repo foo/bar --title T"}}' 0
run_test 'gh write with --repo='         '{"tool_name":"Bash","tool_input":{"command":"gh pr comment --repo=foo/bar 1 --body x"}}' 0
run_test 'GH_REPO env prefix'            '{"tool_name":"Bash","tool_input":{"command":"GH_REPO=foo/bar gh issue create --title T --body B"}}' 0
run_test 'tool_name=Edit (not Bash)'     '{"tool_name":"Edit","tool_input":{"command":"gh issue create --title T"}}' 0
run_test 'empty command'                 '{"tool_name":"Bash","tool_input":{"command":""}}' 0
run_test 'chained: cd && gh read'        '{"tool_name":"Bash","tool_input":{"command":"cd /tmp/foo && gh issue list"}}' 0
run_test 'gh write with -R, --body-file path'     '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body-file /tmp/body.md"}}' 0
run_test 'gh write with -R, --body-file stdin'    '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body-file - <<EOF\nline1\nline2\nEOF"}}' 0
run_test 'body-file: ordered list numbering'      "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/ordered-list.md\"}}" 0
run_test 'body-file: only 2 distinct #N'          "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/two-distinct-refs.md\"}}" 0
run_test 'body-file: #N in backticks'             "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/backtick-refs.md\"}}" 0
run_test 'body-file: #N in fenced code block'     "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/fenced-refs.md\"}}" 0
run_test 'body-file: OWNER/REPO#N form'           "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/cross-repo-refs.md\"}}" 0
run_test 'body-file: multi-digit #N only'         "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/multi-digit-refs.md\"}}" 0
run_test 'body-file: hex color / ordinal #N'      "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/alnum-suffix-refs.md\"}}" 0
run_test 'body-file: nonexistent path (fail-open)' "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/missing.md\"}}" 0
run_test 'pr body: raw Closes #N'                  "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr create -R foo/bar --title T --body-file $tmpdir/raw-closes.md\"}}" 0
run_test 'pr body: quoted placeholder Closes #N'   "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr create -R foo/bar --title T --body-file $tmpdir/quoted-placeholder-closes.md\"}}" 0
run_test 'pr body: quoted closes without #ref'     "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr edit -R foo/bar 1 --body-file $tmpdir/quoted-closes-no-ref.md\"}}" 0
run_test 'pr body: quoted discloses (word bound)'  "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr edit -R foo/bar 1 --body-file $tmpdir/quoted-discloses.md\"}}" 0
run_test 'issue body: quoted Closes #N (scope out)' "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh issue create -R foo/bar --title T --body-file $tmpdir/quoted-closes.md\"}}" 0
run_test 'pr comment: quoted Closes #N (scope out)' "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/quoted-closes.md\"}}" 0

# リポジトリを位置引数で受ける gh repo サブコマンド (exit 0)
run_test 'gh repo edit OWNER/REPO'       '{"tool_name":"Bash","tool_input":{"command":"gh repo edit 178inaba/dotfiles --description x"}}' 0
run_test 'gh repo edit HOST/OWNER/REPO'  '{"tool_name":"Bash","tool_input":{"command":"gh repo edit github.com/178inaba/dotfiles --description x"}}' 0
run_test 'gh repo edit repo URL'         '{"tool_name":"Bash","tool_input":{"command":"gh repo edit https://github.com/178inaba/dotfiles --description x"}}' 0
run_test 'gh repo edit quoted OWNER/REPO' '{"tool_name":"Bash","tool_input":{"command":"gh repo edit \"178inaba/dotfiles\" --description x"}}' 0
run_test 'gh repo delete OWNER/REPO'     '{"tool_name":"Bash","tool_input":{"command":"gh repo delete 178inaba/dotfiles --yes"}}' 0
run_test 'gh repo archive OWNER/REPO'    '{"tool_name":"Bash","tool_input":{"command":"gh repo archive 178inaba/dotfiles --yes"}}' 0
run_test 'gh repo unarchive OWNER/REPO'  '{"tool_name":"Bash","tool_input":{"command":"gh repo unarchive 178inaba/dotfiles --yes"}}' 0
run_test 'gh repo sync OWNER/REPO'       '{"tool_name":"Bash","tool_input":{"command":"gh repo sync 178inaba/dotfiles"}}' 0

# gh repo rename の位置引数は新リポジトリ名なので -R / GH_REPO= のまま (exit 0)
run_test 'gh repo rename with -R'        '{"tool_name":"Bash","tool_input":{"command":"gh repo rename new-name -R 178inaba/dotfiles"}}' 0
run_test 'gh repo rename with GH_REPO'   '{"tool_name":"Bash","tool_input":{"command":"GH_REPO=178inaba/dotfiles gh repo rename new-name"}}' 0

# Issue/PR の selector が完全 URL ならリポジトリが明示されている (exit 0)
run_test 'gh pr comment with PR URL'     '{"tool_name":"Bash","tool_input":{"command":"gh pr comment https://github.com/178inaba/dotfiles/pull/55 --body x"}}' 0
run_test 'gh issue close with issue URL' '{"tool_name":"Bash","tool_input":{"command":"gh issue close https://github.com/178inaba/dotfiles/issues/59"}}' 0

# verb 直後の --help / -h は write ではない (exit 0)
run_test 'gh repo edit --help'           '{"tool_name":"Bash","tool_input":{"command":"gh repo edit --help"}}' 0
run_test 'gh pr create -h'               '{"tool_name":"Bash","tool_input":{"command":"gh pr create -h"}}' 0
run_test 'compound: grep then --help'    '{"tool_name":"Bash","tool_input":{"command":"grep repo hook.sh; gh repo edit --help"}}' 0

# ブロックされるべきケース (exit 2)
run_test 'gh repo edit bare REPO'                   '{"tool_name":"Bash","tool_input":{"command":"gh repo edit dotfiles --description x"}}' 2
run_test 'gh repo edit with GH_REPO (ignored by gh)' '{"tool_name":"Bash","tool_input":{"command":"GH_REPO=178inaba/dotfiles gh repo edit --description x"}}' 2
run_test 'gh repo rename (no -R)'                   '{"tool_name":"Bash","tool_input":{"command":"gh repo rename new-name"}}' 2
run_test 'gh repo rename OWNER/REPO as new name'    '{"tool_name":"Bash","tool_input":{"command":"gh repo rename 178inaba/dotfiles"}}' 2
run_test 'gh pr edit with branch selector'          '{"tool_name":"Bash","tool_input":{"command":"gh pr edit feature/54-add-eli5-mode --body x"}}' 2
run_test 'gh pr comment with bare number'           '{"tool_name":"Bash","tool_input":{"command":"gh pr comment 55 --body x"}}' 2
run_test 'gh pr merge: --help inside body'          '{"tool_name":"Bash","tool_input":{"command":"gh pr merge 55 --body \"see --help\""}}' 2
run_test 'help exempts only that occurrence'        '{"tool_name":"Bash","tool_input":{"command":"gh repo edit --help && gh pr comment 55 --body x"}}' 2
run_test 'per-occurrence: URL then bare repo edit'  '{"tool_name":"Bash","tool_input":{"command":"gh issue close https://github.com/178inaba/dotfiles/issues/59 && gh repo edit --description x"}}' 2
run_test 'per-occurrence: -R does not cover repo edit' '{"tool_name":"Bash","tool_input":{"command":"gh issue comment -R 178inaba/dotfiles 1 --body x && gh repo edit --description x"}}' 2

# ブロックメッセージが noun/verb に応じた復旧手順を示すこと
run_message_test 'message: repo edit shows positional form' \
  '{"tool_name":"Bash","tool_input":{"command":"gh repo edit --description x"}}' \
  'gh repo edit owner/repo' 'GH_REPO'
run_message_test 'message: pr comment shows -R form' \
  '{"tool_name":"Bash","tool_input":{"command":"gh pr comment 55 --body x"}}' \
  '-R owner/repo'

run_test 'gh issue create (no -R)'                  '{"tool_name":"Bash","tool_input":{"command":"gh issue create --title T --body B"}}' 2
run_test 'gh pr create (no -R)'                     '{"tool_name":"Bash","tool_input":{"command":"gh pr create --title T --body B"}}' 2
run_test 'gh issue comment (no -R)'                 '{"tool_name":"Bash","tool_input":{"command":"gh issue comment 1 --body x"}}' 2
run_test 'gh release create (no -R)'                '{"tool_name":"Bash","tool_input":{"command":"gh release create v1 --title v1"}}' 2
run_test 'gh repo edit (no -R)'                     '{"tool_name":"Bash","tool_input":{"command":"gh repo edit --description x"}}' 2
run_test 'gh label create (no -R)'                  '{"tool_name":"Bash","tool_input":{"command":"gh label create bug --color FF0000"}}' 2
run_test 'gh pr merge (no -R)'                      '{"tool_name":"Bash","tool_input":{"command":"gh pr merge 5 --squash"}}' 2
run_test 'chained: cd && gh issue create (no -R)'   '{"tool_name":"Bash","tool_input":{"command":"cd /tmp/foo && gh issue create --title T"}}' 2
run_test 'multiline --body (with -R)'               '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body \"line1\nline2\""}}' 2
run_test 'multiline --body heredoc (with -R)'       '{"tool_name":"Bash","tool_input":{"command":"gh issue comment -R foo/bar 1 --body \"$(cat <<EOF\nline1\nEOF\n)\""}}' 2
run_test 'multiline -b (with -R)'                   '{"tool_name":"Bash","tool_input":{"command":"gh pr create -R foo/bar --title T -b \"line1\nline2\""}}' 2
run_test 'multiline --body= (with -R)'              '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body=\"line1\nline2\""}}' 2
run_test 'body-file: bare #N numbering'             "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr comment -R foo/bar 1 --body-file $tmpdir/hash-numbering.md\"}}" 2
run_test 'body-file: quoted path, bare #N'          "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh issue comment -R foo/bar 1 --body-file \\\"$tmpdir/hash-numbering.md\\\"\"}}" 2
run_test 'body-file: -F short flag, bare #N'        "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh issue create -R foo/bar --title T -F $tmpdir/hash-numbering.md\"}}" 2
run_test 'inline --body: bare #N (single line)'     '{"tool_name":"Bash","tool_input":{"command":"gh issue comment -R foo/bar 1 --body \"fix #1, #2, #3\""}}' 2
run_test 'pr create: quoted Closes #N'              "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr create -R foo/bar --title T --body-file $tmpdir/quoted-closes.md\"}}" 2
run_test 'pr edit: fenced closes #N'                "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr edit -R foo/bar 1 --body-file $tmpdir/fenced-closes.md\"}}" 2
run_test 'pr create: quoted cross-repo Resolves'    "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"gh pr create -R foo/bar --title T --body-file $tmpdir/quoted-cross-repo-closes.md\"}}" 2
run_test 'pr edit inline --body: quoted Fixes #N'   '{"tool_name":"Bash","tool_input":{"command":"gh pr edit -R foo/bar 1 --body \"see `Fixes #12` here\""}}' 2

printf '\n%d passed, %d failed\n' "$pass" "$fail"

if [ "$fail" -gt 0 ]; then
  exit 1
fi
