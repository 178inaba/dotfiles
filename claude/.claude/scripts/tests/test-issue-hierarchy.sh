#!/bin/bash

# issue-hierarchy.sh のリグレッションテスト
#
# 実行: bash claude/.claude/scripts/tests/test-issue-hierarchy.sh
# gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../issue-hierarchy.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
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
    printf 'FAIL  %s\n' "$name"
    [ -n "$detail" ] && printf '      %s\n' "$detail"
  fi
}

# --- gh スタブ ---
# `gh api <endpoint>` は $GH_STUB_DATA/<endpoint をファイル名化>.json を返す。
# ファイル名は endpoint の `/` を `_` に置換したもの（クエリ文字列は落とす）。
# <name>.404 があれば実 gh の 404 挙動（stderr "HTTP 404" + exit 1）、
# <name>.fail があればその他の失敗（stderr 別文言 + exit 1）を模擬する。
# --paginate 指定時は <name>.json に加えて <name>.page2.json があれば連結して返す
# （実 gh の --paginate は各ページの JSON をそのまま連結して出力する）。
mkdir -p "$TMP/stub" "$TMP/data"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
case "$1" in
  repo) printf '%s\n' "${GH_STUB_REPO:-owner/repo}" ;;
  api)
    shift
    endpoint=""
    paginate=0
    for a in "$@"; do
      case "$a" in
        --paginate) paginate=1 ;;
        -*) ;;
        *) [ -z "$endpoint" ] && endpoint=$a ;;
      esac
    done
    name=$(printf '%s' "${endpoint%%\?*}" | tr '/' '_')
    printf '%s\n' "$endpoint" >> "$GH_STUB_DATA/.calls"
    if [ -f "$GH_STUB_DATA/$name.404" ]; then
      printf 'gh: Not Found (HTTP 404)\n' >&2
      exit 1
    fi
    if [ -f "$GH_STUB_DATA/$name.fail" ]; then
      printf 'gh: something else went wrong (HTTP 500)\n' >&2
      exit 1
    fi
    if [ ! -f "$GH_STUB_DATA/$name.json" ]; then
      printf 'stub: no fixture for %s\n' "$endpoint" >&2
      exit 1
    fi
    cat "$GH_STUB_DATA/$name.json"
    if [ "$paginate" = "1" ] && [ -f "$GH_STUB_DATA/$name.page2.json" ]; then
      cat "$GH_STUB_DATA/$name.page2.json"
    fi
    ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_DATA="$TMP/data"

reset_data() {
  rm -rf "$GH_STUB_DATA"
  mkdir -p "$GH_STUB_DATA"
}

issue_json() {
  # issue_json <number> <state> <sub_total> <sub_completed>
  jq -n --argjson n "$1" --arg s "$2" --argjson t "$3" --argjson c "$4" \
    '{number: $n, title: ("Issue " + ($n|tostring)), state: $s, html_url: ("https://github.com/owner/repo/issues/" + ($n|tostring)),
      sub_issues_summary: {total: $t, completed: $c, percent_completed: 0}}'
}

sub_list_json() {
  # sub_list_json <number>:<state> ...
  local items=""
  for spec in "$@"; do
    items="$items{\"number\": ${spec%%:*}, \"title\": \"Sub ${spec%%:*}\", \"state\": \"${spec#*:}\", \"html_url\": \"https://github.com/owner/repo/issues/${spec%%:*}\"},"
  done
  printf '[%s]' "${items%,}"
}

run() { bash "$SCRIPT" "$@" 2>"$TMP/err.txt"; }

# --- ケース1: 単独 Issue（親なし・Sub なし） ---
reset_data
issue_json 10 open 0 0 > "$GH_STUB_DATA/repos_owner_repo_issues_10.json"
touch "$GH_STUB_DATA/repos_owner_repo_issues_10_parent.404"
sub_list_json > "$GH_STUB_DATA/repos_owner_repo_issues_10_sub_issues.json"

out=$(run 10); status=$?
assert "standalone: exit 0" "[ $status -eq 0 ]" "stderr=$(cat "$TMP/err.txt")"
assert "standalone: valid JSON" "printf '%s' \"\$out\" | jq -e . >/dev/null" "$out"
assert "standalone: kind" "[ \"$(printf '%s' "$out" | jq -r .kind)\" = standalone ]" "$out"
assert "standalone: parent null" "[ \"$(printf '%s' "$out" | jq -r .parent)\" = null ]" "$out"
assert "standalone: sub_issues empty" "[ \"$(printf '%s' "$out" | jq -c .sub_issues)\" = '[]' ]" "$out"
assert "standalone: siblings empty" "[ \"$(printf '%s' "$out" | jq -c .siblings)\" = '[]' ]" "$out"
assert "standalone: all_sub_issues_closed false" "[ \"$(printf '%s' "$out" | jq -r .all_sub_issues_closed)\" = false ]" "$out"
assert "standalone: all_siblings_closed false" "[ \"$(printf '%s' "$out" | jq -r .all_siblings_closed)\" = false ]" "$out"
assert "standalone: repo from gh repo view" "[ \"$(printf '%s' "$out" | jq -r .repo)\" = owner/repo ]" "$out"
assert "standalone: no warnings" "[ \"$(printf '%s' "$out" | jq -c .warnings)\" = '[]' ]" "$out"

# --- ケース2: Sub Issue（親あり、兄弟に open が残る） ---
reset_data
issue_json 21 open 0 0 > "$GH_STUB_DATA/repos_owner_repo_issues_21.json"
issue_json 20 open 3 1 > "$GH_STUB_DATA/repos_owner_repo_issues_21_parent.json"
sub_list_json > "$GH_STUB_DATA/repos_owner_repo_issues_21_sub_issues.json"
sub_list_json 21:open 22:closed 23:open > "$GH_STUB_DATA/repos_owner_repo_issues_20_sub_issues.json"

out=$(run 21); status=$?
assert "sub: exit 0" "[ $status -eq 0 ]" "stderr=$(cat "$TMP/err.txt")"
assert "sub: kind" "[ \"$(printf '%s' "$out" | jq -r .kind)\" = sub ]" "$out"
assert "sub: parent number" "[ \"$(printf '%s' "$out" | jq -r .parent.number)\" = 20 ]" "$out"
assert "sub: parent fields" "printf '%s' \"\$out\" | jq -e '(.parent | keys) == [\"number\",\"state\",\"title\",\"url\"]' >/dev/null" "$out"
assert "sub: siblings exclude self" "[ \"$(printf '%s' "$out" | jq -c '[.siblings[].number]')\" = '[22,23]' ]" "$out"
assert "sub: all_siblings_closed false" "[ \"$(printf '%s' "$out" | jq -r .all_siblings_closed)\" = false ]" "$out"
assert "sub: parent's manual close data not fetched" "! grep -q 'issues/20/parent' \"$GH_STUB_DATA/.calls\"" "$(cat "$GH_STUB_DATA/.calls")"

# --- ケース3: Sub Issue（兄弟がすべて closed → 最後の Sub） ---
reset_data
issue_json 21 open 0 0 > "$GH_STUB_DATA/repos_owner_repo_issues_21.json"
issue_json 20 open 3 2 > "$GH_STUB_DATA/repos_owner_repo_issues_21_parent.json"
sub_list_json > "$GH_STUB_DATA/repos_owner_repo_issues_21_sub_issues.json"
sub_list_json 22:closed 21:open 23:closed > "$GH_STUB_DATA/repos_owner_repo_issues_20_sub_issues.json"

out=$(run 21); status=$?
assert "last sub: exit 0" "[ $status -eq 0 ]" "stderr=$(cat "$TMP/err.txt")"
assert "last sub: all_siblings_closed true" "[ \"$(printf '%s' "$out" | jq -r .all_siblings_closed)\" = true ]" "$out"

# --- ケース4: 親 Issue（Sub が 2 ページ、全 closed） ---
reset_data
issue_json 30 open 3 3 > "$GH_STUB_DATA/repos_owner_repo_issues_30.json"
touch "$GH_STUB_DATA/repos_owner_repo_issues_30_parent.404"
sub_list_json 31:closed 32:closed > "$GH_STUB_DATA/repos_owner_repo_issues_30_sub_issues.json"
sub_list_json 33:closed > "$GH_STUB_DATA/repos_owner_repo_issues_30_sub_issues.page2.json"

out=$(run 30); status=$?
assert "parent: exit 0" "[ $status -eq 0 ]" "stderr=$(cat "$TMP/err.txt")"
assert "parent: kind" "[ \"$(printf '%s' "$out" | jq -r .kind)\" = parent ]" "$out"
assert "parent: sub_issues across pages" "[ \"$(printf '%s' "$out" | jq -c '[.sub_issues[].number]')\" = '[31,32,33]' ]" "$out"
assert "parent: sub fields" "printf '%s' \"\$out\" | jq -e '(.sub_issues[0] | keys) == [\"number\",\"state\",\"title\",\"url\"]' >/dev/null" "$out"
assert "parent: all_sub_issues_closed true" "[ \"$(printf '%s' "$out" | jq -r .all_sub_issues_closed)\" = true ]" "$out"
assert "parent: paginate requested" "grep -q 'issues/30/sub_issues?per_page=100' \"$GH_STUB_DATA/.calls\"" "$(cat "$GH_STUB_DATA/.calls")"

# --- ケース5: 親 Issue（open の Sub が残る） ---
reset_data
issue_json 30 open 2 1 > "$GH_STUB_DATA/repos_owner_repo_issues_30.json"
touch "$GH_STUB_DATA/repos_owner_repo_issues_30_parent.404"
sub_list_json 31:closed 32:open > "$GH_STUB_DATA/repos_owner_repo_issues_30_sub_issues.json"

out=$(run 30); status=$?
assert "parent (open subs): all_sub_issues_closed false" "[ \"$(printf '%s' "$out" | jq -r .all_sub_issues_closed)\" = false ]" "$out"

# --- ケース6: 親でもあり Sub でもある（中間ノード） ---
reset_data
issue_json 41 open 1 0 > "$GH_STUB_DATA/repos_owner_repo_issues_41.json"
issue_json 40 open 1 0 > "$GH_STUB_DATA/repos_owner_repo_issues_41_parent.json"
sub_list_json 42:open > "$GH_STUB_DATA/repos_owner_repo_issues_41_sub_issues.json"
sub_list_json 41:open > "$GH_STUB_DATA/repos_owner_repo_issues_40_sub_issues.json"

out=$(run 41); status=$?
assert "middle: kind" "[ \"$(printf '%s' "$out" | jq -r .kind)\" = parent_and_sub ]" "$out"
assert "middle: siblings empty (only child)" "[ \"$(printf '%s' "$out" | jq -c .siblings)\" = '[]' ]" "$out"
assert "middle: all_siblings_closed true when no siblings" "[ \"$(printf '%s' "$out" | jq -r .all_siblings_closed)\" = true ]" "$out"

# --- ケース7: -R でリポジトリを明示（gh repo view を呼ばない） ---
reset_data
issue_json 10 open 0 0 > "$GH_STUB_DATA/repos_acme_widgets_issues_10.json"
touch "$GH_STUB_DATA/repos_acme_widgets_issues_10_parent.404"
sub_list_json > "$GH_STUB_DATA/repos_acme_widgets_issues_10_sub_issues.json"

out=$(run 10 -R acme/widgets); status=$?
assert "-R: exit 0" "[ $status -eq 0 ]" "stderr=$(cat "$TMP/err.txt")"
assert "-R: repo echoed" "[ \"$(printf '%s' "$out" | jq -r .repo)\" = acme/widgets ]" "$out"

# --- ケース8: 親の照会が 404 以外で失敗 → warnings に載せて parent null（縮退） ---
reset_data
issue_json 10 open 0 0 > "$GH_STUB_DATA/repos_owner_repo_issues_10.json"
touch "$GH_STUB_DATA/repos_owner_repo_issues_10_parent.fail"
sub_list_json > "$GH_STUB_DATA/repos_owner_repo_issues_10_sub_issues.json"

out=$(run 10); status=$?
assert "parent lookup failure: exit 0" "[ $status -eq 0 ]" "stderr=$(cat "$TMP/err.txt")"
assert "parent lookup failure: parent null" "[ \"$(printf '%s' "$out" | jq -r .parent)\" = null ]" "$out"
assert "parent lookup failure: warning present" "printf '%s' \"\$out\" | jq -e '.warnings | length == 1 and (.[0] | test(\"parent\"))' >/dev/null" "$out"
assert "parent lookup failure: kind unknown-safe (standalone)" "[ \"$(printf '%s' "$out" | jq -r .kind)\" = standalone ]" "$out"

# --- ケース9: Sub 一覧の取得失敗 → warnings + 空配列（縮退） ---
reset_data
issue_json 30 open 2 1 > "$GH_STUB_DATA/repos_owner_repo_issues_30.json"
touch "$GH_STUB_DATA/repos_owner_repo_issues_30_parent.404"
touch "$GH_STUB_DATA/repos_owner_repo_issues_30_sub_issues.fail"

out=$(run 30); status=$?
assert "sub list failure: exit 0" "[ $status -eq 0 ]" "stderr=$(cat "$TMP/err.txt")"
assert "sub list failure: sub_issues empty" "[ \"$(printf '%s' "$out" | jq -c .sub_issues)\" = '[]' ]" "$out"
assert "sub list failure: kind still parent (summary says total>0)" "[ \"$(printf '%s' "$out" | jq -r .kind)\" = parent ]" "$out"
assert "sub list failure: all_sub_issues_closed false" "[ \"$(printf '%s' "$out" | jq -r .all_sub_issues_closed)\" = false ]" "$out"
assert "sub list failure: warning present" "printf '%s' \"\$out\" | jq -e '.warnings | length == 1 and (.[0] | test(\"sub_issues\"))' >/dev/null" "$out"

# --- ケース10: 前提不成立は非ゼロ exit + 英語 stderr ---
reset_data
issue_json 10 open 0 0 > "$GH_STUB_DATA/repos_owner_repo_issues_10.json"

out=$(run); status=$?
assert "no args: exit 1" "[ $status -eq 1 ]" "status=$status"
assert "no args: usage on stderr" "grep -q 'usage:' \"$TMP/err.txt\"" "$(cat "$TMP/err.txt")"

out=$(run abc); status=$?
assert "non-numeric: exit 1" "[ $status -eq 1 ]" "status=$status"
assert "non-numeric: stdout empty" "[ -z \"\$out\" ]" "stdout=$out"

out=$(run 10 --bogus); status=$?
assert "unknown flag: exit 1" "[ $status -eq 1 ]" "status=$status"

out=$(run 99); status=$?
assert "issue not found: exit 1" "[ $status -eq 1 ]" "status=$status"
assert "issue not found: stderr mentions issue" "grep -qi 'issue' \"$TMP/err.txt\"" "$(cat "$TMP/err.txt")"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
