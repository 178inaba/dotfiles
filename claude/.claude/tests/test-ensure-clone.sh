#!/bin/bash

# ensure-clone.sh のリグレッションテスト
#
# 実行: bash claude/.claude/tests/test-ensure-clone.sh
# 使い捨ての bare リポジトリを origin 代わりに使い、gh スタブが git clone に置き換える。
# 実 gh・実 GitHub には触れない。失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../skills/review-assigned-prs/scripts/ensure-clone.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# --- origin として使う bare リポジトリを組む ---
git init -q --bare -b main "$TMP/origin.git"
git clone -q "$TMP/origin.git" "$TMP/seed" 2>/dev/null
(
  cd "$TMP/seed"
  git config user.email test@example.com
  git config user.name test
  git commit -q --allow-empty -m "initial"
  git push -q origin main
)

# --- gh スタブ ---
# gh repo clone <owner>/<repo> <dest> → git clone <bare> <dest> に置換
# 呼び出し内容は GH_STUB_LOG に追記して、後から clone/fetch のどちらが走ったか判別する
mkdir -p "$TMP/stub"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
printf '%s\n' "$*" >> "$GH_STUB_LOG"
if [ "$1" = "repo" ] && [ "$2" = "clone" ]; then
  git clone -q "$GH_STUB_ORIGIN" "$4"
else
  exit 1
fi
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_ORIGIN="$TMP/origin.git"
export GH_STUB_LOG="$TMP/gh-log"

# HOME・XDG_DATA_HOME を隔離
export HOME="$TMP/home"
export XDG_DATA_HOME="$TMP/xdg"
mkdir -p "$HOME"

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

assert_json() {
  local name=$1 json=$2 expr=$3
  if printf '%s' "$json" | jq -e "$expr" >/dev/null 2>&1; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s\n' "$name"
  fi
}

assert_exit() {
  local name=$1 got=$2 want=$3
  if [ "$got" -eq "$want" ]; then
    pass=$((pass + 1))
    printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1))
    printf 'FAIL  %s (got exit %d, want %d)\n' "$name" "$got" "$want"
  fi
}

# --- ケース1: 未 clone → clone が走る ---
: > "$GH_STUB_LOG"
out=$(bash "$SCRIPT" acme/foo)
assert_exit 'initial clone: exit 0' $? 0
assert_json 'output has path field' "$out" '.path | type == "string"'

path=$(printf '%s' "$out" | jq -r '.path')
expected_path="$XDG_DATA_HOME/claude-review-prs/acme/foo"
assert 'path matches XDG_DATA_HOME layout' "[ '$path' = '$expected_path' ]"
assert 'clone dir has .git' "[ -d '$path/.git' ]"
assert 'clone invocation recorded in log' "grep -q 'repo clone acme/foo' '$GH_STUB_LOG'"

# --- ケース2: 既 clone → fetch のみ、clone は呼ばれない ---
# origin に新しい commit を追加して fetch で取り込まれることを検証
(
  cd "$TMP/seed"
  git commit -q --allow-empty -m "second"
  git push -q origin main
)
: > "$GH_STUB_LOG"
out2=$(bash "$SCRIPT" acme/foo)
assert_exit 'second run (existing clone): exit 0' $? 0
assert 'no clone invocation on second run' "! grep -q 'repo clone' '$GH_STUB_LOG'"

# origin/main が新 commit まで進んだか（fetch が走った証拠）
second_msg=$(git -C "$path" log origin/main --oneline | head -1)
assert 'fetch updated origin/main to new commit' "printf '%s' '$second_msg' | grep -q 'second'"

# --- ケース3: XDG_DATA_HOME 未設定 → $HOME/.local/share にフォールバック ---
: > "$GH_STUB_LOG"
out3=$(env -u XDG_DATA_HOME HOME="$TMP/home" GH_BIN="$GH_BIN" GH_STUB_ORIGIN="$GH_STUB_ORIGIN" GH_STUB_LOG="$GH_STUB_LOG" bash "$SCRIPT" acme/newone)
assert_exit 'fallback to $HOME/.local/share: exit 0' $? 0
path3=$(printf '%s' "$out3" | jq -r '.path')
assert 'path uses $HOME/.local/share when XDG_DATA_HOME unset' \
  "[ '$path3' = '$TMP/home/.local/share/claude-review-prs/acme/newone' ]"

# --- ケース4: 中断 clone の self-heal（partial dir を次回のために掃除する） ---
# gh clone が dest dir を作った直後に失敗する状況を再現し、partial dir が掃除されることを検証
: > "$GH_STUB_LOG"
mkdir -p "$TMP/stub-fail"
cat > "$TMP/stub-fail/gh" <<'EOF'
#!/bin/bash
printf '%s\n' "$*" >> "$GH_STUB_LOG"
mkdir -p "$4"
exit 1
EOF
chmod +x "$TMP/stub-fail/gh"

GH_BIN="$TMP/stub-fail/gh" bash "$SCRIPT" acme/broken 2>/dev/null
assert_exit 'failing clone: non-zero exit' $? 1
assert 'partial clone dir cleaned up on failure' \
  "[ ! -e '$XDG_DATA_HOME/claude-review-prs/acme/broken' ]"

# --- ケース5: 引数不正 → 非ゼロ exit + stderr ---
bash "$SCRIPT" 2>"$TMP/err.txt"
assert_exit 'missing arg: non-zero exit' $? 1
assert 'missing arg: stderr present' "[ -s '$TMP/err.txt' ]"

bash "$SCRIPT" "no-slash" 2>"$TMP/err.txt"
assert_exit 'no slash: non-zero exit' $? 1
assert 'no slash: stderr present' "[ -s '$TMP/err.txt' ]"

for arg in "too/many/slashes" "/missing-owner" "missing-repo/"; do
  bash "$SCRIPT" "$arg" 2>/dev/null
  assert_exit "invalid arg [$arg]: non-zero exit" $? 1
done

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
