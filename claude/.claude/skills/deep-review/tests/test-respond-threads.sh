#!/bin/bash

# respond-threads.sh のリグレッションテスト
#
# 実行: bash claude/.claude/skills/deep-review/tests/test-respond-threads.sh
# gh スタブ（GH_BIN 差し替え）で完結し、実 gh・実リポジトリには触れない。
# 失敗したケースがあれば exit 1 で終了する。

set -u

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT="$SCRIPT_DIR/../scripts/respond-threads.sh"

if [ ! -f "$SCRIPT" ]; then
  printf 'ERROR: script not found: %s\n' "$SCRIPT" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# --- 使い捨て git リポジトリ ---
# スクリプトが投稿前にローカル HEAD == pr.head_oid を再確認するため、実 HEAD を持つ
# リポジトリ内で実行する必要がある（実リポジトリには触れない）
git init -q -b main "$TMP/repo" 2>/dev/null
git -C "$TMP/repo" -c user.email=t@example.com -c user.name=t commit -q --allow-empty -m init
HEAD_OID=$(git -C "$TMP/repo" rev-parse HEAD)

# --- gh スタブ ---
# mutation の種別は query 文字列で判別する（引数位置に依存させない）。
# 失敗の注入は env ではなくファイルで行う: `VAR=x shell_function` の assignment は
# 関数内で spawn する子プロセス（このスタブ）に export されないため、env 注入だと
# 「失敗させたつもりが素通り」で silent に無効化される。
# 呼び出しは CALL_LOG に 1 行 1 呼び出しで記録し、「検証違反時に mutation を
# 1 度も打たない」性質をテストから確認できるようにする
mkdir -p "$TMP/stub"
cat > "$TMP/stub/gh" <<'EOF'
#!/bin/bash
# 成功時にも stderr へ出す（gh の deprecation 警告等）。スクリプトが 2>&1 で stdout に
# 混ぜていると JSON パースが壊れるため、全ケースがこの分離を検証することになる
printf 'gh: warning noise on stderr (must not reach stdout)\n' >&2
if [ "$1" != "api" ] || [ "$2" != "graphql" ]; then exit 1; fi
query=''
thread_id=''
body=''
for a in "$@"; do
  case "$a" in
    query=*) query=${a#query=} ;;
    threadId=*) thread_id=${a#threadId=} ;;
    body=*) body=${a#body=} ;;
  esac
done
fails_for() { [ -f "$1" ] && grep -qxF "$thread_id" "$1"; }
case "$query" in
  *addPullRequestReviewThreadReply*)
    # 改行は行指向のログに載せるため RS(0x1e) へ置換して記録する
    printf 'reply\t%s\t%s\n' "$thread_id" "$(printf '%s' "$body" | tr '\n' '\036')" >> "$GH_STUB_CALL_LOG"
    if fails_for "$GH_STUB_DIR/reply-fail"; then
      printf 'HTTP 403: reply forbidden\n' >&2
      exit 1
    fi
    printf '{"data":{"addPullRequestReviewThreadReply":{"comment":{"url":"https://example.com/reply-%s"}}}}\n' "$thread_id"
    ;;
  *resolveReviewThread*)
    printf 'resolve\t%s\n' "$thread_id" >> "$GH_STUB_CALL_LOG"
    if fails_for "$GH_STUB_DIR/resolve-fail"; then
      printf 'HTTP 403: Resource not accessible by integration\n' >&2
      exit 1
    fi
    printf '{"data":{"resolveReviewThread":{"thread":{"isResolved":true}}}}\n'
    ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$TMP/stub/gh"
export GH_BIN="$TMP/stub/gh"
export GH_STUB_DIR="$TMP"
export GH_STUB_CALL_LOG="$TMP/calls.log"

# --- フィクスチャ ---
# コンテキストは実物と同じ命名（pr-context-<owner>@<repo>-<PR>.json）にする。
# 入力ファイル名の検証はこの名前から `pr-context-` を strip した識別子で行われるため、
# フィクスチャを固定名にすると検証の対象そのものがテストされない
CTX="$TMP/pr-context-owner@repo-5.json"
# PRRT_A / PRRT_B: 対象適格（awaiting_my_confirmation true）
# PRRT_X: 未解決だが起点が他人 / PRRT_Y: 解決済み → どちらも不適格
jq -n --arg oid "$HEAD_OID" '{
  repo: "owner/repo",
  current_user: "testuser",
  is_own_pr: false,
  pr: {number: 5, head_oid: $oid},
  review_threads: [
    {id: "PRRT_A", is_resolved: false, path: "a.go", line: 1, awaiting_my_confirmation: true},
    {id: "PRRT_B", is_resolved: false, path: "b.go", line: 2, awaiting_my_confirmation: true},
    {id: "PRRT_X", is_resolved: false, path: "x.go", line: 3, awaiting_my_confirmation: false},
    {id: "PRRT_Y", is_resolved: true, path: "y.go", line: 4, awaiting_my_confirmation: false}
  ]
}' > "$CTX"

pass=0
fail=0

assert() {
  local name=$1 json=$2 expr=$3
  if printf '%s' "$json" | jq -e "$expr" >/dev/null 2>&1; then
    pass=$((pass + 1)); printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1)); printf 'FAIL  %s\n' "$name"
  fi
}

assert_exit() {
  local name=$1 got=$2 want=$3
  if [ "$got" -eq "$want" ]; then
    pass=$((pass + 1)); printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1)); printf 'FAIL  %s (got exit %d, want %d)\n' "$name" "$got" "$want"
  fi
}

# 条件をそのままコマンドとして渡す（`[ ... ]` / grep 等）
assert_ok() {
  local name=$1
  shift
  if "$@"; then
    pass=$((pass + 1)); printf 'PASS  %s\n' "$name"
  else
    fail=$((fail + 1)); printf 'FAIL  %s\n' "$name"
  fi
}

# 既定では sidecar（投稿済み記録）を消してから実行する。多くのケースが同じ入力ファイルを
# 再利用するため、消さないと run をまたぐ再送ガードに引っかかる。
# ガード自体は run_again で明示的に検証する
run() {
  : > "$GH_STUB_CALL_LOG"
  rm -f "$1.posted"
  (cd "$TMP/repo" && bash "$SCRIPT" "$CTX" "$1" 2>"$TMP/err.txt")
}
# sidecar を残したまま同じ入力で再実行する（run をまたぐ再送の検証用）
run_again() {
  : > "$GH_STUB_CALL_LOG"
  (cd "$TMP/repo" && bash "$SCRIPT" "$CTX" "$1" 2>"$TMP/err.txt")
}
calls() { cat "$GH_STUB_CALL_LOG"; }
inject_fail() { printf '%s\n' "$2" > "$TMP/$1-fail"; }
clear_fail() { rm -f "$TMP/reply-fail" "$TMP/resolve-fail"; }
no_mutation() { [ ! -s "$GH_STUB_CALL_LOG" ]; }
no_call_matching() { ! grep -q "$1" "$GH_STUB_CALL_LOG"; }

# 入力ファイル名は prepare-review.sh が払い出す規約（末尾が <owner>@<repo>-<PR>.json）に従う。
# 固定名だと並列サブエージェント間で上書きされるため、スクリプトが名前を検証する
write_input() { printf '%s' "$2" > "$TMP/$1-owner@repo-5.json"; printf '%s' "$TMP/$1-owner@repo-5.json"; }

# --- 正常系: 返信 + resolve ---
in_ok=$(write_input ok '{"threads": [{"id": "PRRT_A", "body": "対応を確認しました", "resolve": true}]}')
out=$(run "$in_ok")
assert_exit 'reply+resolve: exit 0' $? 0
assert 'reply+resolve: replied/resolved reported' "$out" \
  '.replied == [{"id": "PRRT_A", "url": "https://example.com/reply-PRRT_A"}]
   and .resolved == ["PRRT_A"] and .resolve_failed == [] and .warnings == []'
# 順序が逆だと「解決済みスレッドに返信」になり通知が埋もれる
assert_ok 'reply+resolve: reply precedes resolve for the same thread' \
  [ "$(calls)" = "$(printf 'reply\tPRRT_A\t対応を確認しました\nresolve\tPRRT_A')" ]

# --- resolve: false は返信のみ（resolve mutation を打たない） ---
in_nores=$(write_input noresolve '{"threads": [{"id": "PRRT_A", "body": "まだ解消していません", "resolve": false}]}')
out=$(run "$in_nores")
assert_exit 'reply only: exit 0' $? 0
assert 'reply only: replied but nothing resolved' "$out" \
  '(.replied | length) == 1 and .resolved == []'
assert_ok 'reply only: no resolveReviewThread call' no_call_matching '^resolve'

# --- 複数スレッドを順に処理 ---
in_multi=$(write_input multi '{"threads": [{"id": "PRRT_A", "body": "確認", "resolve": true}, {"id": "PRRT_B", "body": "未解消", "resolve": false}]}')
out=$(run "$in_multi")
assert_exit 'multiple threads: exit 0' $? 0
assert 'multiple threads: both replied, only one resolved' "$out" \
  '[.replied[].id] == ["PRRT_A", "PRRT_B"] and .resolved == ["PRRT_A"]'

# --- 改行を含む本文がそのまま渡ること（返信本文が壊れると議論が読めなくなる） ---
in_ml=$(write_input multiline '{"threads": [{"id": "PRRT_A", "body": "1行目\n\n2行目", "resolve": false}]}')
run "$in_ml" >/dev/null
assert_ok 'multiline body passed through intact' \
  grep -qF "$(printf '1行目\036\0362行目')" "$GH_STUB_CALL_LOG"

# --- 検証違反: 副作用ゼロで停止すること ---
# 不適格スレッド（起点が他人）を resolve すると越権になるため、スクリプト側で構造的に止める
in_inelig=$(write_input inelig '{"threads": [{"id": "PRRT_X", "body": "x", "resolve": true}]}')
run "$in_inelig" >/dev/null
assert_exit 'ineligible thread (opened by someone else): exit 1' $? 1
assert_ok 'ineligible thread: no mutation issued' no_mutation
assert_ok 'ineligible thread: stderr names the id' grep -q 'PRRT_X' "$TMP/err.txt"

in_resolved=$(write_input already '{"threads": [{"id": "PRRT_Y", "body": "y", "resolve": true}]}')
run "$in_resolved" >/dev/null
assert_exit 'already resolved thread: exit 1' $? 1
assert_ok 'already resolved thread: no mutation issued' no_mutation

in_unknown=$(write_input unknown '{"threads": [{"id": "PRRT_ZZZ", "body": "z", "resolve": false}]}')
run "$in_unknown" >/dev/null
assert_exit 'unknown thread id: exit 1' $? 1
assert_ok 'unknown thread id: no mutation issued' no_mutation

# 重複 id は二重返信になるため事前に弾く
in_dup=$(write_input dup '{"threads": [{"id": "PRRT_A", "body": "1", "resolve": false}, {"id": "PRRT_A", "body": "2", "resolve": true}]}')
run "$in_dup" >/dev/null
assert_exit 'duplicate thread id: exit 1' $? 1
assert_ok 'duplicate thread id: no mutation issued' no_mutation

# 適格・不適格の混在でも、適格分だけ先に投稿する部分適用にしない
in_mixed=$(write_input mixed '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}, {"id": "PRRT_X", "body": "x", "resolve": true}]}')
run "$in_mixed" >/dev/null
assert_exit 'mixed eligible/ineligible: exit 1' $? 1
assert_ok 'mixed eligible/ineligible: no partial posting' no_mutation

# --- 入力契約違反 ---
for bad_name in badbodytype badresolve notarray badid nothreads; do
  case "$bad_name" in
    badbodytype) bad='{"threads": [{"id": "PRRT_A", "body": 5, "resolve": true}]}' ;;
    badresolve) bad='{"threads": [{"id": "PRRT_A", "body": "a", "resolve": "yes"}]}' ;;
    notarray)   bad='{"threads": {"id": "PRRT_A"}}' ;;
    badid)      bad='{"threads": [{"id": 5, "body": "a", "resolve": true}]}' ;;
    nothreads)  bad='{}' ;;
  esac
  in_bad=$(write_input "bad-$bad_name" "$bad")
  run "$in_bad" >/dev/null
  assert_exit "malformed input ($bad_name): exit 1" $? 1
  assert_ok "malformed input ($bad_name): no mutation issued" no_mutation
done

# body があるのに空白だけ = 「返信したつもりで中身が無い」ので弾く（省略とは区別する）
in_blank=$(write_input blankbody '{"threads": [{"id": "PRRT_A", "body": "   ", "resolve": true}]}')
run "$in_blank" >/dev/null
assert_exit 'blank body rejected' $? 1
assert_ok 'blank body: no mutation issued' no_mutation
assert_ok 'blank body: stderr distinguishes it from omitting body' \
  grep -q 'omit body entirely' "$TMP/err.txt"

# --- body 省略 = 返信せず resolve のみ ---
# resolve 権限がないリポジトリでは resolve が恒久的に失敗するため、次回レビューで
# 返信を重ねずに resolve だけ再試行する経路が必要（判定が前回と同じ場合の抑止にも使う）
in_resolveonly=$(write_input resolveonly '{"threads": [{"id": "PRRT_A", "resolve": true}]}')
out=$(run "$in_resolveonly")
assert_exit 'resolve-only (body omitted): exit 0' $? 0
assert 'resolve-only: resolved without replying' "$out" \
  '.replied == [] and .resolved == ["PRRT_A"]'
assert_ok 'resolve-only: no reply mutation issued' no_call_matching '^reply'
assert_ok 'resolve-only: resolve mutation issued' grep -q '^resolve' "$GH_STUB_CALL_LOG"
# 返信していないので sidecar にも記録しない（次回も resolve のみ再試行できる）
assert_ok 'resolve-only: nothing recorded in the posted sidecar' \
  [ ! -s "$in_resolveonly.posted" ]

# body 省略 + resolve: false は何もしない指定なので弾く
in_noop=$(write_input noop '{"threads": [{"id": "PRRT_A", "resolve": false}]}')
run "$in_noop" >/dev/null
assert_exit 'neither body nor resolve: exit 1' $? 1
assert_ok 'neither body nor resolve: no mutation issued' no_mutation

# --- 空配列は正常な no-op ---
in_none=$(write_input none '{"threads": []}')
out=$(run "$in_none")
assert_exit 'empty threads: exit 0' $? 0
assert 'empty threads: empty result arrays' "$out" \
  '.replied == [] and .resolved == [] and .resolve_failed == [] and .warnings == []'
assert_ok 'empty threads: no mutation issued' no_mutation

# --- resolve 失敗は縮退（返信は残し、warning で報告して exit 0） ---
# fork PR・write 権限なしで resolveReviewThread だけ失敗するケース。
# ここで exit 1 にするとレビュー運用全体が止まるため、返信済みの事実を出力に残して続行する
inject_fail resolve PRRT_A
out=$(run "$in_multi")
assert_exit 'resolve failure: exit 0 (degraded)' $? 0
assert 'resolve failure: reply kept, resolve reported as failed' "$out" \
  '[.replied[].id] == ["PRRT_A", "PRRT_B"] and .resolved == []
   and [.resolve_failed[].id] == ["PRRT_A"] and (.warnings | length) > 0'
assert 'resolve failure: error text carried in output' "$out" \
  '.resolve_failed[0].error | test("403")'

# resolve 失敗後も後続スレッドの処理を止めない
in_multi2=$(write_input multi2 '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}, {"id": "PRRT_B", "body": "b", "resolve": true}]}')
out=$(run "$in_multi2")
assert_exit 'resolve failure: subsequent threads still processed' $? 0
assert 'resolve failure: later thread resolved normally' "$out" \
  '.resolved == ["PRRT_B"] and [.resolve_failed[].id] == ["PRRT_A"]'
clear_fail

# --- 返信失敗は停止し、投稿済み/未処理を stderr で切り分けられること ---
# （再実行時に二重返信しないため、どこまで進んだかの報告が必須）
inject_fail reply PRRT_A
in_multi3=$(write_input multi3 '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}, {"id": "PRRT_B", "body": "b", "resolve": true}]}')
run "$in_multi3" >/dev/null
assert_exit 'reply failure: exit 1' $? 1
assert_ok 'reply failure: stderr names the failed thread' grep -q 'PRRT_A' "$TMP/err.txt"
assert_ok 'reply failure: stderr names the unprocessed thread' grep -q 'PRRT_B' "$TMP/err.txt"
assert_ok 'reply failure: does not proceed to reply the rest' \
  no_call_matching "$(printf 'reply\tPRRT_B')"

# 途中失敗では投稿済みスレッドを stderr で切り分けられること
inject_fail reply PRRT_B
run "$in_multi3" >/dev/null
assert_exit 'mid-run reply failure: exit 1' $? 1
assert_ok 'mid-run reply failure: already-replied thread reported' \
  grep -q 'PRRT_A' "$TMP/err.txt"
clear_fail

# --- 入力ファイル名の PR 束縛 ---
# 並列サブエージェントは scratchpad を共有するため、固定名の入力は別 PR のレビューに
# 上書きされる。適格性検証でも止まるが原因を誤診させるため、名前の側で先に落とす
printf '%s' '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}]}' > "$TMP/threads.json"
run "$TMP/threads.json" >/dev/null
assert_exit 'fixed-name input rejected: exit 1' $? 1
assert_ok 'fixed-name input: no mutation issued' no_mutation
assert_ok 'fixed-name input: stderr points at threads_path' grep -q 'threads_path' "$TMP/err.txt"

# 別 PR の識別子が付いたファイル（他 PR のレビューに上書きされた状態）も拒否する
printf '%s' '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}]}' > "$TMP/threads-owner@repo-99.json"
run "$TMP/threads-owner@repo-99.json" >/dev/null
assert_exit 'input bound to another PR rejected: exit 1' $? 1
assert_ok 'another PR input: no mutation issued' no_mutation

# --- run をまたぐ再送の拒否 ---
# 適格性は fetch 時点で凍結された context のフラグで判定するため、同じ入力を再実行すると
# 素通りして二重投稿になる。返信失敗時の再実行は正規フローとして案内しているので踏みやすい
in_resend=$(write_input resend '{"threads": [{"id": "PRRT_A", "body": "確認しました", "resolve": false}]}')
out=$(run "$in_resend")
assert_exit 'first run: exit 0' $? 0
assert_ok 'first run: posted id recorded in the sidecar' \
  grep -qxF 'PRRT_A' "$in_resend.posted"
run_again "$in_resend" >/dev/null
assert_exit 'rerunning the same input file: exit 1' $? 1
assert_ok 'rerunning the same input file: no mutation issued' no_mutation
assert_ok 'rerunning: stderr names the id and the sidecar' \
  grep -q 'PRRT_A' "$TMP/err.txt"

# 未処理分だけに書き直せば再実行は通る（案内しているフローが実際に機能すること）
printf '%s' '{"threads": [{"id": "PRRT_B", "body": "確認しました", "resolve": false}]}' > "$in_resend"
out=$(run_again "$in_resend")
assert_exit 'rewritten to the unprocessed thread only: exit 0' $? 0
assert 'rewritten input: the remaining thread is replied to' "$out" '[.replied[].id] == ["PRRT_B"]'

# --- 投稿前の鮮度再確認（post-review.sh と対） ---
# 差分を読んだ時点から head が動いていると、取り消された修正に対して resolve しうる
jq --arg oid 0000000000000000000000000000000000000000 '.pr.head_oid = $oid' "$CTX" \
  > "$TMP/pr-context-owner@repo-9.json"
: > "$GH_STUB_CALL_LOG"
in_stale=$(write_input stale '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}]}')
cp "$in_stale" "$TMP/stale-owner@repo-9.json"
(cd "$TMP/repo" && bash "$SCRIPT" "$TMP/pr-context-owner@repo-9.json" "$TMP/stale-owner@repo-9.json" 2>"$TMP/err.txt")
assert_exit 'stale head: exit 1' $? 1
assert_ok 'stale head: no mutation issued' no_mutation
assert_ok 'stale head: stderr mentions the PR head' grep -q 'differs from PR head' "$TMP/err.txt"

# git リポジトリ外での実行も止める（HEAD が取れないまま投稿しない）
: > "$GH_STUB_CALL_LOG"
(cd "$TMP" && bash "$SCRIPT" "$CTX" "$in_ok" 2>"$TMP/err.txt")
assert_exit 'outside a git repository: exit 1' $? 1
assert_ok 'outside a git repository: no mutation issued' no_mutation

# --- 返信は成立したが url が欠落した場合 ---
# 返信済みなので、投稿済み側に分類して報告し sidecar にも記録しなければ再実行で二重返信になる
cat > "$TMP/stub/gh-nourl" <<'EOF'
#!/bin/bash
for a in "$@"; do case "$a" in query=*) q=${a#query=} ;; threadId=*) t=${a#threadId=} ;; esac; done
case "$q" in
  *addPullRequestReviewThreadReply*)
    printf 'reply\t%s\n' "$t" >> "$GH_STUB_CALL_LOG"
    printf '{"data":{"addPullRequestReviewThreadReply":{"comment":{}}}}\n' ;;
  *) printf 'resolve\t%s\n' "$t" >> "$GH_STUB_CALL_LOG"
     printf '{"data":{"resolveReviewThread":{"thread":{"isResolved":true}}}}\n' ;;
esac
EOF
chmod +x "$TMP/stub/gh-nourl"
in_nourl=$(write_input nourl '{"threads": [{"id": "PRRT_A", "body": "a", "resolve": true}, {"id": "PRRT_B", "body": "b", "resolve": true}]}')
rm -f "$in_nourl.posted"
: > "$GH_STUB_CALL_LOG"
(cd "$TMP/repo" && GH_BIN="$TMP/stub/gh-nourl" bash "$SCRIPT" "$CTX" "$in_nourl" 2>"$TMP/err.txt")
assert_exit 'missing reply url: exit 1' $? 1
assert_ok 'missing reply url: the posted reply is recorded in the sidecar' \
  grep -qxF 'PRRT_A' "$in_nourl.posted"
assert_ok 'missing reply url: reported as already replied, not unprocessed' \
  grep -q 'already replied (do NOT resend on retry): PRRT_A' "$TMP/err.txt"
assert_ok 'missing reply url: the untouched thread is reported as unprocessed' \
  grep -q 'not processed: PRRT_B' "$TMP/err.txt"

# --- 引数・ファイル不備 ---
bash "$SCRIPT" >/dev/null 2>&1
assert_exit 'missing args: exit 1' $? 1
bash "$SCRIPT" "$CTX" >/dev/null 2>&1
assert_exit 'missing threads file: exit 1' $? 1
bash "$SCRIPT" "$TMP/no-such.json" "$in_ok" >/dev/null 2>&1
assert_exit 'nonexistent context file: exit 1' $? 1
bash "$SCRIPT" "$CTX" "$TMP/no-such.json" >/dev/null 2>&1
assert_exit 'nonexistent threads file: exit 1' $? 1

printf '\n%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] || exit 1
