# 投稿（返信・解決・完了報告・再依頼）

## スレッドへの返信と解決

**先に修正をコミットして push する**。ドキュメントは取り直さない — `ccx pr reply-threads` はドキュメントの `pr.head_oid` がローカル HEAD の祖先であれば受理する。push で `line` が null になったスレッドも、push 前のドキュメントから書いたセレクタのまま指せる。`threads_path` に対象スレッドを書き、投稿する:

```bash
ccx pr reply-threads <ドキュメント> <threads_path>
```

書式は `ccx pr reply-threads --help` にある。本スキルが決めるのは中身:

- スレッドは `path` と `line` で指す。`id` は書かなくてよい（必要なときはコマンドが候補を挙げて要求してくる）
- `resolve` は当該スレッドの `resolvable_by_me` と同値にする
- `body` は「判断基準 > 原則」の言語（対象 PR の記述言語）で書く。1行を超える返信は素の Markdown を `work_dir` 直下に Write して `body_file` で指す
- `body` を省いて `resolve` だけにするのは2つの場合 — 前回の返信から言うことが変わっていないとき（bot スレッドの再実行）と、resolve だけが失敗して再試行するとき

`--dry-run` を足すと、同じ検査をすべて通した上で何も投稿せず計画だけを出す（dry-run モードの判断と案の報告で使う）。

実行後:

- 返信・解決したスレッド数をユーザーに報告する
- `resolve_failed[]` が空でない場合: **返信済みなので同じ本文で再実行しない**。`body` を省いた resolve のみのエントリで再試行する。warning はユーザーへの報告に併記する
- 拒否された場合（セレクタの曖昧さ・古いコンテキスト等）は stderr の指示に従う。**指摘を落として通すのではなく、指定か前提の方を直す**

## 修正完了報告フォーマット

構造の規定であり、文面は対象 PR の記述言語で書く（以下の例は日本語 PR の場合）:
```
<修正内容>を対応しました。
https://github.com/<owner>/<repo>/pull/<PR番号>/commits/<コミットハッシュ>
```

PR 本体へのコメントとして投稿する場合は、上記の本文を `work_dir` 直下に Write し、push を済ませてから投稿する:

```bash
ccx pr comment <ドキュメント> --body-file <work_dir 直下のファイル名>
```

## レビューの再依頼（REST）

対象・実行条件・失敗時の扱いは本文の「レビュー再依頼」:
```bash
gh api repos/<owner>/<repo>/pulls/<PR番号>/requested_reviewers \
  -f 'reviewers[]=LOGIN1' -f 'reviewers[]=LOGIN2'
```
- `<owner>/<repo>` は `repo`、`<PR番号>` は `pr.number` を使う
