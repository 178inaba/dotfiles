# 投稿（返信・解決・完了報告・再依頼）

## スレッドへの返信と解決

**先に修正をコミットして push する**（返信コマンドはローカル HEAD が GitHub の live head であることを求めるため、push が済んでいないと拒否される。ドキュメントは取り直さない — コマンドはドキュメントの `pr.head_oid` がローカル HEAD の祖先であることを確認するので、push で先へ進んだ分は受理される）。push でコードが動き `line` が null になったスレッドも、同じ番号を `original_line` として保ち、コマンドはどちらにも一致するので、push 前のドキュメントから書いたセレクタはそのまま解決する。`threads_path` に対象スレッドを書き、投稿する:

```bash
ccx pr reply-threads <ドキュメント> <threads_path>
```

書式は `ccx pr reply-threads --help` にある。本スキルが決めるのは中身:

- スレッドは `path` と `line` で指す。`id` は書かなくてよい — 必要なときはコマンドが候補を挙げて要求してくる
- `resolve` は当該スレッドの `resolvable_by_me` と同値にする
- `body` は「判断基準 > 原則」の言語（対象 PR の記述言語）で書く。1行を超える返信は素の Markdown を `work_dir` 直下に Write して `body_file` で指す — JSON 文字列への手書きエスケープは1文字の欠落で JSON 全体が無効になるため
- `body` を省いて `resolve` だけにするのは2つの場合 — 前回の返信から言うことが変わっていないとき（bot スレッドの再実行）と、resolve だけが失敗して再試行するとき

`--dry-run` を足すと、同じ検査をすべて通した上で何も投稿せず計画だけを出す（dry-run モードの判断と案の報告で使う）。

実行後:

- 返信・解決したスレッド数をユーザーに報告する
- `resolve_failed[]` が空でない場合: **返信済みなので同じ本文で再実行しない**。`body` を省いた resolve のみのエントリで再試行する。warning はユーザーへの報告に併記する
- 拒否された場合（セレクタの曖昧さ・古いコンテキスト等）は stderr の指示に従う。**指摘を落として通すのではなく、指定か前提の方を直す**

## 修正完了報告フォーマット

構造の規定であり、文面は対象 PR の記述言語で書く（「判断基準 > 原則」参照。以下の例は日本語 PR の場合）:
```
<修正内容>を対応しました。
https://github.com/<owner>/<repo>/pull/<PR番号>/commits/<コミットハッシュ>
```

PR 本体へのコメントとして投稿する場合は、上記の本文を `work_dir` 直下に Write して投稿する:

```bash
ccx pr comment <ドキュメント> --body-file <work_dir 直下のファイル名>
```

このコマンドも、ローカル HEAD が GitHub の live head であることと、ドキュメントの `pr.head_oid` がその祖先であることを確認するため、投稿前に push を済ませておく点は返信と同じ（「スレッドへの返信と解決」）。

## レビューの再依頼（REST）

対象・実行条件・失敗時の扱いは「レビュー再依頼」参照：
```bash
gh api repos/<owner>/<repo>/pulls/<PR番号>/requested_reviewers \
  -f 'reviewers[]=LOGIN1' -f 'reviewers[]=LOGIN2'
```
- `<owner>/<repo>` は `repo`、`<PR番号>` は `pr.number` を使う
- 本スキルの他の呼び出しは GraphQL だが、再依頼だけ REST を使う。このエンドポイントは契約として additive（既存のレビュアー集合に追加する）で、リポジトリがパスに明示されるため
