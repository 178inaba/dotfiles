# 投稿（返信・解決・完了報告・再依頼）

## スレッドへの返信と解決

**先に修正をコミットして push する**。ドキュメントは取り直さない（push 前のドキュメントのまま投稿できる）。`threads_path` に対象スレッドを書き、投稿する:

```bash
ccx pr reply-threads <ドキュメント> <threads_path>
```

書式は `ccx pr reply-threads --help` にある。本スキルが決めるのは中身:

- スレッドは `path` と `line` で指す
- `resolve` は当該スレッドの `resolvable_by_me` と同値にする。ユーザーの指示で `theirs` のスレッドに再対応するときは、`resolvable_by_me` が偽なら `false`、真でも自分が起こしたスレッドへの追記なら閉じるかを判断する
- `body` は対象 PR の記述言語で書く。1行を超える返信は素の Markdown を `work_dir` 直下に Write して `body_file` で指す
- **bot が起こしたスレッドで、既に自分が返信済みのもの**（`resolvable_by_me: true` かつ `opened_by` が `current_user` でなく、`last_comment.author` が `current_user`）は、**body を書かず `resolve: true` だけのエントリにする**。自分が起こしたスレッド（`opened_by` が `current_user`）はこの規則の対象外

実行後:

- 返信・解決したスレッド数をユーザーに報告する
- `resolve_failed[]` が空でない場合: **返信済みなので同じ本文で再実行しない**。`body` を省いた resolve のみのエントリで再試行する。warning はユーザーへの報告に併記する
- 拒否された場合（セレクタの曖昧さ・古いコンテキスト等）は stderr の指示に従う。**指摘を落として通すのではなく、指定か前提の方を直す**

## 修正完了報告フォーマット

構造の規定（以下の例は日本語 PR の場合）:
```
<修正内容>を対応しました。
https://github.com/<owner>/<repo>/pull/<PR番号>/commits/<コミットハッシュ>
```

PR 本体へのコメントとして投稿する場合は、上記の本文を `work_dir` 直下に Write し、push を済ませてから投稿する:

```bash
ccx pr comment <ドキュメント> --body-file <work_dir 直下のファイル名>
```

## レビューの再依頼（REST）

```bash
gh api repos/<owner>/<repo>/pulls/<PR番号>/requested_reviewers \
  -f 'reviewers[]=LOGIN1' -f 'reviewers[]=LOGIN2'
```
- `<owner>/<repo>` は `repo`、`<PR番号>` は `pr.number` を使う
