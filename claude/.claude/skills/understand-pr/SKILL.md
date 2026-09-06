---
name: understand-pr
description: PR を理解し、目的・変更内容・現状を構造化して報告（<pr-number> で PR を指定、--worktree で対象 PR の worktree に切替）
argument-hint: "[<pr-number>] [--worktree]"
---

# /understand-pr

PR を理解し、作業を引き継げる状態にする

## 使用方法
```
/understand-pr                  # カレント branch の PR（ローカルの状態も含む）
/understand-pr 123              # PR 123（checkout は動かさない）
/understand-pr --worktree       # カレント branch の PR の worktree に切替して報告
/understand-pr 123 --worktree   # PR 123 の worktree に切替して報告
```

## 引数
- `<pr-number>`: 対象 PR 番号（省略時はカレント branch の PR を推論）
- `--worktree`: 対象 PR の worktree に切替（既存があれば再利用、無ければ作成）。並列で別作業中に他の PR を読む際の主用途

## モード

引数で 3 つに分かれる。違うのは worktree 解決を先に行うかと、ローカルの状態を見るかの 2 点だけで、ブリーフの組み立ては共通:

- **番号なし**: カレント branch の PR。ローカルの状態も報告する
- **`<pr-number>` のみ**: checkout を一切動かさない。「現在の状態」のローカル項目は 1 行に置き換わる（同節参照）
- **`--worktree`**（番号の有無を問わず）: worktree 解決を先に行い、その中で番号なしモードと同じブリーフを出す

## 実行内容

以下の各節は、書かれた順に実行するステップ。他の節からはステップ名で参照する（番号を振ると、ステップの挿入で他所の参照が黙って壊れるため）。

### Worktree 解決（`--worktree` 指定時のみ、最初に実行）

Skill ツールで `worktree-resolution` を起動し、その「PR worktree 解決手順」に従って対象 PR の worktree に session を切り替える。

### PR コンテキストの取得

worktree 解決を行った場合はその中で実行する（番号を省略したとき、worktree の branch の PR が解決されるようにするため）:

```bash
ccx pr context <scratchpadディレクトリ> [<pr-number>]
```

出力とコンテキストの読み方は `ccx pr context --help` にある。PR が解決できない場合はコマンドが止まるので、stderr のメッセージを報告して終了する。

返されたドキュメントは jq で必要部分を段階的に参照する（`cat` での全文表示や `| head` での部分読みはしない。CI bot が多い PR では数百 KB に達する）。

ブリーフの素材（Issue の本文とコメント・PR 本文・コミット・差分・レビュー・待ち状態）はすべてこのドキュメントから取る。ドキュメントが運ばない CI の状態だけを `gh pr checks <pr.number>` で取る（番号はドキュメントの `pr.number` を使い、モードで分岐させない — 番号のみのモードでは checkout が PR を名指さないため）。非ゼロ exit でも停止せず、CI の状態として読む: 保留は専用の exit コード（`gh pr checks --help` にある）、チェックが 1 つも無い PR は失敗と同じ exit なので stderr で見分けて「未実行」とする。失敗しているときはチェック名まで拾って「残作業・注意点」の材料にする。

### PR 全体の読解

Skill ツールで `pr-reading` を起動し、その手順に従って読む。起動時に名指すのは、container が取得したドキュメントの `linked_issues[]`（読めなかったものを説明する `warnings[]` も同じドキュメントのもの）、文書がそのドキュメント、差分・コミットの取得元も同じドキュメント、報告先が「報告」の「変更内容」。

**会話をどこに置くか**（同スキルが呼び出し側に委ねている決定）: 会話の本文は読まない。レビューの状態は「現在の状態」が `reviewers[]` から、待っている分は「残作業・注意点」が `pending` から取り、いずれも本文を必要としないため。

本スキルが持つのはブリーフへの写像だけで、読む順序・差分の読み切り・生成物の扱い・網羅の確認・読めない Issue・compaction の規則はいずれも同スキルの持ち物:

- **「目的」**: 手順が読んだ意図（Issue とその親の本文・コメント、`pr.body`、`commits[]`）から組み立てる
- **「変更内容」**: 同じく読んだ差分から組み立てる。ファイルごとの概要は `diff.files[]`

ブリーフは毎回この読解を経て作る。本スキルは読解を省くゲートを持たない。

### 現在の状態

- PR: 番号と、自分の PR か他人の PR か（`is_own_pr` と `pr.author`）
- ブランチ: `pr.head_ref`
- CI: PR コンテキストの取得で得た `gh pr checks` の結果
- レビュー: `reviewers[]` の各要素を `author` と `state` で 1 行ずつ書く（実効状態はドキュメントが確定させているので、ここで導出しない）
- 未コミット変更: `git status`
- ローカルと PR head の整合: `git rev-parse HEAD` を `pr.head_oid`（PR コンテキストの取得を終えていればローカルに在る — 同コマンドの契約）と比較し、一致しなければ `git merge-base --is-ancestor` で ahead（未 push commit あり）/ behind（未取得 commit あり）/ diverged を判別する。push・pull どちらが必要かの引き継ぎ情報になる。`--worktree` 指定時は worktree 解決が同期済みのため通常は一致する

**`<pr-number>` のみのモード**では、未コミット変更と整合の 2 項目を「ローカルの状態: checkout を見ていないため未確認」の 1 行に置き換える。その head branch を checkout 中の worktree が `git worktree list --porcelain` に見つかれば、そのパスを 1 行添える（読者の次の一手が通常そこへ行くため）。

### 残作業・注意点

- CI の失敗: PR コンテキストの取得で拾った失敗チェック名
- TODO・FIXME 等の未処理、既知の問題: 読解した差分から拾う（PR が触れていない箇所の TODO はこの PR の残作業ではない）
- レビュー由来の未対応: `pending` の 3 つのリストをそのまま列挙する
  - `pending.threads[]`: `path:line`（`line` が null なら `original_line`）と `opened_by`
  - `pending.reviews[]` / `pending.comments[]`: それぞれの author
  - あわせて `pending.since` を書く（null ならその旨）。時刻を基準にする 2 つのリストはこのマシンで前回記録された時点から数えているため、書かないと別の場所の読者が短いリストを「静かな PR」と読み違える
  - 3 つとも実行しているユーザー側に返っている分なので、他人の PR ではその旨を添える（作者側に返っている分は載らない）
- 読解で読めなかった Issue、`warnings[]` の各行、コメントの打ち切り

### 報告
以下の構造で報告する：

```
## 目的
（PRが解決しようとしている課題・背景）

## 変更内容
（主要な変更の要約。ファイル単位ではなく論理的な変更単位で。`pr-reading` の
 「報告」の1文もここに置く。ローカルが PR head より ahead / diverged の場合は、
 この節が PR の head を反映しており手元のコミットではない旨を添える）

## 現在の状態
- PR: #<番号>（自分の PR / <author> の PR）
- ブランチ: xxx
- CI: 成功/失敗/未実行
- レビュー: <author>: <state> を人数分
- 未コミット変更: あり/なし
- ローカルと PR head の整合: 一致 / 乖離（ahead/behind/diverged）
（`<pr-number>` のみのモードでは下 2 項目を「現在の状態」の規則どおり置き換える）

## 残作業・注意点
（「残作業・注意点」で集めた項目）
```

## 注意事項
1. **読み取り専用**: checkout を動かすのは `--worktree` 指定時の worktree 解決だけ。ローカル branch の同期・fast-forward は行わず、実行の記録も残さない（本スキルは何も判断しないため。同期と記録はレビュー系スキルの責務）
2. `--worktree` 指定時の挙動は `worktree-resolution` の注意事項を参照
