---
name: understand-pr
description: PR を理解して引き継げる状態にする。目的・変更内容・現状・残作業を構造化して報告（<pr-number> で PR を指定、--worktree で対象 PR の worktree に切替）
argument-hint: "[<pr-number>] [--worktree]"
---

# /understand-pr

PR を理解し、作業を引き継げる状態にする

## 使用方法
```
/understand-pr                 # カレント branch の PR（ローカルの状態も含めて報告）
/understand-pr 123             # PR 123（checkout は一切変更しない）
/understand-pr --worktree      # カレント branch の PR を worktree に切替して報告
/understand-pr 123 --worktree  # PR 123 を worktree に切替して報告（並列作業中の主要ユースケース）
```

## 引数
- `<pr-number>`: 対象 PR 番号（省略時はカレント branch の PR を推論）
- `--worktree`: 対象 PR の worktree に切替（既存があれば再利用、無ければ作成）。並列で別作業中に他の PR を理解する際に使う

## モード

3モードは「worktree 解決を先に走らせるか」「手順7がどちらの枝を採るか」の2点でしか違わない。手順は1本で、その2ステップだけをガードする。

| モード | worktree 解決（手順0） | 手順7 |
|---|---|---|
| 引数なし | しない | ローカル状態（カレント checkout が対象） |
| `<pr-number>` のみ | しない | 既存 worktree の案内（checkout を切り替えず、branch も switch しない） |
| `--worktree`（番号の有無を問わず） | する | ローカル状態（切り替えた worktree が対象） |

## 実行内容

### 0. Worktree 解決（`--worktree` 指定時のみ、最初に実行）

@~/.claude/skills/worktree-resolution/SKILL.md の「PR worktree 解決手順」に従い、対象 PR の worktree に session を切り替える（`<pr-number>` 省略時はカレント branch の PR が解決される）。

### 1. PR コンテキストの取得

```bash
ccx pr context <scratchpadディレクトリ> [<pr-number>]
```

`<pr-number>` は、ユーザーが指定したときと、手順0が `pr_number` を返したときに渡す（`--worktree` で番号を省略した場合。同じ PR をもう一度推論させないため）。どちらでもなければ省略し、カレント branch の PR に任せる。PR の無い branch ではコマンドが停止するので、stderr をそのまま報告して終了する。出力の読み方は `ccx pr context --help` にある。

返された `path` のファイルは jq で必要部分を段階的に参照する（会話の多い PR では数百 KB に達するため、全文表示はしない）。

### 2. 目的の再構成

**意図を先に、変更を後に読む**。差分やレビューを先に読むと、それがその後の読み方を枠づけてしまうため。以下の順で読む:

1. `linked_issues[]` の `body`（PR が閉じる Issue の本文）と、各要素の `parent.body`（Sub の Issue は横断ルールを親が持つため）
2. `pr.body`
3. `commits[]` の `message`（古い順。headline だけでなく本文まで — 何のための変更かはそこに書かれる）

`body` が null の要素は、対応する `warnings[]` の1行を添えて「読めなかった」と1行で報告し、残りの材料で目的を組み立てる。

### 3. 変更内容の把握

`diff.path` のファイルを **末尾まで** 読む。1回の Read に収まらなければ `offset` を進めて読み切る。サイズを理由に打ち切らない・要約で代替しない（どのファイルに設計判断が書かれているかは事前に分からないため）。

読み終えてから `diff.files[]` と照合し、全ファイルを見たことを確認する（読む**前**に「どれを飛ばすか」の判断には使わない）。報告の変更ファイル概要も `diff.files[]` から書く。

### 4. CI 状態

```bash
gh pr checks <PR番号> -R <owner>/<repo>
```

`<PR番号>` は `pr.number`、`<owner>/<repo>` は `repo` を使う（checkout に依存しないため全モード共通）。**非ゼロ exit をスキルの失敗として扱わない** — このコマンドは CI の状態自体を exit status に載せる（pending・チェック未設定でも非ゼロ）。出力を読んで報告し、停止しない。失敗しているチェックがあれば、その失敗内容まで確認して報告に載せる。

### 5. レビュー状態

`reviews[]` を `submitted_at` の昇順で見て、レビュアーごとに、`state` が `APPROVED` / `CHANGES_REQUESTED` のレビューのうち**最後のもの**を有効な状態とする（`COMMENTED` は有効状態を変えない）。承認・変更要求のどちらも持たないがレビューは提出しているレビュアーは「コメントのみ」と書く。`reviews[]` が空なら「未レビュー」。ここで取り出した `reviews[]` は手順6でも使うので、読み直さない。

`reviews_truncated: true` のときは取得窓の外に有効な状態が残りうるため、レビュー行にその旨を併記する。

### 6. 残作業の数え上げ（本文を読まずに機械的に行う）

基準時刻を2つ取る。存在しない基準は条件として課さない（その条件では全件を数える）:

- **スキル完了報告の最新時刻**: `is_skill_comment: true` の `comments[]` のうち最新の `created_at`
- `head_committed_at`（push は、作者が書かずにレビューへ答える手段のため）

数える対象:

- `review_threads[]` のうち `ball: "mine"` のもの → `path:line`（`line` が null なら `original_line`）と `opened_by` を書く
- `reviews[]` のうち、`author` が `current_user` でなく、`body` が空でなく、`state` が `APPROVED` でないもので、`submitted_at` が上記2つの**遅い方**より後のもの → `author` を書く
- `comments[]` のうち `is_skill_comment: false` で、`created_at` が「スキル完了報告の最新時刻」より後のもの → `author` を書く。`author` は問わない（作者自身が手で書いた追加依頼を落とさないため）

**本文を読んで数える／落とすことはしない**（読んだ時点で「意図 → 変更 → 会話」の順序が崩れる）。数え上げた後、報告に要旨を書く段では読んでよい。

`reviews_truncated` / `threads_truncated` / `comments_truncated` のいずれかが立っていれば、取得窓の外に残作業が残りうる旨を報告に併記する。

### 7. checkout の状態

**手順0で worktree に切り替えた場合と、引数なしの場合**（＝いま対象 PR の checkout の中にいる）:

1. `git status` で未コミットの変更を確認
2. `git rev-parse HEAD` を `pr.head_oid` と比較する。一致すれば整合、不一致なら乖離
3. 乖離時は `git merge-base --is-ancestor` で方向（ahead: 未 push commit あり / behind: 未取得 commit あり / diverged）を判別して併記する。`pr.head_oid` は手順1の成功時点でこのリポジトリに存在する（`ccx pr context --help` の保証）

「現在の状態」に次の2行を書く:

```
- 未コミット変更: あり/なし
- ローカルと PR head の整合: 一致 / 乖離（ahead/behind/diverged）
```

ローカルが `pr.head_oid` より **ahead / diverged** の場合のみ、整合の行に「『変更内容』は PR の head（コンテキストの差分）を反映しており、ローカルの commit は含まない」と添える（behind には添えない）。

レビュー系スキルが共有する鮮度確認サブ手順はここでは使わない（behind のみの checkout を自動で fast-forward してしまい、対象の checkout を変えないという本スキルの契約を破るため）。`--worktree` モードでは worktree 解決が同期を済ませているため、通常は「一致」と出る。

**`<pr-number>` のみの場合**（＝どの checkout も対象 PR のものではない）: 上記は行わず、「現在の状態」に次の1行を書く。

```
- ローカル checkout: 未確認（PR 番号指定で実行したため）
```

あわせて、`pr.head_ref` を checkout している既存の worktree があればそのパスを1行添える（読者の次の一手は多くの場合そこへ移ることのため）。探し方は @~/.claude/skills/worktree-resolution/SKILL.md の共通規約「既存 worktree の検索」に従う（メイン worktree は除外する — そこは読者がいま居る場所で、案内にならない）。無ければ何も書かない。

### 8. 報告

以下の構造で報告する：

```
## 目的
（PR が解決しようとしている課題・背景。読めなかった Issue があれば1行で明記）

## 変更内容
（主要な変更の要約。ファイル単位ではなく論理的な変更単位で）

## 現在の状態
- ブランチ: xxx（`pr.head_ref`）
- CI: 成功/失敗/実行中/未設定
- レビュー: <レビュアー>: 承認 / 変更要求 / コメントのみ ... または 未レビュー
（手順7が選んだ枝の行をここに置く）

## 残作業・注意点
- レビュー由来（手順6で数えた分。それぞれ要旨を添える）
  - `path:line`（<opened_by>）— ...
  - レビュー本文: <author> — ...
  - 通常コメント: <author> — ...
- CI の失敗内容
- TODO・既知の問題
```

## 注意事項
1. **報告するだけで、PR も作業内容も動かさない**: 差分の適用・コミット・push・GitHub への投稿は行わない。書き込みが起きるのは2箇所だけで、いずれも報告の材料を揃えるため — `ccx pr context` の fetch（差分と `pr.head_oid` を手元に置く）と、`--worktree` 指定時の worktree 解決（session の切替、対象 branch の ff 同期、メインリポジトリが対象 branch にいる場合の退避）。`--worktree` を付けなければ checkout は動かない
2. **`--worktree` 指定時の挙動**: @~/.claude/skills/worktree-resolution/SKILL.md の注意事項を参照
