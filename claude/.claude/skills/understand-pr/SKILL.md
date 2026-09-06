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
- **`--worktree`**（番号の有無を問わず）: worktree 解決を先に行い、その中で番号なしモードと同じブリーフを出す。**本スキルが checkout を動かすのはこの経路だけ**で、ユーザーが worktree を要求したケースに当たる

## 実行内容

以下の各節は、書かれた順に実行するステップ。他の節からはステップ名で参照する（番号を振ると、ステップの挿入で他所の参照が黙って壊れるため）。

### Worktree 解決（`--worktree` 指定時のみ、最初に実行）

Skill ツールで `worktree-resolution` を起動し、その「PR worktree 解決手順」に従って対象 PR の worktree に session を切り替える。

### 1. PR情報の取得
1. `git branch --show-current` で現在のブランチを確認
2. `gh pr view` でPRのタイトル・説明・ステータスを取得
3. PRが見つからない場合はその旨を報告して終了
4. PR説明文に関連Issue（`Closes #N`、`Fixes #N`、`#N` への言及等）があれば `gh issue view` で内容を確認

### 2. 変更内容の把握
4. `gh pr view --json baseRefName --jq '.baseRefName'` でベースブランチを取得
5. `git log [base]..HEAD --oneline` でコミット履歴を確認
6. `git diff [base]...HEAD --stat` で変更ファイルの概要を確認
7. 主要な変更ファイルの差分を読み、変更内容を理解する

### 3. 現在の状態の確認
8. `git status` で未コミットの変更を確認
9. `gh pr checks` でCIの状態を確認
10. `gh pr view --json reviewDecision,reviews,headRefOid` でレビュー状態と PR の最新 head を取得
11. `git rev-parse HEAD` を `headRefOid` と比較し、不一致ならローカルが PR の最新 head と乖離していることを記録する（読み取り専用スキルのため fetch・同期はせず、報告への注記のみ）
12. 乖離時、`git cat-file -e <headRefOid>` でオブジェクトがローカルに存在する場合のみ `git merge-base --is-ancestor` で方向（ahead: 未 push commit あり / behind: 未取得 commit あり / diverged）を判別して併記する（push/pull どちらが必要かの引き継ぎ情報になるため。オブジェクト不在なら方向不明のまま報告してよい — fetch はしない）

### 4. 報告
以下の構造で報告する：

```
## 目的
（PRが解決しようとしている課題・背景）

## 変更内容
（主要な変更の要約。ファイル単位ではなく論理的な変更単位で）

## 現在の状態
- ブランチ: xxx
- CI: 成功/失敗/未実行
- レビュー: 承認/変更要求/未レビュー
- 未コミット変更: あり/なし
- ローカルと PR head の整合: 一致 / 乖離（判別できた場合は ahead/behind/diverged を併記。乖離時: コミット履歴・差分の報告はローカル実体基準である旨を明記）

## 残作業・注意点
（レビューコメントの未対応、TODO、既知の問題など）
```

## 注意事項
1. PRの説明文だけでなく、実際の差分を読んで理解する
2. レビューコメントがある場合は未対応のものを特定する
3. CIが失敗している場合は失敗内容も確認する
