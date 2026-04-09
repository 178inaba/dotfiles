---
name: understand-pr
description: 現在のブランチのPRを理解し、目的・変更内容・現状を構造化して報告
---

# /understand-pr

現在のブランチのPRを理解し、作業を引き継げる状態にする

## 使用方法
```
/understand-pr
```

## 実行内容

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
10. `gh pr view --json reviewDecision,reviews` でレビュー状態を確認

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

## 残作業・注意点
（レビューコメントの未対応、TODO、既知の問題など）
```

## 注意事項
1. PRの説明文だけでなく、実際の差分を読んで理解する
2. レビューコメントがある場合は未対応のものを特定する
3. CIが失敗している場合は失敗内容も確認する
