---
description: PRのベースブランチへrebaseし、コンフリクトが発生したら自動解消
---

# /git-rebase

PRのベースブランチへrebaseし、コンフリクトが発生したら自動解消

## 使用方法
```
/git-rebase
```

## 実行内容

### 1. PR文脈・ベースブランチの取得
1. `git branch --show-current` で現在のブランチを確認
2. `gh pr view` でPRのタイトル・説明・ステータスを取得
   - PRが存在しない場合は `gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name'` でデフォルトブランチをベースブランチとして使用
3. PR説明文に関連Issue（`Closes #N` 等）があれば `gh issue view` で内容を確認
4. `gh pr view --json baseRefName --jq '.baseRefName'` でベースブランチを取得し、`origin/[branch-name]` 形式で控えておく
5. `git log [base]..HEAD --oneline` でコミット履歴を確認
6. `git diff [base]...HEAD --stat` で変更ファイルの概要を確認

取得したPR文脈はコンフリクト解消時の判断材料として使用。

### 2. リモート最新化
7. `git fetch origin [branch-name]` でベースブランチを最新化

### 3. rebase実行
8. `git status` で未コミット変更がないことを確認（あればユーザーに確認）
9. `git rebase origin/[branch-name]` を実行
10. コンフリクトなく完了した場合は完了確認へ

### 4. コンフリクト自動解消
コンフリクトが発生した場合、以下を繰り返す：

11. `git status` でコンフリクトファイルを特定
12. 各コンフリクトファイルについて：
    - ファイルを読んでコンフリクトマーカー（`<<<<<<<`, `=======`, `>>>>>>>`）を確認
    - PR文脈と両側の変更意図を踏まえて解消
    - `git log --oneline origin/[branch-name] -- [file]` でmain側の変更履歴を確認（必要に応じて）
    - `git log --oneline HEAD -- [file]` でPR側の変更履歴を確認（必要に応じて）
13. 解消したファイルを `git add [file]` でステージング
14. `git rebase --continue` で次のコミットへ進む
15. さらにコンフリクトが発生したら 11 から繰り返す

### 5. 完了確認
16. `git status` でクリーンな状態を確認
17. `git log --oneline -10` でrebase後の履歴を確認
18. ユーザーに結果を報告（プッシュは `/git-pr` または `git push --force-with-lease` で別途実行）

## 自動解消の判断基準

基本的に自動解消するが、以下のケースは判断が困難なため、ユーザーに確認する：

- **同じ機能の別実装**: PR側で実装した機能が、main側で別アプローチで実装済み
- **大規模リファクタリング**: main側でPRの前提が崩れる構造変更
- **削除 vs 変更**: 一方がファイル/関数を削除、もう一方が変更（意図が不明な場合）
- **意味的コンフリクト**: 構文上は競合しないが、両側の変更が論理的に矛盾

判断に自信がある場合は自動で進め、解消内容を報告する。

## 注意事項
1. rebase中の中断: `git rebase --abort` で元の状態に戻せることを把握しておく
2. force push: rebase後はリモートと履歴が乖離するため、push時は `--force-with-lease` を使用（`--force` は避ける）
3. 共有ブランチでの注意: 他者と共有しているブランチでは事前確認が望ましい
