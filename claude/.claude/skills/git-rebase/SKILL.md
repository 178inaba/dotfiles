---
name: git-rebase
description: PRのベースブランチへrebaseし、コンフリクトが発生したら自動解消
disable-model-invocation: true
---

# /git-rebase

PRのベースブランチへrebaseし、コンフリクトが発生したら自動解消

## 使用方法
```
/git-rebase
```

## 実行内容

### 1. PR文脈の取得
@~/.claude/skills/understand-pr/SKILL.md に従ってPRの目的・変更内容・現状を把握する。

PR文脈はコンフリクト解消時の判断材料として使用。

### 2. ベースブランチの決定
1. `gh pr view --json baseRefName --jq '.baseRefName'` でPRのベースブランチを取得
2. PRが存在しない場合は `gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name'` でデフォルトブランチを取得
3. 取得したブランチ名を `origin/[branch-name]` 形式で使用

### 3. リモート最新化
4. `git fetch origin [branch-name]` でベースブランチを最新化

### 4. rebase実行
5. `git status` で未コミット変更がないことを確認（あればユーザーに確認）
6. `git rebase origin/[branch-name]` を実行
7. コンフリクトなく完了した場合は完了確認へ

### 5. コンフリクト自動解消
コンフリクトが発生した場合、以下を繰り返す：

8. `git status` でコンフリクトファイルを特定
9. 各コンフリクトファイルについて：
    - ファイルを読んでコンフリクトマーカー（`<<<<<<<`, `=======`, `>>>>>>>`）を確認
    - PR文脈と両側の変更意図を踏まえて解消
    - `git log --oneline origin/[branch-name] -- [file]` でmain側の変更履歴を確認（必要に応じて）
    - `git log --oneline HEAD -- [file]` でPR側の変更履歴を確認（必要に応じて）
10. 解消したファイルを `git add [file]` でステージング
11. `git rebase --continue` で次のコミットへ進む
12. さらにコンフリクトが発生したら 8 から繰り返す

### 6. 完了確認
13. `git status` でクリーンな状態を確認
14. `git log --oneline -10` でrebase後の履歴を確認
15. ユーザーに結果を報告（プッシュは `/git-pr` または `git push --force-with-lease` で別途実行）

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
