---
description: プッシュ済みのブランチからプルリクエストを作成
---

# /git-pr

プッシュ済みのブランチからプルリクエストを作成（既存PRがあれば更新）

## 使用方法
```
/git-pr [--base BASE_BRANCH] [--draft]
```

**引数**:
- `--base BASE_BRANCH`: ベースブランチを指定（省略時: デフォルトブランチ）
- `--draft`: ドラフトPRとして作成

**例**:
```
/git-pr                              # デフォルトブランチに通常PR
/git-pr --draft                      # ドラフトPRとして作成
/git-pr --base feature/xxx-xxx       # featureブランチをベースに通常PR
/git-pr --base feature/xxx-xxx --draft  # featureブランチをベースにドラフトPR
```

## 実行内容

### 1. 状態確認
1. `git status` で現在のブランチを確認
2. `gh pr list --head [current-branch]` で既存PRの有無を確認

### 2. 差分確認
3. `git diff [base]...HEAD` でベースブランチからの差分を確認
   - 差分が大きい場合は `--name-only` で変更ファイル一覧を取得後、個別に確認
   - 変更内容を把握してからPR説明文を作成

### 3A. 既存PRがない場合（新規作成）
4. `gh pr list --limit 5` で最近のPRから言語慣例（日本語/英語）を確認
5. プルリクエストテンプレート（.github/pull_request_template.md）があれば参照
6. プロジェクトの言語慣例に従ってタイトルと説明を決定
7. `gh pr create` でPRを作成：
   - `--base` オプション: 指定されたベースブランチ（省略時はデフォルト）
   - `--draft` オプション: 指定された場合のみドラフトPRとして作成

### 3B. 既存PRがある場合（更新判定）
4. `gh pr view` で現在のPRタイトル・説明を取得
5. 差分内容と現在のPR説明を比較し、乖離がないか確認：
   - 新しいコミットが追加されている
   - PR説明に記載されていない変更がある
   - PR説明が古い実装内容を参照している
6. 乖離がある場合、更新内容を提案しユーザーに確認
7. 承認されれば `gh pr edit` でタイトル・説明を更新
