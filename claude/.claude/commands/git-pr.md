---
description: プッシュ済みのブランチからプルリクエストを作成
---

# /git-pr

プッシュ済みのブランチからプルリクエストを作成

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
1. `git status` で現在のブランチを確認
2. `gh pr list --head [current-branch]` で既存PRがないことを確認
3. `gh pr list --limit 5` で最近のPRから言語慣例（日本語/英語）を確認
4. プルリクエストテンプレート（.github/pull_request_template.md）があれば参照
5. プロジェクトの言語慣例に従ってタイトルと説明を決定
6. `gh pr create` でPRを作成：
   - `--base` オプション: 指定されたベースブランチ（省略時はデフォルト）
   - `--draft` オプション: 指定された場合のみドラフトPRとして作成
