# /git-pr

プッシュ済みのブランチからプルリクエストを作成

## 使用方法
```
/git-pr
```

## 実行内容
1. `git status` で現在のブランチを確認
2. `gh pr list --head [current-branch]` で既存PRがないことを確認
3. `gh pr list --limit 5` で最近のPRから言語慣例（日本語/英語）を確認
4. プルリクエストテンプレート（.github/pull_request_template.md）があれば参照
5. プロジェクトの言語慣例に従ってタイトルと説明を決めて `gh pr create` で作成