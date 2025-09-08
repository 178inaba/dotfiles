# /git-ship

状況に応じて適切な処理を実行する統合コマンド（コミット・プッシュ・PR作成）

## 使用方法
```
/git-ship
```

## 実行内容
現在の状況を自動判断して最適な処理を実行：

1. **未コミットの変更がある場合**:
   - @~/.claude/commands/git-commit.md の仕様に従ってコミット
   - @~/.claude/commands/git-push.md の仕様に従ってプッシュ
   - 既存PRがなければ @~/.claude/commands/git-pr.md の仕様に従ってPR作成

2. **未プッシュのコミットがある場合**:
   - @~/.claude/commands/git-push.md の仕様に従ってプッシュ
   - 既存PRがなければ @~/.claude/commands/git-pr.md の仕様に従ってPR作成

3. **既存PRのあるブランチの場合**:
   - コミット・プッシュのみ（PR自動更新）

4. **mainブランチの場合**:
   - フィーチャーブランチ作成
   - @~/.claude/commands/git-push.md の仕様に従ってプッシュ
   - @~/.claude/commands/git-pr.md の仕様に従ってPR作成

## 注意事項
- 既存PRがある場合はプッシュで自動的にPRが更新されるため、PR作成はスキップ
