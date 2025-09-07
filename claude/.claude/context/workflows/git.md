# Git操作ワークフロー

## 基本操作規約

### ブランチ操作
- **切り替え**: `git switch`を使用（`git checkout`ではなく）
- **作成と切り替え**: `git switch -c branch-name`
- **リモートブランチ追跡**: `git switch -c local-branch origin/remote-branch`

### コミットメッセージ
- プロジェクト標準に従う
- 一般的な形式: `type(scope): description`
- 例: `feat(auth): add login functionality`

## サブモジュール操作

### ディレクトリ移動の注意点
サブモジュール構成（例: backend/, frontend/が独立したGitリポジトリ）での作業時の確実な移動方法。

#### 必須手順
1. **現在地確認**: 移動前に必ず`pwd`でディレクトリを確認
2. **絶対パス使用**: 相対パスではなく絶対パスで移動
3. **移動後確認**: 移動後に再度`pwd`で確認

#### 標準パターン
```bash
# 悪い例（相対パス・現在地未確認）
cd ../backend
cd frontend

# 良い例（絶対パス・現在地確認）
pwd  # 現在地確認
cd /path/to/project/backend
pwd  # 移動後確認
```

#### よくある失敗パターンと対策
- **`cd ..`の重複実行**: 意図しない親ディレクトリへの移動
  - 対策: 絶対パスでの移動を徹底
- **サブモジュール間の移動ミス**: `cd ../frontend`と`cd frontend`の混同
  - 対策: 常に現在地確認後に絶対パス移動
- **異なるGitリポジトリの混同**: サブモジュールとルートリポジトリの操作ミス
  - 対策: `git status`でリポジトリ状況を確認してから作業

## トラブルシューティング

### index.lock問題
```bash
# 現象: "fatal: Unable to create '/path/.git/index.lock': File exists"
# 原因: Git操作の異常終了によるロックファイル残存

# 解決手順
1. Git プロセス確認・終了
   ps aux | grep git
   kill [プロセスID]

2. ロックファイル削除
   rm .git/index.lock

3. Git状態確認
   git status
```

### よくある原因と予防策
- **原因**: Ctrl+C での Git コマンド強制終了
- **原因**: システムクラッシュ・ネットワーク切断
- **予防**: Git操作完了まで待機、安定したネットワーク環境

## 高度な操作

### リベースとマージ
- **リベース**: `git rebase main` - 履歴を直線的に保つ
- **マージ**: `git merge feature-branch` - マージコミットを作成
- **インタラクティブリベース**: `git rebase -i HEAD~3` - コミット履歴の編集

### スタッシュ操作
```bash
git stash           # 作業中の変更を一時保存
git stash pop       # 最新のスタッシュを適用して削除
git stash list      # スタッシュ一覧
git stash apply     # スタッシュを適用（削除しない）
```

### チェリーピック
```bash
git cherry-pick <commit-hash>  # 特定のコミットを現在のブランチに適用
```