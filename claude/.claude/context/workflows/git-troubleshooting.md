# Git操作トラブルシューティング

## index.lock問題の対処
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

## よくある原因と予防策
- **原因**: Ctrl+C での Git コマンド強制終了
- **原因**: システムクラッシュ・ネットワーク切断
- **予防**: Git操作完了まで待機、安定したネットワーク環境
