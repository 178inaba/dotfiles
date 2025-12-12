---
description: Issueの調査から実装完了までを一貫して対応
argument-hint: <issue-number> [--english]
---

# /issue-handle

## 使用方法
```
/issue-handle 99
/issue-handle 99 --english
```

## 引数
- `<issue-number>`: 対応するIssue番号（必須）
- `--english`: コミットメッセージを英語で記述（オプション）

## 実行内容
1. **Issue確認・調査**
   - `gh issue view <issue-number>` でIssue内容を確認
   - 関連コードを調査し、実装方針を検討

2. **実装方針をIssueにコメント**
   - 調査結果と実装方針を `gh issue comment` でIssueに投稿

3. **TODOリスト作成**
   - TodoWriteツールで実装タスクを整理

4. **実装・テスト修正**
   - 実装とテストの順序は柔軟に対応
   - 適切な単位でコミット（git-commit.mdの方針に従う）
   - コミットメッセージの言語:
     - デフォルト: `git log` でリポジトリの傾向に合わせる
     - `--english` 指定時: 英語で記述

5. **Test, Lint成功確認**
   - プロジェクトのテスト・Lintコマンドを実行して成功を確認
   - 例: `make all`, `npm test && npm run lint`, `go test ./... && golangci-lint run`

## 注意事項
- **長時間作業時**: compact後は必ずIssueの実装方針コメントを `gh issue view` で再確認し、TODOリストを再構築すること
