---
description: Issueの調査から実装完了までを一貫して対応
argument-hint: <issue-number> [--auto] [--english]
---

# /issue-handle

## 使用方法
```
/issue-handle 99              # 対話型（Planモード）
/issue-handle 99 --auto       # 一気に実装
/issue-handle 99 --auto --english  # 英語コミット
```

## 引数
- `<issue-number>`: 対応するIssue番号（必須）
- `--auto`: 対話・承認なしで一気に実装まで進める（オプション）
- `--english`: コミットメッセージを英語で記述（オプション）

## 実行内容

### --auto指定がない場合（デフォルト）

**EnterPlanModeツールでPlanモードに移行する。**

Planモードにより、ファイル編集はシステム的にブロックされる。

1. **Issue確認・調査**
   - `gh issue view <issue-number>` でIssue内容を確認
   - 関連コードを調査し、実装方針を検討

2. **ユーザーとの対話**
   - 不明点があればAskUserQuestionでユーザーに質問
   - 実装方針をユーザーに提示
   - フィードバックを受けて方針を調整

3. **TODOリスト作成**
   - 方針確定後、TodoWriteツールで実装タスクを整理

4. **計画完了**
   - ExitPlanModeツールで計画完了を通知
   - ユーザーの承認を待つ

5. **実装フェーズへ**（承認後）
   - 以下の「実装フェーズ」を実行

### --auto指定がある場合

Planモードを使用せず、対話・承認なしで一気に進める。

1. **Issue確認・調査**
   - `gh issue view <issue-number>` でIssue内容を確認
   - 関連コードを調査し、実装方針を検討

2. **TODOリスト作成**
   - TodoWriteツールで実装タスクを整理

3. **実装フェーズへ**
   - 以下の「実装フェーズ」を実行

### 実装フェーズ（共通）

1. **作業ブランチ作成**
   - 例: `issue-99`

2. **実装方針をIssueにコメント**
   - 調査結果と実装方針を `gh issue comment` でIssueに投稿

3. **実装・テスト修正**
   - 実装とテストの順序は柔軟に対応
   - 適切な単位でコミット（git-commit.mdの方針に従う）
   - コミットメッセージの言語:
     - デフォルト: `git log` でリポジトリの傾向に合わせる
     - `--english` 指定時: 英語で記述

4. **Test, Lint成功確認**
   - プロジェクトのテスト・Lintコマンドを実行して成功を確認
   - 例: `make all`, `npm test && npm run lint`, `go test ./... && golangci-lint run`

## 注意事項
- **Planモード中**: ファイル編集・ブランチ作成・Issueコメントはシステム的にブロックされる
- **長時間作業時**: compact後は必ずIssueの実装方針コメントを `gh issue view` で再確認し、TODOリストを再構築すること
