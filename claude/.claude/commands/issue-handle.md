---
description: Issueの調査から実装完了までを一貫して対応
argument-hint: <issue-number | --file FILE_PATH> [--auto] [--english]
---

# /issue-handle

## 使用方法
```
/issue-handle 99                    # Issue番号（対話型）
/issue-handle 99 --auto             # Issue番号（一気に実装）
/issue-handle --file spec.md        # ファイル（対話型）
/issue-handle --file spec.md --auto # ファイル（一気に実装）
```

## 引数
- `<issue-number>`: 対応するIssue番号（`--file`と排他）
- `--file FILE_PATH`: 仕様ファイルのパス（`<issue-number>`と排他）
- `--auto`: 対話・承認なしで一気に実装まで進める（オプション）
- `--english`: コミットメッセージを英語で記述（オプション）

## 実行内容

### --auto指定がない場合（デフォルト）

**EnterPlanModeツールでPlanモードに移行する。**

Planモードにより、ファイル編集はシステム的にブロックされる。

1. **要件確認・調査**
   - Issue番号指定時: `gh issue view <issue-number>` でIssue内容を確認
   - --file指定時: Readツールで仕様ファイルを読み込み
   - 関連コードを調査し、実装方針を検討

2. **ユーザーとの対話**
   - 不明点があればAskUserQuestionでユーザーに質問
   - 実装方針をユーザーに提示
   - フィードバックを受けて方針を調整

3. **TODOリスト作成**
   - 方針確定後、TodoWriteツールで実装フェーズの各タスクを整理

4. **計画完了**
   - ExitPlanModeツールで計画完了を通知
   - ユーザーの承認を待つ

5. **実装フェーズへ**（承認後）
   - 以下の「実装フェーズ」を実行

### --auto指定がある場合

Planモードを使用せず、対話・承認なしで一気に進める。

1. **要件確認・調査**
   - Issue番号指定時: `gh issue view <issue-number>` でIssue内容を確認
   - --file指定時: Readツールで仕様ファイルを読み込み
   - 関連コードを調査し、実装方針を検討

2. **TODOリスト作成**
   - TodoWriteツールで実装フェーズの各タスクを整理

3. **実装フェーズへ**
   - 以下の「実装フェーズ」を実行

### 実装フェーズ（共通）

1. **実装方針をIssueにコメント**（Issue番号指定時のみ）
   - **計画ファイルの内容をそのまま投稿**（要約しない）
   - `gh issue comment` でIssueに投稿

2. **作業ブランチ作成**
   - Issue番号指定時: `issue-{番号}` (例: `issue-99`)
   - --file指定時: 計画内容から要約したブランチ名を自動生成 (例: `feature/add-login-validation`)

3. **実装・テスト修正**
   - 実装とテストの順序は柔軟に対応
   - **論理的な単位ごとに都度コミット**（最後にまとめてではなく）
     - 1つの機能・修正が完了し、ビルドが通る状態になったらコミット
     - 小さな関連修正はまとめてもよい
   - コミット方針は git-commit.md に従う
   - コミットメッセージの言語:
     - デフォルト: `git log` でリポジトリの傾向に合わせる
     - `--english` 指定時: 英語で記述

4. **Test, Lint成功確認**
   - プロジェクトのテスト・Lintコマンドを実行して成功を確認
   - 例: `make all`, `npm test && npm run lint`, `go test ./... && golangci-lint run`

## 注意事項
- **Planモード中**: ファイル編集・ブランチ作成はシステム的にブロックされる
- **長時間作業時**: compact後は計画ファイルまたはTODOリストを参照し、必要に応じて再構築すること
