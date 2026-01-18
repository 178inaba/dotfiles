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

## 前提条件
- Gitリポジトリ内で実行すること
- Issue番号指定時: `gh` CLIがインストール・認証済みであること
- mainブランチが最新であること（推奨）

## 実行内容

### 共通: 要件確認・調査
- Issue番号指定時: `gh issue view <issue-number>` でIssue内容を確認
- --file指定時: Readツールで仕様ファイルを読み込み
- 関連コードを調査し、実装方針を検討

### --auto指定がない場合（デフォルト）

**EnterPlanModeツールでPlanモードに移行する。**

Planモードにより、ファイル編集はシステム的にブロックされる。
**計画ファイル**（`~/.claude/plans/` 配下に自動生成）に実装方針を記述する。

1. **ユーザーとの対話**
   - 不明点があればAskUserQuestionでユーザーに質問
   - 実装方針をユーザーに提示
   - フィードバックを受けて方針を調整

2. **TODOリスト作成**
   - 方針確定後、TodoWriteツールで実装フェーズの各タスクを整理

3. **計画完了**
   - **計画ファイルに以下を必ず含める**:
     - ブランチ名（typeを含む完全な形式）
     - 実装手順チェックリスト:
       - [ ] 実装方針をIssueにコメント（Issue番号指定時のみ）
       - [ ] 作業ブランチ作成
       - [ ] 実装・テスト（論理的な単位ごとに都度コミット）
       - [ ] Test, Lint成功確認
       - [ ] プッシュ・PR作成
     - Issueコメントのテンプレート（コンテキストクリア後も参照できるよう、下記「実装方針をIssueにコメント」セクションのテンプレートをコピー）
   - ExitPlanModeツールで計画完了を通知
   - ユーザーの承認を待つ

4. **実装フェーズへ**（承認後）
   - 以下の「実装フェーズ」を実行

### --auto指定がある場合

Planモードを使用せず、対話・承認なしで一気に進める。

1. **TODOリスト作成**
   - TodoWriteツールで実装フェーズの各タスクを整理

2. **実装フェーズへ**
   - 以下の「実装フェーズ」を実行

### 実装フェーズ（共通）

1. **実装方針をIssueにコメント**（Issue番号指定時のみ）
   - 以下のフォーマットで投稿:
     ```
     ## 実装方針

     ### 概要
     （何を実装するかの1-2文の説明）

     ### 主な変更点
     - （変更点1）
     - （変更点2）

     ### 影響範囲
     - （影響するファイル/コンポーネント）

     ### 対応ブランチ
     `<ブランチ名>`
     ```
   - `gh issue comment` でIssueに投稿

2. **作業ブランチ作成**
   - フォーマット: `<type>/<issue-number>-<short-description>`
   - typeはIssue内容から判断: feature, fix, hotfix, refactor, chore, docs
   - 例: `feature/99-add-oauth-login`, `fix/42-null-pointer`
   - --file指定時（Issue番号なし）: `<type>/<short-description>` (例: `feature/add-login-validation`)

3. **実装・テスト修正**
   - 実装とテストの順序は柔軟に対応
   - **論理的な単位ごとに都度コミット**（最後にまとめてではなく）
     - 1つの機能・修正が完了し、ビルドが通る状態になったらコミット
     - 小さな関連修正はまとめてもよい
   - コミット方針は git-commit.md に従う
   - コミットメッセージの言語:
     - デフォルト: `git log` でリポジトリの傾向に合わせる
     - `--english` 指定時: 英語で記述
   - PRタイトル・説明の言語: git-pr.md に従う（`gh pr list --limit 5` で既存PRから慣例を確認）

4. **Test, Lint成功確認**
   - プロジェクトのテスト・Lintコマンドを実行
   - **失敗した場合**: 修正 → コミット → 再テストを繰り返す
   - 例: `make all`, `npm test && npm run lint`, `go test ./... && golangci-lint run`

5. **実装完了処理**
   - @~/.claude/commands/git-ship.md に従ってプッシュ・PR作成
   - PR説明にIssue番号を含める（例: `Closes #99`）

## 完了条件
以下をすべて満たした時点で完了:
- [ ] 実装が完了している
- [ ] テスト・Lintが成功している
- [ ] 変更がプッシュされている
- [ ] PRが作成されている（または既存PRが更新されている）

## 注意事項
- **Planモード中**: ファイル編集・ブランチ作成はシステム的にブロックされる
- **テスト失敗時**: 修正 → コミット → 再テストのサイクルを繰り返す
- **コンテキストクリア後**: 計画ファイル（`~/.claude/plans/`）を読み直し、ブランチ名・チェックリストを確認して作業を再開
