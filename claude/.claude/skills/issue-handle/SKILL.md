---
name: issue-handle
description: Issueの調査から実装完了までを一貫して対応
argument-hint: <issue-number | --file FILE_PATH>
disable-model-invocation: true
---

# /issue-handle

## 使用方法
```
/issue-handle 99              # Issue番号
/issue-handle --file spec.md  # ファイル
```

## Issue情報（自動取得）
!`gh issue view $0 --json title,body,labels,assignees,comments 2>/dev/null || echo "Issue情報の取得をスキップ（--file指定時）"`

## 引数
- `<issue-number>`: 対応するIssue番号（`--file`と排他）
- `--file FILE_PATH`: 仕様ファイルのパス（`<issue-number>`と排他）

## 前提条件
- Gitリポジトリ内で実行すること
- Issue番号指定時: `gh` CLIがインストール・認証済みであること
- mainブランチが最新であること（推奨）

## 実行内容

### 要件確認・調査
- Issue番号指定時: `gh issue view <issue-number>` でIssue内容を確認
- --file指定時: Readツールで仕様ファイルを読み込み
- 関連コードを調査し、実装方針を検討

### 計画フェーズ

**EnterPlanModeツールでPlanモードに移行する。**

Planモードにより、ファイル編集はシステム的にブロックされる。
**計画ファイル**（Planモード開始時に指定されたパス）に実装方針を記述する。

1. **ユーザーとの対話**
   - 不明点があればAskUserQuestionでユーザーに質問
   - 実装方針をユーザーに提示
   - フィードバックを受けて方針を調整

2. **タスク作成**
   - 方針確定後、TaskCreateツールで実装フェーズの各タスクを作成
   - 各タスクには `subject`（命令形）と `activeForm`（進行形）を設定
   - 例: subject「テストを実装」、activeForm「テストを実装中」

3. **計画完了**
   - **計画ファイルに以下を含める**:
     - ブランチ名（typeを含む完全な形式）
     - Issue番号（Issue番号指定時。`gh issue comment` や `Closes #N` で使用）
     - 想定コミット計画（複数コミットになる場合のみ記述）:
       - 例:
         - コミット1: <内容>
         - コミット2: <内容>
       - 同じファイルに無関係な変更が混ざるのを防ぎ、各段階でテストを通せる単位に分ける
       - 実装中の調整は許容（厳密に固定しない）
     - 実装手順チェックリスト:
       - [ ] 実装方針をIssueにコメント（Issue番号指定時のみ）
       - [ ] 作業ブランチ作成
       - [ ] 実装・テスト（想定コミット計画の単位で都度コミット、必要に応じて調整）
       - [ ] Test, Lint成功確認
       - [ ] `/simplify` で品質チェック・修正
       - [ ] プッシュ・PR作成（Issue番号指定時は `Closes #<issue-number>` を含める）
   - ExitPlanModeツールで計画完了を通知
   - ユーザーの承認を待つ

4. **実装フェーズへ**（承認後）
   - 以下の「実装フェーズ」を実行

### 実装フェーズ

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
   - **想定コミット計画の単位で都度コミット**（最後にまとめてではなく）
     - 計画した単位ごとに、実装 → テスト確認 → コミット のサイクルを回す
     - 同じファイルに無関係な変更が混ざる前にコミットすることで、後からの hunk 分割を回避
     - 実装中に計画と現実が乖離した場合は、コミット境界を調整してよい（計画通りの固定にこだわらない）
   - コミット方針は git-commit.md に従う
   - コミットメッセージの言語: `git log` でリポジトリの傾向に合わせる
   - PRタイトル・説明の言語: git-pr.md に従う（`gh pr list --limit 5` で既存PRから慣例を確認）

4. **Test, Lint成功確認**
   - プロジェクトのテスト・Lintコマンドを実行
   - 数分以上かかる見込みの場合は `run_in_background: true` で実行（sleep ポーリングを避けキャッシュを節約）
   - **失敗した場合**: 修正 → コミット → 再テストを繰り返す
   - 例: `make all`, `npm test && npm run lint`, `go test ./... && golangci-lint run`

5. **品質チェック**
   - `/simplify` を実行し、変更コードの再利用性・品質・効率性を確認・修正
   - 修正があればコミット

6. **実装完了処理**
   - 未コミットの変更があれば @~/.claude/skills/git-commit/SKILL.md に従ってコミット
   - @~/.claude/skills/git-pr/SKILL.md に従ってプッシュ・PR作成
   - PR説明にIssue/仕様の背景・動機を含める（リンクだけでなく「なぜこの変更が必要か」を本文に書く）
   - Issue番号指定時: `Closes #<issue-number>` を含める

## 完了条件
以下をすべて満たした時点で完了:
- [ ] 実装が完了している
- [ ] テスト・Lintが成功している
- [ ] 変更がプッシュされている
- [ ] PRが作成されている（または既存PRが更新されている）

## 注意事項
- **Planモード中**: ファイル編集・ブランチ作成はシステム的にブロックされる
- **テスト失敗時**: 修正 → コミット → 再テストのサイクルを繰り返す
