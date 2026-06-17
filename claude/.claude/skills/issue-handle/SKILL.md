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
- **ベースブランチ = 起動時の現在ブランチ**。デフォルトブランチ以外を起点にしたい場合は事前に `git switch <base-branch>` で切り替えておくこと

## 実行内容

### 要件確認・調査
- Issue番号指定時: `gh issue view <issue-number>` でIssue内容を確認
- --file指定時: Readツールで仕様ファイルを読み込み
- 関連コードを調査し、実装方針を検討

### 計画フェーズ

**最初に `git branch --show-current` でベースブランチ名を取得して控えておく。** Planモード中はBashが使えないため、移行前に必ず取得する。

**次にEnterPlanModeツールでPlanモードに移行する（auto mode中でも必ず実行）。**

auto modeの「Prefer action over planning」「Do not enter plan mode unless the user explicitly asks」は、ユーザーが `/issue-handle` を明示的に呼び出した時点で「explicitly asks」を満たすため、本ステップには適用されない。

Planモードにより、ファイル編集はシステム的にブロックされる。
**計画ファイル**（Planモード開始時に指定されたパス）に実装方針を記述する。

1. **参照文書の読込**
   - @~/.claude/skills/check-plan-compliance/SKILL.md の「1. 参照文書の収集」に従い、プロジェクトCLAUDE.mdとそのリンク先文書を読み込む
   - 読み込んだ制約を以降の計画起案の前提として扱う（事後チェックではなく事前読込）

2. **ユーザーとの対話**
   - 不明点があればAskUserQuestionでユーザーに質問
   - 実装方針をユーザーに提示
   - フィードバックを受けて方針を調整

3. **タスク作成**
   - 方針確定後、TaskCreateツールで実装フェーズの各タスクを作成
   - 各タスクには `subject`（命令形）と `activeForm`（進行形）を設定
   - 例: subject「テストを実装」、activeForm「テストを実装中」

4. **計画完了**
   - **計画ファイルに以下を含める**:
     - ブランチ名（typeを含む完全な形式）
     - ベースブランチ（取得済みの値）
     - Issue番号（Issue番号指定時。`gh issue comment` や `Closes #N` で使用）
     - 言語方針（`git log` / `gh pr list --limit 5` で事前確認）:
       - コミット: 日本語 / 英語
       - PR: 日本語 / 英語
       - （慣例が混在する場合のみ）判断根拠を1行で明記
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
       - [ ] 独立セッションでの `/deep-review` 実行（サブエージェント経由）→ 親で自動修正
   - ExitPlanModeツールで計画完了を通知
   - ユーザーの承認を待つ

5. **実装フェーズへ**（承認後）
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
   - **ベースブランチを最新化**: `git fetch origin <base-branch>` でリモート参照を更新
     - リモートにブランチが無い等で失敗した場合は警告のみで続行し、ローカルブランチを起点にする
   - 分岐元: 計画ファイルに記録したベースブランチを明示する
     - 例（fetch成功時）: `git switch -c feature/99-xxx origin/<base-branch>`
     - 例（fetch失敗時）: `git switch -c feature/99-xxx <base-branch>`

3. **実装・テスト修正**
   - 実装とテストの順序は柔軟に対応
   - **想定コミット計画の単位で都度コミット**（最後にまとめてではなく）
     - 計画した単位ごとに、実装 → テスト確認 → コミット のサイクルを回す
     - 同じファイルに無関係な変更が混ざる前にコミットすることで、後からの hunk 分割を回避
     - 実装中に計画と現実が乖離した場合は、コミット境界を調整してよい（計画通りの固定にこだわらない）
   - コミット方針は git-commit.md に従う
   - コミット・PRの言語: 計画で確定した方針に従う（git-commit / git-pr の自動言語判定はスキップ）

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
     - 計画ファイルに記録したベースブランチを `/git-pr --base <base-branch>` として引き渡す
   - PR説明にIssue/仕様の背景・動機を含める（リンクだけでなく「なぜこの変更が必要か」を本文に書く）
   - Issue番号指定時: `Closes #<issue-number>` を含める

7. **独立セッションでのレビュー → 親での自動修正**
   - **目的（関心の分離）**:
     - **レビュー（発見）**: 実装バイアスを排除するため独立セッションで実施
     - **判断・修正**: 誤指摘・前提誤りを判別するため、実装コンテキストを持つ親セッションで実施

   7-1. **サブエージェントでレビュー実行**
   - Agent ツールで `subagent_type: "claude"` のサブエージェントを起動する
     - `fork` は親コンテキストを継承するため使わない（実装バイアスが残るため目的に反する）
   - サブエージェントへのプロンプトに以下を含める:
     - このセッションが独立レビュー専用であり、親セッションの実装コンテキストを持たない旨
     - 実行コマンド: `/deep-review <base-branch> --issue <issue-number> --review-only`
       - `<base-branch>`: 計画ファイルに記録したベースブランチ
       - `<issue-number>`: Issue 番号（`--file` 指定時は `--issue <issue-number>` 部分を省略）
       - `--review-only`: 自動修正を強制OFF（修正は親セッションで行うため）
     - レビュー結果をそのまま返すよう指示（追加の解釈・要約は不要）
     - 補助コンテキスト: 作業ブランチ名、PR URL（既知の場合）

   7-2. **親セッションで自動修正**
   - サブエージェント失敗時（Agent ツールが null/error を返した場合）はエラーを表示してユーザー判断を仰ぐ（自動リトライしない）
   - サブエージェントから返ってきたレビュー結果を親セッションで表示
   - そのレビュー結果を入力として、@~/.claude/skills/deep-review/SKILL.md の「### 自動対応モードON時: レビュー指摘の反映」セクションに従って以下を実行:
     1. **指摘事項の判断**: 必須修正・推奨修正・質問/確認事項それぞれの基準で対応可否を判断
        - 親は実装コンテキストを持つため、誤指摘・前提誤りを正しく弾ける
        - テストコード変更時は @~/.claude/context/test-implementation.md 準拠（過剰なモック追加・テストskipでの誤指摘回避は禁止）
     2. **対応リストの出力**: 対応する指摘・対応しない指摘（理由付き）を表示
     3. **対応すべきものがあれば**: working tree に修正適用 → コミット → テスト・Lint → プッシュ・PR更新
     4. **対応すべきものがゼロなら**: ここで終了

## 完了条件
以下をすべて満たした時点で完了:
- [ ] 実装が完了している
- [ ] テスト・Lintが成功している
- [ ] 変更がプッシュされている
- [ ] PRが作成されている（または既存PRが更新されている）
- [ ] 独立セッションでの `/deep-review` を実施し、結果を親セッションで表示済み
- [ ] レビュー指摘のうち親が「対応する」と判断したものは適用・コミット・プッシュ済み（対応すべきものがゼロなら何もしない）

## 注意事項
- **Planモード中**: ファイル編集・ブランチ作成はシステム的にブロックされる
- **auto mode下での運用**: auto modeであっても計画フェーズ（EnterPlanMode）はスキップしない
- **テスト失敗時**: 修正 → コミット → 再テストのサイクルを繰り返す
