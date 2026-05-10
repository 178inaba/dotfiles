## コーディング・対応方針

- **ファイル形式**: ファイルの最後に改行文字を必ず追加してください（POSIX標準準拠）
- **EditorConfig**: プロジェクトに.editorconfigファイルが存在する場合は、その設定に必ず従ってください
- **コメント規約**:
  - **言語**: 既存のコメント言語（英語/日本語）を踏襲し、コードベース全体で一貫性を保つ
  - **最小主義**: コードを読めば分かる内容はコメント不要。以下は**書かない**
    - 変数代入・関数呼び出しの説明（例: `// Get user data`）
    - 自明な条件分岐（例: `// If no items found, return early`）
    - ループ処理の説明（例: `// Convert to response`）
    - データ構造の準備（例: `// Prepare request body`）
  - **書くべきコメント**: 以下の場合のみコメントを追加
    - **設計判断の理由**: なぜこの実装方法を選んだか（例: パフォーマンスのため特定のJOIN方式を使用）
    - **TODO/FIXME**: 将来の改善予定や既知の制約
    - **複雑なアルゴリズム**: ビジネスロジックや計算式の意図
    - **非自明な回避策**: フレームワークの制約やバグ回避
- **gitコマンド**: `git -C` オプションは使用しない。作業ディレクトリ内でgitコマンドを実行すること
- **ブランチ操作**: `git checkout` ではなく `git switch` / `git restore` を使用する
  - ブランチ作成: `git switch -c branch-name`
  - ブランチ切り替え: `git switch branch-name`
  - ファイル復元: `git restore file`
- **ghコマンド（書き込み系は `-R` 必須）**: `gh issue` / `gh pr` / `gh release` / `gh repo` / `gh label` の書き込み系サブコマンド（create / comment / edit / close / reopen / delete / merge / review / archive / rename 等）は、必ず `-R owner/repo` でリポジトリを明示する
  - 理由: 別リポジトリへ調査目的で `cd` した状態で、cwd の git remote が暗黙参照され、意図しないリポジトリに Issue/PR を作成してしまう事故を防ぐため
  - 実行先が不明な場合: `gh repo view --json nameWithOwner -q .nameWithOwner` で先に取得してから `-R` に渡す
  - 機械的強制: `~/.claude/hooks/gh-require-repo-flag.sh`（PreToolUse フック）が `-R`/`--repo`/`GH_REPO=` のいずれも無い場合に `exit 2` でブロックする。ブロック時は同フックの stderr メッセージに従い、対象リポジトリを明示して再実行する
  - 除外対象: `gh repo create` / `gh repo fork`（新規対象を引数で指定するため `-R` の意味がない）、および read 系（list / view / status / checks / diff / clone / download 等）
- **エラーハンドリング**: 言語別パターン
  - **Go言語**: 戻り値を使わない場合、条件文内でエラーハンドリング
    ```go
    if _, err := someFunction(); err != nil {
        return err
    }
    ```
  - **TypeScript/JavaScript**: エラーオブジェクト不使用時はparameterless catch
    ```typescript
    try {
        riskyOperation()
    } catch {
        // エラー処理
    }
    ```
- **マークダウン記法（GitHub）**: Issue・PR・README等では `@import`・`@use` 等をバッククォートで囲む（ユーザーリンク化を避けるため）

## テスト開発原則

- **テスト失敗時の対応順序**:
  1. 実装コードの動作確認（ブラウザ/CLI等）
  2. 根本原因の特定（実装 or テストの問題）
  3. 適切な修正
  4. 不要なモック・デバッグコードのクリーンアップ
- **禁止パターン**: テスト失敗→即座にモック設定変更
- **最小限修正**: 必要最小限の変更のみ、過度なモック化は避ける

## 仕様遵守原則

仕様書・Issue・チケット等で要件が明示されている場合:
- 要件を勝手に削除・簡略化・変更しない
- 「シンプルにする」等の理由でスコープを縮めない
- スコープ変更が必要と判断したら、実装前にユーザーに確認する
- 参照要件（「Xと同等」「既存のYに準拠」等）は、参照先を実際に確認してから実装する

## 計画立案原則

実装計画を立てる際（plan モード含む）、計画起案の**前**に以下を実施し、制約を思考の前提として組み込む:
- プロジェクトルートの `CLAUDE.md` を読む（`@import` 形式リンクは自動展開されるが、`[text](path)` 形式リンクは自動読込されない場合がある）
- `CLAUDE.md` からリンクされている `.md` 文書を読む（標準Markdownリンク・`@import` 両形式）
- 計画完成後の準拠検証が必要な場合は @~/.claude/skills/check-plan-compliance/SKILL.md を使用

事後チェックではなく事前読込を原則とする。事後の準拠検証に依存しすぎると計画起案自体の質が下がるため。

## 参照コンテキスト

- **テスト実装**: @~/.claude/context/test-implementation.md
- **トラブルシューティング**: @~/.claude/context/troubleshooting.md
- **GitHub Sub-Issues**: @~/.claude/context/github-sub-issues.md
- **スキル作成**: @~/.claude/context/skills.md

## Git操作

- コミット: @~/.claude/skills/git-commit/SKILL.md に従う
- PR作成: @~/.claude/skills/git-pr/SKILL.md に従う

## コンテキスト管理

- **自動更新**: 重要な汎用的コンテキストを発見した場合、このCLAUDE.mdまたは適切な context/ ファイルに自動で追記
- **分離基準**:
  - CLAUDE.md: 基本方針・原則、必須コンテキスト
  - context/: 詳細ドキュメント、特定用途専用のもの
- **リンク形式**: `@~/.claude/context/ファイル名.md`

## Compact instructions

コンパクション時は以下を優先的に保持してください：
- 現在のタスクコンテキストと目標
- エラーメッセージとテスト出力
- コード変更と修正内容
- ユーザーの指示と要件
