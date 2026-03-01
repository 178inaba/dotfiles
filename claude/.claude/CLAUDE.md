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

## テスト開発原則

- **テスト失敗時の対応順序**:
  1. 実装コードの動作確認（ブラウザ/CLI等）
  2. 根本原因の特定（実装 or テストの問題）
  3. 適切な修正
  4. 不要なモック・デバッグコードのクリーンアップ
- **禁止パターン**: テスト失敗→即座にモック設定変更
- **最小限修正**: 必要最小限の変更のみ、過度なモック化は避ける

## 仕様遵守原則

**必須実施事項**: すべての実装作業で以下を厳格に実行

### チェックリスト方式
実装前に必ず確認：
- [ ] 仕様書の全セクションを読み返した
- [ ] 要件を箇条書きで列挙した
- [ ] 各要件に対応する実装方法を明記した
- [ ] 参照要件（「〜と同等」等）を確認した

### 勝手な判断の禁止
- 仕様書に明記された要件は**絶対に削除・変更しない**
- 「シンプルにする」等の理由で勝手に仕様変更しない
- 変更が必要な場合は必ずユーザーに確認・承認を得る

### 段階的確認
1. **設計時**: 仕様書と設計の対応表作成
2. **実装時**: 各コンポーネント完成後に仕様書と照合
3. **統合時**: システム全体で要件を満たしているか確認

## 開発ワークフロー

### パターン集
- **テスト実装**: @~/.claude/context/patterns/test-implementation.md
- **SPA状態保持**: @~/.claude/context/patterns/spa-state.md
- **データテーブル**: @~/.claude/context/patterns/data-table.md
- **Goパフォーマンス**: @~/.claude/context/patterns/go-perf.md
- **Go Clean Archエラー**: @~/.claude/context/patterns/go-clean-arch-errors.md
- **ESLint複雑度**: @~/.claude/context/patterns/eslint-complexity.md

### ツール
- **GitHub Sub-Issues**: @~/.claude/context/tools/github-sub-issues.md
- **マークダウン記載**: @~/.claude/context/tools/markdown-syntax.md
- **スキル**: @~/.claude/context/tools/skills.md

### ワークフロー
- **計画作成**: @~/.claude/context/workflows/plan-writing.md
- **トラブルシューティング**: @~/.claude/context/workflows/troubleshooting.md
- **CI/CDデプロイ**: @~/.claude/context/workflows/cicd-deployment.md
- **大規模機能開発**: @~/.claude/context/workflows/feature-development.md
- **TypeScript/ESLint**: @~/.claude/context/workflows/typescript-eslint-upgrade.md

## Git操作

- コミット: `/git-commit` スキルに従う
- PR作成: `/git-pr` スキルに従う

## 計画モード原則

- **自己完結性**: 計画ファイルはコンテキストクリア後に単独で実装可能な状態にする
- **詳細**: @~/.claude/context/workflows/plan-writing.md に従う
- **必須要素**: 背景・対象ファイル（絶対パス）・既存コード引用・具体的変更内容・実装順序・検証コマンド・受け入れ基準
- **禁止**: 会話への後方参照（「上記で議論した」等）、曖昧な指示（「適切に修正」等）

## コンテキスト管理

- **自動更新**: 重要な汎用的コンテキストを発見した場合、このCLAUDE.mdまたは適切な context/ ファイルに自動で追記
- **分離基準**:
  - CLAUDE.md: 基本方針・原則、必須コンテキスト
  - context/: 詳細ドキュメント、特定用途専用のもの
- **リンク形式**: `@~/.claude/context/カテゴリ/ファイル名.md`

## Compact instructions

コンパクション時は以下を優先的に保持してください：
- 現在のタスクコンテキストと目標
- エラーメッセージとテスト出力
- コード変更と修正内容
- ユーザーの指示と要件
