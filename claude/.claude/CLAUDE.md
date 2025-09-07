## コーディング・対応方針

- **言語**: 日本語での質問・回答をお願いします
- **ファイル形式**: ファイルの最後に改行文字を必ず追加してください（POSIX標準準拠）
- **EditorConfig**: プロジェクトに.editorconfigファイルが存在する場合は、その設定に必ず従ってください
- **コメント**: 既存のコメント言語（英語/日本語）を踏襲し、コードベース全体で一貫性を保ってください
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
- **SPA状態保持**: @~/.claude/context/patterns/spa-state.md
- **データテーブル**: @~/.claude/context/patterns/data-table.md
- **Goパフォーマンス**: @~/.claude/context/patterns/go-perf.md
- **ESLint複雑度**: @~/.claude/context/patterns/eslint-complexity.md

### ツール
- **GitHub Sub-Issues**: @~/.claude/context/tools/github-sub-issues.md
- **マークダウン記載**: @~/.claude/context/tools/markdown-syntax.md
- **スラッシュコマンド**: @~/.claude/context/tools/slash-commands.md

### ワークフロー
- **差分レビュー**: @~/.claude/context/workflows/code-review.md
- **Git操作**: @~/.claude/context/workflows/git.md
- **大規模機能開発**: @~/.claude/context/workflows/feature-development.md
- **TypeScript/ESLint**: @~/.claude/context/workflows/typescript-eslint-upgrade.md

## コンテキスト管理

- **自動更新**: 重要な汎用的コンテキストを発見した場合、このCLAUDE.mdまたは適切な context/ ファイルに自動で追記
- **分離基準**:
  - CLAUDE.md: 基本方針・原則、必須コンテキスト
  - context/: 詳細ドキュメント、特定用途専用のもの
- **リンク形式**: `@~/.claude/context/カテゴリ/ファイル名.md`
