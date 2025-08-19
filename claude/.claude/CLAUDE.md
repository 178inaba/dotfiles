## コーディング・対応方針

- **言語**: 日本語での質問・回答をお願いします
- **ファイル形式**: ファイルの最後に改行文字を必ず追加してください（POSIX標準準拠）
- **EditorConfig**: プロジェクトに.editorconfigファイルが存在する場合は、その設定に必ず従ってください（indent_style, indent_size, insert_final_newline, end_of_line等）
- **コメント**: 既存のコメント言語（英語/日本語）を踏襲し、コードベース全体で一貫性を保ってください
- **エラーハンドリング**: Go言語では戻り値を使わない場合、条件文内でエラーハンドリングを行う慣用パターンを使用する
  ```go
  // 推奨パターン（戻り値を使わない場合）
  if _, err := someFunction(); err != nil {
      return err
  }

  // 避けるパターン（戻り値を使わない場合）
  _, err := someFunction()
  if err != nil {
      return err
  }

  // 戻り値を使う場合は分離してOK
  result, err := someFunction()
  if err != nil {
      return err
  }
  // resultを使用
  ```

## 差分レビュー方針

詳細: @~/.claude/context/workflows/code-review.md

## Git操作規約

- **ブランチ切り替え**: `git switch`を使用（`git checkout`ではなく）
- **コミットメッセージ**: プロジェクト標準に従う

## コンテキスト管理

- **自動更新**: 重要な汎用的コンテキスト（開発ワークフロー、よく使うツール・コマンド、環境設定等）を発見した場合、このCLAUDE.mdまたは適切な context/ ファイルに自動で追記してください
- **分離基準**:
  - CLAUDE.md直接: 基本方針・原則、必須コンテキスト、短くて変更頻度が低いもの
  - context/別ファイル: 詳細ドキュメント、変更頻度が高いもの、特定用途専用のもの
- **リンク形式**: 別ファイル参照時は `@~/.claude/context/カテゴリ/ファイル名.md` 形式を使用
