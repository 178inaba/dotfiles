# /review-response

GitHubのレビューコメントを確認して適切に対応するコマンド

## 使用方法
```
/review-response
```

## 実行内容
1. GitHub GraphQL APIでPRの未解決レビューコメントを取得・分析
2. 各指摘について修正すべきか判断
3. 修正すべき指摘は実装で対応（コミット・プッシュまで）
4. 修正不要な指摘には理由を説明して返信
5. 対応完了後にコメントを解決済みに変更

## 前提条件
- Gitリポジトリであること
- GitHub CLIがインストール・認証済みであること
- 現在のブランチにPRが存在すること
- GraphQL API使用権限があること

## 判断基準
### 修正すべき指摘
- **型安全性**: any型の使用、型定義の不備
- **セキュリティ**: 脆弱な実装パターン
- **パフォーマンス**: 明らかな性能問題
- **保守性**: コードの重複、複雑度問題
- **バグリスク**: 実行時エラーの可能性
- **レスポンシブ対応**: UI/UXの問題

### 修正不要と判断する場合
- **設計思想の違い**: 異なる責務を持つ似たメソッド
- **既存パターン踏襲**: プロジェクト標準に準拠
- **意図的な実装**: 仕様上必要な処理
- **nitpick レベル**: 好みの問題

## プロンプト
現在のPRのレビューコメントを確認して適切に対応してください。

### 手順
1. **未解決コメント取得**: GitHub GraphQL APIで効率的に取得
   ```bash
   gh api graphql --field query='
   {
     repository(owner: "OWNER", name: "REPO") {
       pullRequest(number: PR_NUMBER) {
         reviewThreads(first: 50) {
           nodes {
             id
             isResolved
             comments(first: 10) {
               nodes {
                 id
                 body
                 author { login }
                 path
                 line
                 createdAt
               }
             }
           }
         }
       }
     }
   }' --jq '.data.repository.pullRequest.reviewThreads.nodes[] | select(.isResolved == false)'
   ```
2. **コメント詳細分析**: 各未解決コメントの内容と対象行を詳細に分析
3. **各指摘の分析**:
   - 指摘内容の技術的妥当性を評価
   - 修正による改善効果を判断
   - プロジェクトの実装方針との整合性を確認
4. **対応実施**:
   - **修正対応**: コード変更 + 品質確認 + コミット + プッシュ
     - **品質確認**: プロジェクトの標準ツールで実行（例: `npm run lint`, `go vet`, `cargo check`, `ruff`, `flake8`等）
   - **返信対応**: GraphQL mutationでスレッドに返信
     ```bash
     gh api graphql --field query='
     mutation {
       addPullRequestReviewThreadReply(input: {
         pullRequestReviewThreadId: "THREAD_NODE_ID"
         body: "返信内容"
       }) {
         comment { id body }
       }
     }'
     ```
5. **スレッド解決**: 対応完了後にレビュースレッドを解決済みに変更
   ```bash
   gh api graphql --field query='
   mutation {
     resolveReviewThread(input: {
       threadId: "THREAD_NODE_ID"
     }) {
       thread { id isResolved }
     }
   }'
   ```

### 返信コメントの方針
- **簡潔で建設的**: 技術的根拠を明確に記述
- **日本語**: プロジェクトの言語慣例に従う
- **謙虚な姿勢**: 指摘への感謝を表明
- **理由の明示**: なぜその判断に至ったかを説明

### 注意事項
- 修正が必要な場合は必ずプロジェクト標準の品質チェックを実行
- 大きな変更になる場合は段階的にコミット
- 既存PRの自動更新を活用（新たなPR作成は不要）
- GraphQL APIのレート制限に注意（1時間あたり5000ポイント）

## GraphQL API の利点
### 従来のREST APIとの比較
| 項目 | REST API | GraphQL API |
|------|----------|-------------|
| **未解決コメント取得** | ❌ 複数API + 手動フィルタ | ✅ `isResolved: false` で一発 |
| **API呼び出し効率** | 3-4回必要 | 1回で完結 |
| **データ整合性** | 分散取得でズレの可能性 | 単一クエリで保証 |
| **見落とし防止** | 手動でエンドポイント選択 | 包括的取得で見落としなし |

### 重要な実装ポイント
- **レビュースレッド返信**: `pullRequestReviewId` パラメータは**不要**
  - ❌ `pullRequestReviewId` を含めると `comment: null` が返される
  - ✅ `pullRequestReviewThreadId` のみで正常動作
- **レビュアー別の解決ポリシー**: `author.login` で判定して適切に対応
  - **Copilot** (`"copilot-pull-request-reviewer"`): 修正対応・返信対応ともに自動解決
    - 理由: 自動レビューなので返信により対応完了とみなせる
  - **人間レビュアー**: 修正対応・返信対応ともに未解決のまま
    - 理由: 修正内容や返信内容をレビュアーが再確認すべき
- **修正完了報告**: コミットURLを含めてわかりやすく報告
  ```
  <修正内容>を<変更後の内容>に変更して対応しました。
  https://github.com/<owner>/<repo>/pull/<PR番号>/commits/<コミットハッシュ>
  ```

### 言語別品質確認コマンド例
- **Node.js**: `npm run lint`, `npm run test`, `npm run typecheck`
- **Go**: `go vet ./...`, `go test ./...`, `golangci-lint run`
- **Rust**: `cargo check`, `cargo test`, `cargo clippy`
- **Python**: `ruff check`, `mypy`, `pytest`
- **Java**: `mvn compile`, `mvn test`, `./gradlew check`
