# /review-response

GitHubのレビューコメントを確認して適切に対応

## 使用方法
```
/review-response
```

## 実行内容
1. GitHub GraphQL APIで未解決レビューコメントを取得
2. 各指摘について修正すべきか判断
3. 修正すべき指摘は実装で対応（コミット・プッシュまで）
4. 修正不要な指摘には理由を説明して返信
5. 対応完了後にコメントを解決済みに変更

## 判断基準

### 修正すべき指摘
- **型安全性**: any型の使用、型定義の不備
- **セキュリティ**: 脆弱な実装パターン
- **パフォーマンス**: 明らかな性能問題
- **保守性**: コードの重複、複雑度問題
- **バグリスク**: 実行時エラーの可能性

### 修正不要と判断する場合
- **設計思想の違い**: 意図的な設計判断
- **既存パターン踏襲**: プロジェクト標準に準拠
- **nitpick レベル**: 好みの問題

## GraphQL API 実装

### 未解決コメント取得
```graphql
query {
  repository(owner: "OWNER", name: "REPO") {
    pullRequest(number: PR_NUMBER) {
      reviewThreads(first: 50) {
        nodes {
          id, isResolved
          comments(first: 10) {
            nodes { body, path, line }
          }
        }
      }
    }
  }
}
```

### スレッドに返信
```graphql
mutation {
  addPullRequestReviewThreadReply(input: {
    pullRequestReviewThreadId: "THREAD_ID"
    body: "返信内容"
  }) {
    comment { id }
  }
}
```

### スレッド解決
```graphql
mutation {
  resolveReviewThread(input: {
    threadId: "THREAD_ID"
  }) {
    thread { isResolved }
  }
}
```

## 重要な実装ポイント

### レビュアー別の解決ポリシー
- **Copilot** (`copilot-pull-request-reviewer`): 自動解決
- **人間レビュアー**: 未解決のまま（レビュアーの再確認待ち）

### 修正完了報告フォーマット
```
<修正内容>を対応しました。
https://github.com/<owner>/<repo>/pull/<PR番号>/commits/<コミットハッシュ>
```

### 品質確認
修正後は必ずプロジェクト標準の品質チェックを実行：
- Node.js: `npm run lint`, `npm run typecheck`
- Go: `go vet`, `golangci-lint run`
- Python: `ruff check`, `mypy`

## 注意事項
- **レビュースレッド返信時**: `pullRequestReviewThreadId`のみ使用（`pullRequestReviewId`は不要）
- **GraphQLレート制限**: 1時間あたり5000ポイント
- **既存PRの自動更新**: プッシュでPRは自動更新される