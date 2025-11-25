# テスト実装の3原則

テストコードは以下の3つの観点で評価される。実装時にこの原則を意識することで、高品質なテストを作成できる。

## 1. 無駄なテストがないか

- **過剰なモック**: 実装の詳細に依存しすぎるモック（実装変更でテストが壊れる）
- **重複テスト**: 同じ振る舞いを複数のテストでカバー
- **意味のないテスト**: 実際には何も検証していないテスト

## 2. 必要なテストの抜け漏れがないか

- **正常系**: 期待される動作のテスト
- **異常系**: エラーケース、例外処理のテスト
- **エッジケース**: 境界値、NULL、空文字列、最大値等

## 3. テストの可読性

- **テスト名**: 何をテストしているか明確（例: `should return error when input is null`）
- **構造**: Arrange-Act-Assert パターン等の一貫した構造
- **意図の明確さ**: なぜこのテストが必要かが分かる

## t.Cleanup() のLIFO実行順序

Go の `t.Cleanup()` は**LIFO（後入れ先出し）**で実行される。これを理解していないと外部キー制約エラー等が発生する。

### 基本原則

**「作成直後にCleanupを登録する」** - これにより、依存関係の逆順で削除される

```go
// 良い例: 作成直後にCleanup登録
parent := createParent(t)           // 親エンティティ作成
t.Cleanup(func() {                  // 親のCleanup登録（スタックの底）
    deleteParent(parent.ID)
})

child := createChild(t, parent.ID)  // 子エンティティ作成
t.Cleanup(func() {                  // 子のCleanup登録（スタックの上）
    deleteChild(child.ID)           // ← 先に実行される
})

// 実行順序: 子削除 → 親削除（正しい）
```

```go
// 悪い例: まとめて後でCleanup登録
parent := createParent(t)
child := createChild(t, parent.ID)

t.Cleanup(func() {
    deleteParent(parent.ID)  // ← 先に実行される（エラー！）
    deleteChild(child.ID)
})
```

### ループ内でのCleanup登録

ループで複数のエンティティを作成する場合、**ループ内で個別にCleanupを登録**する必要がある。

```go
// 良い例: ループ内で個別にCleanup登録
for i := 0; i < 5; i++ {
    record := createRecord(t)
    t.Cleanup(func() {
        deleteRecord(record.ID)  // ← 各レコードごとにCleanup登録
    })

    // 依存データを作成
    createRelation(t, record.ID, category.ID)  // 内部でCleanupを登録
}

// 実行順序:
// 1. record5のrelation削除
// 2. record5削除
// 3. record4のrelation削除
// 4. record4削除
// ...（正しい）
```

```go
// 悪い例: ループの後でまとめてCleanup登録
records := []Record{}
for i := 0; i < 5; i++ {
    record := createRecord(t)
    records = append(records, record)
    createRelation(t, record.ID, category.ID)  // 内部でCleanupを登録
}

t.Cleanup(func() {
    for _, record := range records {
        deleteRecord(record.ID)  // ← relationより先に実行される（エラー！）
    }
})

// 実行順序:
// 1. 全recordsの一括削除 ← relationが残っている
// 2. record5のrelation削除
// 3. record4のrelation削除
// ...（外部キー制約エラー）
```

### デバッグ手法

Cleanup順序の問題を特定する際は、ログと SELECT クエリで事実を確認する。

詳細は @~/.claude/context/workflows/troubleshooting.md を参照。
