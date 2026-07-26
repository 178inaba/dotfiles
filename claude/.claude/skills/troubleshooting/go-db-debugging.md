# Go + RDB のデバッグ実例

`troubleshooting` スキルの手順を Go のテスト + RDB 環境に当てはめた実例集。他の言語・環境ではスキル本体の手順だけで足り、本ファイルを読む必要はない。

## 目次

- デバッグログで状態を確認する
- 実行順序を確認する（Cleanup の LIFO）
- 段階的な絞り込みのコマンド
- よくある問題パターン: 外部キー制約エラー
- よくある問題パターン: データの重複エラー
- よくある問題パターン: 実行順序の問題

## デバッグログで状態を確認する

```go
t.Cleanup(func() {
    var count int
    db.Get(&count, "SELECT COUNT(*) FROM child WHERE parent_id = ?", parentID)
    t.Logf("[DEBUG] Cleanup parent_id=%d, remaining children=%d", parentID, count)

    var ids []int
    db.Select(&ids, "SELECT id FROM child WHERE parent_id = ?", parentID)
    t.Logf("[DEBUG] Remaining child IDs: %v", ids)

    if _, err := db.Exec("DELETE FROM parent WHERE id = ?", parentID); err != nil {
        t.Fatal(err)
    }
})
```

## 実行順序を確認する（Cleanup の LIFO）

`t.Cleanup` は後で登録したものが先に実行される。順序が結果を左右する場合はログで明示的に確認する。

```go
t.Logf("[DEBUG] Step 1: Creating parent")
parent := createParent(t)

t.Logf("[DEBUG] Step 2: Registering parent cleanup")
t.Cleanup(func() {
    t.Logf("[DEBUG] Executing parent cleanup")
    deleteParent(parent.ID)
})

t.Logf("[DEBUG] Step 3: Creating child")
child := createChild(t, parent.ID)

t.Logf("[DEBUG] Step 4: Registering child cleanup")
t.Cleanup(func() {
    t.Logf("[DEBUG] Executing child cleanup")
    deleteChild(child.ID)
})
```

期待どおりなら、出力は登録順と逆になる:

```
[DEBUG] Step 1: Creating parent
[DEBUG] Step 2: Registering parent cleanup
[DEBUG] Step 3: Creating child
[DEBUG] Step 4: Registering child cleanup
[DEBUG] Executing child cleanup      ← 後で登録したものが先に実行される（LIFO）
[DEBUG] Executing parent cleanup
```

## 段階的な絞り込みのコマンド

```bash
make test                                          # 全体像の把握
go test ./path/to/package                          # パッケージ単位
go test -v ./path/to/package -run TestSpecificTest # 個別・詳細出力
go test -race -v ./path/to/package -run TestName   # 並行処理の問題を検出
```

データの状態は、エラーメッセージではなく実際のレコードで確認する:

```go
var records []Record
db.Select(&records, "SELECT * FROM records WHERE parent_id = ?", parentID)
t.Logf("[DEBUG] Current records: %+v", records)
```

## よくある問題パターン: 外部キー制約エラー

**症状**: `Cannot delete or update a parent row: a foreign key constraint fails`

```go
var count int
db.Get(&count, "SELECT COUNT(*) FROM child_table WHERE parent_id = ?", parentID)
t.Logf("[DEBUG] Remaining children: %d", count)

if count > 0 {
    var childIDs []int
    db.Select(&childIDs, "SELECT id FROM child_table WHERE parent_id = ?", parentID)
    t.Logf("[DEBUG] Child IDs: %v", childIDs)
}
```

**根本原因**: 削除順序の問題、Cleanup の登録順序の問題

## よくある問題パターン: データの重複エラー

**症状**: `Duplicate entry 'xxx' for key 'unique_constraint'`

```go
var existing []Entity
db.Select(&existing, "SELECT * FROM table WHERE unique_key = ?", key)
t.Logf("[DEBUG] Existing entries: %+v", existing)

var allData []Entity
db.Select(&allData, "SELECT * FROM table")
t.Logf("[DEBUG] All entries in table: %+v", allData)
```

**根本原因**: Cleanup の失敗、テスト間の分離不足、DROP DATABASE の欠如

## よくある問題パターン: 実行順序の問題

**症状**: テストが単独では成功するが、全体では失敗する

```bash
go test -v -run TestA          # → PASS
go test -v                     # → FAIL
go test -v -run "TestA|TestB"  # 特定の組み合わせで切り分け
```

**根本原因**: グローバル変数の共有、Cleanup の欠如、race condition
