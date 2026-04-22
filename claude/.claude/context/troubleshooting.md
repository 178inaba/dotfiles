# トラブルシューティング手法

## 基本原則

### 推測ではなく事実を確認する

**「対処療法じゃ気持ち悪い」** - 症状を一時的に抑えるのではなく、根本原因を特定して解決する。

```
❌ 悪い例:
1. エラーが出た
2. とりあえずクリーンアップコードを追加
3. 別のエラーが出た
4. さらにクリーンアップコードを追加
5. ...（試行錯誤の繰り返し）

✅ 良い例:
1. エラーが出た
2. ログを入れて現在の状態を確認
3. SELECT クエリでデータを確認
4. 実行順序を確認
5. 根本原因を特定
6. 最小限の修正で解決
```

### ログを入れまくって事実を積み重ねる

**「ログとか入れまくっていいから」** - 一時的なデバッグログは積極的に使い、最後に削除すればよい。

```go
// デバッグログの例
t.Cleanup(func() {
    // 現在の状態を確認
    var count int
    db.Get(&count, "SELECT COUNT(*) FROM child WHERE parent_id = ?", parentID)
    t.Logf("[DEBUG] Cleanup parent_id=%d, remaining children=%d", parentID, count)

    // どのデータが残っているか確認
    var ids []int
    db.Select(&ids, "SELECT id FROM child WHERE parent_id = ?", parentID)
    t.Logf("[DEBUG] Remaining child IDs: %v", ids)

    if _, err := db.Exec("DELETE FROM parent WHERE id = ?", parentID); err != nil {
        t.Fatal(err)
    }
})
```

### 実行順序を確認する

処理の順序が重要な場合（LIFO、トランザクション、非同期処理等）は、ログで実行順序を明示的に確認する。

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

実行結果のログで、期待通りの順序かを確認:
```
[DEBUG] Step 1: Creating parent
[DEBUG] Step 2: Registering parent cleanup
[DEBUG] Step 3: Creating child
[DEBUG] Step 4: Registering child cleanup
[DEBUG] Executing child cleanup      ← 後で登録したものが先に実行される（LIFO）
[DEBUG] Executing parent cleanup
```

## 段階的な問題の絞り込み

### 1. 全体像の把握

まず、問題の範囲を特定する。

```bash
# 全テストを実行して失敗箇所を確認
make test

# 特定のパッケージのみ実行
go test ./path/to/package

# 特定のテストのみ実行
go test -v ./path/to/package -run TestSpecificTest
```

### 2. 個別の確認

問題が特定のテストに限定されたら、そのテストを詳細に調査する。

```bash
# 詳細出力で実行
go test -v ./path/to/package -run TestSpecificTest

# race detector 付きで実行（並行処理の問題を検出）
go test -race -v ./path/to/package -run TestSpecificTest
```

### 3. データの確認

エラーメッセージだけでなく、実際のデータの状態を確認する。

```go
// テスト中にDBの状態を確認
var records []Record
db.Select(&records, "SELECT * FROM records WHERE parent_id = ?", parentID)
t.Logf("[DEBUG] Current records: %+v", records)

var relations []Relation
db.Select(&relations, "SELECT * FROM relations WHERE record_id IN (?)", recordIDs)
t.Logf("[DEBUG] Current relations: %+v", relations)
```

### 4. 最小再現ケースの作成

問題を最小限のコードで再現できるか試す。

```go
// 複雑なテストケースを単純化
func TestMinimalReproduction(t *testing.T) {
    // 最小限のセットアップ
    parent := createParent(t)
    child := createChild(t, parent.ID)

    // 問題が再現するか確認
    // ...
}
```

## よくある問題パターン

### 外部キー制約エラー

**症状**: `Cannot delete or update a parent row: a foreign key constraint fails`

**確認方法**:
```go
// どの子データが残っているか確認
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

### データの重複エラー

**症状**: `Duplicate entry 'xxx' for key 'unique_constraint'`

**確認方法**:
```go
// 既存データを確認
var existing []Entity
db.Select(&existing, "SELECT * FROM table WHERE unique_key = ?", key)
t.Logf("[DEBUG] Existing entries: %+v", existing)

// 前回のテストのデータが残っていないか確認
var allData []Entity
db.Select(&allData, "SELECT * FROM table")
t.Logf("[DEBUG] All entries in table: %+v", allData)
```

**根本原因**: Cleanup の失敗、テスト間の分離不足、DROP DATABASE の欠如

### 実行順序の問題

**症状**: テストが単独では成功するが、全体では失敗する

**確認方法**:
```bash
# 単独実行
go test -v -run TestA
# → PASS

# 全体実行
go test -v
# → FAIL

# 特定の組み合わせで実行
go test -v -run "TestA|TestB"
```

**根本原因**: グローバル変数の共有、Cleanup の欠如、race condition

## デバッグログの削除

問題解決後は、デバッグログを削除する。

### 一括削除パターン

```bash
# [DEBUG] を含む行を検索
grep -r "\[DEBUG\]" .

# 削除対象ファイルを確認
git diff

# コミット前に必ず削除
```

### 残すべきログと削除すべきログ

**残すべき:**
- 本番環境でも役立つ情報（エラー、警告、重要な状態変化）
- パフォーマンス監視用のログ

**削除すべき:**
- `[DEBUG]` タグ付きの一時的なログ
- SELECT COUNT(*) 等のデバッグクエリ
- 開発中の試行錯誤コード

## まとめ

1. **推測で対処しない** - 根本原因を特定する
2. **ログで事実を確認** - 現在の状態、実行順序、残存データ
3. **段階的に絞り込む** - 全体 → 個別 → 最小再現
4. **最後にクリーンアップ** - デバッグコードは削除する

**「対処療法じゃ気持ち悪い」を常に意識する。**
