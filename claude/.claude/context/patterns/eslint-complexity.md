# ESLint複雑度対策パターン

複雑度エラー（Complexity > 10）を解決する実証済みリファクタリング手法。

## 分割の基本原則
1. **単一責任**: 各メソッドは1つの明確な責任のみを持つ
2. **命名規則**: 処理内容が分かる動詞ベースの関数名
3. **データフロー**: 前のメソッドの結果を次のメソッドが受け取る設計

## 分割パターン例

### Before: 複雑度15のメソッド
```typescript
processData(): Promise<void> {
    /* 60行の複雑な処理 */
}
```

### After: 5個の小さなメソッドに分割
```typescript
processData(): Promise<void>     // メイン制御フロー
validateInput(): boolean         // 入力値検証
transformData(): DataModel       // データ変換
executeOperation(): Promise<Result> // 主要処理実行
handleResult(result: Result): void  // 結果処理
```

## 効果
- **複雑度**: 15 → 各メソッド3-5に改善
- **可読性**: 処理の流れが明確化
- **保守性**: 個別機能の修正が容易
- **テスタビリティ**: 単体テスト作成が簡単

## 具体的な分割テクニック

### 1. Early Return パターン
```typescript
// Before
function process(data) {
    if (data) {
        if (data.isValid) {
            // 深いネスト
        }
    }
}

// After
function process(data) {
    if (!data) return;
    if (!data.isValid) return;
    // メイン処理
}
```

### 2. ガード節の抽出
```typescript
// Before
function complexLogic(params) {
    if (!params.a || !params.b || params.c > 10) {
        throw new Error();
    }
    // 複雑な処理
}

// After
function validateParams(params) {
    if (!params.a || !params.b || params.c > 10) {
        throw new Error();
    }
}

function complexLogic(params) {
    validateParams(params);
    // シンプルになった処理
}
```

### 3. ループ処理の抽出
```typescript
// Before
function processAll(items) {
    for (const item of items) {
        // 複雑な処理
    }
}

// After
function processItem(item) {
    // 単一アイテムの処理
}

function processAll(items) {
    items.forEach(processItem);
}
```
