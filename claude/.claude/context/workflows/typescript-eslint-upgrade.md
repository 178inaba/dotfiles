# TypeScript/ESLint アップグレード戦略

## @typescript-eslint メジャーバージョンアップ手順

### 事前調査
1. **TypeScript互換性確認**: 現在のTypeScriptバージョンと新ESLintバージョンの対応表を確認
2. **廃止ルール調査**: 削除・非推奨となったルールの洗い出し
3. **新機能理解**: 新規追加された型チェック機能の把握

### 段階的アップグレード手順
1. **依存関係更新**:
   ```bash
   npm update @typescript-eslint/eslint-plugin @typescript-eslint/parser @vue/eslint-config-typescript
   ```

2. **設定ファイル修正**:
   - 廃止ルールの削除（例: `@typescript-eslint/member-delimiter-style`）
   - 新規ルールの適切な設定

3. **コード修正アプローチ**:
   - **優先順位1**: より良い実装パターンがある場合はリファクタリング
   - **優先順位2**: 型定義専用など正当な理由がある場合は`eslint-disable-next-line`使用
   - **最終手段**: 広範囲の`eslint-disable`は避ける

## よくある移行パターン

### 未使用変数エラー対応
```typescript
// Before: @typescript-eslint v5
catch (err) { /* errを使用しない */ }

// After: @typescript-eslint v8
catch { /* parameterless catch */ }
```

### 型定義専用配列
```typescript
// 型推論でのみ使用される配列は eslint-disable が適切
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const typeOnlyArray = [CONST1, CONST2] as const
export type UnionType = ArrayElement<typeof typeOnlyArray>
```

### オブジェクト型推論への移行
```typescript
// Before: 配列ベース
const keys = [KEY1, KEY2] as const
export type KeyType = ArrayElement<typeof keys>

// After: オブジェクトベース（推奨）
export const obj = { [KEY1]: {...}, [KEY2]: {...} } as const
export type KeyType = keyof typeof obj
```

## 品質保証

### 必須チェック項目
- [ ] `npm run lint` 完全通過（max-warnings 0）
- [ ] `npm run test` 全テスト成功
- [ ] 既存機能の振る舞い変更なし
- [ ] 型安全性の向上確認

### 業界標準との適合性確認
```bash
# 主要ライブラリでのeslint-disable使用状況調査
find node_modules -name "*.js" -o -name "*.ts" | xargs grep -l "eslint-disable-next-line @typescript-eslint/no-unused-vars" | wc -l
```

## 移行判断基準

### eslint-disable を使うべきケース
1. **型定義専用**: `ArrayElement<typeof array>` のような型推論専用配列
2. **技術的制約**: フレームワークの制約で回避不可能な場合
3. **一時的措置**: 段階的移行での限定的使用

### リファクタリングすべきケース
1. **実際に値として使用**: オブジェクト型推論に移行可能
2. **@ts-ignore存在**: 適切な型定義で解決可能
3. **any型乱用**: 具体的な型定義で改善可能
