# TypeScript/ESLint アップグレード戦略

## 事前調査

### 互換性確認
- TypeScriptとESLintバージョンの対応表を確認
- 削除・非推奨ルールの洗い出し
- 新規追加された型チェック機能の把握

### Breaking Changesパターン
- **AST仕様変更**: ノード/プロパティの削除・名前変更
- **ルール変更**: オプション削除、デフォルト値変更、スキーマ変更
- **API変更**: 関数・型の削除や非互換な変更

## 段階的アップグレード

### 1. 依存関係更新
```bash
npm update @typescript-eslint/eslint-plugin @typescript-eslint/parser
```

### 2. 設定ファイル修正
- 廃止ルールの削除
- 新規ルールの適切な設定
- recommendedルールセットの変更確認

### 3. コード修正優先順位
1. **リファクタリング**: より良い実装パターンへ移行
2. **限定的無効化**: 正当な理由がある場合のみ`eslint-disable-next-line`
3. **広範囲無効化は回避**: ファイル全体のdisableは最終手段

## よくある移行パターン

### Parameterless Catch
```typescript
// 旧: 未使用変数
catch (err) { /* errを使用しない */ }

// 新: パラメータレス
catch { /* エラーオブジェクト不要 */ }
```

### 型定義専用の扱い
```typescript
// 型推論専用は eslint-disable が適切
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const typeOnlyArray = [CONST1, CONST2] as const
export type UnionType = ArrayElement<typeof typeOnlyArray>

// より良い方法: オブジェクト型推論
export const obj = { [KEY1]: {...}, [KEY2]: {...} } as const
export type KeyType = keyof typeof obj
```

## 品質保証

### 必須チェック
- `npm run lint` 完全通過（max-warnings: 0）
- 全テスト成功
- 既存機能の振る舞い不変
- 型安全性の向上確認

### 移行判断基準

#### eslint-disableを使うべき場合
- 型定義専用の構造
- フレームワーク制約
- 段階的移行の一時措置

#### リファクタリングすべき場合
- 実際に値として使用される箇所
- `@ts-ignore`の存在
- `any`型の乱用

## セマンティックバージョニング
- @typescript-eslintは全パッケージで同一バージョン番号を使用
- メジャーバージョンアップ時はBreaking Changesを必ず確認
- マイナー/パッチバージョンは後方互換性を維持