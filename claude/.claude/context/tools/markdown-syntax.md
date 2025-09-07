# マークダウンでの技術用語記載注意点

## GitHub上での@記法回避

- **問題**: `@import`、`@use`等はGitHubでユーザーリンクになる
- **対策**: バッククォートで囲む → `` `@import` ``、`` `@use` ``
- **適用場面**: Issue、PR、コメント、READMEでの技術文書記載時

## 記載例

```markdown
// 悪い例
@import や @use を使用する

// 良い例
`@import` や `@use` を使用する
```
