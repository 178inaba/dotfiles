# スキルの構成例

`skill-authoring` スキルの規約を具体例で示す。規約本体は SKILL.md を正とする。

## 基本構成

```markdown
---
name: skill-name
description: 概要（1行）
argument-hint: [引数]
disable-model-invocation: true  # 副作用がある場合はtrue
---

# /skill-name

## 使用方法
\`\`\`
/skill-name [引数]
\`\`\`

## 実行内容
1. 手順1
2. 手順2
...

## 前提条件（該当する場合のみ）
- 条件1
- 条件2

## 判断基準（複雑な判断が必要な場合のみ）
### パターンA
- [ ] 条件1
- [ ] 条件2

### パターンB
- [ ] 条件3
- [ ] 条件4

## 注意事項（必要に応じて）
1. 注意点1
2. 注意点2
```

## 良い例

```markdown
---
name: deploy
description: 本番環境へのデプロイを実行
disable-model-invocation: true
---

# /deploy
本番環境へのデプロイを実行

## 使用方法
\`\`\`
/deploy
\`\`\`

## 実行内容
1. テスト実行
2. ビルド
3. デプロイ
```

## 悪い例（冗長）

```markdown
---
name: deploy
description: 本番環境へのデプロイを実行
---

# /deploy
本番環境へのデプロイを実行

## 概要
このスキルは... (長い説明)

## 関連スキル
- /build
- /test

## 更新履歴
- 2024-01-01: 作成
- 2024-01-02: 修正

## 実行内容
...
```

`## 概要`・`## 関連スキル`・`## 更新履歴` はいずれも設計原則 2「必要最小限」に反する。更新履歴は git log が、関連スキルは一覧が持っている。
