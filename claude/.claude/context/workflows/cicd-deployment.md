# CI/CDデプロイメント設計パターン

GitHub ActionsとAWS ECSを使った本番・開発環境のCI/CDパイプライン設計のベストプラクティス。

## 基本構成

### ワークフロートリガー
```yaml
on: push

jobs:
  deploy-dev:
    if: github.ref == 'refs/heads/main'

  deploy-prod:
    if: startsWith(github.ref, 'refs/tags/v')
```

**ポイント**:
- 単一ワークフローで複数環境を管理
- `if`条件で環境を分岐
- タグは`v`始まり（`v1.0.0`形式）

## タスク定義ファイル構成

### ファイル命名規則
```
task-definitions/
├── dev.json          # 開発環境APIサーバー
├── dev-batch.json    # 開発環境バッチ
├── prod.json         # 本番環境APIサーバー
└── prod-batch.json   # 本番環境バッチ
```

### 環境間の差分
- イメージ名: `myapp-dev` / `myapp-prod`
- SSMパラメータARN: `myapp-dev/*` / `myapp-prod/*`
- IAMロール: `myapp-dev-ecs-*` / `myapp-prod-ecs-*`
- ログ設定: `/ecs/myapp-dev*` / `/ecs/myapp-prod*`

## 再利用可能なデプロイアクション

### composite actionパターン
```yaml
# .github/actions/deploy/action.yml
inputs:
  environment:
    description: The deployment environment (e.g., dev, prod)
    required: true

runs:
  using: composite
  steps:
    - name: Build and push Docker image
      # 環境固有のECRリポジトリに自動プッシュ
      # ${{ inputs.environment }}を使って動的に切り替え
```

**利点**:
- 環境ごとにワークフローを複製しない
- メンテナンスコストの削減
- 一貫性の保証

## デプロイフロー

### 開発環境
- **トリガー**: `main`ブランチへのマージ
- **手順**: PR作成 → レビュー → マージ → 自動デプロイ

### 本番環境
- **トリガー**: GitHubリリース作成（`v*`タグ）
- **手順**: Releasesページ → Create a new release → タグ入力 → Publish release → 自動デプロイ

### なぜタグの直接プッシュではなくリリース作成か
- リリースノートによる変更履歴の記録
- UIからの操作で誤操作防止
- チームメンバーへの通知

## ロールバック設計

### 基本方針
**専用のロールバックセクションは不要**。通常のデプロイフローを使用する。

### 実装方法
1. 問題のあるコミットをRevertするPRを作成
2. `main`ブランチにマージ（開発環境で動作確認）
3. 新しいバージョンタグでリリース作成（例: `v1.1.1`）

### なぜ古いタグの再利用ではないのか
- GitHub Actionsは`on: push`で動作するため、新しいpushイベントが必要
- 既存タグで新しいリリースを作成してもワークフローは実行されない
- Git履歴にRevertの記録が残り、変更追跡が容易

## ドキュメント作成の原則

### 削除すべき内容
- **一般的なGitフロー**: featureブランチの作り方、コミット方法など
- **自明な手順**: 開発者が当然知っている内容
- **既存フローの組み合わせ**: ロールバック = Revert + デプロイ

### 残すべき内容
- **トリガー条件**: どの操作でデプロイが実行されるか
- **環境固有の制約**: タグの命名規則、ブランチ保護など
- **確認方法**: GitHub Actionsでの進行状況確認

### 良い例
```markdown
## デプロイ

デプロイ前に必ずLintとテストが実行され、成功した場合のみデプロイされます。
デプロイの進行状況は[GitHub Actions](...)で確認できます。

### 開発環境
`main`ブランチへのマージで自動的にデプロイされます。

### 本番環境
[Releases](...)ページで新しいタグ（例: `v0.1.0`）でリリースを作成してください。
```

### 悪い例（冗長）
```markdown
## デプロイ

### 開発環境
1. featureブランチで開発
2. 変更をコミット・プッシュ
3. GitHubでプルリクエストを作成
4. mainブランチにマージ
5. 自動的に開発環境へデプロイされます

### ロールバック
1. タグを削除
2. 古いタグを再プッシュ
...
```

## GitHub環境設定

### 自動作成される環境
- `environment: Development` / `environment: Production`をワークフローに記述
- 初回実行時に自動的にGitHub環境が作成される
- 事前の手動作成は不要

### 保護ルールの設定（オプション）
- レビュー承認の必須化
- デプロイ前の待機時間
- 特定ブランチからのみデプロイ許可

## トラブルシューティング

### タグが既に存在する場合
```bash
# ローカルとリモートのタグを削除
git tag -d v1.0.0
git push --delete origin v1.0.0

# 新しいコミットに同じタグを作成
git tag v1.0.0 <commit-hash>
git push origin v1.0.0
```

### デプロイが実行されない
- ワークフローの`if`条件を確認
- タグの命名規則（`v`始まり）を確認
- GitHub Actionsのログで詳細を確認
