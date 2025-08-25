# GitHub Sub-Issues の gh CLI での操作方法

## 概要

GitHub Sub-Issues機能をgh CLIで操作する方法。GitHub公式のSub-Issues機能は比較的新しく、gh CLIには専用コマンドがないため、GitHub APIを直接叩く必要がある。

## Sub-Issue作成の手順

### 1. 親Issueの準備
通常のIssue作成と同じ方法で親Issueを作成：
```bash
PARENT_ISSUE_URL=$(gh issue create --title "親Issue Title" --body "..." --label "...")
# 例: https://github.com/owner/repo/issues/123
```

### 2. Sub-Issue作成
Sub-Issue自体は通常のIssue作成と同じ：
```bash
SUB_ISSUE_URL=$(gh issue create --title "【Sub-Issue】Sub-Issue Title" --body "Parent Issue: #123
..." --label "...")
```

### 3. Sub-Issueのリンク（重要）
GitHub REST APIを使用してSub-Issueを親Issueにリンク：
```bash
# Sub-Issue IDの取得（整数型が必要）
SUB_ISSUE_ID=$(gh api ${SUB_ISSUE_URL/github.com/api.github.com/repos} --jq '.id')

# 親IssueにSub-Issueをリンク
gh api --method POST repos/OWNER/REPO/issues/PARENT_ISSUE_NUMBER/sub_issues \
  --field sub_issue_id=${SUB_ISSUE_ID}
```

## 実用的なワンライナー

```bash
# Sub-Issue作成からリンクまでを一括実行
create_sub_issue() {
  local parent_issue=$1
  local title=$2
  local body=$3
  local label=${4:-""}
  
  # Sub-Issue作成
  local sub_issue_url=$(gh issue create --title "$title" --body "Parent Issue: #$parent_issue

$body" ${label:+--label "$label"})
  
  # ID取得とリンク
  local sub_issue_id=$(gh api ${sub_issue_url/github.com/api.github.com/repos} --jq '.id')
  gh api --method POST repos/$(gh repo view --json owner,name --jq '.owner.login + "/" + .name')/issues/$parent_issue/sub_issues \
    --field sub_issue_id=$sub_issue_id
  
  echo "Sub-Issue created and linked: $sub_issue_url"
}

# 使用例
create_sub_issue 123 "【Sub-Issue】API実装" "API設計と実装の詳細" "enhancement"
```

## APIエンドポイント詳細

### Sub-Issue追加
- **エンドポイント**: `POST /repos/{owner}/{repo}/issues/{issue_number}/sub_issues`
- **パラメータ**: `sub_issue_id` (整数型のIssue ID)
- **レスポンス**: 親Issueの情報（`sub_issues_summary`フィールド付き）

### Sub-Issue一覧取得
```bash
# 親IssueのSub-Issues情報を確認
gh api repos/OWNER/REPO/issues/PARENT_ISSUE_NUMBER --jq '.sub_issues_summary'
# 出力例: {"total":4,"completed":1,"percent_completed":25}
```

### Sub-Issue削除
```bash
# Sub-Issueのリンクを削除（Issueは削除されない）
gh api --method DELETE repos/OWNER/REPO/issues/PARENT_ISSUE_NUMBER/sub_issues \
  --field sub_issue_id=SUB_ISSUE_ID
```

## 注意事項

1. **Issue ID形式**: `node_id`ではなく、整数型の`id`を使用
2. **gh CLI制限**: 専用コマンドは存在しないため、APIを直接使用
3. **権限**: リポジトリへの書き込み権限が必要
4. **GitHub版本**: GitHub.com および GitHub Enterprise Server で利用可能

## 実装例（新機能開発プロジェクト）

```bash
# 親Issue作成
PARENT=$(gh issue create --title "新機能XYZの開発" --body "..." --label "enhancement")

# 4つのSub-Issue作成とリンク
for i in {1..4}; do
  case $i in
    1) TITLE="【Sub-Issue】API設計と実装" ;;
    2) TITLE="【Sub-Issue】フロントエンド実装" ;;
    3) TITLE="【Sub-Issue】テスト作成と実行" ;;
    4) TITLE="【Sub-Issue】ドキュメント更新" ;;
  esac
  
  SUB_URL=$(gh issue create --title "$TITLE" --body "Parent Issue: #${PARENT##*/}..." --label "enhancement")
  SUB_ID=$(gh api ${SUB_URL/github.com/api.github.com/repos} --jq '.id')
  gh api --method POST repos/$(gh repo view --json owner,name --jq '.owner.login + "/" + .name')/issues/${PARENT##*/}/sub_issues --field sub_issue_id=$SUB_ID
done
```

## 参考リンク

- [GitHub REST API - Sub-Issues](https://docs.github.com/en/rest/issues/sub-issues?apiVersion=2022-11-28)
- [GitHub Issues - Using sub-issues](https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/adding-sub-issues)
