# GitHub Sub-Issues 操作

## 概要
GitHub Sub-Issues機能をgh CLIで操作する方法。専用コマンドが存在しないためAPIを直接使用。

## 基本操作

### Sub-Issue作成とリンク
```bash
# 1. 親Issue作成
PARENT_ISSUE_URL=$(gh issue create --title "親Issue" --body "...")

# 2. Sub-Issue作成
SUB_ISSUE_URL=$(gh issue create --title "【Sub】タイトル" --body "Parent: #123...")

# 3. APIでリンク
SUB_ISSUE_ID=$(gh api ${SUB_ISSUE_URL/github.com/api.github.com/repos} --jq '.id')
gh api --method POST repos/OWNER/REPO/issues/PARENT_NUMBER/sub_issues \
  --field sub_issue_id=${SUB_ISSUE_ID}
```

### ワンライナー関数
```bash
create_sub_issue() {
  local parent=$1 title=$2 body=$3
  local sub_url=$(gh issue create --title "$title" --body "Parent: #$parent\n\n$body")
  local sub_id=$(gh api ${sub_url/github.com/api.github.com/repos} --jq '.id')
  gh api --method POST repos/$(gh repo view --json owner,name --jq '.owner.login + "/" + .name')/issues/$parent/sub_issues \
    --field sub_issue_id=$sub_id
  echo "Created: $sub_url"
}
```

## 注意事項
- 整数型の`id`を使用（`node_id`ではない）
- リポジトリ書き込み権限が必要
- [GitHub REST API Docs](https://docs.github.com/en/rest/issues/sub-issues)
