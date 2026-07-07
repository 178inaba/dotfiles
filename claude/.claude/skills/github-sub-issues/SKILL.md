---
name: github-sub-issues
description: GitHub Sub-Issues（Issueの親子関係）を作成・リンクする手順。gh CLIに専用コマンドがないためAPIを直接使用する
---

# /github-sub-issues

GitHub Sub-Issues機能をgh CLIで操作する方法。専用コマンドが存在しないためAPIを直接使用。

## 基本操作

### Sub-Issue作成とリンク
```bash
# 0. 対象リポジトリを明示（書き込み系は -R 必須）
REPO=$(gh repo view --json nameWithOwner -q .nameWithOwner)

# 1. 親Issue作成（本文は事前に scratchpad へ Write してから --body-file で渡す）
PARENT_ISSUE_URL=$(gh issue create -R "$REPO" --title "親Issue" --body-file parent-body.md --assignee @me)

# 2. Sub-Issue作成
SUB_ISSUE_URL=$(gh issue create -R "$REPO" --title "【Sub】タイトル" --body-file sub-body.md --assignee @me)

# 3. APIでリンク
SUB_ISSUE_ID=$(gh api ${SUB_ISSUE_URL/github.com/api.github.com/repos} --jq '.id')
gh api --method POST repos/${REPO}/issues/PARENT_NUMBER/sub_issues \
  --field sub_issue_id=${SUB_ISSUE_ID}
```

### ワンライナー関数
```bash
create_sub_issue() {
  local parent=$1 title=$2 body_file=$3
  local repo=$(gh repo view --json nameWithOwner -q .nameWithOwner)
  local sub_url=$(gh issue create -R "$repo" --title "$title" --body-file "$body_file" --assignee @me)
  local sub_id=$(gh api ${sub_url/github.com/api.github.com/repos} --jq '.id')
  gh api --method POST repos/${repo}/issues/$parent/sub_issues \
    --field sub_issue_id=$sub_id
  echo "Created: $sub_url"
}
```

## 注意事項
- 整数型の`id`を使用（`node_id`ではない）
- リポジトリ書き込み権限が必要
- `gh issue create` 等の書き込み系は `-R owner/repo` でリポジトリを明示し、複数行本文は `--body-file` で渡す（グローバル方針・PreToolUseフックで強制。`--body` に `\n` を含む文字列を直渡しすると、bash はリテラルの `\n` として本文に焼き込む）
- [GitHub REST API Docs](https://docs.github.com/en/rest/issues/sub-issues)
