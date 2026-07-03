---
name: review-response
description: GitHubのレビューコメントを確認して適切に対応
argument-hint: [<pr-number>] [--dry-run] [--worktree]
disable-model-invocation: true
---

# /review-response

GitHubのレビューコメントを確認して適切に対応

## 使用方法
```
/review-response                          # 全自動：カレント branch の PR を対象に修正＋コメント返信＋解決
/review-response --dry-run                # 確認のみ：指摘点・修正案・コメント案を報告
/review-response 123                      # PR 123 を対象（カレント branch 以外を明示指定）
/review-response 123 --worktree           # PR 123 の worktree で対応（並列開発時の主要ユースケース）
/review-response 123 --worktree --dry-run # 上記 + 確認のみ
```

## 引数
- `<pr-number>`: 対象 PR 番号（省略時はカレント branch の PR を `gh pr view` で推論）
- `--worktree`: 対象 PR の worktree に切替（既存があれば再利用、無ければ作成）。並列で別 issue を作業中にレビュー対応する際に推奨
- `--dry-run`: 修正案・コメント返信案を確認のみ（実行しない）

## 実行内容

### Worktree 解決（`--worktree` 指定時のみ、最初に実行）

@~/.claude/skills/worktree-resolution/SKILL.md の「PR worktree 解決手順」に従い、対象 PR の worktree に session を切り替える。

### 通常モード（`--dry-run` なし）
1. GitHub GraphQL APIで以下を**両方**取得（片方だけでは不十分）
   - **レビュー本文** (`reviews.nodes[].body`): 総評・優先度付き指摘リスト・サマリー
   - **行コメント** (`reviewThreads`): 未解決スレッド
2. 本文と行コメントを突き合わせ、重複を除いた上で指摘を列挙
3. 各指摘について修正すべきか判断
4. 修正すべき指摘は実装で対応（コミット・プッシュまで）
5. 修正不要な指摘には理由を説明して返信
6. 対応完了後にコメントを解決済みに変更

### dry-runモード（`--dry-run`）
1. GitHub GraphQL APIで**レビュー本文と行コメントの両方**を取得（通常モードと同じ）
2. 本文と行コメントを突き合わせ、重複を除いた上で指摘を列挙
3. 各指摘について修正すべきか判断
4. 指摘ごとに以下を報告（出所が本文か行コメントかを明示）：
   - 修正すべきか否かの判断と理由
   - 修正案（コード変更の具体的内容）
   - コメント返信案
5. ユーザーの承認後、修正を実行（コミット・プッシュまで）
   - コメント返信は行わない（案を再提示し、ユーザーが自分で投稿）

## 判断基準

### 原則
- **技術的事実で判断する**: 修正要否は好みではなく技術的事実・データに基づいて判断する
- **レビュアーのラベルを尊重する**: `issue(blocking):` / `suggestion(non-blocking):` / `nit:` 等の Conventional Comments ラベルや優先度表記が付いている場合、スキル独自の分類より優先する。blocking は原則修正、non-blocking / nit は以下の基準で判断

### 修正すべき指摘
- **バグリスク**: 実行時エラーの可能性、ロジック誤り
- **セキュリティ**: 脆弱な実装パターン
- **型安全性**: any型の使用、型定義の不備
- **パフォーマンス**: 明らかな性能問題
- **保守性**: コードの重複、複雑度問題
- **可読性（理解困難の指摘）**: レビュアーが「分かりにくい」「なぜこうなっているのか」と述べた場合、スレッドの説明返信で済ませず**コード自体を明確化**する（リネーム・分割等）。難しければ理由をコードコメントとして残す。レビューツール上の説明は将来のコード読者に届かないため
- **安価な nitpick**: typo・リネーム等、修正コストがほぼゼロで内容に同意できるもの（再レビューのラウンドトリップ削減を優先）

### 修正不要と判断して返信する場合
- **既存パターン踏襲**: プロジェクト標準・既存規約に準拠している（返信に参照先を明示する）
- **好みの問題**: 技術的根拠のないスタイル選好で、修正コストに見合わないもの
- **スコープ外の正当な指摘**: 本PRでは対応しない場合、握りつぶさず **Issue 化して返信にリンク**する（「後で直す」だけの返信は実行されずに終わるため）

### ユーザーに最終判断を委ねる場合（自動で確定しない）
- **設計思想に関わる指摘**: 意図的な設計判断への異論は、根拠を添えた返信で議論を開くまでを行い、スレッドは解決しない。修正するか押し返すかはユーザーに確認する（合意形成を飛ばして一方的に「修正不要」と確定させない）
- **人間レビュアーの blocking 指摘に反対する場合**: 同上。反論の根拠を整理して提示し、ユーザーの判断を仰ぐ

### レビュー本文の扱い
レビュー本文（`reviews.nodes[].body`）は総評だけでなく、**行コメントに存在しない独立した指摘**が含まれることが多い（優先度付きリスト、「テストが不足」等のサマリー指摘）。以下の方針で扱う：

- **本文の指摘も個別対応対象**: 行コメントと同じ粒度で修正・返信判断を行う
- **重複チェック**: 本文の指摘が行コメントと同じ内容を指していないか照合し、重複は1件として扱う
- **優先度付きリスト**: 本文に「優先度1/2/3」「Must/Should/Nice」等の構造がある場合、各項目を独立した指摘として列挙
- **返信先**: 本文由来の指摘への返信・解決は該当する行コメントが無いため、PR全体へのコメント（`gh pr comment`）で対応完了を報告

## GraphQL API 実装

### 未解決コメント取得
**重要**: レビュー本文（`reviews.nodes[].body`）と未解決スレッド（`reviewThreads`）を**両方**出力する。行コメントだけ取得すると本文の指摘（優先度付きリスト等）を見落とす。

```bash
# GraphQL APIで取得後、jqでレビュー本文と未解決スレッドを両方抽出
gh api graphql --field query='
{
  repository(owner: "OWNER", name: "REPO") {
    pullRequest(number: PR_NUMBER) {
      reviews(first: 50) {
        nodes { author { login } state body submittedAt }
      }
      reviewThreads(first: 50) {
        nodes {
          id, isResolved
          comments(first: 10) {
            nodes { body, path, line, author { login } }
          }
        }
      }
    }
  }
}' --jq '{
  reviews: [.data.repository.pullRequest.reviews.nodes[] | select(.body != null and .body != "")],
  threads: [.data.repository.pullRequest.reviewThreads.nodes[] | select(.isResolved == false)]
}'
```

出力の `reviews[]` に含まれる `body` を必ず読み、本文内の指摘（総評・優先度付きリスト・サマリー）を行コメントと同じ粒度で列挙する。

### スレッドに返信
```bash
gh api graphql --field query='
mutation {
  addPullRequestReviewThreadReply(input: {
    pullRequestReviewThreadId: "THREAD_ID"
    body: "返信内容"
  }) {
    comment { id }
  }
}'
```

### スレッド解決
```bash
gh api graphql --field query='
mutation {
  resolveReviewThread(input: {
    threadId: "THREAD_ID"
  }) {
    thread { isResolved }
  }
}'
```

### 一括返信・解決（推奨）
複数の指摘に同時対応する場合、1リクエストでまとめて実行：
```bash
gh api graphql -f query='
mutation {
  r1: addPullRequestReviewThreadReply(input: {pullRequestReviewThreadId: "PRRT_1", body: "修正しました"}) { comment { id } }
  s1: resolveReviewThread(input: {threadId: "PRRT_1"}) { thread { isResolved } }
  r2: addPullRequestReviewThreadReply(input: {pullRequestReviewThreadId: "PRRT_2", body: "修正しました"}) { comment { id } }
  s2: resolveReviewThread(input: {threadId: "PRRT_2"}) { thread { isResolved } }
}'
```
- エイリアス（r1, s1, r2, s2...）で複数のmutationを1リクエストに結合
- 返信と解決を交互に記述することで、各スレッドの処理が完結

## 重要な実装ポイント

### レビュアー別の解決ポリシー
- **Copilot** (`copilot-pull-request-reviewer`): 自動解決
- **人間レビュアー**: 未解決のまま（レビュアーの再確認待ち）

### 修正完了報告フォーマット
```
<修正内容>を対応しました。
https://github.com/<owner>/<repo>/pull/<PR番号>/commits/<コミットハッシュ>
```

### 品質確認
修正後は必ずプロジェクト標準の品質チェックを実行：
- Node.js: `npm run lint`, `npm run typecheck`
- Go: `go vet`, `golangci-lint run`
- Python: `ruff check`, `mypy`

## 注意事項
- **レビュー本文を見落とさない**: 行コメント（`reviewThreads`）だけを対象にすると、本文に書かれた優先度付きリスト・総評・サマリー指摘を取りこぼす。必ず `reviews.nodes[].body` も読み、本文と行コメントの両方を突き合わせてから対応判断する
- **レビュースレッド返信時**: `pullRequestReviewThreadId`のみ使用（`pullRequestReviewId`は不要）
- **GraphQLレート制限**: 1時間あたり5000ポイント
- **既存PRの自動更新**: プッシュでPRは自動更新される
- **`--worktree` 指定時の挙動**: @~/.claude/skills/worktree-resolution/SKILL.md の注意事項を参照
