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

1. **PR 番号確定**
   - `<pr-number>` が指定されていればそれを使用
   - 無ければ `gh pr view --json number -q .number` でカレント branch の PR を推論
     - 推論失敗時（PR が無い・複数該当など）はエラーメッセージを出して停止し、ユーザーに `<pr-number>` の明示指定を促す（誤った PR に対応しないための安全策）

2. **PR の head branch 名取得**
   - `gh pr view <PR> --json headRefName -q .headRefName`

3. **worktree 名の計算**
   - branch 名から `/` を `-` に置換（`/issue-handle --worktree` と同一規約）
   - 例: `feature/99-add-oauth` → `feature-99-add-oauth`

4. **既存 worktree の検索**
   - `git worktree list --porcelain` を解析
   - `branch refs/heads/<pr-branch>` が登録されている worktree を探す
   - 見つかればそのパスを記録

5-A. **既存 worktree あり**:
   - `EnterWorktree(path: <found-path>)` で session を切替

5-B. **既存 worktree なし**（auto cleanup 後・別 PC 等）:
   - **メインリポジトリの退避**（current branch == PR head branch の時のみ実行）:
     - 理由: 後続の `git switch <pr-branch>` を worktree 内で実行する際、メインリポジトリが同 branch を checkout していると git が二重 checkout を拒否するため、先にメインリポジトリを別 branch へ退避させる
     - dirty 検出: `git status --porcelain | grep -v '^??' | head -n1` で modified/staged 変更を確認。非空なら **abort**（ユーザーに明示的なコミット/stash を促す。untracked のみは無視）
     - clean → デフォルト branch を取得して switch:
       - 取得: `git symbolic-ref refs/remotes/origin/HEAD --short 2>/dev/null | sed 's|^origin/||'`（失敗時は `gh repo view --json defaultBranchRef -q .defaultBranchRef.name` をフォールバック）
       - `git switch <default-branch>` でメインリポジトリを退避
       - ユーザーに 1 行で通知: 「メインリポジトリを <default-branch> に退避しました（worktree 作成のため）」
   - `git fetch origin <pr-branch>` で remote tracking ref を更新
   - `EnterWorktree(name: <worktree-name>)` で新規 worktree 作成
     - 結果: branch `worktree-<worktree-name>` 上の worktree、`WorktreeCreate` hook 発火
   - worktree 内で `git switch <pr-branch>` で PR の実 branch に切替
     - local に `<pr-branch>` が無い場合は git の DWIM 挙動で `origin/<pr-branch>` から自動作成（modern git 2.23+）
     - local に同名の古い `<pr-branch>` が残っている場合（前回作業の残骸等）はそちらに切り替わり、`origin/<pr-branch>` と乖離するリスクがある。`git rev-list --left-right --count <pr-branch>...origin/<pr-branch>` 等で同期状況を確認し、ローカル側に独自 commit が無ければ `git reset --hard origin/<pr-branch>` でリモートに揃える。独自 commit がある場合は警告して停止し、ユーザー判断を仰ぐ
   - `git branch -d worktree-<worktree-name>` で temp branch を削除
   - 補足: この 2 段階方式により hook 発火を確保しつつ、目的の PR branch に到達できる

6. **作業ディレクトリ確認**: worktree 内にいることを `git rev-parse --show-toplevel` で確認した上で、以下のレビュー対応ロジックに進む

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

### 修正すべき指摘
- **型安全性**: any型の使用、型定義の不備
- **セキュリティ**: 脆弱な実装パターン
- **パフォーマンス**: 明らかな性能問題
- **保守性**: コードの重複、複雑度問題
- **バグリスク**: 実行時エラーの可能性

### 修正不要と判断する場合
- **設計思想の違い**: 意図的な設計判断
- **既存パターン踏襲**: プロジェクト標準に準拠
- **nitpick レベル**: 好みの問題

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
- **`--worktree` 指定時の挙動**:
  - 並列で別の issue 作業中に呼び出すと、session が PR の worktree に切り替わる。元の作業に戻るには別途 `EnterWorktree(path: <元のworktree>)` を呼ぶ
  - 別ターミナル/別 tmux ペインで `/review-response` を実行する運用なら、元 session は触らずに済む（推奨）
  - worktree を新規作成する場合、PR の head branch を fetch して checkout するため、PR ブランチ側に未 push のローカル commit があれば事前に push しておくこと
  - **メインリポジトリが PR head branch を checkout 中の場合**: worktree 作成時に自動でメインリポジトリを default branch へ退避する（git の二重 checkout 禁止を回避）。dirty tree の場合は abort されるため、事前にコミット/stash しておくこと
  - 前提: `worktree.baseRef: "head"` 設定（`~/.claude/settings.json`、dotfiles では設定済み）
