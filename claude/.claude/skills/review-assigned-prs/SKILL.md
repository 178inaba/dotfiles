---
name: review-assigned-prs
description: 自分にレビュー依頼が来ているPRのうち、他人だけがレビュー済みで自分が未レビューのものを除いて /deep-review で並列レビュー
disable-model-invocation: true
---

# /review-assigned-prs

担当者が自分に明示的にレビューを求めている open PR — 初回依頼、あるいは自分の過去レビュー後の再レビュー依頼 — を検出し、各 PR に対してサブエージェント経由で `/deep-review <PR番号> --worktree --no-autofix` を並列実行する。`/loop 5m /review-assigned-prs` で常駐運用することを想定。

「他人が先にレビュー済みで自分だけ未レビュー」の PR は除外する（他人のレビューに機械的に上乗せしないため）。Draft PR も対象外（作者が Ready for review にした時点で候補に入る）。

## 対象判定（実装ノート）

`user-review-requested:@me` に該当する open PR を以下で振り分ける（判定はスクリプトで完結）。GitHub は「requested_reviewers に現在いる」PR だけを返し、レビュー投稿で自動的に外れ、再指名で戻る挙動を利用している:

| 自分の過去レビュー | 他人（Bot以外）のレビュー | 判定 |
|---|---|---|
| なし | なし | ✓ 対象（初回依頼） |
| なし | あり | ✗ 除外（他人が先に対応） |
| あり | なし | ✓ 対象（再レビュー依頼） |
| あり | あり | ✓ 対象（再レビュー依頼） |

Bot レビュー（Copilot・github-actions・CodeRabbit 等）は「人間未レビュー」扱いで対象判定に影響しない。PR 作成者自身のレビューコメントも「他人」に含めない（Reply スレッドが GitHub 内部で COMMENTED state の review として記録されるため、作者を除外しないと「Copilot 指摘に作者が返信しただけ」の PR が誤除外される）。自分の過去レビューは状態を問わずカウントする（DISMISSED / PENDING も再レビュー相当）。`/loop` で繰り返し実行しても、再レビュー依頼が来ない限り同じ PR は次回イテレーションで自動的に対象外になる（重複防止ロジック不要）。

## 使用方法
```
/loop 5m /review-assigned-prs    # 5 分間隔で常駐（推奨）
/review-assigned-prs             # 単発実行
```

## 前提条件
- `gh` CLI がインストール・認証済み
- `jq`, `git` がインストール済み
- レビュー対象のリポジトリに対する clone アクセス権

## 実行内容

### 1. 候補 PR 一覧の取得

```bash
bash ~/.claude/skills/review-assigned-prs/scripts/list-pending-reviews.sh
```

候補判定（誰か人間がレビュー済みかどうか）はスクリプトで完結する。判定ロジックの詳細はスクリプト本体を、挙動の担保は `claude/.claude/tests/test-list-pending-reviews.sh` を参照。

#### 出力 JSON の契約

```json
{
  "prs": [
    {"owner": "acme", "repo": "foo", "number": 123, "url": "https://github.com/acme/foo/pull/123"}
  ],
  "degraded": false,
  "warnings": []
}
```

- **`prs`**: レビュー候補の一覧
- **`degraded: true`**: 一部の PR の `gh api reviews` が失敗した（`warnings` 参照）。失敗した PR は候補から除外されるので、残りの PR は通常通り処理してよい
- **`warnings`**: 個別失敗の内容。空でなければ完了報告に併記する

スクリプトが非ゼロ終了した場合（`jq` 未導入・`gh search prs` 失敗など）は stderr のメッセージを提示して停止する。

### 2. 対象 0 件なら終了

`.prs` 配列が空なら「レビュー対象なし」と 1 行報告して終了。

### 3. 各 PR ごとにサブエージェントを並列起動

`.prs` の各要素について、Agent ツール (`subagent_type: "claude"`) を**1 メッセージ内で並列起動**する（対象 PR 数ぶんの Agent 呼び出しを 1 つの tool_use ブロックにまとめる）。

`fork` は使わない。`--worktree` による cwd 切替がメインセッションに漏れないよう、独立コンテキストが必須。

サブエージェントへのプロンプトに以下を含める:

- このセッションが独立レビュー専用であり、親セッションのコンテキストを持たない旨
- レビュー用 clone dir の取得:
  ```bash
  bash ~/.claude/skills/review-assigned-prs/scripts/ensure-clone.sh <owner>/<repo>
  ```
  → JSON `{"path": "..."}` を返す。clone 先パスの規約はスクリプトヘッダーコメントを参照
- `cd <path>` してから実行: `/deep-review <PR番号> --worktree --no-autofix`
  - 各フラグの意味は `@~/.claude/skills/deep-review/SKILL.md` を参照
  - 対象は他人の PR のためコメントモードが ON になり、**レビューを PR に投稿するところまでが必須成果物である**旨を明示する（投稿スキップは失敗扱い）
- レビュー結果と投稿したレビューの URL をそのまま返すよう指示（追加の解釈・要約は不要）
- 補助コンテキスト: PR URL・PR 番号・repo 名

#### `ensure-clone.sh` の出力 JSON 契約

```json
{"path": "/absolute/path/to/clone/dir"}
```

- **`path`**: レビュー用 clone dir の絶対パス（clone 先の規約はスクリプトヘッダー参照）
- 引数不正・clone/fetch 失敗時は非ゼロ exit + 英語 stderr

### 4. 投稿検証と完了報告

サブエージェントの自己申告だけで「投稿済み」とせず、成功報告のあった各 PR についてレビューが実際に投稿されたかを検証する（未投稿を見逃すと次イテレーションでも候補に残り続け、同じ PR が繰り返しレビューされるため）。成功報告のあった PR を1回の呼び出しにまとめて検証する（成功報告が0件なら検証をスキップして完了報告に進む）:

```bash
bash ~/.claude/skills/review-assigned-prs/scripts/verify-posted-reviews.sh <owner>/<repo>#<number> [...]
```

#### `verify-posted-reviews.sh` の出力 JSON 契約

```json
{
  "results": [
    {"owner": "acme", "repo": "foo", "number": 123, "posted": true}
  ],
  "degraded": false,
  "warnings": []
}
```

- **`posted`**: `true` なら投稿済み、`false` なら未投稿（サブエージェントが投稿をスキップした事故）
- **`degraded: true`**: 一部 PR の検証に失敗した（`warnings` 参照）。失敗した PR は `results` に含まれないため「検証失敗」として報告する
- スクリプトが非ゼロ終了した場合（引数不正・login 取得失敗など）は stderr のメッセージを提示して停止する

検証結果を集約し、1〜2 行のサマリを表示:

```
レビュー完了 (N 件):
- acme/foo#123: レビュー投稿済み
- acme/bar#456: サブエージェント失敗（<エラー概要>）
- acme/baz#789: レビュー未投稿（サブエージェントは完了報告 — レビュー結果は返却済みのため要確認）
```

`degraded: true` があれば `warnings` を末尾に併記。

## 注意事項

1. **サブエージェント失敗時は自動リトライしない**: Agent が null/error を返した場合はユーザーに提示（`@~/.claude/skills/issue-handle/SKILL.md` の「7-2. 親セッションで自動修正」に準拠）。次回イテレーションで再挑戦される
2. **fork PR は現状スコープ外**: `gh search prs` は fork 由来 PR も返すが、`/deep-review --worktree` の worktree 解決は「fork 由来 PR は対象外」として停止する。当該 PR は毎イテレーションでサブエージェント失敗として報告される（次イテレーションで自動復旧はしない）
3. **同一リポジトリの並列レビュー**: 同じ owner/repo に属する複数 PR が同時に候補になった場合、初回イテレーションでは `ensure-clone.sh` の並列実行で片方が「destination path already exists」または fetch ロック競合で失敗する可能性がある。次回イテレーションで clone が完了していれば fetch 経路に入り両方成功する
4. **clone dir のクリーンアップは手動**: 累積して困る場合はスクリプトヘッダーに記載された clone 先ディレクトリを手動で削除。自動掃除ロジックは持たない（YAGNI）
5. **プライベートリポジトリ**: `gh` 認証済みで clone アクセス権があれば `gh repo clone` が SSH/HTTPS を自動選択して clone する
