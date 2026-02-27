---
name: code-review
description: コード差分を詳細にレビュー
argument-hint: [base-branch] [--issue NUMBER] [--uncommitted] [--comment] [--no-comment]
disable-model-invocation: false
---

# /code-review

コード差分を経験豊富なシニアエンジニアとして、本番運用を考慮しながら詳細にレビューし、Approve可能か判断

## 使用方法
```
/code-review [base-branch] [--issue ISSUE_NUMBER] [--uncommitted] [--comment] [--no-comment]
```

**引数**:
- `base-branch`: 比較対象のブランチ（省略時: PRがあればPRのベースブランチ、なければデフォルトブランチを自動判定）
- `--issue ISSUE_NUMBER`: 指定したIssue要件を満たしているか確認
- `--uncommitted`: 未コミットの差分をレビュー（`git diff`を使用）
- `--comment`: 強制的にPRにレビューコメントを投稿
- `--no-comment`: 強制的にローカル出力のみ（PRコメント投稿しない）

**例**:
```
/code-review                              # PRがあればPRのベースブランチ、なければデフォルトブランチとの差分をレビュー
/code-review main                         # mainブランチとの差分をレビュー
/code-review origin/feature-auth          # feature-authブランチとの差分をレビュー
/code-review main --issue 123             # Issue #123の要件も確認
/code-review --uncommitted                # 未コミットの差分をレビュー
/code-review --issue 456 --uncommitted    # 未コミット差分をIssue #456の観点で確認
/code-review --comment                    # 強制的にPRにコメント投稿
/code-review --no-comment                 # 他人のPRでもローカル出力のみ
```

## 実行内容

### 1. 引数の解析
- 第1引数（`--`で始まらない）があればベースブランチ名として使用
- 第1引数がない、または`--`で始まる場合はベースブランチを自動判定:
  1. `gh pr view --json baseRefName --jq '.baseRefName'` でPRのベースブランチを取得し、`origin/` を付加
  2. PRがなければ `git symbolic-ref refs/remotes/origin/HEAD` でデフォルトブランチを取得
- `--issue ISSUE_NUMBER`: Issue番号を抽出
- `--uncommitted`: 未コミット差分フラグを設定
- `--comment`: コメント投稿フラグを設定
- `--no-comment`: コメント非投稿フラグを設定

**解析例**:
```
/code-review feature/auth --issue 123
→ base-branch: "feature/auth", issue: 123, uncommitted: false

/code-review --issue 456 --uncommitted
→ base-branch: (自動判定: PRがあれば "origin/<PRのベースブランチ>"), issue: 456, uncommitted: true

/code-review origin/main
→ base-branch: "origin/main", issue: null, uncommitted: false

/code-review --comment
→ base-branch: (自動判定), comment: true

/code-review --no-comment
→ base-branch: (自動判定), comment: false
```

### 2. PRコンテキスト確認（自動）
- セクション1でPRからベースブランチを取得できた場合、同時にPR情報も取得:
  1. PRの説明・目的を確認
  2. 関連Issueがあれば要件を確認（`--issue`未指定時）
  3. レビュー後、説明と実際の変更の整合性を評価
- PRが存在しない場合はスキップ

### 3. コメントモード・個人ルールモード判定

#### コメントモード
PRにレビューコメントを投稿するかを判定:
1. `--comment` 指定時 → コメントモードON
2. `--no-comment` 指定時 → コメントモードOFF
3. 両方未指定時 → 自動判定:
   - PRが存在しない場合 → コメントモードOFF
   - `--uncommitted` 指定時 → コメントモードOFF
   - PR作成者と現在のユーザーを比較:
     - `gh api user --jq '.login'` で現在のユーザー取得
     - `gh pr view --json author --jq '.author.login'` でPR作成者取得
     - 異なる（他人のPR）→ コメントモードON
     - 同じ（自分のPR）→ コメントモードOFF

#### 個人ルールモード
個人グローバル `~/.claude/CLAUDE.md` とそのリンク先ドキュメントも準拠チェック対象に含めるかを判定:
- `--uncommitted` 指定時 → 個人ルールモードON（自分のコード）
- PR作成者が自分 → 個人ルールモードON
- PR作成者が他人 → 個人ルールモードOFF
- PRが存在しない場合 → 個人ルールモードON（自分のコード）

### 4. 差分取得と確認
- `--uncommitted`が指定されている場合:
  - `git diff` で既存ファイルの未コミット差分を取得
  - `git ls-files --others --exclude-standard` でトラックされていない新規ファイルを確認
  - トラックされていないファイルがあれば、その内容も読み込んでレビュー対象に含める
  - トラックされていないファイルがある場合は、レビュー結果で明示的に言及する
- 指定されていない場合:
  - **必ず解析済みのベースブランチを使用** (`git diff <base-branch>...HEAD`)
  - 差分が大きい場合は `git diff <base-branch>...HEAD --name-only` で変更ファイル一覧を先に確認
  - 各ファイルを `git diff <base-branch>...HEAD -- <ファイル名>` で個別に確認
  - 全体を把握してからレビュー評価を実施

**重要原則**:
- 全差分確認: 必ず全ての変更内容を確認してから評価する
- 段階的確認: ファイル一覧 → 個別差分 → 全体評価の順で進める
- 部分的判断禁止: 一部の差分や最初の部分だけを見て結論を出さない
- 見落とし防止: 特に新規追加ファイルや定数・インターフェース変更は必ず確認する
- 指摘の出し切り: 発見した指摘はすべて一度のレビューで出し切る。小出しにして修正→再レビュー→新たな指摘のループを避ける

### 5. Issue情報取得（`--issue`指定時）
- `gh issue view <ISSUE_NUMBER>` でIssue内容を取得
- 要件・仕様を確認

### 6. レビュー実行
以下の観点から詳細にレビュー:

#### プロジェクトルール観点
- [ ] リポジトリのCLAUDE.mdに記載されたルール・方針に準拠しているか
- [ ] CLAUDE.mdからリンクされているドキュメント（設計パターン、ワークフロー等）のルールに準拠しているか
  - リンク先ドキュメントのうち、変更内容に関連するものを確認する

#### 個人ルール観点（個人ルールモードON時のみ）
- [ ] 個人グローバル `~/.claude/CLAUDE.md` に記載されたルール・方針に準拠しているか
- [ ] `~/.claude/CLAUDE.md` からリンクされているドキュメント（`~/.claude/context/` 配下）のルールに準拠しているか
  - リンク先ドキュメントのうち、変更内容に関連するものを確認する
  - 例: Go言語の変更 → Go Clean Archエラー、Goパフォーマンス等のパターンを確認

#### 基本観点
- [ ] コードの品質（可読性、保守性）
- [ ] バグや潜在的な問題の有無
- [ ] セキュリティリスク（OWASP Top 10等）
- [ ] パフォーマンスへの影響
- [ ] テストコードの妥当性
  - 無駄なテストがないか（過剰なモック、重複テスト等）
  - 必要なテストの抜け漏れがないか（正常系・異常系・エッジケース）
  - テストの可読性（テスト名、構造、意図の明確さ）
- [ ] エラーハンドリングの適切性
- [ ] 命名規則やコーディングスタイルの一貫性

#### 本番運用観点
- [ ] 本番環境でのログ・監視は十分か
- [ ] ロールバック可能な実装か
- [ ] データベースマイグレーションの安全性
- [ ] エッジケースや境界値の考慮
- [ ] 障害時の影響範囲と復旧方法
- [ ] スケーラビリティへの影響

#### Issue指定時の追加観点
- [ ] Issue要件を満たしているか
- [ ] 仕様から逸脱していないか
- [ ] 関連する要件がすべて実装されているか

#### コメントモードON時の追加要件
各指摘事項について、該当するファイルパスと行番号を特定する。
- 差分内の具体的な行に紐づく指摘 → ファイルパスと行番号を記録（PRコメント投稿時に使用）
- 行番号が特定できない指摘（設計レベルの問題、テスト不足等）→ レビュー本文に含める

### 7. レビュー結果出力
以下の形式で出力:

```markdown
## レビュー結果

### 総合評価
- Approve可能 / 修正が必要 / 要議論

### 良い点
- ...

### 指摘事項
#### 必須修正
- ...

#### 推奨修正
- ...

#### 質問・確認事項
- ...

### Issue要件の充足状況（--issue指定時のみ）
- ...
```

### コメントモードON時: PRにレビュー投稿

ローカル出力に加えて、PRにレビューコメントを投稿する。

#### 1. PR情報を取得
```bash
gh pr view --json number,headRefOid,url
gh repo view --json owner,name --jq '.owner.login + "/" + .name'
```

#### 2. レビューイベントを決定

| 総合評価 | event |
|---|---|
| Approve可能 | `APPROVE` |
| 修正が必要 | `REQUEST_CHANGES` |
| 要議論 | `COMMENT` |

#### 3. REST APIでレビューを投稿
```bash
gh api repos/{owner}/{repo}/pulls/{pr_number}/reviews \
  --method POST \
  --input - <<'EOF'
{
  "commit_id": "<HEAD commit SHA>",
  "event": "APPROVE" or "REQUEST_CHANGES" or "COMMENT",
  "body": "## レビュー結果\n\n### 総合評価\n...\n\n### 良い点\n...\n\n### 行に紐づかない指摘\n...",
  "comments": [
    {
      "path": "src/main.go",
      "line": 30,
      "body": "指摘内容"
    }
  ]
}
EOF
```

**パラメータ説明**:
- `commit_id`: PRのHEADコミットSHA（`headRefOid`）
- `event`: レビューイベント（上記の表に従う）
- `body`: レビュー全体のサマリー（総合評価、良い点、行に紐づかない指摘）
- `comments[]`: 行単位の指摘コメント
  - `path`: リポジトリルートからの相対パス
  - `line`: 差分内の行番号（新ファイル側の行番号）
  - `body`: 指摘内容

#### 4. 投稿完了後、PRのレビューURLを表示

## 注意事項
- ベースブランチが存在しない場合はエラーを通知
- Issue番号が不正な場合はエラーを通知
- ベースブランチの自動判定優先順位: PRのベースブランチ → デフォルトブランチ → `origin/main`（フォールバック）
- `--comment` 指定時にPRが存在しない場合はエラーを通知
- `--uncommitted` と `--comment` の併用時: PRが存在すればコメント投稿可能だが、未コミット差分はPR上の差分と異なる可能性があるため警告を表示
- `comments` 配列が空の場合（行に紐づく指摘がない場合）でも `body` のみでレビュー投稿は可能
