---
name: cleanup-merged
description: マージ済みのworktreeとlocal branchをまとめてクリーンアップ。ユーザーがマージ済みブランチ・worktreeの掃除・片付けを求めたときに使用（既定で一覧提示→承認後に削除）
argument-hint: [--yes] [--dry-run]
---

# /cleanup-merged

マージ済みの worktree と local branch を検出して削除する。`/issue-handle --worktree` や `/review-response --worktree` で作成された worktree が、session 終了後も残ってゴミ化する問題に対応するためのスキル。

## 使用方法
```
/cleanup-merged                   # 一覧表示 → 承認 → 削除
/cleanup-merged --yes             # 確認スキップで一括削除
/cleanup-merged --dry-run         # 削除候補を表示するのみ
```

## 引数
- `--yes`: 削除前の確認プロンプトをスキップ
- `--dry-run`: 削除候補の一覧表示のみ、実際の削除は行わない

## 前提条件
- Git リポジトリ内で実行すること
- `gh` CLI がインストール・認証済みであること（PR 判定に使用。不通時はオフライン判定にフォールバック）

## 実行内容

### 1. 候補収集（スクリプト実行）

```bash
bash ~/.claude/skills/cleanup-merged/scripts/collect-candidates.sh
```

事前準備（デフォルトブランチ取得・`git fetch`・保護 branch 除外）、worktree/branch の収集、マージ判定、セーフティチェックを一括実行し、JSON を stdout に出力する。判定ロジックの詳細はスクリプト本体を、挙動の担保は `claude/.claude/skills/cleanup-merged/tests/test-collect-candidates.sh` を参照。

#### 出力 JSON の契約

```json
{
  "degraded": false,
  "default_branch": "main",
  "current_worktree": "/path/to/current",
  "candidates": {
    "worktrees": [{"path": "...", "branch": "...", "verdict": "...", "detail": "PR #123 MERGED"}],
    "branches": [{"branch": "...", "verdict": "...", "detail": "..."}]
  },
  "skipped": [{"type": "worktree|branch", "target": "...", "branch": "...", "reason": "...", "detail": "..."}],
  "detached": ["/path/to/detached-worktree"],
  "warnings": ["..."]
}
```

- **`candidates`**: 削除候補。`verdict` は判定根拠の種別:
  - `pr_merged`: PR が MERGED
  - `merged_no_pr`: PR なし & デフォルトブランチにマージ済み（main 直行運用のカバー）
  - `pr_closed`: PR が CLOSED 未マージ、かつ local head == PR head の照合済み（削除しても `gh pr checkout N` で完全復元できる）
  - OPEN の PR が併存する branch は判定対象外（in-flight として保持）
  - `detail` はそのまま一覧表示の「判定」欄に使える文字列
- **`skipped`**: セーフティチェックで弾かれた対象。`reason` は機械用コード、`detail` はそのまま一覧表示に使える文字列。`branch` フィールドは `type: "worktree"` のみ付与。`local_commits_beyond_pr` は「CLOSED 未マージ PR があるが PR に含まれないローカル commit を持つ」ケース
- **`detached`**: detached HEAD の worktree（branch が無く削除判定できないため別枠報告）
- **`degraded: true`**: `gh` 不通でオフライン判定のみ（PR 情報なし。`pr_closed` は PR head 照合ができないため候補に出ない）。一覧のヘッダーに「オフライン判定（PR 情報なし）」と警告を出すこと
- **`warnings`**: fetch 失敗等の注記。空でなければ一覧に併記する

スクリプトが非ゼロ終了した場合（git リポジトリ外・jq 未導入等）は stderr のメッセージを提示して停止する。

### 2. 削除候補の一覧表示

JSON の内容を以下のフォーマットで報告:

```
## 削除候補

### Worktree (N 件)
1. <path>
   branch: <branch>
   判定: <detail>

### Branch (M 件)
1. <branch> — <detail>

### スキップ (K 件)
1. <target> — <detail>
```

削除候補が 0 件の場合はその旨を報告して終了。

### 3. 確認・削除

#### `--dry-run` 指定時
ここで終了。何も削除しない。

#### `--yes` 指定なし
ユーザーに「上記の削除を実行しますか？」と確認。承認後に削除を実行。

#### 削除実行（スクリプト実行）

```bash
bash ~/.claude/skills/cleanup-merged/scripts/delete-candidates.sh < <(承認済み候補の JSON)
```

手順 1 の出力 JSON をそのまま stdin に渡す。ユーザーが一部のみ承認した場合は jq で `candidates` を間引いて渡す（**アドホックなシェルループで削除を再実装しない**。zsh の `path` 特殊変数事故の再発防止としてスクリプトに固定している）。

出力 JSON の契約:

```json
{
  "removed": {"worktrees": ["..."], "branches": ["..."]},
  "failures": [{"type": "worktree|branch", "target": "...", "error": "..."}]
}
```

- 個別の削除失敗は `failures` に記録され処理は継続する（exit 0）。失敗があれば個別に報告する
- 削除の分岐（`-d`/`-D`・`--force` 不使用・カレント worktree の拒否）はスクリプト内に集約されている。詳細はスクリプトのヘッダーコメントを参照

`WorktreeRemove` hook がプロジェクトに設定されていれば、`git worktree remove` の発火に合わせて実行される（per-worktree DB のクリーンアップ等）。

### 4. 完了報告

```
削除完了:
- Worktree: N 件
- Branch: M 件
- スキップ: K 件（理由は上記参照）
- 失敗: F 件（あれば詳細を再掲）
```

## 注意事項

1. **原則 `git branch -d`**: マージ済み判定は git 自身も行うため、未マージ branch は git が拒否する二重セーフティとして機能する。`-D` は `pr_closed`（未マージクローズ）のみ（分岐はスクリプトに集約済み）
2. **保護 branch は常に除外**: `main`, `master`, `develop`, リモートのデフォルトブランチ（スクリプトが除外済み）
3. **メイン worktree（リポジトリ root）は対象外**: 構造上削除できない（スクリプトが除外済み）
4. **PR 判定にはネットワーク必要**: `gh` が失敗する場合、スクリプトが `degraded: true` でオフライン判定（`git branch --merged` のみ）にフォールバックする。警告表示を忘れないこと
5. **未マージクローズ（`pr_closed`）の削除根拠**: local head == PR head を照合済みの branch のみ候補になる。一致していれば GitHub 側に `refs/pull/N/head` が恒久的に残るため、`gh pr checkout N` でいつでも完全復元できる（reflog より強い保証）
6. **dotfiles 特有の運用との整合**: `main` 直行運用では PR を作らないため、`merged_no_pr` 判定でカバーする
